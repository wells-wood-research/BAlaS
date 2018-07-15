"""Contains code for managing and processing alanine scan job requests."""

import contextlib
import glob
import importlib
import json
import multiprocessing as mp
import os
import shutil
import subprocess
import sys
import time
import tempfile

import isambard

import database
from database import (JobStatus, ALANINE_SCAN_JOBS, AUTO_JOBS, MANUAL_JOBS,
                      RESIDUES_JOBS)


@contextlib.contextmanager
def cd(newdir, cleanup=lambda: True):
    """Changes directory to a temporary folder and cleans up on exit."""
    prevdir = os.getcwd()
    os.chdir(os.path.expanduser(newdir))
    try:
        yield
    finally:
        os.chdir(prevdir)
        cleanup()


@contextlib.contextmanager
def tempdir():
    """Creates a temporary directory context for running analysis."""
    dirpath = tempfile.mkdtemp()

    def cleanup():
        shutil.rmtree(dirpath)
    with cd(dirpath, cleanup):
        yield dirpath


def main():
    """Establishes the manager and worker subprocesses."""
    scan_processes = int(os.getenv(key='SCAN_PROCS', default='1'))
    auto_processes = int(os.getenv(key='AUTO_PROCS', default='1'))
    manual_processes = int(os.getenv(key='MANUAL_PROCS', default='1'))
    residues_processes = int(os.getenv(key='RESIDUES_PROCS', default='1'))
    with mp.Manager() as manager:
        scan_queue, scan_assigned, scan_workers = make_queue_components(
            get_and_run_scan_job, scan_processes, manager)
        auto_queue, auto_assigned, auto_workers = make_queue_components(
            get_and_run_auto_job, auto_processes, manager)
        manual_queue, manual_assigned, manual_workers = make_queue_components(
            get_and_run_manual_job, manual_processes, manager)
        residues_queue, residues_assigned, residues_workers = make_queue_components(
            get_and_run_residues_job, residues_processes, manager)
        while True:
            check_for_lost_jobs(scan_assigned, ALANINE_SCAN_JOBS)
            check_for_lost_jobs(auto_assigned, AUTO_JOBS)
            check_for_lost_jobs(manual_assigned, MANUAL_JOBS)
            check_for_lost_jobs(residues_assigned, RESIDUES_JOBS)
            check_for_dead_jobs(get_and_run_scan_job,
                                scan_assigned, scan_queue, scan_workers)
            check_for_dead_jobs(get_and_run_auto_job,
                                auto_assigned, auto_queue, auto_workers)
            check_for_dead_jobs(get_and_run_manual_job,
                                manual_assigned, manual_queue, manual_workers)
            check_for_dead_jobs(get_and_run_residues_job,
                                residues_assigned, residues_queue, residues_workers)
            populate_queue(scan_queue, ALANINE_SCAN_JOBS)
            populate_queue(auto_queue, AUTO_JOBS)
            populate_queue(manual_queue, MANUAL_JOBS)
            populate_queue(residues_queue, RESIDUES_JOBS)
            time.sleep(2)
    return


def make_queue_components(target_fn, processes, manager):
    """Creates the various objects required to create the job queue."""
    queue = manager.Queue()
    assigned_jobs = manager.list([None] * processes)
    workers = [
        mp.Process(target=target_fn,
                   args=(queue, assigned_jobs, proc_i))
        for proc_i in range(processes)
    ]
    for worker in workers:
        worker.start()
    return queue, assigned_jobs, workers


def check_for_lost_jobs(assigned_jobs, collection):
    """Checks for jobs that are assigned and have failed."""
    running_jobs = collection.find(
        {'status': JobStatus.RUNNING.value})
    for job in running_jobs:
        if job['_id'] not in assigned_jobs:
            update_job_status(
                job['_id'], JobStatus.FAILED, collection)
    return


def check_for_dead_jobs(target_fn, assigned_jobs, queue, workers):
    """Checks status of workers and restarts any that are dead."""
    for (i, proc) in enumerate(workers):
        if not proc.is_alive():
            proc.terminate()
            assigned_jobs[i] = None
            workers[i] = mp.Process(
                target=target_fn,
                args=(queue, assigned_jobs, i))
            workers[i].start()
    return


def populate_queue(queue, collection):
    """Adds jobs from the database to the queue shared by workers."""
    submitted_jobs = collection.find(
        {'status': JobStatus.SUBMITTED.value})
    for job in sorted(submitted_jobs,
                      key=lambda x: x['timeSubmitted']):
        queue.put(job['_id'])
        update_job_status(job['_id'], JobStatus.QUEUED, collection)
    return


def get_and_run_scan_job(scan_job_queue, assigned_jobs, proc_i):
    """Collects and runs alanine scan jobs from queue.

    Parameters
    ----------
    scan_job_queue : multiprocessing.Queue
        Scan job queue.
    assigned_jobs : list
        A list of jobs ids currently being processed by the
        workers.
    proc_i : int
        The index of the processor in the worker list and the
        assigned_jobs list.
    """
    # The module is reloaded to establish a new connection
    # to the database for the process fork
    importlib.reload(database)
    while True:
        job_id = scan_job_queue.get()
        print(f"Got scan job {job_id}!", file=sys.stderr)
        scan_job = ALANINE_SCAN_JOBS.find_one(job_id)
        update_job_status(job_id, JobStatus.RUNNING, ALANINE_SCAN_JOBS)
        assigned_jobs[proc_i] = job_id
        print("Running scan job {}!".format(job_id), file=sys.stderr)
        with tempdir() as dirpath:
            results = run_bals_scan(
                job_id, scan_job['pdbFile'],
                scan_job['receptor'], scan_job['ligand'])
            ALANINE_SCAN_JOBS.update_one(
                {'_id': job_id},
                {'$set': results})
        print("Finished scan job {}!".format(job_id), file=sys.stderr)
        assigned_jobs[proc_i] = None
    return


def run_bals_scan(job_id, pdb_string, receptor_chains, ligand_chains):
    """Runs a BALS job in `scan` mode."""
    pdb_filename = f'{job_id}.pdb'
    with open(pdb_filename, 'w') as outf:
        outf.write(pdb_string)
    scan_cmd = [
        '/root/bin/ALAscanApp.py', 'scan',
        '-p', pdb_filename,
        '-r'] + receptor_chains + [
        '-l'] + ligand_chains + [
        '-t']  # Suppresses the plots from being displayed.
    scan_process = subprocess.run(
        scan_cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    try:
        scan_process.check_returncode()
        rec_json_paths = glob.glob('replot/*Rec_scan*.json')
        lig_json_paths = glob.glob('replot/*Lig_scan*.json')
        assert len(rec_json_paths) == 1
        assert len(lig_json_paths) == 1
        with open(rec_json_paths[0], 'r') as inf:
            rec_results = json.load(inf)
        with open(lig_json_paths[0], 'r') as inf:
            lig_results = json.load(inf)
        processed_output = parser_friendly_output(rec_results, lig_results)
    except subprocess.CalledProcessError:
        processed_output = {'status': JobStatus.FAILED.value}
    processed_output['status'] = JobStatus.COMPLETED.value
    processed_output['std_out'] = scan_process.stdout.decode()
    return processed_output


def parser_friendly_output(bals_rec_output, bals_lig_output):
    """Formats the BALS output to make it better structured for Elm."""
    rec_output = bals_rec_output['ala_scan'][0]
    lig_output = bals_lig_output['ala_scan'][0]
    rec_dg = rec_output.pop('dG')
    lig_dg = lig_output.pop('dG')
    assert rec_dg == lig_dg
    pfo = {
        'dG': rec_dg,
        'receptorData': [
            data for (_, data) in sorted(
                rec_output.items(), key=lambda x: int(x[0]))],
        'ligandData': [
            data for (_, data) in sorted(
                lig_output.items(), key=lambda x: int(x[0]))]
    }
    return pfo


def get_and_run_auto_job(auto_job_queue, assigned_jobs, proc_i):
    """Collects and runs auto constellation jobs from the queue.

    Parameters
    ----------
    auto_job_queue : multiprocessing.Queue
        Auto job queue.
    assigned_jobs : list
        A list of jobs ids currently being processed by the
        workers.
    proc_i : int
        The index of the processor in the worker list and the
        assigned_jobs list.
    """
    # The module is reloaded to establish a new connection
    # to the database for the process fork
    importlib.reload(database)
    while True:
        job_id = auto_job_queue.get()
        print(f"Got auto job {job_id}!", file=sys.stderr)
        auto_job = AUTO_JOBS.find_one(job_id)
        update_job_status(job_id, JobStatus.RUNNING, AUTO_JOBS)
        assigned_jobs[proc_i] = job_id
        print("Running auto job {}!".format(job_id), file=sys.stderr)
        with tempdir() as dirpath:
            results = run_bals_auto(
                job_id, auto_job['scanName'], auto_job['pdbFile'],
                auto_job['receptor'], auto_job['ligand'],
                auto_job['ddGCutOff'], auto_job['constellationSize'],
                auto_job['cutOffDistance'])
            AUTO_JOBS.update_one(
                {'_id': job_id},
                {'$set': results})
        print("Finished auto job {}!".format(job_id), file=sys.stderr)
        assigned_jobs[proc_i] = None
    return


def run_bals_auto(job_id, scanName, pdb_string, receptor_chains, ligand_chains,
                  ddg_cutoff, constellation_size, distance_cutoff):
    """Runs a BALS job in `auto` mode."""
    pdb_filename = f'{job_id}.pdb'
    with open(pdb_filename, 'w') as outf:
        outf.write(pdb_string)
    scan_cmd = [
        '/root/bin/ALAscanApp.py', 'auto',
        '-p', pdb_filename,
        '-r'] + receptor_chains + [
        '-l'] + ligand_chains + [
        '-d', str(ddg_cutoff),
        '-z', str(constellation_size),
        '-u', str(distance_cutoff),
        '-t']  # Suppresses the plots from being displayed.
    scan_process = subprocess.run(
        scan_cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    try:
        scan_process.check_returncode()
        rec_json_paths = glob.glob('replot/*Rec_auto*.json')
        lig_json_paths = glob.glob('replot/*Lig_auto*.json')
        assert len(rec_json_paths) == 1
        assert len(lig_json_paths) == 1
        with open(rec_json_paths[0], 'r') as inf:
            rec_results = json.load(inf)
        with open(lig_json_paths[0], 'r') as inf:
            lig_results = json.load(inf)
        scan_results = parser_friendly_output(rec_results, lig_results)
        scan_results['name'] = scanName
        scan_results['pdbFile'] = pdb_string
        scan_results['receptor'] = receptor_chains
        scan_results['ligand'] = ligand_chains
        results = {
            'scanResults': scan_results,
            # Currently the multistate hot constellations only returns the mean
            'hotConstellations': [(k, v[0]) for k, v in
                                  lig_results['mutants'].items()]
        }
    except subprocess.CalledProcessError:
        results = {'status': JobStatus.FAILED.value}
    except AttributeError:
        results = {'status': JobStatus.FAILED.value}
    results['status'] = JobStatus.COMPLETED.value
    results['std_out'] = scan_process.stdout.decode()
    return results


def get_and_run_manual_job(manual_job_queue, assigned_jobs, proc_i):
    """Collects and runs manual constellation jobs from the queue.

    Parameters
    ----------
    manual_job_queue : multiprocessing.Queue
        Manual job queue.
    assigned_jobs : list
        A list of jobs ids currently being processed by the
        workers.
    proc_i : int
        The index of the processor in the worker list and the
        assigned_jobs list.
    """
    # The module is reloaded to establish a new connection
    # to the database for the process fork
    importlib.reload(database)
    while True:
        job_id = manual_job_queue.get()
        print(f"Got manual job {job_id}!", file=sys.stderr)
        manual_job = MANUAL_JOBS.find_one(job_id)
        update_job_status(job_id, JobStatus.RUNNING, MANUAL_JOBS)
        assigned_jobs[proc_i] = job_id
        print("Running manual job {}!".format(job_id), file=sys.stderr)
        with tempdir() as dirpath:
            results = run_bals_manual(
                job_id, manual_job['scanName'], manual_job['pdbFile'],
                manual_job['receptor'], manual_job['ligand'],
                manual_job['residues'])
            MANUAL_JOBS.update_one(
                {'_id': job_id},
                {'$set': results})
        print("Finished manual job {}!".format(job_id), file=sys.stderr)
        assigned_jobs[proc_i] = None
    return


def run_bals_manual(job_id, scanName, pdb_string, receptor_chains, ligand_chains,
                    residues):
    """Runs a BALS job in `manual` mode."""
    pdb_filename = f'{job_id}.pdb'
    with open(pdb_filename, 'w') as outf:
        outf.write(pdb_string)
    scan_cmd = [
        '/root/bin/ALAscanApp.py', 'manual',
        '-p', pdb_filename,
        '-r'] + receptor_chains + [
        '-l'] + ligand_chains + [
        '-c', ','.join(residues), '-t']  # Suppresses the plots from being displayed.
    scan_process = subprocess.run(
        scan_cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    try:
        scan_process.check_returncode()
        rec_json_paths = glob.glob('replot/*Rec_manual*.json')
        lig_json_paths = glob.glob('replot/*Lig_manual*.json')
        assert len(rec_json_paths) == 1
        assert len(lig_json_paths) == 1
        with open(rec_json_paths[0], 'r') as inf:
            rec_results = json.load(inf)
        with open(lig_json_paths[0], 'r') as inf:
            lig_results = json.load(inf)
        scan_results = parser_friendly_output(rec_results, lig_results)
        scan_results['name'] = scanName
        scan_results['pdbFile'] = pdb_string
        scan_results['receptor'] = receptor_chains
        scan_results['ligand'] = ligand_chains
        results = {
            'scanResults': scan_results,
            # Currently the multistate hot constellations only returns the mean
            'hotConstellations': [(k, v[0]) for k, v in
                                  lig_results['mutants'].items()]
        }
    except subprocess.CalledProcessError:
        results = {'status': JobStatus.FAILED.value}
    except AttributeError:
        results = {'status': JobStatus.FAILED.value}
    results['status'] = JobStatus.COMPLETED.value
    results['std_out'] = scan_process.stdout.decode()
    return results


def get_and_run_residues_job(residues_job_queue, assigned_jobs, proc_i):
    """Collects and runs residues constellation jobs from the queue.

    Parameters
    ----------
    residues_job_queue : multiprocessing.Queue
        Residues job queue.
    assigned_jobs : list
        A list of jobs ids currently being processed by the
        workers.
    proc_i : int
        The index of the processor in the worker list and the
        assigned_jobs list.
    """
    # The module is reloaded to establish a new connection
    # to the database for the process fork
    importlib.reload(database)
    while True:
        job_id = residues_job_queue.get()
        print(f"Got residues job {job_id}!", file=sys.stderr)
        residues_job = RESIDUES_JOBS.find_one(job_id)
        update_job_status(job_id, JobStatus.RUNNING, RESIDUES_JOBS)
        assigned_jobs[proc_i] = job_id
        print("Running residues job {}!".format(job_id), file=sys.stderr)
        with tempdir() as dirpath:
            results = run_bals_residues(
                job_id, residues_job['scanName'], residues_job['pdbFile'],
                residues_job['receptor'], residues_job['ligand'],
                residues_job['constellationSize'], residues_job['residues'])
            RESIDUES_JOBS.update_one(
                {'_id': job_id},
                {'$set': results})
        print("Finished residues job {}!".format(job_id), file=sys.stderr)
        assigned_jobs[proc_i] = None
    return


def run_bals_residues(job_id, scanName, pdb_string, receptor_chains, ligand_chains,
                      constellationSize, residues):
    """Runs a BALS job in `residues` mode."""
    pdb_filename = f'{job_id}.pdb'
    with open(pdb_filename, 'w') as outf:
        outf.write(pdb_string)
    scan_cmd = [
        '/root/bin/ALAscanApp.py', 'residues',
        '-p', pdb_filename,
        '-r'] + receptor_chains + [
        '-l'] + ligand_chains + [
        '-e'] + residues + [
        '-z', str(constellationSize),
        '-t']  # Suppresses the plots from being displayed.
    scan_process = subprocess.run(
        scan_cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    try:
        scan_process.check_returncode()
        rec_json_paths = glob.glob('replot/*Rec_residues*.json')
        lig_json_paths = glob.glob('replot/*Lig_residues*.json')
        assert len(rec_json_paths) == 1
        assert len(lig_json_paths) == 1
        with open(rec_json_paths[0], 'r') as inf:
            rec_results = json.load(inf)
        with open(lig_json_paths[0], 'r') as inf:
            lig_results = json.load(inf)
        scan_results = parser_friendly_output(rec_results, lig_results)
        scan_results['name'] = scanName
        scan_results['pdbFile'] = pdb_string
        scan_results['receptor'] = receptor_chains
        scan_results['ligand'] = ligand_chains
        results = {
            'scanResults': scan_results,
            # Currently the multistate hot constellations only returns the mean
            'hotConstellations': [(k, v[0]) for k, v in
                                  lig_results['mutants'].items()]
        }
    except subprocess.CalledProcessError:
        results = {'status': JobStatus.FAILED.value}
    except AttributeError:
        results = {'status': JobStatus.FAILED.value}
    results['status'] = JobStatus.COMPLETED.value
    results['std_out'] = scan_process.stdout.decode()
    return results


def update_job_status(scan_job_id, status, collection):
    """Updates status in database entry for alanine scan job."""
    collection.update_one(
        {'_id': scan_job_id},
        {'$set': {'status': status.value}})
    return


if __name__ == '__main__':
    main()
