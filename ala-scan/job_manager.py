"""Contains code for managing and processing alanine scan job requests."""

import importlib
import multiprocessing as mp
import time
import os
import sys

import database
from database import JobStatus, ALANINE_SCAN_JOBS
import alascanapp.alascan as alascan


def main():
    """Establishes the manager and listener subprocesses"""
    processes = int(os.getenv(key='SCAN_PROCS', default='1'))
    with mp.Manager() as manager:
        queue = manager.Queue()
        assigned_jobs = manager.list([None] * processes)
        listeners = [
            mp.Process(target=get_and_run_scan_job,
                       args=(queue, assigned_jobs, proc_i))
            for proc_i in range(processes)
        ]
        for listener in listeners:
            listener.start()
        while True:
            running_jobs = ALANINE_SCAN_JOBS.find(
                {'status': JobStatus.RUNNING.value})
            # This block checks that all running jobs in the db are actually
            # running.
            for job in running_jobs:
                if job['_id'] not in assigned_jobs:
                    update_job_status(job['_id'],
                                      JobStatus.FAILED)
            # This block restarts any dead listeners
            for (i, proc) in enumerate(listeners):
                if not proc.is_alive():
                    proc.terminate()
                    assigned_jobs[i] = None
                    listeners[i] = mp.Process(
                        target=get_and_run_scan_job,
                        args=(queue, assigned_jobs, i))
                    listeners[i].start()
            submitted_jobs = ALANINE_SCAN_JOBS.find(
                {'status': JobStatus.SUBMITTED.value})
            for job in sorted(submitted_jobs,
                              key=lambda x: x['timeSubmitted']):
                queue.put(job['_id'])
                update_job_status(job['_id'],
                                  JobStatus.QUEUED)
            time.sleep(10)
    return


def get_and_run_scan_job(scan_job_queue, assigned_jobs, proc_i):
    """Collects and runs alanine scan jobs from queue.

    Parameters
    ----------
    scan_job_queue : multiprocessing.Queue
        Optimisation job queue.
    assigned_jobs : list
        A list of jobs ids currently being processed by the
        listeners.
    proc_i : int
        The index of the processor in the listener list and the
        assigned_jobs list.
    """
    # The module is reloaded to establish a new connection
    # to the database for the process fork
    importlib.reload(database)
    while True:
        job_id = scan_job_queue.get()
        print(f"Got scan job {job_id}!", file=sys.stderr)
        scan_job = ALANINE_SCAN_JOBS.find_one(job_id)
        update_job_status(job_id, JobStatus.RUNNING)
        assigned_jobs[proc_i] = job_id
        print("Running scan job {}!".format(job_id), file=sys.stderr)
        ALANINE_SCAN_JOBS.update_one(
            {'_id': job_id},
            {'$set': {'status': JobStatus.COMPLETED.value,
                      'dG': FAKE_RESULTS['dG'],
                      'receptorData': FAKE_RESULTS['receptorData'],
                      'ligandData': FAKE_RESULTS['ligandData'],
                      }})
        print("Finished scan job {}!".format(job_id), file=sys.stderr)
        assigned_jobs[proc_i] = None
    return


def update_job_status(scan_job_id, status):
    """Updates status in database entry for alanine scan job."""
    ALANINE_SCAN_JOBS.update_one(
        {'_id': scan_job_id},
        {'$set': {'status': status.value}})
    return


FAKE_RESULTS = {
    "dG": -154.6038,
    "receptorData":
        [
            ["25", "GLU", "A", -3.3957, 4, 0.0],
            ["26", "THR", "A", 1.0658, 2, 0.0],
            ["27", "LEU", "A", 0.0, 3, 0.0],
            ["28", "VAL", "A", 0.1736, 2, 0.0],
            ["29", "ARG", "A", 0.0, 6, 0.0],
            ["30", "PRO", "A", 0.0, 2, 0.0],
            ["31", "LYS", "A", 0.0, 4, 0.0],
            ["32", "PRO", "A", 0.0, 2, 0.0],
            ["33", "LEU", "A", 0.0, 3, 0.0],
            ["34", "LEU", "A", 0.0215, 3, 0.0],
            ["35", "LEU", "A", 0.0, 3, 0.0],
            ["36", "LYS", "A", 0.0, 4, 0.0],
            ["37", "LEU", "A", 0.0, 3, 0.0],
            ["38", "LEU", "A", 0.0066, 3, 0.0],
            ["39", "LYS", "A", 0.0, 4, 0.0],
            ["40", "SER", "A", 0.0, 1, 0.0],
            ["41", "VAL", "A", 0.0, 2, 0.0],
            ["42", "GLY", "A", 0.0, 0, 0.0],
            ["43", "ALA", "A", 0.0, 0, 0.0],
            ["44", "GLN", "A", 0.0, 4, 0.0],
            ["45", "LYS", "A", 0.0, 4, 0.0],
            ["46", "ASP", "A", 0.0, 3, 0.0],
            ["47", "THR", "A", 0.0, 2, 0.0],
            ["48", "TYR", "A", 0.0, 7, 0.0],
            ["49", "THR", "A", 0.099, 2, 0.0],
            ["50", "MET", "A", 1.9407, 3, 0.0],
            ["51", "LYS", "A", 15.7255, 4, 0.0],
            ["52", "GLU", "A", 0.0, 4, 0.0],
            ["53", "VAL", "A", 0.1434, 2, 0.0],
            ["54", "LEU", "A", 6.9093, 3, 0.0],
            ["55", "PHE", "A", 0.5992, 6, 0.0],
            ["56", "TYR", "A", 0.0017, 7, 0.0],
            ["57", "LEU", "A", 1.9481, 3, 0.0],
            ["58", "GLY", "A", 0.0, 0, 0.0],
            ["59", "GLN", "A", 0.0555, 4, 0.0],
            ["60", "TYR", "A", 0.0812, 7, 0.0],
            ["61", "ILE", "A", 3.5197, 3, 0.0],
            ["62", "MET", "A", 1.0486, 3, 0.0],
            ["63", "THR", "A", 0.0, 2, 0.0],
            ["64", "LYS", "A", 0.0, 4, 0.0],
            ["65", "ARG", "A", 0.055, 6, 0.0],
            ["66", "LEU", "A", 0.1652, 3, 0.0],
            ["67", "TYR", "A", 5.0938, 7, 0.0],
            ["68", "ASP", "A", 0.0, 3, 0.0],
            ["69", "GLU", "A", 0.0, 4, 0.0],
            ["70", "LYS", "A", 0.0, 4, 0.0],
            ["71", "GLN", "A", 0.1976, 4, 0.0],
            ["72", "GLN", "A", 2.3428, 4, 0.0],
            ["73", "HIS", "A", 1.5448, 5, 0.0],
            ["74", "ILE", "A", 0.0607, 3, 0.0],
            ["75", "VAL", "A", 1.6566, 2, 0.0],
            ["76", "TYR", "A", 0.0, 7, 0.0],
            ["77", "CYS", "A", 0.0, 1, 0.0],
            ["78", "SER", "A", 0.0, 1, 0.0],
            ["79", "ASN", "A", 0.0, 3, 0.0],
            ["80", "ASP", "A", 0.0, 3, 0.0],
            ["81", "LEU", "A", 0.0, 3, 0.0],
            ["82", "LEU", "A", 0.5274, 3, 0.0],
            ["83", "GLY", "A", 0.0, 0, 0.0],
            ["84", "ASP", "A", 0.0, 3, 0.0],
            ["85", "LEU", "A", 0.0078, 3, 0.0],
            ["86", "PHE", "A", 0.8994, 6, 0.0],
            ["87", "GLY", "A", 0.0, 0, 0.0],
            ["88", "VAL", "A", 0.0, 2, 0.0],
            ["89", "PRO", "A", 0.0, 2, 0.0],
            ["90", "SER", "A", 0.0, 1, 0.0],
            ["91", "PHE", "A", 2.4162, 6, 0.0],
            ["92", "SER", "A", 0.0, 1, 0.0],
            ["93", "VAL", "A", 5.3015, 2, 0.0],
            ["94", "LYS", "A", 19.6974, 4, 0.0],
            ["95", "GLU", "A", 0.0124, 4, 0.0],
            ["96", "HIS", "A", 1.3488, 5, 0.0],
            ["97", "ARG", "A", 0.1101, 6, 0.0],
            ["98", "LYS", "A", 0.0, 4, 0.0],
            ["99", "ILE", "A", 3.7571, 3, 0.0],
            ["100", "TYR", "A", 6.2154, 7, 0.0],
            ["101", "THR", "A", 0.0, 2, 0.0],
            ["102", "MET", "A", 0.0507, 3, 0.0],
            ["103", "ILE", "A", 1.7513, 3, 0.0],
            ["104", "TYR", "A", 0.078, 7, 0.0],
            ["105", "ARG", "A", 0.0, 6, 0.0],
            ["106", "ASN", "A", 0.0, 3, 0.0],
            ["107", "LEU", "A", 0.0435, 3, 0.0],
            ["108", "VAL", "A", 0.0, 2, 0.0],
            ["109", "VAL", "A", 0.0, 2, 0.0]
        ],
    "ligandData":
        [
            [ "17", "GLU", "B", 19.1207, 4, 0.0 ],
            [ "18", "THR", "B", 0.5949, 2, 0.0 ],
            [ "19", "PHE", "B", 20.3646, 6, 0.0 ],
            [ "20", "SER", "B", 0.0, 1, 0.0 ],
            [ "21", "ASP", "B", 0.0506, 3, 0.0 ],
            [ "22", "LEU", "B", 6.7914, 3, 0.0 ],
            [ "23", "TRP", "B", 22.0543, 9, 0.0 ],
            [ "24", "LYS", "B", 0.5078, 4, 0.0 ],
            [ "25", "LEU", "B", 0.4186, 3, 0.0 ],
            [ "26", "LEU", "B", 8.3254, 3, 0.0 ],
            [ "27", "PRO", "B", 4.1877, 2, 0.0 ],
            [ "28", "GLU", "B", 15.6824, 4, 0.0 ],
            [ "29", "ASN", "B", 0.5048, 3, 0.0 ]
        ]
}

if __name__ == '__main__':
    main()
