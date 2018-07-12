"""Contains code for interacting with the database backend of BALS."""

import datetime
from enum import Enum, auto

from bson.objectid import ObjectId
import pymongo

CLIENT = pymongo.MongoClient('db', 27017)
ALANINE_SCAN_JOBS = CLIENT.bals.alanine_scan_jobs
AUTO_JOBS = CLIENT.bals.auto_contellation_jobs
MANUAL_JOBS = CLIENT.bals.manual_contellation_jobs


def submit_scan_job(scan_submission):
    """Submits an alanine scan job to the queue."""
    scan_submission['status'] = JobStatus.SUBMITTED.value
    scan_submission['timeSubmitted'] = datetime.datetime.now()
    job_id = ALANINE_SCAN_JOBS.insert_one(scan_submission).inserted_id
    return job_id


def get_scan_job(job_id):
    """Gets an alanine scanning job from the database."""
    scan_job = ALANINE_SCAN_JOBS.find_one({'_id': ObjectId(job_id)})
    return scan_job


def submit_auto_job(auto_submission):
    """Submits an auto constellation scan job to the queue."""
    auto_submission['status'] = JobStatus.SUBMITTED.value
    auto_submission['timeSubmitted'] = datetime.datetime.now()
    job_id = AUTO_JOBS.insert_one(auto_submission).inserted_id
    return job_id


def get_auto_job(job_id):
    """Gets an auto constellation scan job from the database."""
    auto_job = AUTO_JOBS.find_one({'_id': ObjectId(job_id)})
    return auto_job


def submit_manual_job(manual_submission):
    """Submits an manual constellation scan job to the queue."""
    manual_submission['status'] = JobStatus.SUBMITTED.value
    manual_submission['timeSubmitted'] = datetime.datetime.now()
    job_id = MANUAL_JOBS.insert_one(manual_submission).inserted_id
    return job_id


def get_manual_job(job_id):
    """Gets an manual constellation scan job from the database."""
    manual_job = MANUAL_JOBS.find_one({'_id': ObjectId(job_id)})
    return manual_job


def export_job(job):
    """Converts job to an exportable format."""
    job['_id'] = str(job['_id'])
    job['timeSubmitted'] = str(job['timeSubmitted'])
    return job


def export_job_details(job):
    """Creates job status details from a job."""
    job_details = {
        '_id': str(job['_id']),
        'name': job['name'],
        'status': job['status'],
    }
    if 'std_out' in job:
        job_details['std_out'] = job['std_out']
    return job_details


class JobStatus(Enum):
    """Represents the possible states of a job."""
    SUBMITTED = auto()
    QUEUED = auto()
    RUNNING = auto()
    COMPLETED = auto()
    FAILED = auto()
