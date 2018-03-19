"""Contains code for interacting with the database backend of BALS."""

import datetime
from enum import Enum, auto
import sys

from bson.objectid import ObjectId
import pymongo

CLIENT = pymongo.MongoClient('db', 27017)
ALANINE_SCAN_JOBS = CLIENT.bals.alanine_scan_jobs


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


def export_job_details(scan_job):
    """Creates job status details from a scan job."""
    job_details = {
        '_id': str(scan_job['_id']),
        'status': scan_job['status'],
    }
    return job_details


def export_scan_job(scan_job):
    """Converts scan job to an exportable format."""
    scan_job['_id'] = str(scan_job['_id'])
    scan_job['timeSubmitted'] = str(scan_job['timeSubmitted'])
    return scan_job


class JobStatus(Enum):
    """Represents the possible states of a job."""
    SUBMITTED = auto()
    QUEUED = auto()
    RUNNING = auto()
    COMPLETED = auto()
    FAILED = auto()
