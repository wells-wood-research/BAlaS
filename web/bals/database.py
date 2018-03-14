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
    scan_submission['status'] = JobStatus.QUEUED.value
    job_id = ALANINE_SCAN_JOBS.insert_one(scan_submission).inserted_id
    return job_id


class JobStatus(Enum):
    """Represents the possible states of a job."""
    QUEUED = auto()
    IN_PROGRESS = auto()
    COMPLETE = auto()
    FAILED = auto()
