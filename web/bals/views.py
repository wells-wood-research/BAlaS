"""Views for BALS"""

import sys

from bson.json_util import dumps, loads, RELAXED_JSON_OPTIONS
from flask import jsonify, redirect, render_template, request
from flask_restful import Resource, Api

from bals import app
from bals import database


@app.route('/')
def home():
    """Returns the home page for the bals web app."""
    return render_template('index.html')


# RESTful API


API = Api(app)


class AlanineScanJobs(Resource):
    def post(self):
        scan_submission = request.json
        if app.debug:
            print("Submitting Scan Job...", file=sys.stderr)
        job_id = database.submit_scan_job(scan_submission)
        job_details = database.export_job_details(
            database.get_scan_job(job_id))
        if app.debug:
            print(f"Scan Job Submitted: {job_id}", file=sys.stderr)
        return job_details, 201


class AlanineScanJob(Resource):
    def get(self, job_id):
        if app.debug:
            print(f"Getting Scan Job {job_id}...", file=sys.stderr)
        job_details = database.export_job_details(
            database.get_scan_job(job_id))
        if app.debug:
            print(f"Got job details for job {job_id}.", file=sys.stderr)
        return job_details, 201


API.add_resource(AlanineScanJobs, '/api/v0.1/alanine-scan-jobs')
API.add_resource(AlanineScanJob, '/api/v0.1/alanine-scan-job/<string:job_id>')
