"""Views for BALS

Notes
-----
As BALS has an SPA architecture, almost all of the business logic for the view
is contained in the Elm application, with this file only really serving the base
html and providing the RESTful API backend.
"""

import sys

import flask
from flask import render_template, request
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
    """RESTful API endpoint for posting scan jobs and getting aggregate data."""

    def post(self):
        """Creates a new alanine scan job on the server.

        Returns
        -------
        job_details : Dict
            Dict containing the ID of the job, time submitted and the
            current job status.
        """
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
    """RESTful API endpoint for information on specific scan jobs."""

    def get(self, job_id):
        """Returns the status or results of an alanine scan job.

        Notes
        -----
        A query string in the URI is used to determine if the status or results
        should be returned.
        """
        if "get-status" in request.args:
            if app.debug:
                print(f"Getting Scan Job {job_id}...", file=sys.stderr)
            job_details = database.export_job_details(
                database.get_scan_job(job_id))
            if job_details is None:
                flask.abort(404)
            if app.debug:
                print(f"Got job details for job {job_id}.", file=sys.stderr)
            return job_details, 200
        elif "get-results" in request.args:
            if app.debug:
                print(f"Getting Scan Job results {job_id}...", file=sys.stderr)
            job = database.export_job(database.get_scan_job(job_id))
            if job is None:
                flask.abort(404)
            elif job['status'] != database.JobStatus.COMPLETED.value:
                flask.abort(404)
            if app.debug:
                print(f"Got job results for job {job_id}.", file=sys.stderr)
            return job, 200
        return "No arguments supplied.", 400


class AutoConstellationJobs(Resource):
    """RESTful API endpoint for posting auto jobs and getting aggregate data."""

    def post(self):
        """Creates a new auto job on the server.

        Returns
        -------
        job_details : Dict
            Dict containing the ID of the job, time submitted and the
            current job status.
        """
        auto_submission = request.json
        if app.debug:
            print("Submitting auto constellation scan job...", file=sys.stderr)
        job_id = database.submit_auto_job(auto_submission)
        job_details = database.export_job_details(
            database.get_auto_job(job_id))
        if app.debug:
            print(
                f"Auto constellation job submitted: {job_id}", file=sys.stderr)
        return job_details, 201


class AutoConstellationJob(Resource):
    """RESTful API endpoint for information on specific auto jobs."""

    def get(self, job_id):
        """Returns the status or results of an auto job.

        Notes
        -----
        A query string in the URI is used to determine if the status or results
        should be returned.
        """
        if "get-status" in request.args:
            if app.debug:
                print(f"Getting auto constellation job status{job_id}...",
                      file=sys.stderr)
            job_details = database.export_job_details(
                database.get_auto_job(job_id))
            if job_details is None:
                flask.abort(404)
            if app.debug:
                print(f"Got job details for job {job_id}.", file=sys.stderr)
            return job_details, 200
        elif "get-results" in request.args:
            if app.debug:
                print(f"Getting auto constellation job results {job_id}...",
                      file=sys.stderr)
            job = database.export_job(database.get_auto_job(job_id))
            if job is None:
                flask.abort(404)
            elif job['status'] != database.JobStatus.COMPLETED.value:
                flask.abort(404)
            if app.debug:
                print(f"Got job results for job {job_id}.", file=sys.stderr)
            return job, 200
        return "No arguments supplied.", 400


API.add_resource(AlanineScanJobs, '/api/v0.1/alanine-scan-jobs')
API.add_resource(AlanineScanJob, '/api/v0.1/alanine-scan-job/<string:job_id>')
API.add_resource(AutoConstellationJobs, '/api/v0.1/auto-jobs')
API.add_resource(AutoConstellationJob, '/api/v0.1/auto-job/<string:job_id>')
