"""Views for BALS"""

import sys

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
        scan_submission_id = database.submit_scan_job(scan_submission)
        if app.debug:
            print(f"Scan Job Submitted: {scan_submission_id}", file=sys.stderr)
        return str(scan_submission_id), 201


API.add_resource(AlanineScanJobs, '/api/v0.1/alanine-scan-jobs')
