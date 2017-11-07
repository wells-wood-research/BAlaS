"""Views for BALS"""

from flask import jsonify, redirect, render_template, request

from bals import app


@app.route('/')
def home():
    """Returns the home page for the bals web app."""
    return render_template('index.html')
