"""The main entry point for the BALS web app.

This is used by uWSGI to start instances of the application.
"""
import bals
import config

app = bals.app
app.config.from_object(config.get_config())
