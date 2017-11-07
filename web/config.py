"""This module contains configuration options for Flask."""

import os


def get_config():
    """Used to create the config object for the Flask app.

    Notes
    -----
    This function looks for the `BALS_CONFIG` environmental
    variable and sets the config based its value. For example,
    if the `BALS_CONFIG = production`, then `ProductionConfig`
    will be used. If the environmental variable is not set
    then `DevelopmentConfig` will be used. This variable is
    usually set in `docker-compose.yml`.

    Returns
    -------
    config
        Object containing BALS config options.
    """
    config_options = {
        "development": DevelopmentConfig(),
        "production": ProductionConfig(),
        "default": DevelopmentConfig()
    }
    config_name = os.getenv(key='BALS_CONFIG', default='default')
    return config_options[config_name]


class BaseConfig:
    """Contains shared configuration options."""
    pass


class DevelopmentConfig(BaseConfig):
    """Contains development configuration options."""
    DEBUG = True


class ProductionConfig(BaseConfig):
    """Contains production configuration options."""
    DEBUG = False
