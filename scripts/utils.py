#!/usr/bin/env python3

from os import environ


def path(dir_, handle):
    return "{}/{}/{}".format(environ["WD"], dir_, handle)
