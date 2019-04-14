#!/usr/bin/env python3

from os import environ


def path(dir_, handle):
    return "{}/{}/{}".format(environ["WD"], dir_, handle)


def pipe(x, *fs):
    for f in fs:
        x = f(x)
    return x
