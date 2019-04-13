#!/usr/bin/env python3

from os import environ

from pandas import DataFrame
from sklearn.datasets import make_circles


def pipe(x, *fs):
    for f in fs:
        x = f(x)
    return x


def extract(xyz):
   return {"Bool": xyz[1], "x": xyz[0].T[0], "y": xyz[0].T[1]}


def export(handle):
    def f(df):
        filename = "{}/data/{}.csv".format(environ["WD"], handle)
        df.to_csv(filename, index=False)
    return f


def main():
    pipe( make_circles()
        , extract
        , DataFrame
        , export("circles")
        )


if __name__ == "__main__":
    main()
