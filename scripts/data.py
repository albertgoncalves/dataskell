#!/usr/bin/env python3

from sys import argv

from pandas import DataFrame
from sklearn.datasets import make_blobs, make_circles, make_moons

from utils import path

ARGS = \
    { "blobs": make_blobs(n_features=2, centers=2, cluster_std=4)
    , "circles": make_circles(noise=0.2, factor=0.5)
    , "moons": make_moons(noise=0.3)
    }


def pipe(x, *fs):
    for f in fs:
        x = f(x)
    return x


def extract(xyz):
   return {"Bool": xyz[1], "x": xyz[0].T[0], "y": xyz[0].T[1]}


def export(filename):
    def f(df):
        df.to_csv(filename, index=False)
    return f


def main():
    try:
        pipe( ARGS[argv[1]]
            , extract
            , DataFrame
            , export(path("data", "input.csv"))
            )
    except:
        pass


if __name__ == "__main__":
    main()
