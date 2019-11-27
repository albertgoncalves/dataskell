#!/usr/bin/env python3

from sys import argv

from pandas import DataFrame
from sklearn.datasets import make_blobs, make_circles, make_moons

from path import path


def extract(xyz):
    return {"z": xyz[1], "x": xyz[0].T[0], "y": xyz[0].T[1]}


def main():
    dataset = {
        "blobs": lambda: make_blobs(n_features=2, centers=2, cluster_std=4),
        "circles": lambda: make_circles(noise=0.2, factor=0.5),
        "moons": lambda: make_moons(noise=0.3),
    }
    try:
        DataFrame(extract(dataset[argv[1]]())) \
            .to_csv(path("data", "input.csv"), index=False)
    except:
        args = (argv[0], "|".join(dataset.keys()))
        exit("{} unknown argument, valid arguments: {}".format(*args))


if __name__ == "__main__":
    main()
