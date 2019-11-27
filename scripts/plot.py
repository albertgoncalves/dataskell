#!/usr/bin/env python3

from sys import argv

from matplotlib.pyplot import close, savefig, subplots, tight_layout
from pandas import read_csv

from path import path


def main():
    try:
        input_ = read_csv(path("data", "input.csv"))
        output = read_csv(
            path("data", "output.csv"),
            header=None,
            names=["z", "x", "y"],
        )
        _, ax = subplots(figsize=(5, 5))
        kwargs = {"cmap": "bwr", "vmin": 0, "vmax": 1}
        ax.tricontourf(output.x, output.y, output.z, alpha=0.75, **kwargs)
        ax.scatter(input_.x, input_.y, c=input_.z, edgecolor="w", **kwargs)
        ax.set_aspect("equal")
        savefig(path("out", "plot.png"))
        tight_layout()
        close()
    except:
        exit("{} unable to plot data".format(argv[0]))


if __name__ == "__main__":
    main()
