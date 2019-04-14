#!/usr/bin/env python3

from sys import argv

from matplotlib.pyplot import close, savefig, subplots, tight_layout
from pandas import read_csv

from utils import path


def render_plot(input_, output):
    _, ax = subplots()
    ax.tricontourf(output[1], output[2], output[0], cmap="bwr", alpha=0.75)
    ax.scatter(input_.x, input_.y, c=input_.Bool, cmap="bwr", edgecolor="w")


def export_plot(path):
    savefig(path)
    tight_layout()
    close()


def main():
    try:
        input_ = read_csv(path("data", "input.csv"))
        output = read_csv(path("data", "output.csv"), header=None)
        render_plot(input_, output)
        export_plot(path("pngs", "plot.png"))
    except:
        exit("{} unable to plot data".format(argv[0]))


if __name__ == "__main__":
    main()
