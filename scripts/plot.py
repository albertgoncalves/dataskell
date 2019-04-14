#!/usr/bin/env python3

from matplotlib.pyplot import close, savefig, subplots, tight_layout
from pandas import read_csv

from utils import path


def render_plot(input_, output):
    _, ax = subplots()
    ax.scatter(output[1], output[2], c=output[0], marker="s", cmap="coolwarm")
    ax.scatter(input_.x, input_.y, c=input_.Bool, cmap="bwr", edgecolor="w")


def export_plot():
    savefig(path("pngs", "plot.png"))
    tight_layout()
    close()


def main():
    input_ = read_csv(path("data", "input.csv"))
    output = read_csv(path("data", "output.csv"), header=None)
    render_plot(input_, output)
    export_plot()


if __name__ == "__main__":
    main()
