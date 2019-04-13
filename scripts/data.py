#!/usr/bin/env python3

from pandas import concat, DataFrame
from sklearn.datasets import make_circles


def main():
    xyz = make_circles()
    print(DataFrame({"Bool": xyz[1], "x": xyz[0].T[0], "y": xyz[0].T[1]}))


if __name__ == "__main__":
    main()
