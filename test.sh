#!/usr/bin/env bash

set -e

ghc test/Test.hs src/GaussianPDF.hs -o test/Test
test/Test
