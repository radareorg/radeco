#!/bin/bash

# Script to generate PNGs from dot file.
dot -Tpng $1 -o $1.png
