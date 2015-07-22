#!/bin/bash

# Script to generate SVGs from dot file.
dot -Tsvg $1 -o $1.svg
