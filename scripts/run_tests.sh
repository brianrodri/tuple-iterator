#!/usr/bin/env bash
mkdir -p build
cd build
cmake ..
make
./bin/unit_tests
cd ..
