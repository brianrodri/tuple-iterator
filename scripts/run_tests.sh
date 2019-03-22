#!/usr/bin/env bash
rm -rf build
mkdir -p build
cmake -B build . && make -C build && ./build/bin/unit_tests
