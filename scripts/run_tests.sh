#!/usr/bin/env bash
mkdir -p build
cmake -B build . && make -C build && ./build/bin/unit_tests
