#!/usr/bin/env bash
if [ $1 ] && [ $1 = '--clean' ]; then rm -rf build; fi
mkdir -p build
cmake -B build . && make -C build && ./build/bin/unit_tests
