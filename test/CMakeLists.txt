add_executable(
    unit-tests
    tuple-iterator_test.cpp
    stl-algorithms_test.cpp
    )

target_link_libraries(
    unit-tests
    gtest_main
    tuple-iterator
    )

add_test(
  NAME
    unit
  COMMAND
    ${CMAKE_BINARY_DIR}/${CMAKE_INSTALL_BINDIR}/unit-tests
  )
