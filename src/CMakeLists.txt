add_library(tuple-iterator "")

target_sources(
    tuple-iterator
    PRIVATE
        tuple-iterator.cpp
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}/tuple-iterator.h
    )

target_include_directories(
    tuple-iterator
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
    )
