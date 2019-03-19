add_library(tuple_iterator "")

target_sources(
    tuple_iterator
    PRIVATE
        tuple_iterator.cpp
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}/tuple_iterator.h
    )

target_include_directories(
    tuple_iterator
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
    )
