#include <algorithm>
#include <functional>
#include <iterator>
#include <vector>
#include "gtest/gtest.h"
#include "tuple-iterator.h"

class TupleIteratorTest : public ::testing::Test {
  protected:
    using Tuple = std::tuple<int, std::vector<double>, std::string>;
    using TupleIterator = tuple_ext::TupleIterator<Tuple>;
    using TupleRange = tuple_ext::TupleRange<Tuple>;

    Tuple tuple_{1, {1.61803, 2.71828, 3.14159}, "inf"};
    TupleRange tuple_range_{tuple_};
};

TEST_F(TupleIteratorTest, SizeOfIteratorIsTwoPointers) {
    const TupleIterator i;
    EXPECT_EQ(sizeof(i), 2 * sizeof(void*));
}

TEST_F(TupleIteratorTest, CopyConstructibleConceptSatisfied) {
    const TupleIterator i = tuple_range_.begin();

    // { T a = b; } -> { a == b; }
    TupleIterator i_copy = i;
    ASSERT_EQ(i_copy, i);

    // { T(a) == a; }
    EXPECT_EQ(TupleIterator{i}, i);
}

TEST_F(TupleIteratorTest, CopyAssignableConceptSatisfied) {
    const TupleIterator i = tuple_range_.begin();

    TupleIterator j;
    ASSERT_NE(j, i);

    // { a = b; } -> { a == b }
    j = i;
    EXPECT_EQ(j, i);
}

TEST_F(TupleIteratorTest, SwappableConceptSatisfied) {
    TupleIterator i = tuple_range_.begin();
    TupleIterator j = tuple_range_.end();
    ASSERT_NE(i, j);

    using std::swap;
    swap(i, j);

    ASSERT_NE(i, j);
    EXPECT_EQ(i, tuple_range_.end());
    EXPECT_EQ(j, tuple_range_.begin());
}

TEST_F(TupleIteratorTest, IteratorConceptSatisfied) {
    TupleIterator i = tuple_range_.begin();

    using reference = typename std::iterator_traits<TupleIterator>::reference;
    EXPECT_TRUE((std::is_same_v<decltype(*i), reference>));
    EXPECT_TRUE((std::is_same_v<decltype(++i), TupleIterator&>));
}

TEST_F(TupleIteratorTest, DefaultConstructibleConceptSatisfied) {
    const TupleIterator default_initialized_iter;
    EXPECT_EQ(default_initialized_iter, nullptr);

    const TupleIterator value_initialized_iter{};
    EXPECT_EQ(value_initialized_iter, nullptr);

    EXPECT_EQ(TupleIterator(), nullptr);
    EXPECT_EQ(TupleIterator{}, nullptr);
}

TEST_F(TupleIteratorTest, CopyEqualityConceptSatisfied) {
    TupleIterator i = tuple_range_.begin();
    TupleIterator j = tuple_range_.begin();

    ASSERT_EQ(i, j);
    EXPECT_EQ(++i, ++j);
}

TEST_F(TupleIteratorTest, ForwardIteratorConceptSatisfied) {
    TupleIterator i = tuple_range_.begin();
    std::vector<TupleIterator> i_copies(3, i);

    EXPECT_EQ(++i, std::next(tuple_range_.begin()));
    for (TupleIterator& j : i_copies) { EXPECT_EQ(j, tuple_range_.begin()); }

    i = tuple_range_.begin();
    EXPECT_EQ(i++, tuple_range_.begin());
    EXPECT_EQ(i, std::next(tuple_range_.begin()));

    // TODO: Add dereference tests.
}

TEST_F(TupleIteratorTest, BidirectionalIteratorConceptSatisfied) {
    TupleIterator i = tuple_range_.end();
    std::vector<TupleIterator> i_copies(3, i);

    EXPECT_EQ(--i, std::prev(tuple_range_.end()));
    for (TupleIterator& j : i_copies) { EXPECT_EQ(j, tuple_range_.end()); }

    i = tuple_range_.end();
    EXPECT_EQ(i--, tuple_range_.end());
    EXPECT_EQ(i, std::prev(tuple_range_.end()));

    // TODO: Add dereference tests.
}

TEST_F(TupleIteratorTest, RandomAccessIteratorConceptSatisfied) {
    // Test for addition
    {
        TupleIterator i = tuple_range_.begin();
        i += 2;
        TupleIterator j = tuple_range_.begin() + 2;
        TupleIterator k = 2 + tuple_range_.begin();
        EXPECT_EQ(i, j);
        EXPECT_EQ(i, k);
        EXPECT_EQ(j, k);
    }
    // Test for subtraction
    {
        TupleIterator i = tuple_range_.end();
        i -= 2;
        TupleIterator j = tuple_range_.end() - 2;
        EXPECT_EQ(i, j);
    }
    // Test for difference
    {
        EXPECT_EQ(tuple_range_.end() - tuple_range_.begin(), 3);
    }
    // Test for index-access
    {
        TupleIterator::reference e = tuple_range_.begin()[2];
    }
}
