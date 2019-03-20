#include <algorithm>
#include <iterator>
#include <vector>
#include "gtest/gtest.h"
#include "tuple_iterator.h"

template <typename T, typename U>
constexpr bool operator==(const std::reference_wrapper<T>& lhs,
                          const std::reference_wrapper<U>& rhs) {
    return lhs.get() == rhs.get();
}

class TupleIteratorTest : public ::testing::Test {
  protected:
    using Tuple = std::tuple<int, std::vector<double>, std::string>;
    using TupleIterator = tuple_ext::TupleIterator<Tuple>;
    using TupleRange = tuple_ext::TupleRange<Tuple>;

    Tuple tuple_{1, {1.61803, 2.71828, 3.14159}, "inf"};
    TupleRange tuple_range_{tuple_};
};

TEST_F(TupleIteratorTest, CopyConstructibleConceptSatisfied) {
    const TupleIterator tuple_begin = tuple_range_.begin();

    // { T a = b; } -> { a == b; }
    TupleIterator tuple_begin_copy = tuple_begin;
    ASSERT_EQ(tuple_begin_copy, tuple_begin);

    // { T(a) == a; }
    EXPECT_EQ(TupleIterator{tuple_begin}, tuple_begin);
}

TEST_F(TupleIteratorTest, CopyAssignableConceptSatisfied) {
    const TupleIterator tuple_begin = tuple_range_.begin();

    TupleIterator tuple_iter = tuple_range_.end();
    ASSERT_NE(tuple_iter, tuple_begin);

    // { a = b; } -> { a == b }
    tuple_iter = tuple_begin;
    EXPECT_EQ(tuple_iter, tuple_begin);
}

TEST_F(TupleIteratorTest, SwappableConceptSatisfied) {
    ASSERT_NE(tuple_range_.begin(), tuple_range_.end());
    TupleIterator iter_a = tuple_range_.begin();
    TupleIterator iter_b = tuple_range_.end();

    using std::swap;
    swap(iter_a, iter_b);

    ASSERT_NE(iter_a, iter_b);
    EXPECT_EQ(iter_a, tuple_range_.end());
    EXPECT_EQ(iter_b, tuple_range_.begin());
}

TEST_F(TupleIteratorTest, IteratorConceptSatisfied) {
    TupleIterator iter = tuple_range_.begin();

    using reference = typename std::iterator_traits<TupleIterator>::reference;
    EXPECT_TRUE((std::is_same_v<decltype(*iter), reference>));
    EXPECT_TRUE((std::is_same_v<decltype(++iter), TupleIterator&>));
}

TEST_F(TupleIteratorTest, DefaultConstructibleConceptSatisfied) {
    TupleIterator default_initialized_iter;
    EXPECT_EQ(default_initialized_iter, nullptr);

    TupleIterator value_initialized_iter{};
    EXPECT_EQ(value_initialized_iter, nullptr);

    EXPECT_EQ(TupleIterator(), nullptr);
    EXPECT_EQ(TupleIterator{}, nullptr);
}

TEST_F(TupleIteratorTest, CopyEqualityConceptSatisfied) {
    TupleIterator a = tuple_range_.begin();
    TupleIterator b = tuple_range_.begin();

    ASSERT_EQ(a, b);
    EXPECT_EQ(++a, ++b);
}

TEST_F(TupleIteratorTest, ForwardIteratorConceptSatisfied) {
    TupleIterator i = tuple_range_.begin();
    std::vector<TupleIterator> i_copies(3, i);

    EXPECT_EQ(++i, std::next(tuple_range_.begin()));
    for (TupleIterator& j : i_copies) { EXPECT_EQ(j, tuple_range_.begin()); }

    i = tuple_range_.begin();
    EXPECT_EQ(i++, tuple_range_.begin());
    EXPECT_EQ(i, std::next(tuple_range_.begin()));

    /*
    i = tuple_range_.begin();
    EXPECT_EQ(*i++, *tuple_range_.begin());
    EXPECT_EQ(*i, *std::next(tuple_range_.begin()));
    */
}
