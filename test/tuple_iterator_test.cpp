#include "tuple_iterator.h"
#include "gtest/gtest.h"
#include <iterator>

using tuple_ext::TupleIterator;
using tuple_ext::TupleRange;

class TupleIteratorTest : public ::testing::Test {
  protected:
    using tuple_t = std::tuple<int, float, std::string>;
    using iterator_t = TupleIterator<tuple_t>;
    using range_t = TupleRange<tuple_t>;

    tuple_t tuple_{1, 2.5, "inf"};
    range_t tuple_range_{tuple_};
};

TEST_F(TupleIteratorTest, CopyConstructibleConcept) {
    const iterator_t tuple_begin = tuple_range_.begin();

    // { T a = b; } -> { a == b; }
    iterator_t tuple_begin_copy = tuple_begin;
    ASSERT_EQ(tuple_begin_copy, tuple_begin);

    // { T(a) == a; }
    ASSERT_EQ(iterator_t{tuple_begin}, tuple_begin);
}

TEST_F(TupleIteratorTest, CopyAssignableConcept) {
    const iterator_t tuple_begin = tuple_range_.begin();

    iterator_t tuple_iter = tuple_range_.end();
    ASSERT_NE(tuple_iter, tuple_begin);

    // { a = b; } -> { a == b }
    tuple_iter = tuple_begin;
    ASSERT_EQ(tuple_iter, tuple_begin);
}

TEST_F(TupleIteratorTest, SwappableConcept) {
    ASSERT_NE(tuple_range_.begin(), tuple_range_.end());
    iterator_t iter_a = tuple_range_.begin();
    iterator_t iter_b = tuple_range_.end();

    using std::swap;
    swap(iter_a, iter_b);

    ASSERT_EQ(iter_a, tuple_range_.end());
    ASSERT_EQ(iter_b, tuple_range_.begin());
    ASSERT_NE(iter_a, iter_b);
}

TEST_F(TupleIteratorTest, IteratorConcept) {
    iterator_t iter = tuple_range_.begin();

    using reference = typename std::iterator_traits<iterator_t>::reference;
    EXPECT_TRUE((std::is_same_v<decltype(*iter), reference>));
    EXPECT_TRUE((std::is_same_v<decltype(++iter), iterator_t&>));
}
