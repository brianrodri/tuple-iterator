#include <algorithm>
#include <functional>
#include <iterator>
#include <vector>
#include "gtest/gtest.h"
#include "tuple-iterator.h"

class TupleIteratorTest : public ::testing::Test {
  protected:
    using Tuple = std::tuple<int, std::vector<double>, std::string>;
    using TupleItr = tuple_ext::TupleIterator<Tuple>;
    using TupleRng = tuple_ext::TupleRange<Tuple>;

    Tuple tuple_{1, {1.61803, 2.71828, 3.14159}, "inf"};
    TupleRng tuple_range_{tuple_};

    template <typename T>
    static constexpr auto IsA = TupleRng::MakeVisitor([](const auto& arg) {
        return std::is_same_v<std::decay_t<decltype(arg)>, T>;
    });
};

TEST_F(TupleIteratorTest, CopyConstructibleConceptSatisfied) {
    const TupleItr i = tuple_range_.begin();
    TupleItr i_copy = i;

    ASSERT_EQ(i_copy, i);
    EXPECT_EQ(TupleItr{i}, i);
}

TEST_F(TupleIteratorTest, CopyAssignableConceptSatisfied) {
    const TupleItr i = tuple_range_.begin();
    TupleItr j;

    ASSERT_NE(j, i);

    j = i;

    EXPECT_EQ(j, i);
}

TEST_F(TupleIteratorTest, SwappableConceptSatisfied) {
    TupleItr i = tuple_range_.begin();
    TupleItr j = tuple_range_.end();
    ASSERT_NE(i, j);

    using std::swap;
    swap(i, j);

    EXPECT_EQ(i, tuple_range_.end());
    EXPECT_EQ(j, tuple_range_.begin());
    ASSERT_NE(i, j);
}

TEST_F(TupleIteratorTest, IteratorConceptSatisfied) {
    using reference = typename std::iterator_traits<TupleItr>::reference;

    TupleItr i = tuple_range_.begin();
    EXPECT_TRUE(IsA<int>(*i));
    EXPECT_TRUE(IsA<std::vector<double>>(*++i));
}

TEST_F(TupleIteratorTest, DefaultConstructibleConceptSatisfied) {
    const TupleItr default_initialized_iter;
    EXPECT_EQ(default_initialized_iter, nullptr);

    const TupleItr value_initialized_iter{};
    EXPECT_EQ(value_initialized_iter, nullptr);

    EXPECT_EQ(TupleItr(), nullptr);
    EXPECT_EQ(TupleItr{}, nullptr);
}

TEST_F(TupleIteratorTest, CopyEqualityConceptSatisfied) {
    TupleItr i = tuple_range_.begin();
    TupleItr j = tuple_range_.begin();

    ASSERT_EQ(i, j);
    EXPECT_EQ(++i, ++j);
}

TEST_F(TupleIteratorTest, ForwardIteratorConceptSatisfied) {
    TupleItr i = tuple_range_.begin();
    std::vector<TupleItr> i_copies(3, i);

    EXPECT_EQ(++i, std::next(tuple_range_.begin()));
    for (const TupleItr& j : i_copies) { EXPECT_EQ(j, tuple_range_.begin()); }

    i = tuple_range_.begin();
    EXPECT_EQ(i++, tuple_range_.begin());
    EXPECT_EQ(i, std::next(tuple_range_.begin()));

    i = tuple_range_.begin();
    EXPECT_TRUE(IsA<int>(*i++));
    EXPECT_TRUE(IsA<std::vector<double>>(*i));
}

TEST_F(TupleIteratorTest, BidirectionalIteratorConceptSatisfied) {
    TupleItr i = tuple_range_.end();
    std::vector<TupleItr> i_copies(3, i);

    EXPECT_EQ(--i, std::prev(tuple_range_.end()));
    for (const TupleItr& j : i_copies) { EXPECT_EQ(j, tuple_range_.end()); }

    i = tuple_range_.end();
    EXPECT_EQ(i--, tuple_range_.end());
    EXPECT_EQ(i, std::prev(tuple_range_.end()));
    EXPECT_TRUE(IsA<std::string>(*i));
}

TEST_F(TupleIteratorTest, RandomAccessIteratorConceptSatisfied) {
    // Test for addition
    {
        TupleItr i = tuple_range_.begin();
        TupleItr j = i + 2;
        TupleItr k = 2 + i;
        i += 2;
        EXPECT_EQ(i, j);
        EXPECT_EQ(i, k);
        EXPECT_EQ(j, k);
    }
    // Test for subtraction
    {
        TupleItr i = tuple_range_.end();
        TupleItr j = i - 2;
        i -= 2;
        EXPECT_EQ(i, j);
    }
    // Test for difference
    {
        EXPECT_EQ(tuple_range_.end() - tuple_range_.begin(), 3);
    }
    // Test for index-access
    {
        TupleItr i = tuple_range_.begin();
        EXPECT_TRUE(IsA<int>(i[0]));
        EXPECT_TRUE(IsA<std::vector<double>>(i[1]));
        EXPECT_TRUE(IsA<std::string>(i[2]));
    }
}
