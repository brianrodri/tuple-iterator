#ifndef BRIANRODRI_TUPLE_ITERATOR_TUPLE_ITERATOR_H
#define BRIANRODRI_TUPLE_ITERATOR_TUPLE_ITERATOR_H
#include <functional>
#include <iostream>
#include <optional>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>

namespace tuple_ext {
namespace detail {

// Exposes types required by TupleIterator to be standards-compliant.
//
// The types are derived from the given type parameter, which is assumed to be a "tuple-like"
// structure. Specifically, `TupleLike` must satisfy the following:
//   - std::get<I>(std::declval<TupleLike&>()) -> std::tuple_element_t<I, TupleLike>&
//   - std::tuple_size_v<TupleLike> -> constexpr size_t
//
// std::tuple, std::pair, and std::array define these overloads by default, but you can create
// overloads for your own custom classes as necessary.
template <typename TupleLike>
struct IterTraitsImpl {
  private:
    template <size_t... I> static constexpr auto ReferenceTypeImpl(std::index_sequence<I...> _) ->
        std::variant<std::reference_wrapper<std::tuple_element_t<I, TupleLike>>...>;

  public:
    using ReferenceType =
        decltype(ReferenceTypeImpl(std::make_index_sequence<std::tuple_size_v<TupleLike>>()));
    using ValueType = ReferenceType;
};

// Builds an array of std::get accessors for the given type TupleLike.
template <typename TupleLike>
struct GetterImpl {
  private:
    using ReferenceType = typename IterTraitsImpl<TupleLike>::ReferenceType;

  public:
    using GetterPointer = ReferenceType(*)(TupleLike&);
    using GetterArray = std::array<const GetterPointer, std::tuple_size_v<TupleLike>>;

    static constexpr const GetterArray MakeGetters() {
        return MakeGettersImpl(std::make_index_sequence<std::tuple_size_v<TupleLike>>());
    }

  private:
    template <size_t... I>
    static constexpr const GetterArray MakeGettersImpl(std::index_sequence<I...> _) {
        return {
            +[](TupleLike& t) constexpr { return ReferenceType{std::get<I>(t)}; }
            ...
        };
    }
};

}  // namespace detail

// Provides interface for creating tuple iterators.
template <typename TupleLike>
class TupleRange;

template <typename TupleLike>
class TupleIterator {
    static constexpr const auto kGetters = detail::GetterImpl<TupleLike>::MakeGetters();
    using GetterIter = typename decltype(kGetters)::const_iterator;

  public:
    using reference = typename detail::IterTraitsImpl<TupleLike>::ReferenceType;
    using value_type = typename detail::IterTraitsImpl<TupleLike>::ValueType;
    using pointer = typename std::iterator_traits<GetterIter>::pointer;
    using difference_type = typename std::iterator_traits<GetterIter>::difference_type;
    using iterator_category = typename std::iterator_traits<GetterIter>::iterator_category;

    // Returns a *singular iterator*, that is, an iterator that is not associated with any tuple.
    // Such instances are semantically equivalent to nullptr, and should therefore never be
    // incremented or dereferenced; only reassignment is allowed.
    //
    // You can check if an instance is singular by comparing it against std::nullptr_t.
    constexpr explicit TupleIterator(std::nullptr_t _ = nullptr)
        : tuple_ptr_{nullptr}, getter_itr_{std::cend(kGetters)} {}

    ~TupleIterator() = default;
    constexpr TupleIterator(const TupleIterator<TupleLike>& src) = default;
    constexpr TupleIterator(TupleIterator<TupleLike>&& src) = default;
    constexpr TupleIterator& operator=(const TupleIterator<TupleLike>& src) = default;
    constexpr TupleIterator& operator=(TupleIterator<TupleLike>&& src) = default;

    constexpr TupleIterator& operator=(std::nullptr_t _) {
        tuple_ptr_ = nullptr;
        getter_itr_ = std::cend(kGetters);
        return *this;
    }

    constexpr reference operator*() { return *getter_itr_(*tuple_ptr_); }
    constexpr reference operator[](difference_type i) { return getter_itr_[i](*tuple_ptr_); }
    constexpr TupleIterator& operator++() { ++getter_itr_; return *this; }
    constexpr TupleIterator operator++(int _) { TupleIterator i{*this}; ++getter_itr_; return i; }
    constexpr TupleIterator& operator--() { --getter_itr_; return *this; }
    constexpr TupleIterator operator--(int _) { TupleIterator i{*this}; --getter_itr_; return i; }
    constexpr TupleIterator& operator+=(difference_type n) { getter_itr_ += n; return *this; }
    constexpr TupleIterator& operator-=(difference_type n) { getter_itr_ -= n; return *this; }

    constexpr reference operator*() const { return *(*this); }
    constexpr reference operator[](difference_type i) const { return (*this)[i](*tuple_ptr_); }
    constexpr TupleIterator operator+(difference_type n) const { return TupleIterator{*this} += n; }
    constexpr TupleIterator operator-(difference_type n) const { return TupleIterator{*this} -= n; }

    constexpr difference_type operator-(const TupleIterator& rhs) const {
        return getter_itr_ - rhs.getter_itr_;
    }

    constexpr bool operator<(const TupleIterator& rhs) const {
        return getter_itr_ < rhs.getter_itr_;
    }

    constexpr bool operator>(const TupleIterator& rhs) const {
        return getter_itr_ > rhs.getter_itr_;
    }

    constexpr bool operator<=(const TupleIterator& rhs) const {
        return getter_itr_ <= rhs.getter_itr_;
    }

    constexpr bool operator>=(const TupleIterator& rhs) const {
        return getter_itr_ >= rhs.getter_itr_;
    }

    // NOTE: operator-> is not defined because there is no way to make it standard-compliant.
    //
    // The standard expects that values returned by operator-> may eventually be resolved by 1+
    // repeated applications. However, we can only return a std::variant of pointers. This implies
    // that eventually, a call to std::visit *must be made*.
    //
    // For now, I've chosen to simply leave out the definition of operator-> while keeping the
    // definition of a "pointer"-type. I kept the pointer-type because it is required by
    // std::distance.

  private:
    // This constructor will be called by the TupleRange class methods.
    constexpr TupleIterator(TupleLike* t, GetterIter i)
        : tuple_ptr_{t}, getter_itr_{tuple_ptr_ == nullptr ? std::cend(kGetters) : i} {};

    // Provides interface for creating tuple iterators.
    friend class TupleRange<TupleLike>;

    // Provides interface for comparing tuple iterators.
    template <typename U>
    friend constexpr bool operator==(const TupleIterator<U>& lhs, const TupleIterator<U>& rhs);
    template <typename U>
    friend constexpr bool operator==(const TupleIterator<U>& lhs, std::nullptr_t rhs);
    template <typename U>
    friend constexpr bool operator==(std::nullptr_t lhs, const TupleIterator<U>& rhs);

    TupleLike* tuple_ptr_;
    GetterIter getter_itr_;
};

template <typename U>
constexpr TupleIterator<U> operator+(typename TupleIterator<U>::difference_type n,
                                     const TupleIterator<U>& i) {
    return TupleIterator{i} + n;
}

template <typename TupleLike>
constexpr bool operator==(const TupleIterator<TupleLike>& lhs,
                          const TupleIterator<TupleLike>& rhs) {
    return lhs.tuple_ptr_ == rhs.tuple_ptr_ && lhs.getter_itr_ == rhs.getter_itr_;
}

template <typename TupleLike, typename U>
constexpr bool operator==(const TupleIterator<TupleLike>& lhs, const TupleIterator<U>& rhs) {
    return false;
}

template <typename TupleLike>
constexpr bool operator==(const TupleIterator<TupleLike>& lhs, std::nullptr_t rhs) {
    return lhs.tuple_ptr_ == rhs;
}

template <typename TupleLike>
constexpr bool operator==(std::nullptr_t lhs, const TupleIterator<TupleLike>& rhs) {
    return lhs == rhs.tuple_ptr_;
}

template <typename TupleLike, typename U>
constexpr bool operator!=(const TupleIterator<TupleLike>& lhs, const TupleIterator<U>& rhs) {
    return !(lhs == rhs);
}

template <typename TupleLike>
constexpr bool operator!=(const TupleIterator<TupleLike>& lhs, std::nullptr_t rhs) {
    return !(lhs == rhs);
}

template <typename TupleLike>
constexpr bool operator!=(std::nullptr_t lhs, const TupleIterator<TupleLike>& rhs) {
    return !(lhs == rhs);
}

// Provides interface for creating tuple iterators.
template <typename TupleLike>
class TupleRange {
  public:
    constexpr TupleRange(TupleLike& t) : tuple_ptr_(&t) {}

    constexpr TupleIterator<TupleLike> begin() const {
        return {tuple_ptr_, std::cbegin(TupleIterator<TupleLike>::kGetters)};
    }

    constexpr TupleIterator<TupleLike> end() const {
        return {tuple_ptr_, std::cend(TupleIterator<TupleLike>::kGetters)};
    }

    static constexpr TupleIterator<TupleLike> begin(TupleLike& t) { return TupleRange{t}.begin(); }
    static constexpr TupleIterator<TupleLike> end(TupleLike& t) { return TupleRange{t}.end(); }

    template <typename VisitorImpl>
    static constexpr decltype(auto) MakeVisitor(VisitorImpl&& v) {
        using ReferenceType = typename TupleIterator<TupleLike>::reference;
        return [v=std::forward<VisitorImpl>(v)](ReferenceType& iter_ref) {
            return std::visit(v, iter_ref);
        };
    }

  private:
    TupleLike* tuple_ptr_;
};

}  // namespace tuple_ext
#endif  // BRIANRODRI_TUPLE_ITERATOR_TUPLE_ITERATOR_H
