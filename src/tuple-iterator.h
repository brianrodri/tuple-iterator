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
// structure. Specifically, `T` must satisfy the following:
//   - std::get<I>(std::declval<T&>()) -> std::tuple_element_t<I, T>&
//   - std::tuple_size_v<T> -> constexpr size_t
//
// std::tuple, std::pair, and std::array define these overloads by default, but you can create
// overloads for your own custom classes as necessary.
template <typename T>
struct IterTraitsImpl {
  private:
    template <size_t... I> static constexpr auto ReferenceTypeImpl(std::index_sequence<I...> _) ->
        std::variant<std::reference_wrapper<std::tuple_element_t<I, T>>...>;

  public:
    using ReferenceType =
		decltype(ReferenceTypeImpl(std::make_index_sequence<std::tuple_size_v<T>>()));
    using ValueType = ReferenceType;
};

// Builds an array of std::get accessors for the given type T.
template <typename T>
struct GetterImpl {
  private:
    using ReferenceType = typename IterTraitsImpl<T>::ReferenceType;

  public:
    using GetterPointer = ReferenceType(*)(T&);
    using GetterArray = std::array<const GetterPointer, std::tuple_size_v<T>>;

    static constexpr const GetterArray MakeGetters() {
        return MakeGettersImpl(std::make_index_sequence<std::tuple_size_v<T>>());
    }

  private:
    template <size_t... I>
    static constexpr const GetterArray MakeGettersImpl(std::index_sequence<I...> _) {
        return {
            +[](T& t) constexpr { return ReferenceType{std::get<I>(t)}; }
            ...
        };
    }
};

}  // namespace detail

// Provides interface for creating tuple iterators.
template <typename T>
class TupleRange;

template <typename T>
class TupleIterator {
    static constexpr const auto kGetters = detail::GetterImpl<T>::MakeGetters();
    using GetterIter = typename decltype(kGetters)::const_iterator;

  public:
    // Type aliases expected by the standard.
    using reference = typename detail::IterTraitsImpl<T>::ReferenceType;
    using value_type = typename detail::IterTraitsImpl<T>::ValueType;
    using pointer = typename std::iterator_traits<GetterIter>::pointer;
    using difference_type = typename std::iterator_traits<GetterIter>::difference_type;
    using iterator_category = typename std::iterator_traits<GetterIter>::iterator_category;

	// Returns a *singular iterator*, that is, an iterator that is not associated with any tuple.
	// Such instances are semantically equivalent to nullptr, and should therefore never be
	// incremented or dereferenced; only reassignment is allowed.
    //
    // You can check if an instance is singular by comparing it against std::nullptr_t.
    constexpr explicit TupleIterator(std::nullptr_t _ = nullptr)
        : tuple_ptr_{nullptr}, getter_iter_{std::cend(kGetters)} {}

    ~TupleIterator() = default;

    constexpr TupleIterator(const TupleIterator<T>& src) = default;
    constexpr TupleIterator(TupleIterator<T>&& src) = default;

    constexpr TupleIterator& operator=(const TupleIterator<T>& src) = default;
    constexpr TupleIterator& operator=(TupleIterator<T>&& src) = default;

    constexpr TupleIterator& operator=(std::nullptr_t _) {
        tuple_ptr_ = nullptr;
        getter_iter_ = std::cend(kGetters);
        return *this;
    }

    constexpr reference operator*() { return *getter_iter_(*tuple_ptr_); }
    constexpr reference operator[](difference_type i) { return getter_iter_[i](*tuple_ptr_); }
    constexpr TupleIterator& operator++() { ++getter_iter_; return *this; }
    constexpr TupleIterator operator++(int _) { TupleIterator i{*this}; ++getter_iter_; return i; }
    constexpr TupleIterator& operator--() { --getter_iter_; return *this; }
    constexpr TupleIterator operator--(int _) { TupleIterator i{*this}; --getter_iter_; return i; }
    constexpr TupleIterator& operator+=(difference_type n) { getter_iter_ += n; return *this; }
    constexpr TupleIterator& operator-=(difference_type n) { getter_iter_ -= n; return *this; }

    constexpr reference operator*() const { return *getter_iter_(*tuple_ptr_); }
    constexpr reference operator[](difference_type i) const { return getter_iter_[i](*tuple_ptr_); }

    constexpr TupleIterator operator+(difference_type n) const { return TupleIterator{*this} += n; }
    constexpr TupleIterator operator-(difference_type n) const { return TupleIterator{*this} -= n; }

    constexpr difference_type operator-(const TupleIterator& rhs) const {
        return getter_iter_ - rhs.getter_iter_;
    }

    constexpr bool operator<(const TupleIterator& rhs) const {
        return getter_iter_ < rhs.getter_iter_;
    }

    constexpr bool operator>(const TupleIterator& rhs) const {
        return getter_iter_ > rhs.getter_iter_;
    }

    constexpr bool operator<=(const TupleIterator& rhs) const {
        return getter_iter_ <= rhs.getter_iter_;
    }

    constexpr bool operator>=(const TupleIterator& rhs) const {
        return getter_iter_ >= rhs.getter_iter_;
    }

    // NOTE: operator-> is not defined because there is no way to make it
    // standard-compliant.
    //
    // The standard expects that values returned by operator-> may eventually be
    // resolved by 1+ repeated applications. However, we can only return a
    // std::variant of pointers. This implies that eventually, a call to
    // std::visit *must be made*.
    //
    // For now, I've chosen to simply leave out the definition of operator->
    // while keeping the definition of a "pointer"-type. I kept the pointer-type
    // because it is required by std::distance.

  private:
    // This constructor will be called by the TupleRange class methods.
    constexpr TupleIterator(T* t, GetterIter i)
        : tuple_ptr_{t}, getter_iter_{tuple_ptr_ == nullptr ? std::cend(kGetters) : i} {};

    // Provides interface for creating tuple iterators.
    friend class TupleRange<T>;

    // Provides interface for comparing tuple iterators.
    template <typename U>
    friend constexpr bool operator==(const TupleIterator<U>& lhs, const TupleIterator<U>& rhs);
    template <typename U>
    friend constexpr bool operator==(const TupleIterator<U>& lhs, std::nullptr_t rhs);
    template <typename U>
    friend constexpr bool operator==(std::nullptr_t lhs, const TupleIterator<U>& rhs);

    T* tuple_ptr_;
    GetterIter getter_iter_;
};

template <typename U>
constexpr TupleIterator<U> operator+(typename TupleIterator<U>::difference_type n,
                                     const TupleIterator<U>& i) {
    return TupleIterator{i} + n;
}

template <typename T>
constexpr bool operator==(const TupleIterator<T>& lhs, const TupleIterator<T>& rhs) {
    return lhs.tuple_ptr_ == rhs.tuple_ptr_ && lhs.getter_iter_ == rhs.getter_iter_;
}

template <typename T, typename U>
constexpr bool operator==(const TupleIterator<T>& lhs, const TupleIterator<U>& rhs) {
    return false;
}

template <typename T>
constexpr bool operator==(const TupleIterator<T>& lhs, std::nullptr_t rhs) {
    return lhs.tuple_ptr_ == rhs;
}

template <typename T>
constexpr bool operator==(std::nullptr_t lhs, const TupleIterator<T>& rhs) {
    return lhs == rhs.tuple_ptr_;
}

template <typename T, typename U>
constexpr bool operator!=(const TupleIterator<T>& lhs, const TupleIterator<U>& rhs) {
    return !(lhs == rhs);
}

template <typename T>
constexpr bool operator!=(const TupleIterator<T>& lhs, std::nullptr_t rhs) {
    return !(lhs == rhs);
}

template <typename T>
constexpr bool operator!=(std::nullptr_t lhs, const TupleIterator<T>& rhs) {
    return !(lhs == rhs);
}

// Provides interface for creating tuple iterators.
template <typename T>
class TupleRange {
  public:
    constexpr TupleRange(T& t) : tuple_ptr_(&t) {}

    constexpr TupleIterator<T> begin() const {
        return {tuple_ptr_, std::cbegin(TupleIterator<T>::kGetters)};
    }

    constexpr TupleIterator<T> end() const {
        return {tuple_ptr_, std::cend(TupleIterator<T>::kGetters)};
    }

    static constexpr TupleIterator<T> begin(T& t) { return TupleRange{t}.begin(); }
    static constexpr TupleIterator<T> end(T& t) { return TupleRange{t}.end(); }

  private:
    T* tuple_ptr_;
};

}  // namespace tuple_ext
#endif  // BRIANRODRI_TUPLE_ITERATOR_TUPLE_ITERATOR_H
