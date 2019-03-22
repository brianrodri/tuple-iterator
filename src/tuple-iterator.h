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

// Exposes types required by TupleIterator to be standard-compliant.
//
// The types are derived from the given type parameter, which is assumed to be a
// "tuple-like" structure. Specifically, `Tup` must compile in the following
// contexts:
//   - std::get<I>(std::declval<Tup&>())
//   - std::tuple_size<Tup>
//
// std::tuple defines these overloads thanks to the standard, but you can create
// overloads for custom classes as necessary.
template <typename Tup>
struct IterTraitsImpl;

template <typename... T>
struct IterTraitsImpl<std::tuple<T...>> {
    using PointerType = std::variant<T*...>;
    using ReferenceType = std::variant<std::reference_wrapper<T>...>;
    using ValueType = ReferenceType;
    using DifferenceType = std::ptrdiff_t;
};

template <typename... T>
struct IterTraitsImpl<const std::tuple<T...>> {
    using PointerType = std::variant<const T*...>;
    using ReferenceType = std::variant<std::reference_wrapper<const T>...>;
    using ValueType = ReferenceType;
    using DifferenceType = std::ptrdiff_t;
};

template <typename T>
struct GetterImpl {
  private:
    static constexpr size_t kTupleSize = std::tuple_size_v<T>;
    using ReferenceType = typename IterTraitsImpl<T>::ReferenceType;

  public:
    using GetterPointer = ReferenceType(*)(T&);
    using GetterArray = std::array<const GetterPointer, kTupleSize>;

    static constexpr GetterArray MakeGetters() {
        return MakeGettersImpl(std::make_index_sequence<kTupleSize>());
    }

  private:
    template <size_t... I>
    static constexpr GetterArray MakeGettersImpl(std::index_sequence<I...> _) {
        return {
            +[](T& t) constexpr -> ReferenceType { return {std::get<I>(t)}; }...
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
    using GetterIter = typename decltype(kGetters)::iterator;

  public:
    // Type aliases expected by the standard.
    using value_type = typename detail::IterTraitsImpl<T>::ValueType;
    using reference = typename detail::IterTraitsImpl<T>::ReferenceType;
    using pointer = typename detail::IterTraitsImpl<T>::PointerType;
    using difference_type = typename detail::IterTraitsImpl<T>::DifferenceType;
    using iterator_category = std::random_access_iterator_tag;

    // Returns a *singular iterator*, that is, an iterator that is not
    // associated with any tuple. Such instances are semantically equivalent to
    // nullptr, and should therefore never be modified or dereferenced.
    //
    // You can check if an instance is singular by comparing it against nullptr.
    explicit TupleIterator(std::nullptr_t _ = {})
        : tuple_ptr_{nullptr}, getter_iter_{std::cend(kGetters)} {}

    TupleIterator& operator=(std::nullptr_t _) {
        tuple_ptr_ = nullptr;
        getter_iter_ = std::cend(kGetters);
        return *this;
    }

    TupleIterator(const TupleIterator<T>& src) = default;
    TupleIterator(TupleIterator<T>&& src) = default;
    TupleIterator& operator=(const TupleIterator<T>& src) = default;
    TupleIterator& operator=(TupleIterator<T>&& src) = default;

    constexpr TupleIterator& operator++() {
        ++getter_iter_;
        return *this;
    }

    constexpr TupleIterator operator++(int _) {
        TupleIterator curr_iter{*this};
        ++getter_iter_;
        return curr_iter;
    }

    constexpr TupleIterator& operator--() {
        --getter_iter_;
        return *this;
    }

    constexpr TupleIterator operator--(int _) {
        TupleIterator curr_iter{*this};
        --getter_iter_;
        return curr_iter;
    }

    constexpr TupleIterator& operator+=(difference_type n) {
        getter_iter_ += n;
        return *this;
    }

    constexpr TupleIterator operator+(difference_type n) const {
        TupleIterator temp = *this;
        return temp += n;
    }

    template <typename U>
    friend constexpr TupleIterator<U>
    operator+(typename TupleIterator<U>::difference_type n,
              const TupleIterator<U>& i);

    constexpr TupleIterator& operator-=(difference_type n) {
        getter_iter_ -= n;
        return *this;
    }

    constexpr TupleIterator operator-(difference_type n) const {
        TupleIterator temp = *this;
        return temp -= n;
    }

    constexpr difference_type operator-(const TupleIterator& rhs) const {
        return getter_iter_ - rhs.getter_iter_;
    }

    constexpr bool operator<(const TupleIterator& rhs) const {
        return getter_iter_ < rhs.getter_iter_;
    }

    constexpr bool operator>(const TupleIterator& rhs) const {
        return rhs < *this;
    }

    constexpr bool operator>=(const TupleIterator& rhs) const {
        return !(*this < rhs);
    }

    constexpr bool operator<=(const TupleIterator& rhs) const {
        return !(*this > rhs);
    }

    constexpr reference operator[](difference_type i) {
        return (getter_iter_[i])(*tuple_ptr_);
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

    constexpr reference operator*() { return (*getter_iter_)(*tuple_ptr_); }

    // Returns the index of the element currently being pointed to by the
    // iterator, or the size of the tuple if the iterator points to it's end.
    constexpr size_t index() const {
        return std::distance(std::cbegin(kGetters), getter_iter_);
    }

  private:
    // This constructor will be called by the TupleRange class methods.
    constexpr TupleIterator(T* t, const bool to_end)
        : tuple_ptr_{t},
          getter_iter_{(tuple_ptr_ == nullptr || to_end)
                           ? std::cend(kGetters) : std::cbegin(kGetters)} {
    };

    // Provides interface for creating tuple iterators.
    friend class TupleRange<T>;

    // Provides interface for comparing tuple iterators.
    template <typename U>
    friend constexpr bool operator==(const TupleIterator<U>& lhs,
                                     const TupleIterator<U>& rhs);
    template <typename U>
    friend constexpr bool operator==(const TupleIterator<U>& lhs,
                                     std::nullptr_t rhs);
    template <typename U>
    friend constexpr bool operator==(std::nullptr_t lhs,
                                     const TupleIterator<U>& rhs);

    T* tuple_ptr_;
    GetterIter getter_iter_;
};

template <typename U>
constexpr TupleIterator<U> operator+(
        typename TupleIterator<U>::difference_type n,
        const TupleIterator<U>& i) {
    TupleIterator i_plus_n = i;
    return i + n;
}

template <typename T>
constexpr bool operator==(const TupleIterator<T>& lhs,
                          const TupleIterator<T>& rhs) {
    return lhs.tuple_ptr_ == rhs.tuple_ptr_ &&
           lhs.getter_iter_ == rhs.getter_iter_;
}

template <typename T, typename U>
constexpr bool operator==(const TupleIterator<T>& lhs,
                          const TupleIterator<U>& rhs) {
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
constexpr bool operator!=(const TupleIterator<T>& lhs,
                          const TupleIterator<U>& rhs) {
    return true;
}

template <typename T>
constexpr bool operator!=(const TupleIterator<T>& lhs,
                          const TupleIterator<T>& rhs) {
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
        return {tuple_ptr_, /*to_end=*/false};
    }

    constexpr TupleIterator<T> end() const {
        return {tuple_ptr_, /*to_end=*/true};
    }

    static constexpr TupleIterator<T> begin(T& t) {
        return TupleRange{t}.begin();
    }

    static constexpr TupleIterator<T> end(T& t) {
        return TupleRange{t}.end();
    }

  private:
    T* tuple_ptr_;
};

}  // namespace tuple_ext
#endif  // BRIANRODRI_TUPLE_ITERATOR_TUPLE_ITERATOR_H
