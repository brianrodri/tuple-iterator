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

// Helper aliases to help cut down boilerplate when working with compile-time
// index values.
template <size_t I>
using IndexType = std::integral_constant<size_t, I>;

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
  private:
    // Returns a variant of compile-time index values from the provided sequence
    // of index values.
    template <size_t... I> static constexpr auto
    ToIndexVariant(std::index_sequence<I...>) -> std::variant<IndexType<I>...> {
        // NOTE: This function is only inspected at compile-time — never called.
        return {};
    };

  public:
    using PointerType = std::variant<T*...>;
    using ReferenceType = std::variant<std::reference_wrapper<T>...>;
    using ValueType = ReferenceType;
    using DifferenceType = std::ptrdiff_t;

    // An implementation detail for remembering the current index which a tuple
    // iterator is pointing to.
    using IndexVariant =
        decltype(ToIndexVariant(std::index_sequence_for<T...>()));
};

}  // namespace detail

// Provides interface for creating tuple iterators.
template <typename T>
class TupleRange;

template <typename T>
class TupleIterator {
    using IndexVariantOpt =
        std::optional<typename detail::IterTraitsImpl<T>::IndexVariant>;

  public:
    // Type aliases expected by the standard.
    using value_type = typename detail::IterTraitsImpl<T>::ValueType;
    using reference = typename detail::IterTraitsImpl<T>::ReferenceType;
    using pointer = typename detail::IterTraitsImpl<T>::PointerType;
    using difference_type = typename detail::IterTraitsImpl<T>::DifferenceType;
    // TODO: Investigate promoting to random-access.
    using iterator_category = std::bidirectional_iterator_tag;

    // Returns a *singular iterator*, that is, an iterator that is not
    // associated with any tuple. Such instances are semantically equivalent to
    // nullptr, and should therefore never be modified or dereferenced.
    //
    // You can check if an instance is singular by comparing it against nullptr.
    explicit TupleIterator(std::nullptr_t _ = {})
        : tuple_ptr_{nullptr}, index_opt_{std::nullopt} {}

    TupleIterator& operator=(std::nullptr_t _) {
        tuple_ptr_ = nullptr;
        index_opt_ = std::nullopt;
    }

    TupleIterator(const TupleIterator<T>& src) = default;
    TupleIterator(TupleIterator<T>&& src) = default;
    TupleIterator& operator=(const TupleIterator<T>& src) = default;
    TupleIterator& operator=(TupleIterator<T>&& src) = default;

    constexpr TupleIterator& operator++() {
        Increment();
        return *this;
    }

    constexpr TupleIterator operator++(int _) {
        TupleIterator curr_iter{*this};
        Increment();
        return curr_iter;
    }

    constexpr TupleIterator& operator--() {
        Decrement();
        return *this;
    }

    constexpr TupleIterator operator--(int _) {
        TupleIterator curr_iter{*this};
        Decrement();
        return curr_iter;
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

    constexpr reference operator*() {
        return std::visit([this](auto i) -> reference {
            return {std::ref(std::get<i>(*tuple_ptr_))};
        }, *index_opt_);
    }

    constexpr reference operator*() const {
        return std::visit([this](auto i) -> reference {
            return {std::cref(std::get<i>(*tuple_ptr_))};
        }, *index_opt_);
    }

    // Returns the index of the element currently being pointed to by the
    // iterator, or the size of the tuple if the iterator points to it's end.
    constexpr size_t index() const {
        return IsEnd() ? std::tuple_size_v<T> : index_opt_->index();
    }

  private:
    // This constructor will be called by the TupleRange class methods.
    constexpr TupleIterator(T& t, IndexVariantOpt i = {})
        : tuple_ptr_{&t}, index_opt_{i} {};

    // Provides interface for creating tuple iterators.
    friend class TupleRange<T>;

    // Provides interface for comparing tuple iterators.
    template <typename U, typename V>
    friend constexpr bool operator==(const TupleIterator<U>&,
                                     const TupleIterator<V>&);
    template <typename U>
    friend constexpr bool operator==(const TupleIterator<U>&, std::nullptr_t);
    template <typename U>
    friend constexpr bool operator==(std::nullptr_t, const TupleIterator<U>&);

    constexpr void Increment() {
        if (!IsEnd()) {
            index_opt_ = std::visit([](auto i) -> IndexVariantOpt {
                if constexpr (i + 1 < std::tuple_size_v<T>) {
                    return {detail::IndexType<i + 1>{}};
                } else {
                    return {};
                }
            }, *index_opt_);
        }
    }

    constexpr void Decrement() {
        if (IsEnd()) {
            // Can iterate backwards from end() when target tuple is non-empty.
            if constexpr (0 < std::tuple_size_v<T>) {
                index_opt_ = detail::IndexType<std::tuple_size_v<T> - 1>{};
            }
        } else {
            index_opt_ = std::visit([](auto i) -> IndexVariantOpt {
                if constexpr (i > 0) {
                    return {detail::IndexType<i - 1>{}};
                } else {
                    return {detail::IndexType<0>{}};
                }
            }, *index_opt_);
        }
    }

    constexpr bool IsEnd() const { return index_opt_ == std::nullopt; }

    T* tuple_ptr_;
    IndexVariantOpt index_opt_;
};

template <typename T, typename U>
constexpr bool operator==(const TupleIterator<T>& lhs,
                          const TupleIterator<U>& rhs) {
    if constexpr (std::is_same_v<T, U>) {
        return lhs.tuple_ptr_ == rhs.tuple_ptr_ &&
               lhs.index_opt_ == rhs.index_opt_;
    } else {
        return false;
    }
}

template <typename T>
constexpr bool operator==(const TupleIterator<T>& lhs, std::nullptr_t _) {
    return lhs.tuple_ptr_ == nullptr;
}

template <typename T>
constexpr bool operator==(std::nullptr_t _, const TupleIterator<T>& rhs) {
    return rhs.tuple_ptr_ == nullptr;
}

template <typename T, typename U>
constexpr bool operator!=(const TupleIterator<T>& lhs,
                          const TupleIterator<U>& rhs) {
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
        if constexpr (0 < std::tuple_size_v<T>) {
            return {*tuple_ptr_, detail::IndexType<0>{}};
        } else {
            return end();
        }
    }

    constexpr TupleIterator<T> end() const { return {*tuple_ptr_}; }

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
