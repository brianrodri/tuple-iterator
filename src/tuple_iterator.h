#ifndef BRIANRODRI_TUPLE_ITERATOR_TUPLE_ITERATOR_H
#define BRIANRODRI_TUPLE_ITERATOR_TUPLE_ITERATOR_H
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
using index_t = std::integral_constant<size_t, I>;

template <size_t I>
constexpr auto index_c = index_t<I>{};

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
struct IterTypeTraitsImpl;

template <typename... T>
struct IterTypeTraitsImpl<std::tuple<T...>> {
  private:
    // Returns a variant of compile-time index values from the provided sequence
    // of index values.
    template <size_t... I> static constexpr auto
    MakeIndexVariant(std::index_sequence<I...>) -> std::variant<index_t<I>...> {
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
        decltype(MakeIndexVariant(std::index_sequence_for<T...>()));
};

}  // namespace detail

// Provides interface for creating tuple iterators.
template <typename T>
class TupleRange;

template <typename T>
class TupleIterator {
    using IndexVariantOpt =
        std::optional<typename detail::IterTypeTraitsImpl<T>::IndexVariant>;

  public:
    // Type aliases expected by the standard.
    using value_type = typename detail::IterTypeTraitsImpl<T>::ValueType;
    using reference = typename detail::IterTypeTraitsImpl<T>::ReferenceType;
    using pointer = typename detail::IterTypeTraitsImpl<T>::PointerType;
    using difference_type =
        typename detail::IterTypeTraitsImpl<T>::DifferenceType;

    // TODO: Investigate making this iterator random-access.
    using iterator_category = std::bidirectional_iterator_tag;

    TupleIterator() = delete;
    TupleIterator(const TupleIterator<T>& src) = default;
    TupleIterator& operator=(const TupleIterator<T>& src) = default;

    constexpr TupleIterator& operator++() {
        IncrementIndex();
        return *this;
    }

    constexpr TupleIterator operator++(int _) {
        TupleIterator curr_iter{*this};
        ++(*this);
        return curr_iter;
    }

    constexpr TupleIterator& operator--() {
        DecrementIndex();
        return *this;
    }

    constexpr TupleIterator operator--(int _) {
        TupleIterator curr_iter{*this};
        --(*this);
        return curr_iter;
    }

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

    constexpr difference_type operator-(const TupleIterator& rhs) const {
        return ptrdiff_t(index()) - ptrdiff_t(rhs.index());
    }

    constexpr bool operator==(const TupleIterator& rhs) const {
        return tuple_ptr_ == rhs.tuple_ptr_ && index_opt_ == rhs.index_opt_;
    }

    constexpr bool operator!=(const TupleIterator& rhs) const {
        return !(*this == rhs);
    }

    constexpr size_t index() const {
        return IsEnd() ? std::tuple_size_v<T> : index_opt_->index();
    }

  private:
    constexpr TupleIterator(T& t, IndexVariantOpt i = {})
        : tuple_ptr_{&t}, index_opt_{i} {};

    // Provides interface for creating tuple iterators.
    friend class TupleRange<T>;

    constexpr void IncrementIndex() {
        if (!IsEnd()) {
            index_opt_ = std::visit([](auto i) -> IndexVariantOpt {
                if constexpr (i + 1 < std::tuple_size_v<T>) {
                    return {detail::index_c<i + 1>};
                } else {
                    return {};
                }
            }, *index_opt_);
        }
    }

    constexpr void DecrementIndex() {
        if (IsEnd()) {
            // Can iterate backwards from end() when target tuple is non-empty.
            if constexpr (0 < std::tuple_size_v<T>) {
                index_opt_ = detail::index_c<std::tuple_size_v<T> - 1>;
            }
        } else {
            index_opt_ = std::visit([](auto i) -> IndexVariantOpt {
                if constexpr (i > 0) {
                    return {detail::index_c<i - 1>};
                } else {
                    return {detail::index_c<0>};
                }
            }, *index_opt_);
        }
    }

    constexpr bool IsEnd() const { return index_opt_ == std::nullopt; }

    T* tuple_ptr_;
    IndexVariantOpt index_opt_;
};

// Provides interface for creating tuple iterators.
template <typename T>
class TupleRange {
  public:
    constexpr TupleRange(T& t) : tuple_ptr_(&t) {}

    constexpr TupleIterator<T> begin() const {
        if constexpr (0 < std::tuple_size_v<T>) {
            return {*tuple_ptr_, detail::index_c<0>};
        } else {
            return end();
        }
    }

    constexpr TupleIterator<T> end() const {
        return {*tuple_ptr_};
    }

  private:
    T* tuple_ptr_;
};

}  // namespace tuple_ext
#endif  // BRIANRODRI_TUPLE_ITERATOR_TUPLE_ITERATOR_H