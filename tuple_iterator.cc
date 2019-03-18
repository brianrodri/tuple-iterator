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
template <size_t I> using index_t = std::integral_constant<size_t, I>;
template <size_t I> constexpr auto index_c = index_t<I>{};

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
template <typename Tup> struct IterTypeTraitsImpl;
template <typename... T> struct IterTypeTraitsImpl<std::tuple<T...>> {
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

    // An implementation detail for remembering the current index which a tuple
    // iterator is pointing to.
    using IndexVariant =
        decltype(MakeIndexVariant(std::index_sequence_for<T...>()));
};

}  // namespace detail

template <typename T>
class TupleIterator {
    using index_variant_opt =
        std::optional<typename detail::IterTypeTraitsImpl<T>::IndexVariant>;

  public:
    // Type aliases expected by the standard.
    using value_type = typename detail::IterTypeTraitsImpl<T>::ValueType;
    using reference = typename detail::IterTypeTraitsImpl<T>::ReferenceType;
    using pointer = typename detail::IterTypeTraitsImpl<T>::PointerType;
    using difference_type = ptrdiff_t;

    // NOTE: This can be upgraded to random-access, but more work is required :)
    using iterator_category = std::bidirectional_iterator_tag;

    TupleIterator(const TupleIterator& src)
        : t_ptr_(src.t_ptr_), i_opt_(src.i_opt_) {}

    TupleIterator& operator=(const TupleIterator& src) {
        t_ptr_ = src.t_ptr_;
        i_opt_ = src.i_opt_;
        return *this;
    }

    constexpr TupleIterator& operator++() {
        increment_index();
        return *this;
    }

    constexpr TupleIterator operator++(int _) {
        TupleIterator next_iter = *this;
        ++(*this);
        return next_iter;
    }

    constexpr TupleIterator& operator--() {
        decrement_index();
        return *this;
    }

    constexpr TupleIterator operator--(int _) {
        TupleIterator next_iter = *this;
        ++(*this);
        return next_iter;
    }

    constexpr reference operator*() {
        return std::visit([this](auto i) constexpr {
            return reference(std::reference_wrapper(std::get<i()>(*t_ptr_)));
        }, *i_opt_);
    }

    constexpr reference operator*() const {
        return std::visit([this](auto i) constexpr {
            return reference(std::reference_wrapper(std::get<i()>(*t_ptr_)));
        }, *i_opt_);
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
        return get_index() - rhs.get_index();
    }

    constexpr bool operator==(const TupleIterator& rhs) const {
        return t_ptr_ == rhs.t_ptr_ && i_opt_ == rhs.i_opt_;
    }

    constexpr bool operator!=(const TupleIterator& rhs) const {
        return !(*this == rhs);
    }

  private:
    constexpr TupleIterator(T& t, index_variant_opt i = {})
        : t_ptr_(&t), i_opt_(i) {};
    template <typename U> friend TupleIterator<U> tuple_begin(U& tup);
    template <typename U> friend TupleIterator<U> tuple_end(U& tup);

    constexpr void increment_index() {
        if (i_opt_ != std::nullopt) {
            i_opt_ = std::visit([](auto i) constexpr {
                if constexpr (i + 1 < std::tuple_size_v<T>) {
                    return index_variant_opt(detail::index_c<i + 1>);
                } else {
                    return index_variant_opt(std::nullopt);
                }
            }, *i_opt_);
        }
    }

    constexpr void decrement_index() {
        if (i_opt_ == std::nullopt) {
            i_opt_ = detail::index_c<std::tuple_size_v<T> - 1>;
        } else {
            i_opt_ = std::visit([](auto i) constexpr {
                if constexpr (i > 0) {
                    return index_variant_opt(detail::index_c<i - 1>);
                } else {
                    return index_variant_opt(detail::index_c<0>);
                }
            }, *i_opt_);
        }
    }

    constexpr ptrdiff_t get_index() const {
        return i_opt_ ? i_opt_->index() : std::tuple_size_v<T>;
    }

    index_variant_opt i_opt_;
    T* t_ptr_;
};

template <typename T>
TupleIterator<T> tuple_begin(T& tup) { return {tup, detail::index_c<0>}; }

template <typename T>
TupleIterator<T> tuple_end(T& tup) { return {tup}; }

}  // namespace tuple_ext

int main() {
    using namespace std::string_literals;

    std::tuple t(1, 3.14, "olive"s);  // "olive" is my favorite cat :)
    using tuple_ext::tuple_begin;
    using tuple_ext::tuple_end;

    auto element_printer = [](const auto& e) { std::cout << e.get() << '\n'; };

    std::cout << "# Forward Iteration\n";
    auto b = tuple_begin(t);
    auto e = tuple_end(t);
    while (b != e) {
        std::visit(element_printer, *b++);
    }

    std::cout << "# Backwards Iteration\n";
    b = tuple_begin(t);
    e = tuple_end(t);
    auto n = std::distance(b, e);
    for (int i = 0; i < n; ++i) {
        std::visit(element_printer, *--e);
    }

    std::cout << "# <algorithm> for_each\n";
    std::for_each(
        tuple_begin(t), tuple_end(t), [&](const auto& variant_of_refs) {
            std::visit(element_printer, variant_of_refs);
        });

    std::cout << "# Tuple Iterator size\n";
    std::cout << sizeof(tuple_begin(t)) << '\n';

    return 0;
}
