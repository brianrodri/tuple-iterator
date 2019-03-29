#ifndef BRIANRODRI_TUPLE_ITERATOR_TUPLE_ITERATOR_H
#define BRIANRODRI_TUPLE_ITERATOR_TUPLE_ITERATOR_H
#include <array>
#include <cstddef>
#include <functional>
#include <iterator>
#include <tuple>
#include <utility>
#include <variant>

namespace tuple_ext {
namespace detail {

// Exposes types required by TupleIterator for standards compliance.
//
// The types are derived from TupleLike, which is assumed to satisfy the following:
//   - constexpr auto std::get<I>(std::declval<TupleLike&>()) -> std::tuple_element_t<I, TupleLike>&
//   - constexpr auto std::tuple_size<TupleLike>() -> size_t
//
// std::tuple, std::pair, and std::array define these overloads by default, but you can create
// overloads for your own custom classes as necessary.
template <typename TupleLike>
struct ItrTraitsImpl {
  private:
    template <size_t... I>
    static constexpr auto ValTypeImpl(std::index_sequence<I...> _) ->
        std::variant<std::reference_wrapper<std::tuple_element_t<I, TupleLike>>...>;

  public:
    using ValType = decltype(ValTypeImpl(std::make_index_sequence<std::tuple_size_v<TupleLike>>()));
    using RefType = ValType;
};

// Builds an array of std::get accessors for the given type: TupleLike.
template <typename TupleLike>
struct GettersImpl {
    static constexpr auto MakeGetters() {
        return MakeGettersImpl(std::make_index_sequence<std::tuple_size_v<TupleLike>>());
    }

  private:
    using RefType = typename ItrTraitsImpl<TupleLike>::RefType;
    using GetterPtr = RefType(*)(TupleLike&);
    using GetterArray = std::array<const GetterPtr, std::tuple_size_v<TupleLike>>;

    template <size_t... I>
    static constexpr GetterArray MakeGettersImpl(std::index_sequence<I...> _) {
        return {
            +[](TupleLike& t) constexpr {
                return RefType{std::in_place_index<I>, std::reference_wrapper{std::get<I>(t)}};
            }
            ...  // Expands to one function pointer for each index: I.
        };
    }
};

}  // namespace detail

// Provides interface for creating tuple iterators.
template <typename TupleLike>
class TupleRange;

template <typename TupleLike>
class TupleIterator {
    static constexpr const auto kGetters = detail::GettersImpl<TupleLike>::MakeGetters();
    using GetterItr = typename decltype(kGetters)::const_iterator;

  public:
    using reference = typename detail::ItrTraitsImpl<TupleLike>::RefType;
    using value_type = typename detail::ItrTraitsImpl<TupleLike>::ValType;
    using pointer = typename std::iterator_traits<GetterItr>::pointer;
    using difference_type = typename std::iterator_traits<GetterItr>::difference_type;
    using iterator_category = typename std::iterator_traits<GetterItr>::iterator_category;

    // Provides the interface for creating tuple iterators.
    friend class TupleRange<TupleLike>;

    // Returns a singular iterator, that is, an iterator that is not associated with any tuple.
    // Such instances are semantically equivalent to nullptr, and should therefore never be modified
    // or dereferenced; only reassignment is allowed.
    constexpr TupleIterator() : tuple_ptr_{nullptr}, getter_itr_{std::cend(kGetters)} {}

    ~TupleIterator() = default;
    constexpr TupleIterator(const TupleIterator& src) = default;
    constexpr TupleIterator(TupleIterator&& src) = default;
    constexpr TupleIterator& operator=(const TupleIterator& src) = default;
    constexpr TupleIterator& operator=(TupleIterator&& src) = default;

    constexpr reference operator*() { return (*getter_itr_)(*tuple_ptr_); }
    constexpr reference operator[](difference_type i) { return getter_itr_[i](*tuple_ptr_); }
    // NOTE: operator-> is not defined because there is no way to make it standard compliant.
    //
    // The standard expects that values returned by operator-> may eventually be resolved by 1+
    // repeated applications. However, we can only return a std::variant of pointers. This implies
    // that eventually, a call to std::visit *must be made*.
    //
    // For now, I've chosen to simply leave out the definition of operator-> while keeping the
    // definition of a "pointer"-type because it is required by std::distance.

    constexpr TupleIterator& operator++() { ++getter_itr_; return *this; }
    constexpr TupleIterator& operator--() { --getter_itr_; return *this; }
    constexpr TupleIterator& operator+=(difference_type n) { getter_itr_ += n; return *this; }
    constexpr TupleIterator& operator-=(difference_type n) { getter_itr_ -= n; return *this; }

    // Returns the distance from the given iterator.
    constexpr difference_type operator-(const TupleIterator& rhs) const {
        return getter_itr_ - rhs.getter_itr_;
    }

    // Returns whether this iterator is singular.
    constexpr bool operator==(std::nullptr_t rhs) const { return tuple_ptr_ == rhs; }

    constexpr bool operator==(const TupleIterator& rhs) const {
        return tuple_ptr_ == rhs.tuple_ptr_ && getter_itr_ == rhs.getter_itr_;
    }

    constexpr bool operator<(const TupleIterator& rhs) const {
        return tuple_ptr_ == rhs.tuple_ptr_ ? getter_itr_ < rhs.getter_itr_
                                            : tuple_ptr_ < rhs.tuple_ptr_;
    }

    // All of the following operators are derived from those defined above.

    constexpr reference operator*() const { return *(*this); }
    constexpr reference operator[](difference_type i) const { return (*this)[i]; }

    constexpr TupleIterator operator++(int _) { TupleIterator i{*this}; ++(*this); return i; }
    constexpr TupleIterator operator--(int _) { TupleIterator i{*this}; --(*this); return i; }
    constexpr TupleIterator operator+(difference_type n) const { return TupleIterator{*this} += n; }
    constexpr TupleIterator operator-(difference_type n) const { return TupleIterator{*this} -= n; }

    constexpr bool operator!=(std::nullptr_t rhs) const { return !(*this == rhs); }
    constexpr bool operator!=(const TupleIterator& rhs) const { return !(*this == rhs); }
    constexpr bool operator>(const TupleIterator& rhs) const { return rhs < *this; }
    constexpr bool operator<=(const TupleIterator& rhs) const { return !(rhs < *this); }
    constexpr bool operator>=(const TupleIterator& rhs) const { return !(*this < rhs); }

  private:
    // This constructor is called by: TupleRange<T>.
    constexpr TupleIterator(TupleLike& t, GetterItr i) : tuple_ptr_{&t}, getter_itr_{i} {};

    TupleLike* tuple_ptr_;
    GetterItr getter_itr_;
};

// The following operators are defined as free functions because they can not be defined as members.

template <typename T>
constexpr bool operator==(std::nullptr_t lhs, const TupleIterator<T>& rhs) { return rhs == lhs; }

template <typename T>
constexpr bool operator!=(std::nullptr_t lhs, const TupleIterator<T>& rhs) { return rhs != lhs; }

template <typename T>
constexpr TupleIterator<T> operator+(
        typename TupleIterator<T>::difference_type n, const TupleIterator<T>& i) { return i + n; }

// Provides interface for creating tuple iterators.
template <typename TupleLike>
class TupleRange {
  public:
    constexpr TupleRange(TupleLike& t) : tuple_ref_{t} {}

    constexpr TupleIterator<TupleLike> begin() const {
        return {tuple_ref_, std::cbegin(TupleIterator<TupleLike>::kGetters)};
    }

    constexpr TupleIterator<TupleLike> end() const {
        return {tuple_ref_, std::cend(TupleIterator<TupleLike>::kGetters)};
    }

    static constexpr TupleIterator<TupleLike> begin(TupleLike& t) { return TupleRange{t}.begin(); }
    static constexpr TupleIterator<TupleLike> end(TupleLike& t) { return TupleRange{t}.end(); }

    // Creates a wrapper which will call .get() on the std::reference_wrappers variant returned by
    // TupleIterator.
    template <typename Function>
    static constexpr decltype(auto) MakeVisitor(Function&& f) {
        // Recall: TupleIterators return a variant of std::reference_wrappers.
        using ValType = typename TupleIterator<TupleLike>::value_type;
        return [f_=std::forward<Function>(f)](auto&&... vs) {
            static_assert(std::conjunction_v<std::is_same<std::decay_t<decltype(vs)>, ValType>...>);
            auto visitor = [&](auto&& ref_wrapper) { return f_(ref_wrapper.get()); };
            return std::visit(visitor, std::forward<std::decay_t<decltype(vs)>>(vs)...);
        };
    }

  private:
    TupleLike& tuple_ref_;
};

}  // namespace tuple_ext
#endif  // BRIANRODRI_TUPLE_ITERATOR_TUPLE_ITERATOR_H
