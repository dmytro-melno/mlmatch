// MIT License

// Copyright (c) 2025 Dmytro Melnychuk

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.


#pragma once

#define func auto
#define let auto

struct wildcard {};

template <typename... case_ts>
struct mlmatch_unhandled_case {};

template <int clause_number, typename... pattern_ts>
struct mlmatch_unreachable_clause {};

template <int clause_number, int expected_patterns, int actual_patterns, typename... pattern_ts>
struct mlmatch_clause_arity_mismatch {};

template <int clause_number, int pattern_position, typename scrutinee_type, typename pattern_type>
struct mlmatch_invalid_pattern {};

namespace  __match_impl {
    template <typename A>
    struct some {
        A v;

        consteval func is_some() -> bool { return true; }
    };
    struct none {
        consteval func is_some() -> bool { return false; }
    };

    // template <typename A> struct is_some {};
    // template <typename A> struct is_some<some<A>> { static constexpr bool value = true; };
    // template <>           struct is_some<none> { static constexpr bool value = false; };

    template <typename A, typename B>
    struct is_same {
        static constexpr bool value = false;
    };

    template <typename A>
    struct is_same<A, A> {
        static constexpr bool value = true;
    };

    template <typename A, typename B>
    static constexpr bool is_same_v = is_same<A, B>::value;

    template <typename...>
    static constexpr bool always_false_v = false;

    template <bool cond, typename true_T, typename false_T>
    struct type_if {
        using type = true_T;
    };

    template <typename true_T, typename false_T>
    struct type_if<false, true_T, false_T> {
        using type = false_T;
    };


    template <typename A, typename B>
    struct tkey_val {
        using key_type = A;
        using val_type = B;

        B val;

        static constexpr func make(B val) -> tkey_val<A, B> { return {val}; }
    };


    template <typename... > struct meta_list;

    template <>
    struct meta_list<> {
        template <typename a>
        using cons_t = meta_list<a>;

        template <typename a>
        using snoc_t = meta_list<a>;

        static constexpr int size = 0;

        static constexpr bool is_empty_t = true;

        template <typename a>
        static constexpr bool mem_t = false;
    };

    template <typename head, typename... tail>
    struct meta_list<head, tail...> {
        using head_type = head;

        template <typename a>
        using cons_t = meta_list<a, head, tail...>;

        template <typename a>
        using snoc_t = meta_list<head, tail..., a>;

        static constexpr int size = sizeof...(tail) + 1;

        static constexpr bool is_empty_t = false;

        template <typename a>
        static constexpr bool mem_t = is_same_v<a, head> || (is_same_v<a, tail> || ...);
    };

    template <typename... >
    struct type_list {
        template <typename... ts1>
        static constexpr auto make(ts1... vs);
    };


    template <>
    struct type_list<> {
        template <typename a>
        using cons_t = type_list<a>;

        static constexpr auto make() {
            return type_list<>::nil();
        }

        template <typename t, typename... ts>
        static constexpr auto make(t el, ts... els) {
            return make(els...).cons(el);
        }

        static constexpr int size = 0;

        static constexpr bool is_empty_t = true;

        constexpr func is_empty() -> bool { return true; }

        template <typename a> static constexpr int get_idx() {
            static_assert(false, "Type `a` not found in type_list");
            return -1;
        }

        template <typename a>
        constexpr none get_opt() { return none{}; }

        static constexpr func nil() -> type_list<> { return type_list<>{}; }
        template <typename a>
        constexpr func cons(a x) -> type_list<a> { return type_list<a>{.x = x, .xs = nil()}; }

        template <typename a> constexpr func insert_if_not_exists(a x) -> type_list<a> {
            return type_list<a>{.x = x, .xs = nil()};
        }

        template <typename F, typename... arg_ts>
        constexpr auto apply(F f, arg_ts... acc) const {
            return f(acc...);
        }

        template <typename F>
        constexpr func iter(F f) -> void {
            return;
        }

        template <typename F>
        static constexpr func iter_t(F f) -> void {
            return;
        }

        template <typename a>
        static constexpr bool mem_t = false;

        template <typename a>
        constexpr bool mem_v(a v) const { return false; }
    };

    template <typename head, typename... tail>
    struct type_list<head, tail...> {
        using head_type = head;
        // using tail_type = tail;

        template <typename a>
        using cons_t = type_list<a, head, tail...>;

        static constexpr int size = sizeof...(tail) + 1;
        head x;
        type_list<tail...> xs;

        static constexpr bool is_empty_t = false;

        constexpr func is_empty() -> bool { return false; }

        template <typename a> static constexpr int get_idx()       { return type_list<tail...>::template get_idx<a>() + 1; }
        template <>           constexpr int get_idx<head>() { return 0; }

        template <typename a> constexpr a    get()       { return type_list<tail...>::template get<a>(); }
        template <>           constexpr head get<head>() { return x; }

        template <typename a, int idx> constexpr a    get_by_idx()          { return xs.template get_by_idx<a, idx - 1>(); }
        template <>                    constexpr head get_by_idx<head, 0>() { return x; }

        template <typename a> constexpr auto get_opt()       { return xs.template get_opt<a>(); }
        template <>           constexpr auto get_opt<head>() { return some{x}; }

        static constexpr func nil() -> type_list<> { return type_list<>{}; }
        template <typename a>
        constexpr func cons(a x) -> type_list<a, head, tail...> { return type_list<a, head, tail...>{.x = x, .xs = *this}; }

        // template <typename a> static constexpr int idx       = type_list<tail...>::template idx<a> + 1;
        // template <>           static constexpr int idx<head> = 0;

        template <typename T> constexpr func insert_if_not_exists(T v) -> auto {
            auto new_xs = xs.template insert_if_not_exists<T>(v);
            return new_xs.cons(x);
        }
        template <> constexpr func insert_if_not_exists<head>(head v) -> auto {
            return *this;
        }

        template <typename F, typename... arg_ts>
        constexpr auto apply(F f, arg_ts... acc) const {
            return xs.apply(f, acc..., x);
        }

        template <typename F>
        constexpr func iter(F f) -> void {
            f(x);
            xs.iter(f);
        }

        template <typename F>
        static constexpr func iter_t(F f) -> void {
            f.template operator()<head>();
            type_list<tail...>::iter_t(f);
        }

        template <typename a>
        static constexpr bool mem_t = is_same_v<a, head> || (is_same_v<a, tail> || ...);

        template <typename a>
        constexpr bool mem_v(a v) const { return v == x || xs.template mem_v<a>(v); }
    };


    template <typename xs_LS, typename YS_LS>
    struct type_list_concat_rev;

    template <typename... xs_ts, typename ys_t, typename... ys_ts>
    struct type_list_concat_rev<type_list<xs_ts...>, type_list<ys_t, ys_ts...>> {
        static constexpr func go(type_list<xs_ts...> xs, type_list<ys_t, ys_ts...> ys) {
            return type_list_concat_rev<type_list<ys_t, xs_ts...>, type_list<ys_ts...>>::go(xs.cons(ys.x), ys.xs);
        }
    };

    template <typename... xs_ts>
    struct type_list_concat_rev<type_list<xs_ts...>, type_list<>> {
        static constexpr func go(type_list<xs_ts...> xs, type_list<> ys) {
            return xs;
        }
    };


    template <typename tkey, typename ts>
    struct type_list_assoc_opt;

    template <typename tkey>
    struct type_list_assoc_opt<tkey, type_list<>> {
        static constexpr auto go(const type_list<>) { return none{}; }
    };

    template <typename tkey, typename val_t, typename... ts>
    struct type_list_assoc_opt<tkey, type_list<tkey_val<tkey, val_t>, ts...>> {
        static constexpr auto go(const type_list<tkey_val<tkey, val_t>, ts...> ls) { return some<val_t>{ls.x.val}; }
    };

    template <typename tkey, typename head, typename... ts>
    struct type_list_assoc_opt<tkey, type_list<head, ts...>> {
        static constexpr auto go(const type_list<head, ts...> ls) { return type_list_assoc_opt<tkey, type_list<ts...>>::go(ls.xs); }
    };


    template <typename tkey, typename ts>
    struct type_list_remove_assoc_impl;

    template <typename tkey>
    struct type_list_remove_assoc_impl<tkey, type_list<>> {
        static constexpr auto go(const type_list<>) { return type_list<>::nil(); }
    };

    template <typename tkey, typename val_t, typename... ts>
    struct type_list_remove_assoc_impl<tkey, type_list<tkey_val<tkey, val_t>, ts...>> {
        static constexpr auto go(const type_list<tkey_val<tkey, val_t>, ts...> ls) { return type_list_remove_assoc_impl<tkey, type_list<ts...>>::go(ls.xs); }
    };

    template <typename tkey, typename head, typename... ts>
    struct type_list_remove_assoc_impl<tkey, type_list<head, ts...>> {
        static constexpr auto go(const type_list<head, ts...> ls) { return type_list_remove_assoc_impl<tkey, type_list<ts...>>::go(ls.xs).cons(ls.x); }
    };

    template <typename tkey, typename ts>
    constexpr auto type_list_remove_assoc(const ts ls) { return type_list_remove_assoc_impl<tkey, ts>::go(ls); }


    template <typename t, t...>
    struct value_list;

    template <typename t>
    struct value_list<t> {
        template <t v>
        using cons_t = value_list<t, v>;

        static constexpr int size = 0;

        static constexpr bool is_empty_t = true;

        template <t v>
        static constexpr bool mem_t = false;

        template <t v>
        static consteval bool mem_v() { return false; }

        template <typename F>
        static consteval void iter(F) {}
    };

    template <typename t, t val, t... vals>
    struct value_list<t, val, vals...> {
        template <t v>
        using cons_t = value_list<t, v, val, vals...>;

        static constexpr int size = sizeof...(vals) + 1;

        static constexpr bool is_empty_t = false;

        template <t v>
        static constexpr bool mem_t = (v == val) || ((v == vals) || ...);

        template <t v>
        static consteval bool mem_v() { return mem_t<v>; }

        template <template <t a> typename F>
        static consteval void iter(F<val> f) {
            f();
            value_list<t, vals...>::iter(f);
        }
    };


    template <typename xs_LS, typename YS_LS>
    struct value_list_concat;

    template <typename t, t... xs_vals, t... ys_vals>
    struct value_list_concat<value_list<t, xs_vals...>, value_list<t, ys_vals...>> {
        using type = value_list<t, xs_vals..., ys_vals...>;
    };



    // Until c++26.
    // template<int len>
    // struct compile_time_str {
    //     char data[len];

    //     static constexpr int slen = len;

    //     static consteval func from_int(unsigned int num) -> compile_time_str<len> {
    //         struct compile_time_str<10> res{};

    //         char* ptr = res.data;
    //         while (num > 0) {
    //             *(--ptr) = '0' + (num % 10);
    //             num /= 10;
    //         }

    //         return res;
    //     }

    //     template<int len1, int len2>
    //     static constexpr compile_time_str<len1 + len2 - 1> concat(const char s1[len1], const char s2[len2]) {
    //         struct compile_time_str<len1 + len2 - 1> res{};

    //         int i = 0;
    //         for (int j = 0; j < len1 - 1; ++j) {
    //             res.data[i++] = s1[j];
    //         }
    //         for (int j = 0; j < len2; ++j) {
    //             res.data[i++] = s2[j];
    //         }

    //         return res;
    //     }
    // };



    template <int... nums>
    using int_list = value_list<int, nums...>;

    template <int n, int... nums>
    struct make_int_list_impl {
        using type = typename make_int_list_impl<n - 1, n - 1, nums...>::type;
    };

    template <int... nums>
    struct make_int_list_impl<0, nums...> {
        using type = int_list<nums...>;
    };

    template <int n>
    using make_int_list = typename make_int_list_impl<n>::type;


    template <typename F>
    struct lambda_clause_helper;

    template <int _cl_idx, typename pats_LS, typename F>
    struct clause {
        //using rty = typename lambda_clause_helper<decltype(&F::operator())>::rty;
        using args_LS = typename lambda_clause_helper<decltype(&F::operator())>::args;
        using done_F = F;
        static constexpr int cl_idx = _cl_idx;

        const F done_f;
    };

    template <typename R, typename C, typename... Args>
    struct lambda_clause_helper<R(C::*)(Args...) const> {
        using rty = R;
        using args = meta_list<Args...>;
    };

    // template<class... Ts> lambda_clause_helper(Ts...) -> lambda_clause_helper<Ts...>;

    template <int cl_idx, typename F>
    constexpr func make_clause(F f) -> clause<cl_idx, typename lambda_clause_helper<decltype(&F::operator())>::args, F> {
        return clause<cl_idx, typename lambda_clause_helper<decltype(&F::operator())>::args, F>{f};
    }

    template <typename F>
    using fn_get_arg_tys = typename lambda_clause_helper<decltype(&F::operator())>::args;


    struct case_tree {
        struct empty {};

        template <typename case_LS>
        struct missing {};

        struct invalid {};

        template <int scrutinee_idx, typename scru_type, typename catch_all_T, typename branches_LS>
        struct split {branches_LS branches; catch_all_T catch_all;};

        template <typename F, typename args_LS>
        struct done {F f;};
    };



    template <typename result_t, typename reached_cls_idxs_LS>
    struct cc_result {
        using reached_cls_idxs = reached_cls_idxs_LS;

        result_t v;
    };

    template <int scrutinee_idx, typename scru_types_LS, typename clauses_LS, typename case_LS = meta_list<>>
    struct cc {
        static constexpr func go(clauses_LS cls) = delete;
    };

    template <typename a>
    struct deref_t {
        using type = a;
    };

    template <typename a>
    struct deref_t<a*> {
        using type = a;
    };

    template <typename scru_t, typename pat_t>
    struct is_same_gadt_family {
        static constexpr bool value = false;
    };

    template <typename scru_t, typename pat_t>
        requires requires {
            typename scru_t::__gadt_family;
            typename pat_t::__gadt_family;
        }
    struct is_same_gadt_family<scru_t, pat_t> {
        static constexpr bool value = is_same_v<typename scru_t::__gadt_family, typename pat_t::__gadt_family>;
    };

    template <typename scru_t, typename pat_t>
    struct is_possible_gadt_ctor {
        static constexpr bool value = false;
    };

    template <typename scru_t, typename pat_t>
        requires requires {
            typename scru_t::__gadt_family;
            typename pat_t::__gadt_family;
            typename pat_t::__gadt_enclosing_t;
            typename pat_t::__gadt_result_t;
        }
    struct is_possible_gadt_ctor<scru_t, pat_t> {
        static constexpr bool value =
            is_same_gadt_family<scru_t, pat_t>::value &&
            is_same_v<scru_t, typename pat_t::__gadt_enclosing_t> &&
            is_same_v<scru_t, typename pat_t::__gadt_result_t>;
    };

    template <typename scru_t, typename pat_t>
    struct is_valid_ctor_pattern {
        static constexpr bool value =
            scru_t::__ctors::template mem_t<pat_t> ||
            is_same_gadt_family<scru_t, pat_t>::value;
    };

    template <typename scru_t, typename pat_t>
    struct is_possible_ctor_pattern {
        static constexpr bool value =
            scru_t::__ctors::template mem_t<pat_t> ||
            is_possible_gadt_ctor<scru_t, pat_t>::value;
    };

    template <typename scru_type, typename pat>
    struct is_impossible_pattern_for_scrutinee {
        using pat_t = typename deref_t<pat>::type;
        using scru_t = typename deref_t<scru_type>::type;

        static constexpr bool is_wildcard_or_scrutinee =
            is_same_v<pat, wildcard> ||
            is_same_v<pat_t, scru_t>;

        static constexpr bool value =
            !is_wildcard_or_scrutinee &&
            is_valid_ctor_pattern<scru_t, pat_t>::value &&
            !is_possible_ctor_pattern<scru_t, pat_t>::value;
    };

    template <typename scru_types_LS, typename pats_LS>
    struct clause_is_impossible {
        static constexpr bool value = false;
    };

    template <typename scru_type, typename... scru_types, typename pat, typename... pats>
    struct clause_is_impossible<type_list<scru_type, scru_types...>, meta_list<pat, pats...>> {
        static constexpr bool value =
            is_impossible_pattern_for_scrutinee<scru_type, pat>::value ||
            clause_is_impossible<type_list<scru_types...>, meta_list<pats...>>::value;
    };

    template <typename scru_t, typename ctors_LS>
    struct gadt_possible_ctors;

    template <typename scru_t>
    struct gadt_possible_ctors<scru_t, meta_list<>> {
        using type = meta_list<>;
    };

    template <typename scru_t, typename ctor_t, typename... ctor_ts>
    struct gadt_possible_ctors<scru_t, meta_list<ctor_t, ctor_ts...>> {
        using tail_type = typename gadt_possible_ctors<scru_t, meta_list<ctor_ts...>>::type;
        using type = typename type_if<
            is_possible_gadt_ctor<scru_t, ctor_t>::value,
            typename tail_type::template cons_t<ctor_t>,
            tail_type
        >::type;
    };

    template <typename scru_type, typename cls_LS, typename head_pats_LS>
    struct cc_cls_get_head_pats;

    template <bool should_add, typename scru_type, typename cls_LS, typename head_pats_LS, typename pat>
    struct cc_cls_get_head_pats_next;

    template <typename scru_type, typename cls_LS, typename head_pats_LS, typename pat>
    struct cc_cls_get_head_pats_next<true, scru_type, cls_LS, head_pats_LS, pat> {
        using pat_t = typename deref_t<pat>::type;
        using type = typename cc_cls_get_head_pats<scru_type, cls_LS, typename head_pats_LS::template cons_t<pat_t>>::type::template cons_t<pat_t>;
    };

    template <typename scru_type, typename cls_LS, typename head_pats_LS, typename pat>
    struct cc_cls_get_head_pats_next<false, scru_type, cls_LS, head_pats_LS, pat> {
        using type = typename cc_cls_get_head_pats<scru_type, cls_LS, head_pats_LS>::type;
    };

    template <typename scru_type, int cl_idx, typename done_F, typename... cls_ts, typename head_pats_LS>
    struct cc_cls_get_head_pats<scru_type, type_list<clause<cl_idx, meta_list<>, done_F>, cls_ts...>, head_pats_LS> {
        static_assert(false, "error: too few patterns, less patterns than scrutinee values.");
    };

    template <typename scru_type, int cl_idx, typename pat, typename... pats, typename done_F, typename... cls_ts, typename head_pats_LS>
    struct cc_cls_get_head_pats<scru_type, type_list<clause<cl_idx, meta_list<pat, pats...>, done_F>, cls_ts...>, head_pats_LS> {
        // static_assert(
        //     is_same_v<pat, wildcard> || is_same_v<typename pat::__datacon_ty, scru_type>,
        //     "error: pattern must be either 'wildcard' or one of constructors of the type of value in scrutinee."
        // );

        using pat_t = typename deref_t<pat>::type;
        using scru_t = typename deref_t<scru_type>::type;

        static constexpr bool should_add =
            !is_same_v<pat_t, scru_t> &&
            !is_same_v<pat, wildcard> &&
            is_possible_ctor_pattern<scru_t, pat_t>::value &&
            !head_pats_LS::template mem_t<pat_t>;

        using type = typename cc_cls_get_head_pats_next<
            should_add,
            scru_type,
            type_list<cls_ts...>,
            head_pats_LS,
            pat
        >::type;
    };

    template <typename scru_type, typename head_pats_LS>
    struct cc_cls_get_head_pats<scru_type, type_list<>, head_pats_LS> {
        using type = meta_list<>;
    };

    template <typename head_pat, typename scru_type, typename cls_LS>
    struct cc_specialize;

    template <typename head_pat, typename scru_type, int cl_idx, typename pat, typename... pats, typename done_F, typename... cls_ts>
    struct cc_specialize<head_pat, scru_type, type_list<clause<cl_idx, meta_list<pat, pats...>, done_F>, cls_ts...>> {
        static constexpr func go(type_list<clause<cl_idx, meta_list<pat, pats...>, done_F>, cls_ts...> cls) {
            if constexpr (
                is_same_v<typename deref_t<pat>::type, typename deref_t<head_pat>::type> ||
                is_same_v<pat, wildcard> ||
                is_same_v<typename deref_t<pat>::type, typename deref_t<scru_type>::type>
            ) {
                return cc_specialize<head_pat, scru_type, type_list<cls_ts...>>::go(cls.xs)
                    .cons(clause<cl_idx, meta_list<pats...>, done_F>{cls.x.done_f});
            } else {
                return cc_specialize<head_pat, scru_type, type_list<cls_ts...>>::go(cls.xs);
            }
        }
    };

    template <typename head_pat, typename scru_type>
    struct cc_specialize<head_pat, scru_type, type_list<>> {
        static constexpr func go(type_list<> cls) {
            return type_list<>::nil();
        }
    };

    template <typename scru_type, typename cls_LS>
    struct cc_default;

    template <typename scru_type, int cl_idx, typename pat, typename... pats, typename done_F, typename... cls_ts>
    struct cc_default<scru_type, type_list<clause<cl_idx, meta_list<pat, pats...>, done_F>, cls_ts...>> {
        static constexpr func go(type_list<clause<cl_idx, meta_list<pat, pats...>, done_F>, cls_ts...> cls) {
            if constexpr (
                is_same_v<pat, wildcard> ||
                is_same_v<typename deref_t<pat>::type, typename deref_t<scru_type>::type>
            ) {
                return cc_default<scru_type, type_list<cls_ts...>>::go(cls.xs)
                    .cons(clause<cl_idx, meta_list<pats...>, done_F>{cls.x.done_f});
            } else {
                return cc_default<scru_type, type_list<cls_ts...>>::go(cls.xs);
            }
        }
    };

    template <typename scru_type>
    struct cc_default<scru_type, type_list<>> {
        static constexpr func go(type_list<> cls) {
            return type_list<>::nil();
        }
    };


    template <typename ctor_LS, typename excluded_ctors_LS>
    struct first_ctor_not_in;

    template <bool is_excluded, typename head_ctor, typename tail_ctors_LS, typename excluded_ctors_LS>
    struct first_ctor_not_in_next;

    template <typename head_ctor, typename tail_ctors_LS, typename excluded_ctors_LS>
    struct first_ctor_not_in_next<false, head_ctor, tail_ctors_LS, excluded_ctors_LS> {
        using type = head_ctor;
    };

    template <typename head_ctor, typename tail_ctors_LS, typename excluded_ctors_LS>
    struct first_ctor_not_in_next<true, head_ctor, tail_ctors_LS, excluded_ctors_LS> {
        using type = typename first_ctor_not_in<tail_ctors_LS, excluded_ctors_LS>::type;
    };

    template <typename head_ctor, typename... tail_ctors, typename excluded_ctors_LS>
    struct first_ctor_not_in<meta_list<head_ctor, tail_ctors...>, excluded_ctors_LS> {
        using type = typename first_ctor_not_in_next<
            excluded_ctors_LS::template mem_t<head_ctor>,
            head_ctor,
            meta_list<tail_ctors...>,
            excluded_ctors_LS
        >::type;
    };


    template <typename case_LS, typename scru_types_LS>
    struct complete_case_with_defaults;

    template <typename case_LS>
    struct complete_case_with_defaults<case_LS, type_list<>> {
        using type = case_LS;
    };

    template <typename case_LS, typename scru_type, typename... scru_types>
    struct complete_case_with_defaults<case_LS, type_list<scru_type, scru_types...>> {
        using default_pat = typename deref_t<scru_type>::type::__ctors::head_type;
        using next_case = typename case_LS::template snoc_t<default_pat>;
        using type = typename complete_case_with_defaults<next_case, type_list<scru_types...>>::type;
    };


    template <typename head_pats_LS, int scrutinee_idx, typename scru_types_LS, typename cls_LS, typename case_LS>
    struct cc_split;

    template<int scrutinee_idx, typename scru_types_LS, typename cls_LS, typename case_LS>
    struct cc_split<meta_list<>, scrutinee_idx, scru_types_LS, cls_LS, case_LS> {
        static constexpr func go(cls_LS cls) {
            return cc_result<type_list<>, int_list<>>{type_list<>::nil()};
        }
    };

    template<typename head_pat, typename... head_pats, int scrutinee_idx, typename scru_type, typename... scru_types, typename cls_LS, typename case_LS>
    struct cc_split<meta_list<head_pat, head_pats...>, scrutinee_idx, type_list<scru_type, scru_types...>, cls_LS, case_LS> {
        static constexpr func go(cls_LS cls) {
            let s_cls = cc_specialize<head_pat, scru_type, cls_LS>::go(cls);
            let res_ct_next = cc<
                scrutinee_idx + 1,
                type_list<scru_types...>,
                decltype(s_cls),
                typename case_LS::template snoc_t<head_pat>
            >::go(s_cls);
            let res_branches = cc_split<
                meta_list<head_pats...>,
                scrutinee_idx,
                type_list<scru_type, scru_types...>,
                cls_LS,
                case_LS
            >::go(cls);
            let branches = res_branches.v.cons(tkey_val<head_pat, decltype(res_ct_next.v)>::make(res_ct_next.v));
            using reached_cls_ids = value_list_concat<
                                        typename decltype(res_ct_next)::reached_cls_idxs,
                                        typename decltype(res_branches)::reached_cls_idxs>::type;
            return cc_result<decltype(branches), reached_cls_ids>{branches};
        }
    };

    template <int scrutinee_idx, typename scru_types_LS, typename cls_LS, typename head_pats_LS, typename case_LS>
    struct cc_catch_all;

    template <int scrutinee_idx, typename scru_type, typename... scru_types, typename cls_LS, typename head_pats_LS, typename case_LS>
    struct cc_catch_all<scrutinee_idx, type_list<scru_type, scru_types...>, cls_LS, head_pats_LS, case_LS> {
        static constexpr func go(cls_LS cls) {
            using default_pat = typename first_ctor_not_in<typename deref_t<scru_type>::type::__ctors, head_pats_LS>::type;
            let d_cls = cc_default<scru_type, cls_LS>::go(cls);
            return cc<
                scrutinee_idx + 1,
                type_list<scru_types...>,
                decltype(d_cls),
                typename case_LS::template snoc_t<default_pat>
            >::go(d_cls);
        }
    };

    template <int scrutinee_idx, typename case_LS>
    struct cc<scrutinee_idx, type_list<>, type_list<>, case_LS> {
        static constexpr func go(type_list<> cls) {
            return cc_result<case_tree::missing<case_LS>, int_list<>>{case_tree::missing<case_LS>{}};
        }
    };

    template <int scrutinee_idx, typename scru_type, typename... scru_types, typename case_LS>
    struct cc<scrutinee_idx, type_list<scru_type, scru_types...>, type_list<>, case_LS> {
        static constexpr func go(type_list<> cls) {
            using scru_types_LS = type_list<scru_type, scru_types...>;
            using missing_case = typename complete_case_with_defaults<case_LS, scru_types_LS>::type;
            return cc_result<case_tree::missing<missing_case>, int_list<>>{case_tree::missing<missing_case>{}};
        }
    };

    template <int scrutinee_idx, int cl_idx, typename pats_LS, typename done_F, typename... cls_ts, typename case_LS>
    struct cc<scrutinee_idx, type_list<>, type_list<clause<cl_idx, pats_LS, done_F>, cls_ts...>, case_LS> {
        static constexpr func go(type_list<clause<cl_idx, pats_LS, done_F>, cls_ts...> cls) {
            static_assert(pats_LS::is_empty_t, "error: excessive patterns. more pattern than scrutinee values.");
            return cc_result<case_tree::done<done_F, fn_get_arg_tys<done_F>>, int_list<cl_idx>>{
                case_tree::done<done_F, fn_get_arg_tys<done_F>>{cls.x.done_f}};
        }
    };

    template <int scrutinee_idx, typename scru_type, typename... scru_types, typename cls_LS, typename case_LS>
    struct cc<scrutinee_idx, type_list<scru_type, scru_types...>, cls_LS, case_LS> {
        static constexpr func go(cls_LS cls) {
            using head_pats = cc_cls_get_head_pats<scru_type, cls_LS, meta_list<>>::type;
            if constexpr (head_pats::is_empty_t) {
                return cc_catch_all<scrutinee_idx, type_list<scru_type, scru_types...>, cls_LS, head_pats, case_LS>::go(cls);
            } else {
                let res_branches = cc_split<head_pats, scrutinee_idx, type_list<scru_type, scru_types...>, cls_LS, case_LS>::go(cls);
                let res_catch_all = [=](){
                    if constexpr (decltype(res_branches.v)::size == deref_t<scru_type>::type::ctors_count) {
                        return cc_result<case_tree::empty, int_list<>>{case_tree::empty{}};
                    } else {
                        return cc_catch_all<scrutinee_idx, type_list<scru_type, scru_types...>, cls_LS, head_pats, case_LS>::go(cls);
                    }
                }();
                using reached_cls_ids = value_list_concat<
                                            typename decltype(res_branches)::reached_cls_idxs,
                                            typename decltype(res_catch_all)::reached_cls_idxs>::type;
                return cc_result<case_tree::split<scrutinee_idx, scru_type, decltype(res_catch_all.v), decltype(res_branches.v)>, reached_cls_ids> {
                    {
                        .branches = res_branches.v,
                        .catch_all = res_catch_all.v
                    }
                };
            }
        }
    };

    template <int expected_count, typename clauses_LS>
    struct first_clause_arity_mismatch {
        static constexpr bool has_mismatch = false;
        using type = none;
    };

    template <int expected_count, int cl_idx, typename pats_LS, typename done_F, typename... cl_ts>
    struct first_clause_arity_mismatch<expected_count, type_list<clause<cl_idx, pats_LS, done_F>, cl_ts...>> {
        using tail_mismatch = first_clause_arity_mismatch<expected_count, type_list<cl_ts...>>;

        static constexpr bool head_mismatch = pats_LS::size != expected_count;
        static constexpr bool has_mismatch = head_mismatch || tail_mismatch::has_mismatch;
        using type = typename type_if<
            head_mismatch,
            clause<cl_idx, pats_LS, done_F>,
            typename tail_mismatch::type
        >::type;
    };

    template <int expected_count, typename cl_T>
    struct report_clause_arity_mismatch;

    template <int expected_count, int cl_idx, typename... pats, typename done_F>
    struct report_clause_arity_mismatch<expected_count, clause<cl_idx, meta_list<pats...>, done_F>> {
        static constexpr func go() -> void {
            static_assert(
                always_false_v<mlmatch_clause_arity_mismatch<cl_idx + 1, expected_count, sizeof...(pats), pats...>>,
                "mlmatch error: clause has the wrong number of patterns; the 1-based clause index, expected count, actual count, and patterns are shown in the failed requirement as mlmatch_clause_arity_mismatch<...>."
            );
        }
    };


    template <int cl_idx, int pat_idx, typename scru_type, typename pat>
    struct invalid_pattern_info {};

    template <int cl_idx, int pat_idx, typename scru_types_LS, typename pats_LS>
    struct first_invalid_pattern_in_clause {
        static constexpr bool has_invalid = false;
        using type = none;
    };

    template <int cl_idx, int pat_idx, typename scru_type, typename... scru_types, typename pat, typename... pats>
    struct first_invalid_pattern_in_clause<cl_idx, pat_idx, type_list<scru_type, scru_types...>, meta_list<pat, pats...>> {
        using pat_t = typename deref_t<pat>::type;
        using scru_t = typename deref_t<scru_type>::type;
        using tail_invalid = first_invalid_pattern_in_clause<cl_idx, pat_idx + 1, type_list<scru_types...>, meta_list<pats...>>;

        static constexpr bool head_invalid =
            !is_same_v<pat, wildcard> &&
            !is_same_v<pat_t, scru_t> &&
            !is_valid_ctor_pattern<scru_t, pat_t>::value;
        static constexpr bool has_invalid = head_invalid || tail_invalid::has_invalid;
        using type = typename type_if<
            head_invalid,
            invalid_pattern_info<cl_idx, pat_idx, scru_type, pat>,
            typename tail_invalid::type
        >::type;
    };

    template <typename scru_types_LS, typename clauses_LS>
    struct first_invalid_pattern {
        static constexpr bool has_invalid = false;
        using type = none;
    };

    template <typename scru_types_LS, int cl_idx, typename pats_LS, typename done_F, typename... cl_ts>
    struct first_invalid_pattern<scru_types_LS, type_list<clause<cl_idx, pats_LS, done_F>, cl_ts...>> {
        using head_invalid = first_invalid_pattern_in_clause<cl_idx, 0, scru_types_LS, pats_LS>;
        using tail_invalid = first_invalid_pattern<scru_types_LS, type_list<cl_ts...>>;

        static constexpr bool has_invalid = head_invalid::has_invalid || tail_invalid::has_invalid;
        using type = typename type_if<
            head_invalid::has_invalid,
            typename head_invalid::type,
            typename tail_invalid::type
        >::type;
    };

    template <typename invalid_pattern_info_T>
    struct report_invalid_pattern;

    template <int cl_idx, int pat_idx, typename scru_type, typename pat>
    struct report_invalid_pattern<invalid_pattern_info<cl_idx, pat_idx, scru_type, pat>> {
        static constexpr func go() -> void {
            static_assert(
                always_false_v<mlmatch_invalid_pattern<cl_idx + 1, pat_idx + 1, scru_type, pat>>,
                "mlmatch error: invalid pattern type; each pattern must be _, the scrutinee type, or one of the scrutinee constructors. The 1-based clause index, 1-based pattern position, scrutinee type, and pattern type are shown in the failed requirement as mlmatch_invalid_pattern<...>."
            );
        }
    };


    template <typename case_tree_T>
    struct case_tree_first_missing {
        static constexpr bool has_missing = false;
        using type = meta_list<>;
    };

    template <typename case_LS>
    struct case_tree_first_missing<case_tree::missing<case_LS>> {
        static constexpr bool has_missing = true;
        using type = case_LS;
    };

    template <typename branches_LS>
    struct case_tree_branches_first_missing {
        static constexpr bool has_missing = false;
        using type = meta_list<>;
    };

    template <typename branch_key, typename branch_T, typename... branch_ts>
    struct case_tree_branches_first_missing<type_list<tkey_val<branch_key, branch_T>, branch_ts...>> {
        using head_missing = case_tree_first_missing<branch_T>;
        using tail_missing = case_tree_branches_first_missing<type_list<branch_ts...>>;

        static constexpr bool has_missing = head_missing::has_missing || tail_missing::has_missing;
        using type = typename type_if<
            head_missing::has_missing,
            typename head_missing::type,
            typename tail_missing::type
        >::type;
    };

    template <int scrutinee_idx, typename scru_type, typename catch_all_T, typename branches_LS>
    struct case_tree_first_missing<case_tree::split<scrutinee_idx, scru_type, catch_all_T, branches_LS>> {
        using branch_missing = case_tree_branches_first_missing<branches_LS>;
        using catch_all_missing = case_tree_first_missing<catch_all_T>;

        static constexpr bool has_missing = branch_missing::has_missing || catch_all_missing::has_missing;
        using type = typename type_if<
            branch_missing::has_missing,
            typename branch_missing::type,
            typename catch_all_missing::type
        >::type;
    };


    template <typename case_LS>
    struct report_unhandled_case;

    template <typename... case_ts>
    struct report_unhandled_case<meta_list<case_ts...>> {
        static constexpr func go() -> void {
            static_assert(
                always_false_v<mlmatch_unhandled_case<case_ts...>>,
                "mlmatch error: pattern match is not exhaustive; an unhandled case is shown in the failed requirement as mlmatch_unhandled_case<...>."
            );
        }
    };

    template <typename reached_cls_idxs_LS, typename scru_types_LS, typename clauses_LS>
    struct first_unreachable_clause {
        static constexpr bool has_unreachable = false;
        using type = none;
    };

    template <typename reached_cls_idxs_LS, typename scru_types_LS, typename cl_T, typename... cl_ts>
    struct first_unreachable_clause<reached_cls_idxs_LS, scru_types_LS, type_list<cl_T, cl_ts...>> {
        using tail_unreachable = first_unreachable_clause<reached_cls_idxs_LS, scru_types_LS, type_list<cl_ts...>>;

        static constexpr bool head_impossible = clause_is_impossible<scru_types_LS, typename cl_T::args_LS>::value;
        static constexpr bool head_unreachable = !head_impossible && !reached_cls_idxs_LS::template mem_t<cl_T::cl_idx>;
        static constexpr bool has_unreachable = head_unreachable || tail_unreachable::has_unreachable;
        using type = typename type_if<
            head_unreachable,
            cl_T,
            typename tail_unreachable::type
        >::type;
    };

    template <typename cl_T>
    struct report_unreachable_clause;

    template <int cl_idx, typename... pats, typename done_F>
    struct report_unreachable_clause<clause<cl_idx, meta_list<pats...>, done_F>> {
        static constexpr func go() -> void {
            static_assert(
                always_false_v<mlmatch_unreachable_clause<cl_idx + 1, pats...>>,
                "mlmatch error: unreachable clause; the 1-based clause index and its patterns are shown in the failed requirement as mlmatch_unreachable_clause<...>."
            );
        }
    };


    template <typename scru_types_LS, typename clauses_LS>
    struct compile_clauses {
        static constexpr func go(clauses_LS cls) {
            using arity_mismatch = first_clause_arity_mismatch<scru_types_LS::size, clauses_LS>;
            if constexpr (arity_mismatch::has_mismatch) {
                report_clause_arity_mismatch<scru_types_LS::size, typename arity_mismatch::type>::go();
                return case_tree::invalid{};
            } else {
                using invalid_pattern = first_invalid_pattern<scru_types_LS, clauses_LS>;
                if constexpr (invalid_pattern::has_invalid) {
                    report_invalid_pattern<typename invalid_pattern::type>::go();
                    return case_tree::invalid{};
                } else {
                    let res = cc<0, scru_types_LS, clauses_LS>::go(cls);

                    using missing = case_tree_first_missing<decltype(res.v)>;
                    if constexpr (missing::has_missing) {
                        report_unhandled_case<typename missing::type>::go();
                    }

                    using unreachable = first_unreachable_clause<typename decltype(res)::reached_cls_idxs, scru_types_LS, decltype(cls)>;
                    if constexpr (unreachable::has_unreachable) {
                        report_unreachable_clause<typename unreachable::type>::go();
                    }

                    return res.v;
                }
            }
        }
        // private:
        // static consteval func get_error_str(int cl_idx) {
        //     let n_str = compile_time_str<10>::from_int(cl_idx);
        //     let s1 = "error: clause ";
        //     let s2 = compile_time_str<0>::concat<sizeof((const char*)s1), 10>(s1, n_str.data);
        //     let s3 = "is unreachable ";
        //     let s4 = compile_time_str<0>::concat<decltype(s2)::slen, sizeof((const char*)s3)>(s2.data, s3);
        //     return s4
        // }
    };


    template <typename F, typename scrutinee_LS, typename args_LS, typename... arg_acc_ts>
    struct apply_scrutinee;

    template <typename F, typename... arg_acc_ts>
    struct apply_scrutinee<F, type_list<>, meta_list<>, arg_acc_ts...> {
        static constexpr func go(F f, type_list<> scrutinee_ls, arg_acc_ts... acc) {
            return f(acc...);
        }
    };

    template <typename F, typename scru_type, typename... scru_types, typename arg_t, typename... arg_ts, typename... arg_acc_ts>
    struct apply_scrutinee<F, type_list<scru_type*, scru_types...>, meta_list<arg_t, arg_ts...>, arg_acc_ts...> {
        static constexpr func go(F f, type_list<scru_type*, scru_types...> scrutinee_ls, arg_acc_ts... acc) {
            return apply_scrutinee<F, type_list<scru_types...>, meta_list<arg_ts...>, arg_acc_ts..., arg_t>::go(
                f, scrutinee_ls.xs, acc..., scrutinee_ls.x->template as<arg_t>());
        }
    };

    template <typename F, typename scru_type, typename... scru_types, typename arg_t, typename... arg_ts, typename... arg_acc_ts>
    struct apply_scrutinee<F, type_list<scru_type, scru_types...>, meta_list<arg_t, arg_ts...>, arg_acc_ts...> {
        static constexpr func go(F f, type_list<scru_type, scru_types...> scrutinee_ls, arg_acc_ts... acc) {
            return apply_scrutinee<F, type_list<scru_types...>, meta_list<arg_ts...>, arg_acc_ts..., arg_t>::go(
                f, scrutinee_ls.xs, acc..., scrutinee_ls.x.template as<arg_t>());
        }
    };

    template <typename case_tree_T, typename scrutinee_LS>
    struct case_tree_gen_switch_tree {
        static constexpr func go(case_tree::empty ct, scrutinee_LS scrutinee_ls) = delete;
    };

    template <typename scrutinee_LS>
    struct case_tree_gen_switch_tree<case_tree::empty, scrutinee_LS> {
        static constexpr func go(case_tree::empty ct, scrutinee_LS scrutinee_ls) {
            static_assert(false, "error: unhandled case.");
        }
    };

    template <int scrutinee_idx, typename scru_type, typename catch_all_T, typename branches_LS, typename scrutinee_LS>
    struct case_tree_gen_switch_tree<case_tree::split<scrutinee_idx, scru_type*, catch_all_T, branches_LS>, scrutinee_LS> {
        static constexpr func go(case_tree::split<scrutinee_idx, scru_type*, catch_all_T, branches_LS> ct_split, scrutinee_LS scrutinee_ls) {
            return scru_type::template __elim_ptr<scrutinee_idx, catch_all_T, branches_LS, scrutinee_LS>(ct_split, scrutinee_ls);
        }
    };

    template <int scrutinee_idx, typename scru_type, typename catch_all_T, typename branches_LS, typename scrutinee_LS>
    struct case_tree_gen_switch_tree<case_tree::split<scrutinee_idx, scru_type, catch_all_T, branches_LS>, scrutinee_LS> {
        static constexpr func go(case_tree::split<scrutinee_idx, scru_type, catch_all_T, branches_LS> ct_split, scrutinee_LS scrutinee_ls) {
            return scru_type::template __elim<scrutinee_idx, catch_all_T, branches_LS, scrutinee_LS>(ct_split, scrutinee_ls);
        }
    };

    template <typename F, typename args_LS, typename scrutinee_LS>
    struct case_tree_gen_switch_tree<case_tree::done<F, args_LS>, scrutinee_LS> {
        static constexpr func go(case_tree::done<F, args_LS> done, scrutinee_LS scrutinee_ls) {
            return apply_scrutinee<F, scrutinee_LS, args_LS>::go(done.f, scrutinee_ls);
        }
    };


    template <typename tkey, int scrutinee_idx, typename scru_type, typename catch_all_T, typename branches_LS>
    static constexpr func case_tree_split_get_branch(case_tree::split<scrutinee_idx, scru_type, catch_all_T, branches_LS> ct_split) {
        let branch = type_list_assoc_opt<tkey, branches_LS>::go(ct_split.branches);
        if constexpr (branch.is_some()) {
            return branch.v;
        } else {
            return ct_split.catch_all;
        }
    }

    template <typename tkey, int scrutinee_idx, typename scru_type, typename catch_all_T, typename branches_LS, typename scrutinee_LS>
    static constexpr func case_tree_gen_branch(case_tree::split<scrutinee_idx, scru_type, catch_all_T, branches_LS> ct_split, scrutinee_LS scrutinee_ls) {
        let branch = case_tree_split_get_branch<tkey>(ct_split);
        return case_tree_gen_switch_tree<decltype(branch), scrutinee_LS>::go(branch, scrutinee_ls);
    }

    struct invalid_match_expression {
        template <typename result_t>
        constexpr operator result_t() const {
            __builtin_unreachable();
        }
    };
};


template <typename... scru_types>
struct match {
    match(scru_types... scru_vals) : scrutinee_ls(__match_impl::type_list<>::make(scru_vals...)) {}

    template <typename... clause_fn_ts>
    constexpr func with(clause_fn_ts... clause_fns) {
        let cls = make_cls_impl(__match_impl::make_int_list<sizeof...(clause_fn_ts)>{}, clause_fns...);
        let ct = __match_impl::compile_clauses<__match_impl::type_list<scru_types...>, decltype(cls)>::go(cls);
        if constexpr (
            __match_impl::is_same_v<decltype(ct), __match_impl::case_tree::invalid> ||
            __match_impl::case_tree_first_missing<decltype(ct)>::has_missing
        ) {
            return __match_impl::invalid_match_expression{};
        } else {
            return __match_impl::case_tree_gen_switch_tree<decltype(ct), __match_impl::type_list<scru_types...>>::go(ct, scrutinee_ls);
        }
    }
private:
    __match_impl::type_list<scru_types...> scrutinee_ls;

    template <int... cl_idxs, typename... clause_fn_ts>
    static constexpr auto make_cls_impl(__match_impl::int_list<cl_idxs...>, clause_fn_ts... clause_fns) {
        return __match_impl::type_list<>::make(__match_impl::make_clause<cl_idxs>(clause_fns)...);
    }

};

using _ = wildcard;

#undef func
#undef let


template<typename ...Args>
constexpr auto __tagged_va_count(Args&&...) { return sizeof...(Args); }

#define __tagged_XCAT2(a, b) a##b
#define __tagged_XCAT(a, b) __tagged_XCAT2(a, b)

#define __tagged_XEXPAND(x) x
#define __tagged_make_name(x) __tagged_XCAT(make_, __tagged_XEXPAND(__tagged_fst x))

#define __tagged_make_fields(...)                                    \
  __VA_OPT__(__tagged_make_fields_HELPER(__VA_ARGS__))
#define __tagged_make_fields_HELPER(a1, ...)                         \
  a1;                                                     \
  __VA_OPT__(__tagged_make_fields_AGAIN PARENS (__VA_ARGS__))
#define __tagged_make_fields_AGAIN() __tagged_make_fields_HELPER

#define __tagged_make_va_count_dummies(...)                                    \
  __VA_OPT__(__tagged_make_va_count_dummies_HELPER(__VA_ARGS__))
#define __tagged_make_va_count_dummies_HELPER(a1, ...)                         \
  0                                                     \
  __VA_OPT__(,) \
  __VA_OPT__(__tagged_make_va_count_dummies_AGAIN PARENS (__VA_ARGS__))
#define __tagged_make_va_count_dummies_AGAIN() __tagged_make_va_count_dummies_HELPER

#define __tagged_fst(f, s) f
#define __tagged_snd(f, s) s
#define __tagged_pick_1(f, s, t) f
#define __tagged_pick_2(f, s, t) s
#define __tagged_pick_3(f, s, t) t
#define __tagged_unparen(...) __VA_ARGS__
#define __tagged_gadt_result_type(f, s, t) __tagged_unparen t
#define __tagged_fst_comma_sep(x) __tagged_fst x,
#define __tagged_fst_comma_sep2(x) __tagged_fst x{}
#define __tagged_fst_type(x) __tagged_fst x
#define __tagged_pick_1_comma_sep(x) __tagged_pick_1 x,
#define __tagged_pick_1_type(x) __tagged_pick_1 x

#define __tagged_part1_1(x) x;
#define __tagged_part1(x) \
    struct __tagged_fst x { \
        __tagged_make_fields __tagged_snd x \
        \
        operator self_t() const { return self_t::from(*this); } \
        \
        template <typename... ts> \
        static auto make(ts... args) -> self_t { \
            return self_t{.tag = tag_t::__tagged_fst x, .d = {.__tagged_fst x = __tagged_fst x{args...}}}; \
        } \
    };

#define __tagged_part2_20(x) [[no_unique_address]] __tagged_fst x __tagged_fst x;

#define __tagged_part3(x) template <> constexpr tag_t tag_of<__tagged_fst x> = tag_t::__tagged_fst x;

#define __tagged_part4(x) \
    template <> inline auto as<__tagged_fst x>() -> __tagged_fst x { return d.__tagged_fst x; }; \
    template <> inline auto as<__tagged_fst x *>() -> __tagged_fst x * { return &d.__tagged_fst x; };

#define __tagged_part5(x) \
    case tag_t::__tagged_fst x: return __match_impl::case_tree_gen_branch<__tagged_fst x>(ct_split, scrutinee_ls);

#define __tagged_part5_ptr(x) \
    case tag_t::__tagged_fst x: return __match_impl::case_tree_gen_branch<__tagged_fst x>(ct_split, scrutinee_ls);

#define __tagged_part6(x) \
    template<> inline constexpr auto from<__tagged_fst x>(__tagged_fst x v) -> self_t { \
        return {.tag = tag_t::__tagged_fst x, .d = {.__tagged_fst x = v}}; \
    }


#define PARENS ()

#define EXPAND(...) EXPAND4(EXPAND4(EXPAND4(EXPAND4(__VA_ARGS__))))
#define EXPAND4(...) EXPAND3(EXPAND3(EXPAND3(EXPAND3(__VA_ARGS__))))
#define EXPAND3(...) EXPAND2(EXPAND2(EXPAND2(EXPAND2(__VA_ARGS__))))
#define EXPAND2(...) EXPAND1(EXPAND1(EXPAND1(EXPAND1(__VA_ARGS__))))
#define EXPAND1(...) __VA_ARGS__

// #define __tagged_FOR_EACH2(m, ty, ...) \
//     __VA_OPT__(EXPAND(__tagged_FOR_EACH2_IMPL(m, ty, __VA_ARGS__)))
// #define __tagged_FOR_EACH2_IMPL(m, ty, a1, ...) \
//     m(ty, a1) \
//     __VA_OPT__(__tagged_FOR_EACH2_AGAIN PARENS (m, ty, __VA_ARGS__))
// #define __tagged_FOR_EACH2_AGAIN() __tagged_FOR_EACH2_IMPL

#define __tagged_FOR_EACH(macro, ...)                                    \
  __VA_OPT__(EXPAND(__tagged_FOR_EACH_HELPER(macro, __VA_ARGS__)))
#define __tagged_FOR_EACH_HELPER(macro, a1, ...)                         \
  macro(a1)                                                     \
  __VA_OPT__(__tagged_FOR_EACH_AGAIN PARENS (macro, __VA_ARGS__))
#define __tagged_FOR_EACH_AGAIN() __tagged_FOR_EACH_HELPER

#define __tagged_FOR_EACH_comma(macro, ...)                                    \
  __VA_OPT__(EXPAND(__tagged_FOR_EACH_comma_HELPER(macro, __VA_ARGS__)))
#define __tagged_FOR_EACH_comma_HELPER(macro, a1, ...)                         \
  macro(a1)                                                     \
  __VA_OPT__(,) \
  __VA_OPT__(__tagged_FOR_EACH_comma_AGAIN PARENS (macro, __VA_ARGS__))
#define __tagged_FOR_EACH_comma_AGAIN() __tagged_FOR_EACH_comma_HELPER


#define __tagged_gadt_part1(x) \
    struct __tagged_pick_1 x { \
        using __gadt_family = self_t::__gadt_family; \
        using __gadt_enclosing_t = self_t; \
        using __gadt_result_t = __tagged_gadt_result_type x; \
        __tagged_make_fields __tagged_pick_2 x \
        \
        template <typename... ts> \
        static auto make(ts... args) -> __gadt_result_t requires __match_impl::is_same_v<self_t, __gadt_result_t> { \
            return __gadt_result_t{.tag = __gadt_result_t::tag_t::__tagged_pick_1 x, .d = {.__tagged_pick_1 x = typename __gadt_result_t::__tagged_pick_1 x{args...}}}; \
        } \
        \
        operator __gadt_result_t() const requires __match_impl::is_same_v<self_t, __gadt_result_t> { \
            return self_t::from(*this); \
        } \
    };

#define __tagged_gadt_part2(x) [[no_unique_address]] __tagged_pick_1 x __tagged_pick_1 x;

#define __tagged_gadt_part3(x) template <> constexpr tag_t tag_of<__tagged_pick_1 x> = tag_t::__tagged_pick_1 x;

#define __tagged_gadt_part4(x) \
    template <> inline auto as<__tagged_pick_1 x>() -> __tagged_pick_1 x { return d.__tagged_pick_1 x; }; \
    template <> inline auto as<__tagged_pick_1 x *>() -> __tagged_pick_1 x * { return &d.__tagged_pick_1 x; };

#define __tagged_gadt_part5(x) \
    case tag_t::__tagged_pick_1 x: \
        if constexpr (__match_impl::is_same_v<self_t, typename __tagged_pick_1 x::__gadt_result_t>) { \
            return __match_impl::case_tree_gen_branch<__tagged_pick_1 x>(ct_split, scrutinee_ls); \
        } else { \
            __builtin_unreachable(); \
        }

#define __tagged_gadt_part5_ptr(x) \
    case tag_t::__tagged_pick_1 x: \
        if constexpr (__match_impl::is_same_v<self_t, typename __tagged_pick_1 x::__gadt_result_t>) { \
            return __match_impl::case_tree_gen_branch<__tagged_pick_1 x>(ct_split, scrutinee_ls); \
        } else { \
            __builtin_unreachable(); \
        }

#define __tagged_gadt_part6(x) \
    template<> inline constexpr auto from<__tagged_pick_1 x>(__tagged_pick_1 x v) -> self_t { \
        static_assert(__match_impl::is_same_v<self_t, typename __tagged_pick_1 x::__gadt_result_t>, "mlmatch error: this GADT constructor does not construct this indexed type."); \
        return {.tag = tag_t::__tagged_pick_1 x, .d = {.__tagged_pick_1 x = v}}; \
    }

#define tagged_gadt(tycon_name) \
    tycon_name { \
        using self_t = tycon_name; \
        using __gadt_family = struct __tagged_XCAT(tycon_name, __gadt_family); \
        tagged_gadt_end

#define tagged_gadt_end(...) \
    enum class tag_t : unsigned char { \
        __tagged_FOR_EACH(__tagged_pick_1_comma_sep, __VA_ARGS__) \
    }; \
    \
    __tagged_FOR_EACH(__tagged_gadt_part1, __VA_ARGS__) \
    \
    using __all_ctors = __match_impl::meta_list<__tagged_FOR_EACH_comma(__tagged_pick_1_type, __VA_ARGS__)>; \
    using __ctors = typename __match_impl::gadt_possible_ctors<self_t, __all_ctors>::type; \
    static constexpr int ctors_count = __ctors::size; \
    \
    template <typename> static constexpr tag_t tag_of{}; \
    __tagged_FOR_EACH(__tagged_gadt_part3, __VA_ARGS__) \
    \
    tag_t tag; \
    [[no_unique_address]] union { \
        __tagged_FOR_EACH(__tagged_gadt_part2, __VA_ARGS__) \
    } d; \
    \
    template <typename ctor_t> inline auto as() -> ctor_t; \
    template <> inline auto as<wildcard>() -> wildcard { return wildcard{}; }; \
    template <> inline auto as<self_t>() -> self_t { return *this; }; \
    template <> inline auto as<self_t*>() -> self_t* { return this; }; \
    __tagged_FOR_EACH(__tagged_gadt_part4, __VA_ARGS__) \
    \
    template <int scrutinee_idx, typename catch_all_T, typename branches_LS, typename scrutinee_LS> \
    static constexpr auto __elim(__match_impl::case_tree::split<scrutinee_idx, self_t, catch_all_T, branches_LS> ct_split, scrutinee_LS scrutinee_ls) { \
        switch (scrutinee_ls.template get_by_idx<self_t, scrutinee_idx>().tag) { \
            __tagged_FOR_EACH(__tagged_gadt_part5, __VA_ARGS__) \
        } \
    } \
    \
    template <int scrutinee_idx, typename catch_all_T, typename branches_LS, typename scrutinee_LS> \
    static constexpr auto __elim_ptr(__match_impl::case_tree::split<scrutinee_idx, self_t*, catch_all_T, branches_LS> ct_split, scrutinee_LS scrutinee_ls) { \
        switch (scrutinee_ls.template get_by_idx<self_t*, scrutinee_idx>()->tag) { \
            __tagged_FOR_EACH(__tagged_gadt_part5_ptr, __VA_ARGS__) \
        } \
    } \
    \
    template <typename __a> \
    static constexpr auto from(__a v) -> self_t; \
    __tagged_FOR_EACH(__tagged_gadt_part6, __VA_ARGS__) \
    \
    template <typename __t> \
    static auto emit_at(self_t* mem_ptr, __t val) -> void { \
        static_assert(__match_impl::is_same_v<self_t, typename __t::__gadt_result_t>, "mlmatch error: this GADT constructor does not construct this indexed type."); \
        mem_ptr->tag = self_t::tag_of<__t>; \
        *mem_ptr->as<__t*>() = val; \
        return; \
    } \
}

#define tagged(tycon_name) \
    tycon_name { \
        using self_t = tycon_name; \
        tagged_end

#define tagged_end(...) \
        enum class tag_t : unsigned char { \
            __tagged_FOR_EACH(__tagged_fst_comma_sep, __VA_ARGS__) \
        }; \
        \
        __tagged_FOR_EACH(__tagged_part1, __VA_ARGS__) \
        \
        using __ctors = __match_impl::meta_list<__tagged_FOR_EACH_comma(__tagged_fst_type, __VA_ARGS__)>; \
        static constexpr int ctors_count = __tagged_va_count(__tagged_FOR_EACH_comma(__tagged_fst_comma_sep2, __VA_ARGS__)); \
        \
        template <typename> static constexpr tag_t tag_of{}; \
        __tagged_FOR_EACH(__tagged_part3, __VA_ARGS__) \
        \
        tag_t tag; \
        [[no_unique_address]] union { \
            __tagged_FOR_EACH(__tagged_part2_20, __VA_ARGS__) \
        } d; \
        \
        template <typename ctor_t> inline auto as() -> ctor_t; \
        template <> inline auto as<wildcard>() -> wildcard { return wildcard{}; }; \
        template <> inline auto as<self_t>() -> self_t { return *this; }; \
        template <> inline auto as<self_t*>() -> self_t* { return this; }; \
        __tagged_FOR_EACH(__tagged_part4, __VA_ARGS__) \
        \
        template <int scrutinee_idx, typename catch_all_T, typename branches_LS, typename scrutinee_LS> \
        static constexpr auto __elim(__match_impl::case_tree::split<scrutinee_idx, self_t, catch_all_T, branches_LS> ct_split, scrutinee_LS scrutinee_ls) { \
            switch (scrutinee_ls.template get_by_idx<self_t, scrutinee_idx>().tag) { \
                __tagged_FOR_EACH(__tagged_part5, __VA_ARGS__) \
            } \
        } \
        \
        template <int scrutinee_idx, typename catch_all_T, typename branches_LS, typename scrutinee_LS> \
        static constexpr auto __elim_ptr(__match_impl::case_tree::split<scrutinee_idx, self_t*, catch_all_T, branches_LS> ct_split, scrutinee_LS scrutinee_ls) { \
            switch (scrutinee_ls.template get_by_idx<self_t*, scrutinee_idx>()->tag) { \
                __tagged_FOR_EACH(__tagged_part5_ptr, __VA_ARGS__) \
            } \
        } \
        \
        template <typename __a> \
        static constexpr auto from(__a v) -> self_t; \
        __tagged_FOR_EACH(__tagged_part6, __VA_ARGS__) \
        \
        template <typename __t> \
        static auto emit_at(self_t* mem_ptr, __t val) -> void { \
            mem_ptr->tag = self_t::tag_of<__t>; \
            *mem_ptr->as<__t*>() = val; \
            return; \
        } \
    }
