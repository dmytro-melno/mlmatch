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

#include <type_traits>

#define func auto
#define let auto

struct wildcard {};

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
    struct tkey_val {
        using key_type = A;
        using val_type = B;

        B val;

        static consteval func make(B val) -> tkey_val<A, B> { return {val}; }
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

        template <typename a>
        static constexpr bool mem_t = type_list<tail...>::template mem_t<a>;
        template <>
        constexpr bool mem_t<head> = true;

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
        static consteval auto go(const type_list<>) { return none{}; }
    };

    template <typename tkey, typename val_t, typename... ts>
    struct type_list_assoc_opt<tkey, type_list<tkey_val<tkey, val_t>, ts...>> {
        static consteval auto go(const type_list<tkey_val<tkey, val_t>, ts...> ls) { return some<val_t>{ls.x.val}; }
    };

    template <typename tkey, typename head, typename... ts>
    struct type_list_assoc_opt<tkey, type_list<head, ts...>> {
        static consteval auto go(const type_list<head, ts...> ls) { return type_list_assoc_opt<tkey, type_list<ts...>>::go(ls.xs); }
    };


    template <typename tkey, typename ts>
    struct type_list_remove_assoc_impl;

    template <typename tkey>
    struct type_list_remove_assoc_impl<tkey, type_list<>> {
        static consteval auto go(const type_list<>) { return type_list<>::nil(); }
    };

    template <typename tkey, typename val_t, typename... ts>
    struct type_list_remove_assoc_impl<tkey, type_list<tkey_val<tkey, val_t>, ts...>> {
        static consteval auto go(const type_list<tkey_val<tkey, val_t>, ts...> ls) { return type_list_remove_assoc_impl<tkey, type_list<ts...>>::go(ls.xs); }
    };

    template <typename tkey, typename head, typename... ts>
    struct type_list_remove_assoc_impl<tkey, type_list<head, ts...>> {
        static consteval auto go(const type_list<head, ts...> ls) { return type_list_remove_assoc_impl<tkey, type_list<ts...>>::go(ls.xs).cons(ls.x); }
    };

    template <typename tkey, typename ts>
    consteval auto type_list_remove_assoc(const ts ls) { return type_list_remove_assoc_impl<tkey, ts>::go(ls); }


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

        template <t v> static constexpr bool mem_t = value_list<t, vals...>::template mem_t<v>;
        template <> constexpr bool mem_t<val> = true;

        template <t v>
        static consteval bool mem_v() { return v == val || value_list<t, vals...>::template mem_v<v>(); }

        template <typename F>
        static consteval void iter(F f) {
            f(std::integral_constant<t, val>{});
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
        using args = type_list<Args...>;
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

    template <int scrutinee_idx, typename scru_types_LS, typename clauses_LS>
    struct cc {
        static constexpr func go(clauses_LS cls) = delete;
    };

    template <typename scru_type, typename cls_LS, typename head_pats_LS>
    struct cc_cls_get_head_pats;

    template <typename scru_type, int cl_idx, typename done_F, typename... cls_ts, typename head_pats_LS>
    struct cc_cls_get_head_pats<scru_type, type_list<clause<cl_idx, type_list<>, done_F>, cls_ts...>, head_pats_LS> {
        static_assert(false, "error: too few patterns, less pattern than scrutinee values.");
    };

    template <typename scru_type, int cl_idx, typename pat, typename... pats, typename done_F, typename... cls_ts, typename head_pats_LS>
    struct cc_cls_get_head_pats<scru_type, type_list<clause<cl_idx, type_list<pat, pats...>, done_F>, cls_ts...>, head_pats_LS> {
        // static_assert(
        //     std::is_same<pat, wildcard>::value || std::is_same<typename pat::__datacon_ty, scru_type>::value,
        //     "error: pattern must be either 'wildcard' or one of constructors of the type of value in scrutinee."
        // );

        using type =
            std::conditional<(!std::is_same<pat, scru_type>::value && !std::is_same<pat, wildcard>::value && !head_pats_LS::template mem_t<pat>),
                typename cc_cls_get_head_pats<scru_type, type_list<cls_ts...>, typename head_pats_LS::template cons_t<pat>>::type::template cons_t<pat>,
                typename cc_cls_get_head_pats<scru_type, type_list<cls_ts...>, head_pats_LS>::type
            >::type;
    };

    template <typename scru_type, typename head_pats_LS>
    struct cc_cls_get_head_pats<scru_type, type_list<>, head_pats_LS> {
        using type = type_list<>;
    };

    template <typename head_pat, typename scru_type, typename cls_LS>
    struct cc_specialize;

    template <typename head_pat, typename scru_type, int cl_idx, typename pat, typename... pats, typename done_F, typename... cls_ts>
    struct cc_specialize<head_pat, scru_type, type_list<clause<cl_idx, type_list<pat, pats...>, done_F>, cls_ts...>> {
        static constexpr func go(type_list<clause<cl_idx, type_list<pat, pats...>, done_F>, cls_ts...> cls) {
            if constexpr (std::is_same<pat, head_pat>::value || std::is_same<pat, wildcard>::value || std::is_same<pat, scru_type>::value) {
                return cc_specialize<head_pat, scru_type, type_list<cls_ts...>>::go(cls.xs)
                    .cons(clause<cl_idx, type_list<pats...>, done_F>{cls.x.done_f});
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
    struct cc_default<scru_type, type_list<clause<cl_idx, type_list<pat, pats...>, done_F>, cls_ts...>> {
        static constexpr func go(type_list<clause<cl_idx, type_list<pat, pats...>, done_F>, cls_ts...> cls) {
            if constexpr (std::is_same<pat, wildcard>::value || std::is_same<pat, scru_type>::value) {
                return cc_default<scru_type, type_list<cls_ts...>>::go(cls.xs)
                    .cons(clause<cl_idx, type_list<pats...>, done_F>{cls.x.done_f});
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

    template <typename head_pats_LS, int scrutinee_idx, typename scru_types_LS, typename cls_LS>
    struct cc_split;

    template<int scrutinee_idx, typename scru_types_LS, typename cls_LS>
    struct cc_split<type_list<>, scrutinee_idx, scru_types_LS, cls_LS> {
        static constexpr func go(cls_LS cls) {
            return cc_result<type_list<>, int_list<>>{type_list<>::nil()};
        }
    };

    template<typename head_pat, typename... head_pats, int scrutinee_idx, typename scru_type, typename... scru_types, typename cls_LS>
    struct cc_split<type_list<head_pat, head_pats...>, scrutinee_idx, type_list<scru_type, scru_types...>, cls_LS> {
        static constexpr func go(cls_LS cls) {
            let s_cls = cc_specialize<head_pat, scru_type, cls_LS>::go(cls);
            let res_ct_next = cc<scrutinee_idx + 1, type_list<scru_types...>, decltype(s_cls)>::go(s_cls);
            let res_branches = cc_split<type_list<head_pats...>, scrutinee_idx, type_list<scru_type, scru_types...>, cls_LS>::go(cls);
            let branches = res_branches.v.cons(tkey_val<head_pat, decltype(res_ct_next.v)>::make(res_ct_next.v));
            using reached_cls_ids = value_list_concat<
                                        typename decltype(res_ct_next)::reached_cls_idxs,
                                        typename decltype(res_branches)::reached_cls_idxs>::type;
            return cc_result<decltype(branches), reached_cls_ids>{branches};
        }
    };

    template <int scrutinee_idx, typename scru_types_LS, typename cls_LS>
    struct cc_catch_all;

    template <int scrutinee_idx, typename scru_type, typename... scru_types, typename cls_LS>
    struct cc_catch_all<scrutinee_idx, type_list<scru_type, scru_types...>, cls_LS> {
        static constexpr func go(cls_LS cls) {
            let d_cls = cc_default<scru_type, cls_LS>::go(cls);
            return cc<scrutinee_idx + 1, type_list<scru_types...>, decltype(d_cls)>::go(d_cls);
        }
    };

    template <int scrutinee_idx, typename scru_types_LS>
    struct cc<scrutinee_idx, scru_types_LS, type_list<>> {
        static consteval func go(type_list<> cls) {
            static_assert(false, "error: unhandled case.");
        }
    };

    template <int scrutinee_idx, int cl_idx, typename pats_LS, typename done_F, typename... cls_ts>
    struct cc<scrutinee_idx, type_list<>, type_list<clause<cl_idx, pats_LS, done_F>, cls_ts...>> {
        static consteval func go(type_list<clause<cl_idx, pats_LS, done_F>, cls_ts...> cls) {
            static_assert(pats_LS::is_empty_t, "error: excessive patterns. more pattern than scrutinee values.");
            return cc_result<case_tree::done<done_F, fn_get_arg_tys<done_F>>, int_list<cl_idx>>{
                case_tree::done<done_F, fn_get_arg_tys<done_F>>{cls.x.done_f}};
        }
    };

    template <int scrutinee_idx, typename scru_type, typename... scru_types, typename cls_LS>
    struct cc<scrutinee_idx, type_list<scru_type, scru_types...>, cls_LS> {
        static consteval func go(cls_LS cls) {
            using head_pats = cc_cls_get_head_pats<scru_type, cls_LS, type_list<>>::type;
            if constexpr (head_pats::is_empty_t) {
                return cc_catch_all<scrutinee_idx, type_list<scru_type, scru_types...>, cls_LS>::go(cls);
            } else {
                let res_branches = cc_split<head_pats, scrutinee_idx, type_list<scru_type, scru_types...>, cls_LS>::go(cls);
                let res_catch_all = [=](){
                    if constexpr (decltype(res_branches.v)::size == scru_type::ctors_count) {
                        return cc_result<case_tree::empty, int_list<>>{case_tree::empty{}};
                    } else {
                        return cc_catch_all<scrutinee_idx, type_list<scru_type, scru_types...>, cls_LS>::go(cls);
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

    template <typename scru_types_LS, typename clauses_LS>
    struct compile_clauses {
        static consteval func go(clauses_LS cls) {
            let res = cc<0, scru_types_LS, clauses_LS>::go(cls);
            cls.iter([](auto cl){
                using cl_t = decltype(cl);
                static_assert(decltype(res)::reached_cls_idxs::template mem_t<cl_t::cl_idx>, "error: unreachable clause.");
            });
            return res.v;
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
    struct apply_scrutinee<F, type_list<>, type_list<>, arg_acc_ts...> {
        static constexpr func go(F f, type_list<> scrutinee_ls, arg_acc_ts... acc) {
            return f(acc...);
        }
    };

    template <typename F, typename scru_type, typename... scru_types, typename arg_t, typename... arg_ts, typename... arg_acc_ts>
    struct apply_scrutinee<F, type_list<scru_type, scru_types...>, type_list<arg_t, arg_ts...>, arg_acc_ts...> {
        static constexpr func go(F f, type_list<scru_type, scru_types...> scrutinee_ls, arg_acc_ts... acc) {
            return apply_scrutinee<F, type_list<scru_types...>, type_list<arg_ts...>, arg_acc_ts..., arg_t>::go(
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
    struct case_tree_gen_switch_tree<case_tree::split<scrutinee_idx, scru_type, catch_all_T, branches_LS>, scrutinee_LS> {
        static constexpr func go(case_tree::split<scrutinee_idx, scru_type, catch_all_T, branches_LS> ct_split, scrutinee_LS scrutinee_ls) {
            return scru_type::template elim<scrutinee_idx, catch_all_T, branches_LS, scrutinee_LS>(ct_split, scrutinee_ls);
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
};


template <typename... scru_types>
struct match {
    match(scru_types... scru_vals) {
        scrutinee_ls = __match_impl::type_list<>::make(scru_vals...);
    }

    template <typename... clause_fn_ts>
    inline func with(clause_fn_ts... clause_fns) {
        let cls = make_cls<0, clause_fn_ts...>::go(clause_fns...);
        let ct = __match_impl::compile_clauses<__match_impl::type_list<scru_types...>, decltype(cls)>::go(cls);
        return __match_impl::case_tree_gen_switch_tree<decltype(ct), __match_impl::type_list<scru_types...>>::go(ct, scrutinee_ls);
    }
private:
    __match_impl::type_list<scru_types...> scrutinee_ls;

    template <int cl_idx, typename... ts>
    struct make_cls;

    template <int cl_idx>
    struct make_cls<cl_idx> {
        static constexpr auto go() {
            return __match_impl::type_list<>::nil();
        }
    };

    template <int cl_idx, typename t, typename... ts>
    struct make_cls<cl_idx, t, ts...> {
        static constexpr auto go(t el, ts... els) {
            return make_cls<cl_idx + 1, ts...>::go(els...).cons(__match_impl::make_clause<cl_idx>(el));
        }
    };
};

using _ = wildcard;

#undef func
#undef let


template<typename ...Args>
constexpr std::size_t __tagged_va_count(Args&&...) { return sizeof...(Args); }

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
#define __tagged_fst_comma_sep(x) __tagged_fst x,
#define __tagged_fst_comma_sep2(x) __tagged_fst x{}

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
#define __tagged_part4(x) template <> inline auto as<__tagged_fst x>() -> __tagged_fst x { return d.__tagged_fst x; };
#define __tagged_part5(x) \
case tag_t::__tagged_fst x: return __match_impl::case_tree_gen_switch_tree<decltype(__match_impl::case_tree_split_get_branch<__tagged_fst x>(ct_split)), scrutinee_LS>::go( \
    case_tree_split_get_branch<__tagged_fst x>(ct_split), scrutinee_ls);
    // #define __tagged_part6(x) static constexpr inline auto __tagged_make_name(x)(__tagged_fst x v) { return self_t {.d = {.__tagged_fst x = v}}; }
#define __tagged_part6(x) template<> inline constexpr auto from<__tagged_fst x>(__tagged_fst x v) -> self_t { \
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
        tag_t tag; \
        [[no_unique_address]] union { \
            __tagged_FOR_EACH(__tagged_part2_20, __VA_ARGS__) \
        } d; \
        \
        static constexpr int ctors_count = __tagged_va_count(__tagged_FOR_EACH_comma(__tagged_fst_comma_sep2, __VA_ARGS__)); \
        \
        template <typename> static constexpr tag_t tag_of{}; \
        __tagged_FOR_EACH(__tagged_part3, __VA_ARGS__) \
        \
        template <typename ctor_t> inline auto as() -> ctor_t; \
        template <> inline auto as<wildcard>() -> wildcard { return wildcard{}; }; \
        template <> inline auto as<self_t>() -> self_t { return *this; }; \
        __tagged_FOR_EACH(__tagged_part4, __VA_ARGS__) \
        \
        template <int scrutinee_idx, typename catch_all_T, typename branches_LS, typename scrutinee_LS> \
        constexpr static auto elim(__match_impl::case_tree::split<scrutinee_idx, self_t, catch_all_T, branches_LS> ct_split, scrutinee_LS scrutinee_ls) { \
            switch (scrutinee_ls.template get_by_idx<self_t, scrutinee_idx>().tag) { \
                __tagged_FOR_EACH(__tagged_part5, __VA_ARGS__) \
            } \
        } \
        \
        template <typename a> \
        static inline constexpr auto from(a v) -> self_t; \
        __tagged_FOR_EACH(__tagged_part6, __VA_ARGS__) \
    }


