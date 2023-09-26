#ifndef __CPP2_PARSE_TREE
#define __CPP2_PARSE_TREE

#include "lex.h"
#include <memory>
#include <variant>
#include <iostream>

namespace cpp2 {
//-----------------------------------------------------------------------
//
//  Parse tree node types
//
//-----------------------------------------------------------------------
//

//-----------------------------------------------------------------------
//  try_visit
//
//  Helper to visit whatever is in a variant where each
//  alternative is a smart pointer
//
template <int I>
auto try_visit(auto& variant, auto& visitor, int depth)
    -> void
{
    if (variant.index() == I) {
        auto const& s = std::get<I>(variant);
        assert (s);
        s->visit(visitor, depth+1);
    }
}


struct expression_list_node;
struct id_expression_node;
struct declaration_node;
struct inspect_expression_node;
struct literal_node;
struct template_argument;


struct primary_expression_node
{
    enum active { empty=0, identifier, expression_list, id_expression, declaration, inspect, literal };
    std::variant<
        std::monostate,
        token const*,
        std::unique_ptr<expression_list_node>,
        std::unique_ptr<id_expression_node>,
        std::unique_ptr<declaration_node>,
        std::unique_ptr<inspect_expression_node>,
        std::unique_ptr<literal_node>
    > expr;


    //  API
    //
    auto is_fold_expression() const
        -> bool;

    auto is_identifier() const
        -> bool;

    auto is_id_expression() const
        -> bool;

    auto is_expression_list() const
        -> bool;

    auto get_expression_list() const
        -> expression_list_node const*;

    auto is_literal() const
        -> bool;

    auto template_arguments() const -> std::vector<template_argument> const&;

    auto get_token() const -> token const*;

    auto to_string() const
        -> std::string;

    //  Internals
    //
    auto position() const -> source_position;
    auto visit(auto& v, int depth) -> void;
};


struct literal_node {
    token const* literal             = {};
    token const* user_defined_suffix = {};

    //  API
    //
    auto get_token() const
        -> token const*
    {
        return literal;
    }

    auto to_string() const
        -> std::string
    {
        assert (literal);
        auto ret = literal->to_string();
        if (user_defined_suffix) {
            ret += user_defined_suffix->to_string();
        }
        return ret;
    }

    //  Internals
    //
    auto position() const
        -> source_position
    {
        assert (literal);
        return literal->position();
    }

    auto visit(auto& v, int depth) -> void
    {
        v.start(*this, depth);
        assert (literal);
        literal->visit(v, depth+1);
        if (user_defined_suffix) {
            user_defined_suffix->visit(v, depth+1);
        }
        v.end(*this, depth);
    }
};


struct postfix_expression_node;

struct prefix_expression_node
{
    std::vector<token const*> ops;
    std::unique_ptr<postfix_expression_node> expr;

    //  API
    //
    auto is_fold_expression() const
        -> bool;

    auto is_identifier() const
        -> bool;

    auto is_id_expression() const
        -> bool;

    auto is_expression_list() const
        -> bool;

    auto get_expression_list() const
        -> expression_list_node const*;

    auto get_postfix_expression_node() const
        -> postfix_expression_node *
    {
        assert(expr);
        return expr.get();
    }

    auto is_literal() const
        -> bool;

    auto is_result_a_temporary_variable() const -> bool;

    auto to_string() const
        -> std::string;

    //  Internals
    //
    auto position() const -> source_position;
    auto visit(auto& v, int depth) -> void;
};

struct expression_node;


template<
    String   Name,
    typename Term
>
struct binary_expression_node
{
    std::unique_ptr<Term>  expr;
    expression_node const* my_expression = {};

    binary_expression_node();

    struct term
    {
        token const* op;
        std::unique_ptr<Term> expr;
    };
    std::vector<term> terms;


    //  API
    //
    auto is_fold_expression() const
        -> bool
    {
        //  This is a fold-expression if any subexpression
        //  has an identifier named "..."
        auto ret = expr->is_fold_expression();
        for (auto& x : terms) {
            ret |= x.expr->is_fold_expression();
        }
        return ret;
    }

    auto lhs_is_id_expression() const
        -> bool
    {
        return expr->is_id_expression();
    }

    auto is_standalone_expression() const
        -> bool;

    auto terms_size() const
        -> int
    {
        return std::ssize(terms);
    }

    auto is_identifier() const
        -> bool
    {
        return terms.empty() && expr->is_identifier();
    }

    auto is_id_expression() const
        -> bool
    {
        return terms.empty() && expr->is_id_expression();
    }

    auto is_expression_list() const
        -> bool
    {
        return terms.empty() && expr->is_expression_list();
    }

    auto get_expression_list() const
        -> expression_list_node const*
    {
        if (is_expression_list()) {
            return expr->get_expression_list();
        }
        return {};
    }

    auto is_literal() const
        -> bool
    {
        return terms.empty() && expr->is_literal();
    }

    //  Get left-hand postfix-expression
    auto get_postfix_expression_node() const
        -> postfix_expression_node *
    {
        assert(expr);
        return expr->get_postfix_expression_node();
    }

    //  Get first right-hand postfix-expression, if there is one
    auto get_second_postfix_expression_node() const
        -> postfix_expression_node *
    {
        if (!terms.empty()) {
            assert(terms.front().expr);
            return terms.front().expr->get_postfix_expression_node();
        }
        //  else
        return {};
    }

    //  "Simple" means binary (size>0) and not chained (size<2)
    struct get_lhs_rhs_if_simple_binary_expression_with_ret {
        postfix_expression_node* lhs;
        Term*                    rhs;
    };
    auto get_lhs_rhs_if_simple_binary_expression_with(lexeme op) const
        -> get_lhs_rhs_if_simple_binary_expression_with_ret
    {
        if (
            std::ssize(terms) == 1
            && terms[0].op->type() == op
            )
        {
            return {
                get_postfix_expression_node(),
                terms.front().expr.get()
            };
        }
        //  Else
        return { nullptr, nullptr };
    }

    auto is_result_a_temporary_variable() const -> bool {
        if constexpr (std::string_view(Name.value) == "assignment") {
            assert(expr);
            return expr->is_result_a_temporary_variable();
        } else {
            if (terms.empty()) {
                assert(expr);
                return expr->is_result_a_temporary_variable();
            } else {
                return true;
            }
        }
    }

    auto to_string() const
        -> std::string
    {
        assert (expr);
        auto ret = expr->to_string();
        for (auto const& x : terms) {
            assert (x.op);
            ret += " " + x.op->to_string();
            assert (x.expr);
            ret += " " + x.expr->to_string();
        }
        return ret;
    }


    //  Internals
    //
    auto position() const
        -> source_position
    {
        assert (expr);
        return expr->position();
    }

    auto visit(auto& v, int depth)
        -> void
    {
        v.start(*this, depth);
        assert (expr);
        expr->visit(v, depth+1);
        for (auto const& x : terms) {
            assert (x.op);
            v.start(*x.op, depth+1);
            assert (x.expr);
            x.expr->visit(v, depth+1);
        }
        v.end(*this, depth);
    }
};


struct is_as_expression_node;

using multiplicative_expression_node = binary_expression_node< "multiplicative" , is_as_expression_node          >;
using additive_expression_node       = binary_expression_node< "additive"       , multiplicative_expression_node >;
using shift_expression_node          = binary_expression_node< "shift"          , additive_expression_node       >;
using compare_expression_node        = binary_expression_node< "compare"        , shift_expression_node          >;
using relational_expression_node     = binary_expression_node< "relational"     , compare_expression_node        >;
using equality_expression_node       = binary_expression_node< "equality"       , relational_expression_node     >;
using bit_and_expression_node        = binary_expression_node< "bit-and"        , equality_expression_node       >;
using bit_xor_expression_node        = binary_expression_node< "bit-xor"        , bit_and_expression_node        >;
using bit_or_expression_node         = binary_expression_node< "bit-or"         , bit_xor_expression_node        >;
using logical_and_expression_node    = binary_expression_node< "logical-and"    , bit_or_expression_node         >;
using logical_or_expression_node     = binary_expression_node< "logical-or"     , logical_and_expression_node    >;
using assignment_expression_node     = binary_expression_node< "assignment"     , logical_or_expression_node     >;


struct assignment_expression_lhs_rhs {
    postfix_expression_node*    lhs;
    logical_or_expression_node* rhs;
};


struct expression_statement_node;

struct expression_node
{
    static inline std::vector<expression_node*> current_expressions = {};

    std::unique_ptr<assignment_expression_node> expr;
    int num_subexpressions = 0;
    expression_statement_node const* my_statement = {};

    expression_node();

    // API
    //
    auto is_fold_expression() const
        -> bool
    {
        //  This is a fold-expression if any subexpression
        //  has an identifier named "..."
        return expr->is_fold_expression();
    }

    auto is_standalone_expression() const
        -> bool;

    auto subexpression_count() const
        -> int
    {
        return num_subexpressions;
    }

    auto is_identifier() const
        -> bool
    {
        return expr->is_identifier();
    }

    auto is_id_expression() const
        -> bool
    {
        return expr->is_id_expression();
    }

    auto is_expression_list() const
        -> bool
    {
        return expr->is_expression_list();
    }

    auto get_expression_list() const
        -> expression_list_node const*
    {
        if (is_expression_list()) {
            return expr->get_expression_list();
        }
        return {};
    }

    auto is_literal() const
        -> bool
    {
        return expr->is_literal();
    }

    auto get_lhs_rhs_if_simple_assignment() const
        -> assignment_expression_lhs_rhs;

    auto to_string() const
        -> std::string
    {
        assert (expr);
        return expr->to_string();
    }

    //  Internals
    //
    auto position() const -> source_position
    {
        assert (expr);
        return expr->position();
    }

    auto visit(auto& v, int depth) -> void
    {
        v.start(*this, depth);
        assert (expr);
        expr->visit(v, depth+1);
        v.end(*this, depth);
    }
};


template<
    String   Name,
    typename Term
>
binary_expression_node<Name, Term>::binary_expression_node() {
    if (!expression_node::current_expressions.empty()) {
        my_expression = expression_node::current_expressions.back();
    }
}


template<
    String   Name,
    typename Term
>
auto binary_expression_node<Name, Term>::is_standalone_expression() const
    -> bool
{
    return
        my_expression
        && my_expression->is_standalone_expression()
        ;
}


enum class passing_style { in=0, copy, inout, out, move, forward, invalid };
auto to_passing_style(token const& t) -> passing_style;
auto to_string_view(passing_style pass) -> std::string_view;

struct expression_list_node
{
    token const* open_paren  = {};
    token const* close_paren = {};
    bool inside_initializer  = false;

    struct term {
        passing_style                    pass = {};
        std::unique_ptr<expression_node> expr;

        auto visit(auto& v, int depth) -> void
        {
            v.start(*this, depth);
            assert(expr);
            expr->visit(v, depth+1);
            v.end(*this, depth);
        }
    };
    std::vector< term > expressions;


    //  API
    //
    auto is_fold_expression() const
        -> bool
    {
        //  This is a fold-expression if any subexpression
        //  has an identifier named "..."
        auto ret = false;
        for (auto& x : expressions) {
            ret |= x.expr->is_fold_expression();
        }
        return ret;
    }


    //  Internals
    //
    auto position() const
        -> source_position
    {
        //  Make sure this got set
        assert (open_paren);
        return open_paren->position();
    }

    auto visit(auto& v, int depth)
        -> void
    {
        v.start(*this, depth);
        for (auto& x : expressions) {
            x.visit(v, depth+1);
        }
        v.end(*this, depth);
    }
};

struct expression_statement_node
{
    static inline std::vector<expression_statement_node*> current_expression_statements = {};

    std::unique_ptr<expression_node> expr;
    bool has_semicolon = false;

    //  API
    //
    auto subexpression_count() const
        -> int
    {
        assert (expr);
        return expr->subexpression_count();
    }

    auto to_string() const
        -> std::string
    {
        assert (expr);
        return expr->to_string();
    }

    //  Internals
    //
    auto position() const
        -> source_position
    {
        assert (expr);
        return expr->position();
    }

    auto visit(auto& v, int depth)
        -> void
    {
        v.start(*this, depth);
        assert (expr);
        expr->visit(v, depth+1);
        v.end(*this, depth);
    }
};

struct capture {
    postfix_expression_node* capture_expr;
    std::string              cap_sym = {};
    std::string              str = {};
    std::string              str_suppressed_move = {};
    auto operator==(postfix_expression_node* p) { return capture_expr == p; }
};

struct capture_group {
    std::vector<capture> members;

    auto add(postfix_expression_node* p)
        -> void
    {
        members.push_back({p});
    }

    auto remove(postfix_expression_node* p)
        -> void;

    ~capture_group();
};


struct postfix_expression_node
{
    std::unique_ptr<primary_expression_node> expr;

    struct term
    {
        token const* op;

        //  This is used if *op is . - can be null
        std::unique_ptr<id_expression_node> id_expr = {};

        //  These are used if *op is [ or ( - can be null
        std::unique_ptr<expression_list_node> expr_list = {};
        token const* op_close = {};
    };
    std::vector<term> ops;
    capture_group* cap_grp = {};

    ~postfix_expression_node()
    {
        if (cap_grp) {
            cap_grp->remove(this);
        }
    }

    //  API
    //
    auto is_fold_expression() const
        -> bool
    {
        //  This is a fold-expression if any subexpression
        //  has an identifier named "..."
        return expr->is_fold_expression();
    }

    auto is_identifier() const
        -> bool
    {
        return ops.empty() && expr->is_identifier();
    }

    auto is_id_expression() const
        -> bool
    {
        return ops.empty() && expr->is_id_expression();
    }

    auto is_expression_list() const
        -> bool
    {
        return ops.empty() && expr->is_expression_list();
    }

    auto get_expression_list() const
        -> expression_list_node const*
    {
        if (is_expression_list()) {
            return expr->get_expression_list();
        }
        return {};
    }

    auto is_literal() const
        -> bool
    {
        return ops.empty() && expr->is_literal();
    }

    auto get_first_token_ignoring_this() const
        -> token const*;

    auto is_result_a_temporary_variable() const -> bool {
        if (ops.empty()) {
            return false;
        } else {
            return (ops.front().op->type() == lexeme::Ampersand
                    || ops.front().op->type() == lexeme::Tilde);
        }
    }

    auto to_string() const
        -> std::string;

    //  Internals
    //
    auto position() const -> source_position
    {
        assert (expr);
        return expr->position();
    }

    auto visit(auto& v, int depth) -> void;
};

struct type_id_node;
struct template_args_tag { };

struct template_argument
{
    enum active { empty=0, expression, type_id };
    source_position comma;
    std::variant<
        std::monostate,
        std::unique_ptr<expression_node>,
        std::unique_ptr<type_id_node>
    > arg;

    auto to_string() const
        -> std::string;
};

// Used by functions that must return a reference to an empty arg list
inline std::vector<template_argument> const no_template_args;

struct unqualified_id_node
{
    token const* identifier      = {};  // required

    // These are used only if it's a template-id
    source_position open_angle  = {};
    source_position close_angle = {};

    std::vector<template_argument> template_args;

    auto template_arguments() const
        -> std::vector<template_argument> const&
    {
        return template_args;
    }

    auto get_token() const
        -> token const*
    {
        if (open_angle == source_position{}) {
            assert (identifier);
            return identifier;
        }
        // else
        return {};
    }

    auto to_string() const
        -> std::string;

    auto position() const
        -> source_position
    {
        assert (identifier);
        return identifier->position();
    }

    auto visit(auto& v, int depth)
        -> void
    {
        v.start(*this, depth);
        assert (identifier);
        v.start(*identifier, depth+1);

        if (open_angle != source_position{}) {
            //  Inform the visitor that this is a template args list
            v.start(template_args_tag{}, depth);
            assert(open_angle  != source_position{});
            assert(close_angle != source_position{});
            assert(template_args.empty()
                   || template_args.front().comma == source_position{});
            for (auto& a : template_args) {
                try_visit<template_argument::expression>(a.arg, v, depth+1);
                try_visit<template_argument::type_id   >(a.arg, v, depth+1);
            }
            v.end(template_args_tag{}, depth);
        }

        v.end(*this, depth);
    }
};


struct qualified_id_node
{
    struct term {
        token const* scope_op;
        std::unique_ptr<unqualified_id_node> id = {};

        term( token const* o ) : scope_op{o} { }
    };
    std::vector<term> ids;

    auto template_arguments() const
        -> std::vector<template_argument> const&
    {
        return ids.back().id->template_arguments();
    }

    auto get_token() const
        -> token const*
    {
        if (
            std::ssize(ids) == 1
            && !ids.front().scope_op
            )
        {
            assert (ids.front().id);
            return ids.front().id->get_token();
        }
        // else
        return {};
    }

    auto to_string() const
        -> std::string
    {
        auto ret = std::string{};
        for (auto& term : ids) {
            if (term.scope_op) {
                ret += term.scope_op->as_string_view();
            }
            assert (term.id);
            ret += term.id->to_string();
        }
        return ret;
    }

    auto get_first_token() const
        -> token const*
    {
        assert (
            !ids.empty()
            && ids.front().id
        );
        return ids.front().id->get_token();
    }

    auto position() const
        -> source_position
    {
        assert (!ids.empty());
        if (ids.front().scope_op) {
            return ids.front().scope_op->position();
        }
        else {
            assert (ids.front().id);
            return ids.front().id->position();
        }
    }

    auto visit(auto& v, int depth)
        -> void
    {
        v.start(*this, depth);
        for (auto const& x : ids) {
            if (x.scope_op) {
                x.scope_op->visit(v, depth+1);
            }
            assert(x.id);
            x.id->visit(v, depth+1);
        }
        v.end(*this, depth);
    }
};


struct type_id_node
{
    source_position pos;

    std::vector<token const*> pc_qualifiers;
    token const* address_of                 = {};
    token const* dereference_of             = {};
    int dereference_cnt                     = {};
    token const* suspicious_initialization  = {};

    enum active { empty=0, qualified, unqualified, keyword };
    std::variant<
        std::monostate,
        std::unique_ptr<qualified_id_node>,
        std::unique_ptr<unqualified_id_node>,
        token const*
    > id;

    auto is_wildcard() const
        -> bool
    {
        return
            id.index() == type_id_node::empty
            || (get_token() && *get_token() == "_")
            ;
    }

    auto is_pointer_qualified() const
        -> bool
    {
        for (auto q : pc_qualifiers) {
            if (q->type() == lexeme::Multiply) {
                return true;
            }
        }
        return false;
    }

    auto is_concept() const
        -> bool
    {
        auto tok = get_token();
        return tok && *tok == "concept";
    }

    auto template_arguments() const
        -> std::vector<template_argument> const&
    {
        if (id.index() == unqualified) {
            return std::get<unqualified>(id)->template_arguments();
        }
        // else
        return std::get<qualified>(id)->template_arguments();
    }

    auto to_string() const
        -> std::string
    {
        switch (id.index()) {
        break;case empty:
            return {};
        break;case qualified:
            return std::get<qualified>(id)->to_string();
        break;case unqualified:
            return std::get<unqualified>(id)->to_string();
        break;case keyword:
            return std::get<keyword>(id)->to_string();
        break;default:
            assert(!"ICE: invalid type_id state");
        }
        // else
        return {};
    }

    auto get_token() const
        -> token const*
    {
        switch (id.index()) {
        break;case empty:
            return {};
        break;case qualified:
            return {};
        break;case unqualified:
            return get<unqualified>(id)->get_token();
        break;case keyword:
            return get<keyword>(id);
        break;default:
            assert(!"ICE: invalid type_id state");
        }
        // else
        return {};
    }

    auto position() const
        -> source_position
    {
        return pos;
    }

    auto visit(auto& v, int depth)
        -> void
    {
        v.start(*this, depth);
        for (auto q : pc_qualifiers) {
            v.start(*q, depth+1);
        }
        try_visit<qualified  >(id, v, depth);
        try_visit<unqualified>(id, v, depth);
        try_visit<keyword    >(id, v, depth);
        v.end(*this, depth);
    }
};


struct is_as_expression_node
{
    std::unique_ptr<prefix_expression_node> expr;

    struct term
    {
        token const* op = {};

        //  This is used if *op is a type - can be null
        std::unique_ptr<type_id_node> type = {};

        //  This is used if *op is an expression - can be null
        std::unique_ptr<expression_node> expr = {};
    };
    std::vector<term> ops;


    //  API
    //
    auto is_fold_expression() const
        -> bool
    {
        //  This is a fold-expression if any subexpression
        //  has an identifier named "..."
        return expr->is_fold_expression();
    }

    auto is_identifier() const
        -> bool
    {
        return ops.empty() && expr->is_identifier();
    }

    auto is_id_expression() const
        -> bool
    {
        return ops.empty() && expr->is_id_expression();
    }

    auto is_expression_list() const
        -> bool
    {
        return ops.empty() && expr->is_expression_list();
    }

    auto get_expression_list() const
        -> expression_list_node const*
    {
        if (is_expression_list()) {
            return expr->get_expression_list();
        }
        return {};
    }

    auto is_literal() const
        -> bool
    {
        return ops.empty() && expr->is_literal();
    }

    auto get_postfix_expression_node() const
        -> postfix_expression_node *
    {
        assert(expr);
        return expr->get_postfix_expression_node();
    }

    auto is_result_a_temporary_variable() const -> bool {
        if (ops.empty()) {
            assert(expr);
            return expr->is_result_a_temporary_variable();
        } else {
            return true;
        }
    }

    auto to_string() const
        -> std::string
    {
        assert (expr);
        auto ret = expr->to_string();
        for (auto const& x : ops) {
            assert (x.op);
            ret += " " + x.op->to_string();
            if (x.type) {
                ret += " " + x.type->to_string();
            }
            if (x.expr) {
                ret += " " + x.expr->to_string();
            }
        }
        return ret;
    }

    //  Internals
    //
    auto position() const
        -> source_position
    {
        assert (expr);
        return expr->position();
    }

    auto visit(auto& v, int depth)
        -> void
    {
        v.start(*this, depth);
        assert (expr);
        expr->visit(v, depth+1);
        for (auto const& x : ops) {
            assert (x.op);
            v.start(*x.op, depth+1);
            if (x.type) {
                x.type->visit(v, depth+1);
            }
            if (x.expr) {
                x.expr->visit(v, depth+1);
            }
        }
        v.end(*this, depth);
    }
};


struct id_expression_node
{
    source_position pos;

    enum active { empty=0, qualified, unqualified };
    std::variant<
        std::monostate,
        std::unique_ptr<qualified_id_node>,
        std::unique_ptr<unqualified_id_node>
    > id;

    auto template_arguments() const
        -> std::vector<template_argument> const&
    {
        if (is_unqualified()) {
            return std::get<unqualified>(id)->template_arguments();
        }
        // else
        return std::get<qualified>(id)->template_arguments();
    }

    auto is_fold_expression() const
        -> bool
    {
        //  This is a fold-expression if any subexpression has
        //  has an identifier named "..."
        auto tok = get_token();
        return tok && *tok == "...";
    }

    auto is_empty() const
        -> bool
    {
        return id.index() == empty;
    }

    auto is_qualified() const
        -> bool
    {
        return id.index() == qualified;
    }

    auto is_unqualified() const
        -> bool
    {
        return id.index() == unqualified;
    }

    auto get_token() const
        -> token const*
    {
        if (id.index() == unqualified) {
            return std::get<unqualified>(id)->get_token();
        }
        // else
        return {};
    }

    auto to_string() const
        -> std::string
    {
        if (id.index() == qualified) {
            return std::get<qualified>(id)->to_string();
        }
        else if (id.index() == unqualified) {
            return std::get<unqualified>(id)->to_string();
        }
        // else
        return {};
    }

    auto position() const
        -> source_position
    {
        return pos;
    }

    auto visit(auto& v, int depth)
        -> void
    {
        v.start(*this, depth);
        try_visit<qualified  >(id, v, depth);
        try_visit<unqualified>(id, v, depth);
        v.end(*this, depth);
    }
};

struct statement_node;

struct compound_statement_node
{
    source_position open_brace;
    source_position close_brace;
    std::vector<std::unique_ptr<statement_node>> statements;

    colno_t body_indent = 0;

    compound_statement_node(source_position o = source_position{})
        : open_brace{o}
    { }

    auto position() const
        -> source_position
    {
        return open_brace;
    }

    auto visit(auto& v, int depth) -> void;
};


struct selection_statement_node
{
    bool                                        is_constexpr = false;
    token const*                                identifier   = {};
    source_position                             else_pos;
    std::unique_ptr<logical_or_expression_node> expression;
    std::unique_ptr<compound_statement_node>    true_branch;
    std::unique_ptr<compound_statement_node>    false_branch;
    bool                                        has_source_false_branch = false;

    auto position() const
        -> source_position
    {
        assert (identifier);
        return identifier->position();
    }

    auto visit(auto& v, int depth)
        -> void
    {
        v.start(*this, depth);
        assert (identifier);
        v.start(*identifier, depth+1);
        assert (expression);
        expression->visit(v, depth+1);
        assert (true_branch);
        true_branch->visit(v, depth+1);
        if (false_branch) {
            false_branch->visit(v, depth+1);
        }
        v.end(*this, depth);
    }
};


struct parameter_declaration_node;

struct iteration_statement_node
{
    token const*                                label      = {};
    token const*                                identifier = {};
    std::unique_ptr<assignment_expression_node> next_expression;    // if used, else null
    std::unique_ptr<logical_or_expression_node> condition;          // used for "do" and "while", else null
    std::unique_ptr<compound_statement_node>    statements;         // used for "do" and "while", else null
    std::unique_ptr<expression_node>            range;              // used for "for", else null
    std::unique_ptr<parameter_declaration_node> parameter;          // used for "for", else null
    std::unique_ptr<statement_node>             body;               // used for "for", else null
    bool                                        for_with_in = false;// used for "for," says whether loop variable is 'in'

    auto position() const
        -> source_position
    {
        if (label) {
            return label->position();
        }
        assert(identifier);
        return identifier->position();
    }

    auto visit(auto& v, int depth)
        -> void;
};


struct return_statement_node
{
    token const*                     identifier = {};
    std::unique_ptr<expression_node> expression;

    auto position() const
        -> source_position
    {
        assert(identifier);
        return identifier->position();
    }

    auto visit(auto& v, int depth)
        -> void
    {
        v.start(*this, depth);
        if (expression) {
            expression->visit(v, depth+1);
        }
        v.end(*this, depth);
    }
};


struct alternative_node
{
    std::unique_ptr<unqualified_id_node> name;
    token const*                         is_as_keyword = {};

    //  One of these will be used
    std::unique_ptr<type_id_node>            type_id;
    std::unique_ptr<postfix_expression_node> value;

    source_position                      equal_sign;
    std::unique_ptr<statement_node>      statement;

    auto position() const
        -> source_position
    {
        assert(is_as_keyword);
        return is_as_keyword->position();
    }

    auto visit(auto& v, int depth)
        -> void;
};


struct inspect_expression_node
{
    bool                                     is_constexpr = false;
    token const*                             identifier   = {};
    std::unique_ptr<expression_node>         expression;
    std::unique_ptr<type_id_node>            result_type;
    source_position                          open_brace;
    source_position                          close_brace;

    std::vector<std::unique_ptr<alternative_node>> alternatives;

    auto position() const
        -> source_position
    {
        assert(identifier);
        return identifier->position();
    }

    auto visit(auto& v, int depth)
        -> void
    {
        v.start(*this, depth);
        assert (identifier);
        v.start(*identifier, depth+1);
        assert (expression);
        expression->visit(v, depth+1);
        if (result_type) {
            result_type->visit(v, depth+1);
        }
        for (auto&& alt : alternatives) {
            alt->visit(v, depth+1);
        }
        v.end(*this, depth);
    }
};


struct contract_node
{
    //  Declared first, because it should outlive any owned
    //  postfix_expressions that could refer to it
    capture_group captures;

    source_position                             open_bracket;
    token const*                                kind = {};
    std::unique_ptr<id_expression_node>         group;
    std::unique_ptr<logical_or_expression_node> condition;
    token const*                                message = {};

    contract_node( source_position pos )
        : open_bracket{pos}
    { }

    auto position() const
        -> source_position
    {
        return open_bracket;
    }

    auto visit(auto& v, int depth)
        -> void
    {
        v.start(*this, depth);

        assert(kind);
        kind->visit(v, depth+1);

        if (group) {
            group->visit(v, depth+1);
        }

        assert(condition);
        condition->visit(v, depth+1);

        v.end(*this, depth);
    }
};


struct jump_statement_node
{
    token const* keyword;
    token const* label;

    auto position() const
        -> source_position
    {
        assert(keyword);
        return keyword->position();
    }

    auto visit(auto& v, int depth)
        -> void
    {
        v.start(*this, depth);
        if (keyword) {
            keyword->visit(v, depth+1);
        }
        if (label) {
            label->visit(v, depth+1);
        }
        v.end(*this, depth);
    }
};


struct parameter_declaration_list_node;

struct statement_node
{
    std::unique_ptr<parameter_declaration_list_node> parameters;
    compound_statement_node* compound_parent = nullptr;

    statement_node(compound_statement_node* compound_parent_ = nullptr)
        : compound_parent{ compound_parent_ }
    { }

    enum active { expression=0, compound, selection, declaration, return_, iteration, contract, inspect, jump };
    std::variant<
        std::unique_ptr<expression_statement_node>,
        std::unique_ptr<compound_statement_node>,
        std::unique_ptr<selection_statement_node>,
        std::unique_ptr<declaration_node>,
        std::unique_ptr<return_statement_node>,
        std::unique_ptr<iteration_statement_node>,
        std::unique_ptr<contract_node>,
        std::unique_ptr<inspect_expression_node>,
        std::unique_ptr<jump_statement_node>
    > statement;

    bool emitted = false;               // a note field that's used during lowering to Cpp1

    bool marked_for_removal = false;    // for use during metafunctions which may replace members

    //  API
    //
    auto is_expression () const -> bool { return statement.index() == expression;  }
    auto is_compound   () const -> bool { return statement.index() == compound;    }
    auto is_selection  () const -> bool { return statement.index() == selection;   }
    auto is_declaration() const -> bool { return statement.index() == declaration; }
    auto is_return     () const -> bool { return statement.index() == return_;     }
    auto is_iteration  () const -> bool { return statement.index() == iteration;   }
    auto is_contract   () const -> bool { return statement.index() == contract;    }
    auto is_inspect    () const -> bool { return statement.index() == inspect;     }
    auto is_jump       () const -> bool { return statement.index() == jump;        }

    template<typename Node>
    auto get_if()
        -> Node*
    {
        auto pnode = std::get_if<std::unique_ptr<Node>>(&statement);
        if (pnode) {
            return pnode->get();
        }
        //  else
        return nullptr;
    }

    template<typename Node>
    auto get_if() const
        -> Node const*
    {
        auto pnode = std::get_if<std::unique_ptr<Node>>(&statement);
        if (pnode) {
            return pnode->get();
        }
        //  else
        return nullptr;
    }

    auto get_lhs_rhs_if_simple_assignment() const
        -> assignment_expression_lhs_rhs
    {
        if (is_expression()) {
            return std::get<expression>(statement)->expr->get_lhs_rhs_if_simple_assignment();
        }
        //  Else
        return {};
    }

    auto to_string() const
        -> std::string
    {
        switch (statement.index()) {
        break;case expression:
            return std::get<expression>(statement)->to_string();
        break;default:
            return "(*ERROR*) temporary alpha limitation: type metafunctions cannot stringize expressions that involve initializer statements other than expression-statements";
        }
    }

    //  Internals
    //
    auto position() const
        -> source_position;

    auto visit(auto& v, int depth)
        -> void;
};


struct parameter_declaration_node
{
    source_position pos = {};
    passing_style pass  = passing_style::in;
    int ordinal = 1;

    enum class modifier { none=0, implicit, virtual_, override_, final_ };
    modifier mod = modifier::none;

    std::unique_ptr<declaration_node> declaration;

    //  API
    //
    auto has_name() const
        -> bool;

    auto name() const
        -> token const*;

    auto has_name(std::string_view) const
        -> bool;

    auto direction() const
        -> passing_style
    {
        return pass;
    }

    auto is_implicit() const
        -> bool
    {
        return mod == modifier::implicit;
    }

    auto is_virtual() const
        -> bool
    {
        return mod == modifier::virtual_;
    }

    auto make_virtual()
        -> void
    {
        mod = modifier::virtual_;
    }

    auto is_override() const
        -> bool
    {
        return mod == modifier::override_;
    }

    auto is_final() const
        -> bool
    {
        return mod == modifier::final_;
    }

    auto is_polymorphic() const
        -> bool
    {
        switch (mod) {
        break;case modifier::virtual_:
              case modifier::override_:
              case modifier::final_:
            return true;
        break;default:
            return false;
        }
    }

    //  Internals
    //
    auto position() const
        -> source_position;

    auto visit(auto& v, int depth)
        -> void;
};

struct parameter_declaration_list_node
{
    token const* open_paren  = {};
    token const* close_paren = {};

    std::vector<std::unique_ptr<parameter_declaration_node>> parameters;

    //  API
    //
    auto ssize() const -> auto {
        return std::ssize(parameters);
    }

    auto operator[](int i)
        -> parameter_declaration_node*
    {
        return parameters[i].get();
    }

    auto operator[](int i) const
        -> parameter_declaration_node const*
    {
        return parameters[i].get();
    }

    //  Internals
    //
    auto position() const
        -> source_position
    {
        assert(open_paren);
        return open_paren->position();
    }

    auto visit(auto& v, int depth)
        -> void
    {
        v.start(*this, depth);
        for (auto const& x : parameters) {
            assert(x);
            x->visit(v, depth+1);
        }
        v.end(*this, depth);
    }
};


struct function_returns_tag { };

struct function_type_node
{
    declaration_node* my_decl;

    std::unique_ptr<parameter_declaration_list_node> parameters;
    bool throws = false;

    struct single_type_id {
        std::unique_ptr<type_id_node> type;
        passing_style pass = passing_style::move;
    };

    enum active { empty = 0, id, list };
    std::variant<
        std::monostate,
        single_type_id,
        std::unique_ptr<parameter_declaration_list_node>
    > returns;

    std::vector<std::unique_ptr<contract_node>> contracts;

    function_type_node(declaration_node* decl)
        : my_decl{decl}
    { }

    //  API
    //
    auto is_function_with_this() const
        -> bool;

    auto is_virtual_function() const
        -> bool;

    auto make_function_virtual()
        -> bool;

    auto is_defaultable() const
        -> bool;

    auto is_constructor() const
        -> bool;

    auto is_default_constructor() const
        -> bool;

    auto is_move() const
        -> bool;

    auto is_swap() const
        -> bool;

    auto is_constructor_with_that() const
        -> bool;

    auto is_constructor_with_in_that() const
        -> bool;

    auto is_constructor_with_move_that() const
        -> bool;

    auto is_comparison() const
        -> bool;

    auto is_compound_assignment() const
        -> bool;

    auto is_assignment() const
        -> bool;

    auto is_assignment_with_that() const
        -> bool;

    auto is_assignment_with_in_that() const
        -> bool;

    auto is_assignment_with_move_that() const
        -> bool;

    auto is_destructor() const
        -> bool;

    auto has_declared_return_type() const
        -> bool
    {
        return returns.index() != empty;
    }

    auto unnamed_return_type_to_string() const
        -> std::string
    {
        if (auto id = std::get_if<function_type_node::id>(&returns)) {
            return (*id).type->to_string();
        }
        return {};
    }

    auto has_bool_return_type() const
        -> bool
    {
        if (auto id = std::get_if<function_type_node::id>(&returns)) {
            if (auto name = (*id).type->get_token()) {
                return *name == "bool";
            }
        }
        return false;
    }

    auto has_non_void_return_type() const
        -> bool
    {
        if (auto id = std::get_if<function_type_node::id>(&returns)) {
            if (auto name = (*id).type->get_token()) {
                return *name != "void";
            }
        }
        return returns.index() != empty;
    }

    auto parameter_count() const
        -> int
    {
        return std::ssize(parameters->parameters);
    }

    auto index_of_parameter_named(std::string_view s) const
        -> int
    {
        auto ret = 0;
        for (auto& param : parameters->parameters) {
            if (param->has_name(s)) {
                return ret;
            }
            ++ret;
        }
        return -1;
    }

    auto has_parameter_named(std::string_view s) const
        -> bool
    {
        for (auto& param : parameters->parameters) {
            if (param->has_name(s)) {
                return true;
            }
        }
        return false;
    }

    auto has_parameter_with_name_and_pass(
        std::string_view s,
        passing_style    pass
    ) const
        -> bool
    {
        for (auto& param : parameters->parameters) {
            if (
                param->has_name(s)
                && param->pass == pass
                )
            {
                return true;
            }
        }
        return false;
    }

    auto nth_parameter_type_name(int n) const
        -> std::string;

    auto has_in_parameter_named(std::string_view s) const
        -> bool
    {
        return has_parameter_with_name_and_pass(s, passing_style::in);
    }

    auto has_out_parameter_named(std::string_view s) const
        -> bool
    {
        return has_parameter_with_name_and_pass(s, passing_style::out);
    }

    auto has_move_parameter_named(std::string_view s) const
        -> bool
    {
        return has_parameter_with_name_and_pass(s, passing_style::move);
    }

    //  Internals
    //
    auto position() const
        -> source_position
    {
        assert (parameters);
        return parameters->position();
    }

    auto visit(auto& v, int depth)
        -> void
    {
        v.start(*this, depth);
        assert(parameters);
        parameters->visit(v, depth+1);

        if (returns.index() == id) {
            auto& r = std::get<id>(returns);
            assert(r.type);
            r.type->visit(v, depth+1);
        }
        else if (returns.index() == list) {
            auto& r = std::get<list>(returns);
            assert(r);
            //  Inform the visitor that this is a returns list
            v.start(function_returns_tag{}, depth);
            r->visit(v, depth+1);
            v.end(function_returns_tag{}, depth);
        }
        v.end(*this, depth);
    }
};


struct type_node
{
    token const* type;
    bool         final = false;

    type_node(
        token const* t,
        bool         final_ = false
    )
        : type{t}
        , final{final_}
    { }

    //  API
    //
    auto is_final() const
        -> bool
    {
        return final;
    }

    auto make_final()
        -> void
    {
        final = true;
    }

    //  Internals
    //
    auto position() const
        -> source_position
    {
        assert(type);
        return type->position();
    }

    auto visit(auto& v, int depth)
        -> void
    {
        v.start(*this, depth);
        v.end(*this, depth);
    }
};


struct namespace_node
{
    token const* namespace_;

    namespace_node(token const* ns) : namespace_{ns} { }

    auto position() const
        -> source_position
    {
        assert(namespace_);
        return namespace_->position();
    }

    auto visit(auto& v, int depth)
        -> void
    {
        v.start(*this, depth);
        v.end(*this, depth);
    }
};


struct alias_node
{
    token const* type = {};
    std::unique_ptr<type_id_node> type_id;   // for objects

    enum active : std::uint8_t { a_type, a_namespace, an_object };
    std::variant<
        std::unique_ptr<type_id_node>,
        std::unique_ptr<id_expression_node>,
        std::unique_ptr<expression_node>
    > initializer;

    alias_node( token const* t ) : type{t} { }

    //  API
    //
    auto is_type_alias     () const -> bool
        { return initializer.index() == a_type;      }
    auto is_namespace_alias() const -> bool
        { return initializer.index() == a_namespace; }
    auto is_object_alias   () const -> bool
        { return initializer.index() == an_object;   }

    //  Internals
    //
    auto position() const
        -> source_position
    {
        assert (type);
        return type->position();
    }

    auto visit(auto& v, int depth)
        -> void
    {
        v.start(*this, depth);

        try_visit<a_type     >(initializer, v, depth+1);
        try_visit<a_namespace>(initializer, v, depth+1);
        try_visit<an_object  >(initializer, v, depth+1);

        v.end(*this, depth);
    }
};


enum class accessibility { default_ = 0, public_, protected_, private_ };

auto to_string(accessibility a)
    -> std::string;

struct declaration_identifier_tag { };

struct declaration_node
{
    //  The capture_group is declared first, because it should outlive
    //  any owned postfix_expressions that could refer to it
    capture_group                        captures;
    source_position                      pos;
    bool                                 is_variadic = false;
    bool                                 is_constexpr = false;
    std::unique_ptr<unqualified_id_node> identifier;
    accessibility                        access = accessibility::default_;

    enum active : std::uint8_t { a_function, an_object, a_type, a_namespace, an_alias };
    std::variant<
        std::unique_ptr<function_type_node>,
        std::unique_ptr<type_id_node>,
        std::unique_ptr<type_node>,
        std::unique_ptr<namespace_node>,
        std::unique_ptr<alias_node>
    > type;

    std::vector<std::unique_ptr<id_expression_node>> metafunctions;
    std::unique_ptr<parameter_declaration_list_node> template_parameters;
    source_position                                  requires_pos = {};
    std::unique_ptr<logical_or_expression_node>      requires_clause_expression;

    source_position                 equal_sign = {};
    std::unique_ptr<statement_node> initializer;

    declaration_node*               parent_declaration = {};
    statement_node*                 my_statement = {};

    //  Attributes currently configurable only via metafunction API,
    //  not directly in the base language grammar
    bool member_function_generation = true;

    //  Cache some context
    bool is_template_parameter = false;
    bool is_parameter          = false;

    //  Constructor
    //
    declaration_node(declaration_node* parent)
        : parent_declaration{parent}
    { }

    //  API
    //
    auto type_member_mark_for_removal()
        -> bool
    {
        if (my_statement) {
            my_statement->marked_for_removal = true;
            return true;
        }
        return false;
    }

    auto type_remove_marked_members()
        -> void
    {
        assert (is_type() && initializer && initializer->is_compound());
        auto compound_stmt = initializer->get_if<compound_statement_node>();
        assert (compound_stmt);

        //  Note: This loop is a careful use of the brittle STL "erase" idiom. Do not change this
        //        loop without carefully ensuring it remains safe against iterator invalidation.
        //        (Especially don't change this to a for loop with a "++i" iteration-expression.)
        auto i = compound_stmt->statements.begin();
        while (i != compound_stmt->statements.end())
        {
            if ((*i)->marked_for_removal) {
                i = compound_stmt->statements.erase(i); // these two branches ...
            }
            else {
                ++i;                                    // ... must stay together
            }
        }
    }

    auto type_remove_all_members()
        -> void
    {
        assert (is_type() && initializer && initializer->is_compound());
        auto body = initializer->get_if<compound_statement_node>();
        assert (body);

        //  Drop all statements in the body, which should self-deregister all our 'captures'
        //  - (only) statements in the body should have been able to refer to 'captures'
        body->statements.clear();
        assert(captures.members.empty());
    }

    auto type_disable_member_function_generation()
        -> void
    {
        member_function_generation = false;
    }

    auto object_type() const
        -> std::string
    {
        if (!is_object()) {
            return "(*ERROR*) not an object";
        }
        //  Else
        return std::get<an_object>(type)->to_string();
    }

    auto object_initializer() const
        -> std::string
    {
        if (!is_object()) {
            return "(*ERROR*) not an object";
        }
        else if (initializer) {
            return initializer->to_string();
        }
        //  Else
        return "";
    }

    auto get_parent() const
        -> declaration_node*
    {
        return parent_declaration;
    }

    auto is_public() const
        -> bool
    {
        return access == accessibility::public_;
    }

    auto is_protected() const
        -> bool
    {
        return access == accessibility::protected_;
    }

    auto is_private() const
        -> bool
    {
        return access == accessibility::private_;
    }

    auto is_default_access() const
        -> bool
    {
        return access == accessibility::default_;
    }

private:
    auto set_access(accessibility a)
        -> bool
    {
        if (is_default_access()) {
            access = a;
        }
        return access == a;
    }

public:
    auto make_public()
        -> bool
    {
        return set_access( accessibility::public_ );
    }

    auto make_protected()
        -> bool
    {
        return set_access( accessibility::protected_ );
    }

    auto make_private()
        -> bool
    {
        return set_access( accessibility::private_ );
    }

    auto has_name() const
        -> bool
    {
        return
            identifier
            && identifier->identifier
            ;
    }

    auto name() const
        -> token const*
    {
        if (!identifier) {
            return nullptr;
        }
        //  Else
        return identifier->identifier;
    }

    auto has_name(std::string_view s) const
        -> bool
    {
        return
            has_name()
            && *name() == s
            ;
    }

    auto has_initializer() const
        -> bool
    {
        return initializer != nullptr;
    }

    auto parameter_count() const
        -> int
    {
        if (!is_function()) {
            return -1;
        }
        return std::get<a_function>(type)->parameter_count();
    }

    auto index_of_parameter_named(std::string_view s) const
        -> int
    {
        if (!is_function()) {
            return -1;
        }
        return std::get<a_function>(type)->index_of_parameter_named(s);
    }

    auto has_parameter_named(std::string_view s) const
        -> bool
    {
        if (!is_function()) {
            return false;
        }
        return std::get<a_function>(type)->has_parameter_named(s);
    }

    auto has_in_parameter_named(std::string_view s) const
        -> bool
    {
        if (!is_function()) {
            return false;
        }
        return std::get<a_function>(type)->has_in_parameter_named(s);
    }

    auto has_out_parameter_named(std::string_view s) const
        -> bool
    {
        if (!is_function()) {
            return false;
        }
        return std::get<a_function>(type)->has_out_parameter_named(s);
    }

    auto has_move_parameter_named(std::string_view s) const
        -> bool
    {
        if (!is_function()) {
            return false;
        }
        return std::get<a_function>(type)->has_move_parameter_named(s);
    }

    auto nth_parameter_type_name(int n) const
        -> std::string
    {
        if (!is_function()) {
            return "";
        }
        return std::get<a_function>(type)->nth_parameter_type_name(n);
    }

    auto is_global   () const -> bool
        { return !parent_declaration;         }

    auto is_function () const -> bool
        { return type.index() == a_function;  }
    auto is_object   () const -> bool
        { return type.index() == an_object;   }
    auto is_concept  () const -> bool
        { return type.index() == an_object && get<an_object>(type)->is_concept();   }
    auto is_type     () const -> bool
        { return type.index() == a_type;      }
    auto is_namespace() const -> bool
        { return type.index() == a_namespace; }
    auto is_alias() const -> bool
        { return type.index() == an_alias;    }

    auto is_type_alias     () const -> bool
        { return is_alias() && std::get<an_alias>(type)->is_type_alias(); }
    auto is_namespace_alias() const -> bool
        { return is_alias() && std::get<an_alias>(type)->is_namespace_alias(); }
    auto is_object_alias   () const -> bool
        { return is_alias() && std::get<an_alias>(type)->is_object_alias(); }

    auto is_function_expression () const -> bool
        { return is_function() && !identifier;  }

    auto is_polymorphic() const // has base types or virtual functions
        -> bool
    {
        for (auto& decl : get_type_scope_declarations()) {
            if (
                decl->has_name("this")
                || decl->is_virtual_function()
                )
            {
                return true;
            }
        }
        return false;
    }

    auto parent_is_function   () const -> bool
        { return  parent_declaration && parent_declaration->type.index() == a_function;  }
    auto parent_is_object     () const -> bool
        { return  parent_declaration && parent_declaration->type.index() == an_object;   }
    auto parent_is_type       () const -> bool
        { return  parent_declaration && parent_declaration->type.index() == a_type;      }
    auto parent_is_namespace  () const -> bool
        { return !parent_declaration || parent_declaration->type.index() == a_namespace; }
    auto parent_is_alias      () const -> bool
        { return  parent_declaration && parent_declaration->type.index() == an_alias;    }
    auto parent_is_polymorphic() const -> bool
        { return  parent_declaration && parent_declaration->is_polymorphic(); }

    enum which {
        functions = 1,
        objects   = 2,
        types     = 4,
        aliases   = 8,
        all       = functions|objects|types|aliases
    };

private:
    //  This helper is a const function that delivers pointers
    //  to non-const... because this is the best way I can
    //  think of right now to write the following two get_
    //  functions (without duplicating their bodies, and
    //  without resorting to const_casts)
    auto gather_type_scope_declarations(which w) const
        -> std::vector<declaration_node*>
    {
        if (
            !is_type()
            || !initializer
            || !initializer->is_compound()
            )
        {
            return {};
        }

        auto compound_stmt = initializer->get_if<compound_statement_node>();
        assert (compound_stmt);

        auto ret = std::vector<declaration_node*>{};
        for (auto& o : compound_stmt->statements)
        {
            auto decl = o->get_if<declaration_node>();
            if (decl)
            {
                assert(
                    !decl->is_namespace()
                    && "ICE: a type shouldn't be able to contain a namespace"
                );
                if (
                    (w & functions  && decl->is_function())
                    || (w & objects && decl->is_object()  )
                    || (w & types   && decl->is_type()    )
                    || (w & aliases && decl->is_alias()   )
                    )
                {
                    ret.push_back(decl);
                }
            }
        }

        return ret;
    }

public:
    auto get_type_scope_declarations(which w = all)
        -> std::vector<declaration_node*>
    {
        //  Only want to return the gather_ results as
        //  non-const* in a non-const function
        return gather_type_scope_declarations(w);
    }

    auto get_type_scope_declarations(which w = all) const
        -> std::vector<declaration_node const*>
    {
        //  Convert the gather_ results to const*
        auto tmp = gather_type_scope_declarations(w);
        return std::vector<declaration_node const*>(tmp.begin(), tmp.end());
    }


    auto add_type_member( std::unique_ptr<statement_node> statement )
        -> bool
    {
        if (
            !is_type()
            || !initializer
            || !initializer->is_compound()
            || !statement->is_declaration()
            )
        {
            return false;
        }

        //  Tell this declaration statement that we are its new parent
        //  and check to ensure that it doesn't already have a parent
        //  (that shouldn't happen because we should only get here for a
        //  generated statement that hasn't been added elsewhere yet)
        auto decl = statement->get_if<declaration_node>();
        assert(
            decl
            && !decl->parent_declaration
        );
        decl->parent_declaration = this;

        //  And actually adopt it into our list of statements
        auto compound_stmt = initializer->get_if<compound_statement_node>();
        assert (compound_stmt);
        compound_stmt->statements.push_back(std::move(statement));
        return true;
    }


    auto get_decl_if_type_scope_object_name_before_a_base_type( std::string_view s ) const
        -> declaration_node const*
    {
        declaration_node const* ret = {};

        //  If it's 'this' then it can't be an object name
        if (s == "this") {
            return {};
        }

        //  Navigate to the nearest enclosing type
        auto decl = this;
        while (
            !decl->is_type()
            && decl->parent_declaration
            )
        {
            decl = decl->parent_declaration;
        }

        if (!decl->is_type()) {
            return {};
        }

        //  Look for a name match and if so remember the type,
        //  and look for a base type after that match
        auto objects               = decl->get_type_scope_declarations();
        auto found_name            = false;
        auto found_later_base_type = false;

        for (auto& o : objects) {
            if (o->has_name(s)) {
                found_name = true;
                ret        = o;
            }
            if (o->has_name("this")) {
                if (found_name) {
                    found_later_base_type = true;
                    break;
                }
            }
        }

        //  If we didn't find a later base type, discard any name match
        if (!found_later_base_type) {
            ret = {};
        }

        return ret;
    }


    auto get_initializer_statements() const
        -> std::vector<statement_node*>
    {
        if (!initializer) {
            return {};
        }

        auto ret = std::vector<statement_node*>{};
        //  For non-compound initializers, we want just that statement
        if (!initializer->is_compound())
        {
            ret.push_back(initializer.get());
        }

        //  Else for compound initializers, we want the compound_statement's statements
        else
        {
            auto compound_stmt = initializer->get_if<compound_statement_node>();
            assert (compound_stmt);
            for (auto& o : compound_stmt->statements) {
                ret.push_back(o.get());
            }
        }

        return ret;
    }

    auto is_function_with_this() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->is_function_with_this();
        }
        //  else
        return false;
    }

    auto is_virtual_function() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->is_virtual_function();
        }
        //  else
        return false;
    }

    auto is_type_final() const
        -> bool
    {
        if (auto t = std::get_if<a_type>(&type)) {
            return (*t)->is_final();
        }
        //  else
        return false;
    }

    auto make_type_final()
        -> bool
    {
        if (auto t = std::get_if<a_type>(&type)) {
            (*t)->make_final();
            return true;
        }
        //  else
        return false;
    }

    auto make_function_virtual()
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->make_function_virtual();
        }
        //  else
        return false;
    }

    auto is_defaultable_function() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->is_defaultable();
        }
        //  else
        return false;
    }

    auto is_constructor() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->is_constructor();
        }
        //  else
        return false;
    }

    auto is_default_constructor() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->is_default_constructor();
        }
        //  else
        return false;
    }

    auto is_move() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->is_move();
        }
        //  else
        return false;
    }

    auto is_swap() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->is_swap();
        }
        //  else
        return false;
    }

    auto is_constructor_with_that() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->is_constructor_with_that();
        }
        //  else
        return false;
    }

    auto is_constructor_with_in_that() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->is_constructor_with_in_that();
        }
        //  else
        return false;
    }

    auto is_constructor_with_move_that() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->is_constructor_with_move_that();
        }
        //  else
        return false;
    }

    auto is_comparison() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->is_comparison();
        }
        //  else
        return false;
    }

    auto is_compound_assignment() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->is_compound_assignment();
        }
        //  else
        return false;
    }

    auto is_assignment() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->is_assignment();
        }
        //  else
        return false;
    }

    auto is_assignment_with_that() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->is_assignment_with_that();
        }
        //  else
        return false;
    }

    auto is_assignment_with_in_that() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->is_assignment_with_in_that();
        }
        //  else
        return false;
    }

    auto is_assignment_with_move_that() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->is_assignment_with_move_that();
        }
        //  else
        return false;
    }

    struct declared_value_set_funcs {
        declaration_node const*  out_this_in_that     = {};
        declaration_node const*  out_this_move_that   = {};
        declaration_node const*  inout_this_in_that   = {};
        declaration_node const*  inout_this_move_that = {};
        std::vector<std::string> assignments_from     = {};
    };

    auto find_declared_value_set_functions() const
        -> declared_value_set_funcs
    {
        if (!initializer) {
            return {};
        }

        auto compound_stmt = initializer->get_if<compound_statement_node>();
        assert (compound_stmt);

        auto ret = declared_value_set_funcs{};
        for (auto& o : compound_stmt->statements)
        {
            auto decl = o->get_if<declaration_node>();
            if (decl)
            {
                if (decl->is_constructor_with_in_that()) {
                    ret.out_this_in_that = decl;
                }
                if (decl->is_constructor_with_move_that()) {
                    ret.out_this_move_that = decl;
                }
                if (decl->is_assignment_with_in_that()) {
                    ret.inout_this_in_that = decl;
                }
                if (decl->is_assignment_with_move_that()) {
                    ret.inout_this_move_that = decl;
                }
                if (decl->is_assignment() && !decl->is_assignment_with_that()) {
                    ret.assignments_from.emplace_back( decl->nth_parameter_type_name(2) );
                }
            }
        }

        return ret;
    }

    auto find_parent_declared_value_set_functions() const
        -> declared_value_set_funcs
    {
        if (parent_is_type()) {
            return parent_declaration->find_declared_value_set_functions();
        }
        //  else
        return {};
    }


    auto is_destructor() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->is_destructor();
        }
        //  else
        return false;
    }

    auto has_declared_return_type() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->has_declared_return_type();
        }
        //  else
        return false;
    }

    auto unnamed_return_type_to_string() const
        -> std::string
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->unnamed_return_type_to_string();
        }
        //  else
        return {};
    }

    auto has_bool_return_type() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->has_bool_return_type();
        }
        //  else
        return false;
    }

    auto has_non_void_return_type() const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->has_non_void_return_type();
        }
        //  else
        return false;
    }

    auto has_parameter_with_name_and_pass(
        std::string_view s,
        passing_style    pass
    ) const
        -> bool
    {
        if (auto func = std::get_if<a_function>(&type)) {
            return (*func)->has_parameter_with_name_and_pass(s, pass);
        }
        //  else
        return false;
    }

    auto is_binary_comparison_function() const
        -> bool
    {
        return
            is_function()
            && (
                has_name("operator==")
                || has_name("operator!=")
                || has_name("operator<")
                || has_name("operator<=")
                || has_name("operator>")
                || has_name("operator>=")
                );
    }

    auto is_const() const
        -> bool
    {
        return
            type.index() == an_object
            && !std::get<an_object>(type)->pc_qualifiers.empty()
            && *std::get<an_object>(type)->pc_qualifiers.front() == "const"
            ;
    }

    auto has_wildcard_type() const
        -> bool
    {
        return
            type.index() == an_object
            && std::get<an_object>(type)->is_wildcard()
            ;
    }

    auto get_object_type() const
        -> type_id_node const*
    {
        if (type.index() == an_object) {
            return std::get<an_object>(type).get();
        }
        //  Else
        return {};
    }

    //  Internals
    //
    auto position() const
        -> source_position
    {
        if (identifier) {
            return identifier->position();
        }
        return pos;
    }

    auto visit(auto& v, int depth)
        -> void
    {
        v.start(*this, depth);

        v.start(declaration_identifier_tag{}, depth);
        if (identifier) {
            identifier->visit(v, depth+1);
        }
        v.end(declaration_identifier_tag{}, depth);

        try_visit<a_function >(type, v, depth+1);
        try_visit<an_object  >(type, v, depth+1);
        try_visit<a_type     >(type, v, depth+1);
        try_visit<a_namespace>(type, v, depth+1);
        try_visit<an_alias   >(type, v, depth+1);

        for (auto& m : metafunctions) {
            assert(m);
            m->visit(v, depth+1);
        }

        if (initializer) {
            initializer->visit(v, depth+1);
        }

        v.end(*this, depth);
    }
};

struct next_expression_tag { };

struct translation_unit_node
{
    std::vector< std::unique_ptr<declaration_node> > declarations;

    auto position() const -> source_position
    {
        if (std::ssize(declarations) > 0) {
            return declarations.front()->position();
        }
        return {};
    }

    auto visit(auto& v, int depth) -> void
    {
        v.start(*this, depth);
        for (auto const& x : declarations) {
            assert(x);
            x->visit(v, depth + 1);
        }
        v.end(*this, depth);
    }
};

auto postfix_expression_node::visit(auto& v, int depth)
    -> void
{
    v.start(*this, depth);
    assert (expr);
    expr->visit(v, depth+1);
    for (auto const& x : ops) {
        assert (x.op);
        v.start(*x.op, depth+1);
        if (x.id_expr) {
            x.id_expr->visit(v, depth+1);
        }
        if (x.expr_list) {
            x.expr_list->visit(v, depth+1);
        }
    }
    v.end(*this, depth);
}

auto prefix_expression_node::visit(auto& v, int depth)
    -> void
{
    v.start(*this, depth);
    for (auto const& x : ops) {
        assert (x);
        v.start(*x, depth+1);
    }
    assert (expr);
    expr->visit(v, depth+1);
    v.end(*this, depth);
}

auto alternative_node::visit(auto& v, int depth)
    -> void
{
    v.start(*this, depth);
    if (name) {
        v.start(*name, depth+1);
    }
    assert (is_as_keyword);
    v.start(*is_as_keyword, depth+1);
    if (type_id) {
        type_id->visit(v, depth+1);
    }
    else {
        assert (value);
        value->visit(v, depth+1);
    }
    assert (statement);
    statement->visit(v, depth+1);
    v.end(*this, depth);
}


auto compound_statement_node::visit(auto& v, int depth)
    -> void
{
    v.start(*this, depth);
    for (auto const& x : statements) {
        assert(x);
        x->visit(v, depth+1);
    }
    v.end(*this, depth);
}

auto statement_node::visit(auto& v, int depth)
    -> void
{
    v.start(*this, depth);
    if (parameters) {
        parameters->visit(v, depth+1);
    }
    try_visit<expression >(statement, v, depth);
    try_visit<compound   >(statement, v, depth);
    try_visit<selection  >(statement, v, depth);
    try_visit<declaration>(statement, v, depth);
    try_visit<return_    >(statement, v, depth);
    try_visit<iteration  >(statement, v, depth);
    try_visit<contract   >(statement, v, depth);
    try_visit<inspect    >(statement, v, depth);
    try_visit<jump       >(statement, v, depth);
    v.end(*this, depth);
}

auto parameter_declaration_node::visit(auto& v, int depth)
    -> void
{
    v.start(*this, depth);
    assert(declaration);
    declaration->visit(v, depth + 1);
    v.end(*this, depth);
}

auto primary_expression_node::visit(auto& v, int depth)
    -> void
{
    v.start(*this, depth);
    try_visit<identifier     >(expr, v, depth);
    try_visit<expression_list>(expr, v, depth);
    try_visit<id_expression  >(expr, v, depth);
    try_visit<declaration    >(expr, v, depth);
    try_visit<inspect        >(expr, v, depth);
    try_visit<literal        >(expr, v, depth);
    v.end(*this, depth);
}

auto iteration_statement_node::visit(auto& v, int depth)
    -> void
{
    v.start(*this, depth);
    if (label) {
        label->visit(v, depth+1);
    }
    if (identifier) {
        identifier->visit(v, depth+1);
    }
    if (statements) {
        statements->visit(v, depth+1);
    }
    if (next_expression) {
        v.start(next_expression_tag{}, depth);
        next_expression->visit(v, depth+1);
        v.end(next_expression_tag{}, depth);
    }
    if (condition) {
        assert(!range && !body);
        condition->visit(v, depth+1);
    }
    else {
        assert(range && parameter && body);
        range->visit(v, depth+1);
        parameter->visit(v, depth+1);
        body->visit(v, depth+1);
    }
    v.end(*this, depth);
}
}

#endif
