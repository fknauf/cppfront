#include "parse_tree.h"

namespace cpp2 {

auto to_passing_style(token const& t) -> passing_style {
    if (t.type() == lexeme::Identifier) {
        if (t == "in"     ) { return passing_style::in; }
        if (t == "copy"   ) { return passing_style::copy; }
        if (t == "inout"  ) { return passing_style::inout; }
        if (t == "out"    ) { return passing_style::out; }
        if (t == "move"   ) { return passing_style::move; }
        if (t == "forward") { return passing_style::forward; }
    }
    return passing_style::invalid;
}
auto to_string_view(passing_style pass) -> std::string_view {
    switch (pass) {
    break;case passing_style::in     : return "in";
    break;case passing_style::copy   : return "copy";
    break;case passing_style::inout  : return "inout";
    break;case passing_style::out    : return "out";
    break;case passing_style::move   : return "move";
    break;case passing_style::forward: return "forward";
    break;default                    : return "INVALID passing_style";
    }
}

auto primary_expression_node::is_identifier() const
    -> bool
{
    return expr.index() == identifier;
}

auto primary_expression_node::is_id_expression() const
    -> bool
{
    return expr.index() == id_expression;
}

auto primary_expression_node::is_expression_list() const
    -> bool
{
    return expr.index() == expression_list;
}

auto primary_expression_node::get_expression_list() const
    -> expression_list_node const*
{
    if (is_expression_list()) {
        return std::get<expression_list>(expr).get();
    }
    return {};
}

auto primary_expression_node::is_literal() const
    -> bool
{
    return expr.index() == literal;
}

auto primary_expression_node::is_fold_expression() const
    -> bool
{
    //  This is a fold-expression if any subexpression has
    //  has an identifier named "..."
    switch (expr.index()) {
    break;case identifier:
        return *std::get<identifier>(expr) == "...";
    break;case expression_list:
        return std::get<expression_list>(expr)->is_fold_expression();
    break;case id_expression:
        return std::get<id_expression>(expr)->is_fold_expression();
    break;default: ; // the others can't contain folds
    }
    return false;
}


auto postfix_expression_node::get_first_token_ignoring_this() const
    -> token const*
{
    if (
        expr->get_token()
        && *expr->get_token() == "this"
        && std::ssize(ops) == 1
        && ops[0].op->type() == lexeme::Dot
        )
    {
        return ops[0].id_expr->get_token();
    }
    return expr->get_token();
}


auto postfix_expression_node::to_string() const
    -> std::string
{
    assert (expr);
    auto ret = expr->to_string();

    for (auto const& x : ops) {
        assert (x.op);
        ret += x.op->as_string_view();
        if (x.id_expr) {
            ret += x.id_expr->to_string();
        }
        if (x.expr_list) {
            return "(*ERROR*) temporary alpha limitation: type metafunctions cannot stringize expressions that involve nested expression-lists, declarations, or inspect expressions";
        }
    }

    return ret;
}

expression_node::expression_node()
{
    if (!expression_statement_node::current_expression_statements.empty()) {
        my_statement = expression_statement_node::current_expression_statements.back();
    }
}


auto expression_node::is_standalone_expression() const
    -> bool
{
    return
        my_statement
        && my_statement->subexpression_count() == subexpression_count()
        ;
}

auto prefix_expression_node::is_fold_expression() const
    -> bool
{
    //  This is a fold-expression if any subexpression
    //  has an identifier named "..."
    return expr->is_fold_expression();
}

auto prefix_expression_node::is_identifier() const
    -> bool
{
    return ops.empty() && expr->is_identifier();
}

auto prefix_expression_node::is_id_expression() const
    -> bool
{
    return ops.empty() && expr->is_id_expression();
}

auto prefix_expression_node::is_expression_list() const
    -> bool
{
    return ops.empty() && expr->is_expression_list();
}

auto prefix_expression_node::get_expression_list() const
    -> expression_list_node const*
{
    if (is_expression_list()) {
        return expr->get_expression_list();
    }
    return {};
}

auto prefix_expression_node::is_literal() const
    -> bool
{
    return ops.empty() && expr->is_literal();
}

auto prefix_expression_node::is_result_a_temporary_variable() const -> bool {
    if (ops.empty()) {
        return expr->is_result_a_temporary_variable();
    } else {
        return true;
    }
}


auto expression_node::get_lhs_rhs_if_simple_assignment() const
    -> assignment_expression_lhs_rhs
{
    auto ret = expr->get_lhs_rhs_if_simple_binary_expression_with(lexeme::Assignment);
    return { ret.lhs, ret.rhs };
}


auto capture_group::remove(postfix_expression_node* p)
    -> void
{
    p->cap_grp = {};
    auto old_size = members.size();
    std::erase(members, p);
    assert (members.size() == old_size-1);
}


capture_group::~capture_group()
{
    assert (members.empty());
    //  We shouldn't need to do this:
    //      while (!members.empty()) {
    //          remove(members.front().capture_expr);
    //      }
    //  if the capture_group outlives the tree of things that can point to it
    //   => each node with a capture_group should declare it as the first member
    //      before any other node that could own a postfix_expression that could
    //      point back up to that capture_group
}


auto prefix_expression_node::to_string() const
    -> std::string
{
    auto ret = std::string{};

    for (auto const& x : ops) {
        assert (x);
        ret += x->as_string_view();
    }

    assert (expr);
    return ret + expr->to_string();
}


auto prefix_expression_node::position() const
    -> source_position
{
    if (std::ssize(ops) > 0) {
        return ops.front()->position();
    }
    assert (expr);
    return expr->position();
}

auto unqualified_id_node::to_string() const
    -> std::string
{
    assert(identifier);
    auto ret = identifier->to_string();
    if (open_angle != source_position{}) {
        auto separator = std::string{"<"};
        for (auto& t : template_args) {
            ret += separator;
            assert(t.arg.index() != template_argument::empty);
            if (t.arg.index() == template_argument::expression) {
                ret += std::get<template_argument::expression>(t.arg)->to_string();
            }
            else if (t.arg.index() == template_argument::type_id) {
                ret += std::get<template_argument::type_id>(t.arg)->to_string();
            }
            separator = ",";
        }
        ret += ">";
    }
    return ret;
}

auto template_argument::to_string() const
    -> std::string
{
    switch (arg.index()) {
    break;case empty:
        return {};
    break;case expression:
        return std::get<expression>(arg)->to_string();
    break;case type_id:
        return std::get<type_id>(arg)->to_string();
    break;default:
        assert(!"ICE: invalid template_argument state");
    }
    // else
    return {};
}

auto parameter_declaration_node::has_name() const
    -> bool
{
    return declaration->has_name();
}


auto parameter_declaration_node::name() const
    -> token const*
{
    return declaration->name();
}


auto parameter_declaration_node::has_name(std::string_view s) const
    -> bool
{
    return declaration->has_name(s);
}


auto function_type_node::nth_parameter_type_name(int n) const
    -> std::string
{
    if (std::ssize(parameters->parameters) >= n)
    {
        return parameters->parameters[n-1]->declaration->get_object_type()->to_string();
    }
    //  Else
    return "";
}


auto function_type_node::is_function_with_this() const
    -> bool
{
    if (
        (*parameters).ssize() > 0
        && (*parameters)[0]->has_name("this")
        )
    {
        return true;
    }
    return false;
}


auto function_type_node::is_virtual_function() const
    -> bool
{
    if (
        (*parameters).ssize() > 0
        && (*parameters)[0]->has_name("this")
        && (*parameters)[0]->is_virtual()
        )
    {
        return true;
    }
    return false;
}


auto function_type_node::make_function_virtual()
    -> bool
{
    if (is_function_with_this()) {
        (*parameters)[0]->make_virtual();
        return true;
    }
    return false;
}


auto function_type_node::is_defaultable() const
    -> bool
{
    if (
        my_decl->has_name("operator==")
        || my_decl->has_name("operator<=>")
        )
    {
        return true;
    }
    return false;
}


auto function_type_node::is_constructor() const
    -> bool
{
    if (
        (*parameters).ssize() > 0
        && (*parameters)[0]->has_name("this")
        && (*parameters)[0]->direction() == passing_style::out
        )
    {
        assert(my_decl->has_name("operator="));
        return true;
    }
    return false;
}


auto function_type_node::is_default_constructor() const
    -> bool
{
    if (
        is_constructor()
        && (*parameters).ssize() == 1
        )
    {
        return true;
    }
    return false;
}


auto function_type_node::is_move() const
    -> bool
{
    if (
        (is_constructor() || is_assignment())
        && (*parameters).ssize() == 2
        && (*parameters)[1]->has_name("that")
        && (*parameters)[1]->direction() == passing_style::move
        )
    {
        return true;
    }
    return false;
}


auto function_type_node::is_swap() const
    -> bool
{
    assert (my_decl);
    if (
        my_decl->has_name("swap")
        && (*parameters).ssize() == 2
        && (*parameters)[1]->has_name("that")
        )
    {
        return true;
    }
    return false;
}


auto function_type_node::is_constructor_with_that() const
    -> bool
{
    if (
        is_constructor()
        && (*parameters).ssize() == 2
        && (*parameters)[1]->has_name("that")
        )
    {
        return true;
    }
    return false;
}


auto function_type_node::is_assignment_with_that() const
    -> bool
{
    if (
        is_assignment()
        && (*parameters).ssize() == 2
        && (*parameters)[1]->has_name("that")
        )
    {
        return true;
    }
    return false;
}


auto function_type_node::is_constructor_with_in_that() const
    -> bool
{
    if (
        is_constructor()
        && (*parameters).ssize() == 2
        && (*parameters)[1]->has_name("that")
        && (*parameters)[1]->direction() == passing_style::in
        )
    {
        return true;
    }
    return false;
}


auto function_type_node::is_constructor_with_move_that() const
    -> bool
{
    if (
        is_constructor()
        && (*parameters).ssize() == 2
        && (*parameters)[1]->has_name("that")
        && (*parameters)[1]->direction() == passing_style::move
        )
    {
        return true;
    }
    return false;
}


auto function_type_node::is_comparison() const
    -> bool
{
    if (
        (
            my_decl->has_name("operator==")
            || my_decl->has_name("operator!=")
            || my_decl->has_name("operator<")
            || my_decl->has_name("operator<=")
            || my_decl->has_name("operator>")
            || my_decl->has_name("operator>=")
            || my_decl->has_name("operator<=>")
        )
        )
    {
        return true;
    }
    return false;
}


auto function_type_node::is_compound_assignment() const
    -> bool
{
    if (
        (
            my_decl->has_name("operator+=")
            || my_decl->has_name("operator-=")
            || my_decl->has_name("operator*=")
            || my_decl->has_name("operator/=")
            || my_decl->has_name("operator%=")
            || my_decl->has_name("operator&=")
            || my_decl->has_name("operator|=")
            || my_decl->has_name("operator^=")
            || my_decl->has_name("operator<<=")
            || my_decl->has_name("operator>>=")
        )
        && (*parameters).ssize() > 1
        && (*parameters)[0]->has_name("this")
        && (*parameters)[0]->direction() == passing_style::inout
        )
    {
        return true;
    }
    return false;
}


auto function_type_node::is_assignment() const
    -> bool
{
    if (
        my_decl->has_name("operator=")
        && (*parameters).ssize() > 1
        && (*parameters)[0]->has_name("this")
        && (*parameters)[0]->direction() == passing_style::inout
        )
    {
        return true;
    }
    return false;
}


auto function_type_node::is_assignment_with_in_that() const
    -> bool
{
    if (
        is_assignment()
        && (*parameters).ssize() == 2
        && (*parameters)[1]->has_name("that")
        && (*parameters)[1]->direction() == passing_style::in
        )
    {
        return true;
    }
    return false;
}


auto function_type_node::is_assignment_with_move_that() const
    -> bool
{
    if (
        is_assignment()
        && (*parameters).ssize() == 2
        && (*parameters)[1]->has_name("that")
        && (*parameters)[1]->direction() == passing_style::move
        )
    {
        return true;
    }
    return false;
}


auto function_type_node::is_destructor() const
    -> bool
{
    if (
        my_decl->has_name("operator=")
        && (*parameters).ssize() == 1
        && (*parameters)[0]->has_name("this")
        && (*parameters)[0]->direction() == passing_style::move
        )
    {
        return true;
    }
    return false;
}


auto primary_expression_node::template_arguments() const
    -> std::vector<template_argument> const&
{
    if (expr.index() == id_expression) {
        return std::get<id_expression>(expr)->template_arguments();
    }
    // else
    return no_template_args;
}


auto primary_expression_node::get_token() const
    -> token const*
{
    if (expr.index() == identifier) {
        return std::get<identifier>(expr);
    }
    else if (expr.index() == id_expression) {
        return std::get<id_expression>(expr)->get_token();
    }
    else if (expr.index() == literal) {
        return std::get<literal>(expr)->get_token();
    }
    // else (because we're deliberately ignoring the other
    //       options which are more than a single token)
    return {};
}


auto primary_expression_node::to_string() const
    -> std::string
{
    switch (expr.index())
    {
    break;case empty:
        return {};

    break;case identifier: {
        auto const& s = std::get<identifier>(expr);
        assert (s);
        return s->to_string();
    }

    break;case id_expression: {
        auto const& s = std::get<id_expression>(expr);
        assert (s);
        return s->to_string();
    }

    break;case literal: {
        auto const& i = std::get<literal>(expr);
        assert (i);
        return i->to_string();
    }

    break;default:
        return "(*ERROR*) temporary alpha limitation: type metafunctions cannot stringize expressions that involve nested expression-lists, declarations, or inspect expressions";
    }
}


auto primary_expression_node::position() const
    -> source_position
{
    switch (expr.index())
    {
    break;case empty:
        return { 0, 0 };

    break;case identifier: {
        auto const& s = std::get<identifier>(expr);
        assert (s);
        return s->position();
    }

    break;case expression_list: {
        auto const& s = std::get<expression_list>(expr);
        assert (s);
        return s->position();
    }

    break;case id_expression: {
        auto const& s = std::get<id_expression>(expr);
        assert (s);
        return s->position();
    }

    break;case declaration: {
        auto const& s = std::get<declaration>(expr);
        assert (s);
        return s->position();
    }

    break;case inspect: {
        auto const& i = std::get<inspect>(expr);
        assert (i);
        return i->position();
    }

    break;case literal: {
        auto const& i = std::get<literal>(expr);
        assert (i);
        return i->position();
    }

    break;default:
        assert (!"illegal primary_expression_node state");
        return { 0, 0 };
    }
}

auto statement_node::position() const
    -> source_position
{
    switch (statement.index())
    {
    break;case expression: {
        auto const& s = std::get<expression>(statement);
        assert (s);
        return s->position();
    }

    break;case compound: {
        auto const& s = std::get<compound>(statement);
        assert (s);
        return s->position();
    }

    break;case selection: {
        auto const& s = std::get<selection>(statement);
        assert (s);
        return s->position();
    }

    break;case declaration: {
        auto const& s = std::get<declaration>(statement);
        assert (s);
        return s->position();
    }

    break;case return_: {
        auto const& s = std::get<return_>(statement);
        assert (s);
        return s->position();
    }

    break;case iteration: {
        auto const& s = std::get<iteration>(statement);
        assert (s);
        return s->position();
    }

    break;case contract: {
        auto const& s = std::get<contract>(statement);
        assert (s);
        return s->position();
    }

    break;default:
        assert (!"illegal statement_node state");
        return { 0, 0 };
    }
}


auto parameter_declaration_node::position() const
    -> source_position
{
    assert (declaration);
    return pos;
}

auto to_string(accessibility a)
    -> std::string
{
    switch (a) {
    break;case accessibility::public_   : return "public";
    break;case accessibility::protected_: return "protected";
    break;case accessibility::private_  : return "private";
    break;default: assert(a == accessibility::default_);
    }
    return "default";
}

}