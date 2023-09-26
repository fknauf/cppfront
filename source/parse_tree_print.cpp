#include "parse_tree_print.h"

namespace cpp2 {
auto pretty_print_visualize(token const& t, int)
    -> std::string
{
    return t.to_string();
}


auto pretty_print_visualize(primary_expression_node const& n, int indent)
    -> std::string
{
    auto ret = std::string{};

    ret += try_pretty_print_visualize<primary_expression_node::identifier     >(n.expr, indent);
    ret += try_pretty_print_visualize<primary_expression_node::expression_list>(n.expr, indent);
    ret += try_pretty_print_visualize<primary_expression_node::id_expression  >(n.expr, indent);
    ret += try_pretty_print_visualize<primary_expression_node::declaration    >(n.expr, indent);
    ret += try_pretty_print_visualize<primary_expression_node::inspect        >(n.expr, indent);
    ret += try_pretty_print_visualize<primary_expression_node::literal        >(n.expr, indent);

    return ret;
}


auto pretty_print_visualize(literal_node const& n, int)
    -> std::string
{
    //  TODO: This is an initial visualizer implementation, and still
    //        skips a few rarer things (such as raw string literals)

    assert(n.literal);

    auto ret = n.literal->to_string();

    if (n.user_defined_suffix) {
        ret += n.user_defined_suffix->as_string_view();
    }

    return ret;
}


auto pretty_print_visualize(prefix_expression_node const& n, int indent)
    -> std::string
{
    assert(n.expr);

    auto ret = std::string{};

    for (auto& op : n.ops) {
        assert(op);
        ret += op->as_string_view();
    }

    ret += pretty_print_visualize(*n.expr, indent);

    return ret;
}


template<
    String   Name,
    typename Term
>
auto pretty_print_visualize(binary_expression_node<Name,Term> const& n, int indent)
    -> std::string
{
    assert(n.expr);

    auto ret = pretty_print_visualize(*n.expr, indent);
    for (auto& term : n.terms) {
        assert(term.op && term.expr);
        ret += " " + term.op->to_string()
            + " " + pretty_print_visualize(*term.expr, indent);
    }
    return ret;
}


auto pretty_print_visualize(expression_node const& n, int indent)
    -> std::string
{
    assert(n.expr);
    return pretty_print_visualize(*n.expr, indent);
}


auto pretty_print_visualize(expression_list_node const& n, int indent)
    -> std::string
{
    assert(n.open_paren && n.close_paren);

    auto ret = n.open_paren->to_string();

    for (auto i = 0; auto& expr : n.expressions) {
        assert(expr.expr);
        if (
            expr.pass == passing_style::out
            || expr.pass == passing_style::move
            || expr.pass == passing_style::forward
            )
        {
            ret += to_string_view(expr.pass) + std::string{" "};
        }
        ret += pretty_print_visualize(*expr.expr, indent);
        if (++i < std::ssize(n.expressions)) {
            ret += ", ";
        }
    }

    ret += n.close_paren->as_string_view();

    return ret;
}


auto pretty_print_visualize(expression_statement_node const& n, int indent)
    -> std::string
{
    assert(n.expr);

    auto ret = pretty_print_visualize(*n.expr, indent);

    if (n.has_semicolon) {
        ret += ";";
    }

    return ret;
}


auto pretty_print_visualize(postfix_expression_node const& n, int indent)
    -> std::string
{
    assert(n.expr);

    auto ret = pretty_print_visualize(*n.expr, indent);

    for (auto& op : n.ops)
    {
        assert(op.op);
        if (op.expr_list) {
            assert (op.op_close);
            ret += pretty_print_visualize(*op.expr_list, indent);
        }
        else {
            ret += op.op->as_string_view();
            if (op.id_expr) {
                ret += pretty_print_visualize(*op.id_expr, indent);
            }
        }
    }

    return ret;
}


auto pretty_print_visualize(unqualified_id_node const& n, int indent)
    -> std::string
{
    assert(n.identifier);

    auto ret = n.identifier->to_string();

    if (n.open_angle != source_position{})
    {
        ret += "<";
        for (bool first = true; auto& arg : n.template_args)
        {
            if (!first) {
                ret += ", ";
            }
            first = false;
            ret += try_pretty_print_visualize<template_argument::expression>(arg.arg, indent);
            ret += try_pretty_print_visualize<template_argument::type_id   >(arg.arg, indent);
        }
        ret += ">";
    }

    return ret;
}


auto pretty_print_visualize(qualified_id_node const& n, int indent)
    -> std::string
{
    auto ret = std::string{};

    for (auto& id : n.ids) {
        if (id.scope_op) { ret += id.scope_op->as_string_view(); }
        assert (id.id);
        ret += pretty_print_visualize(*id.id, indent);
    }

    return ret;
}


auto pretty_print_visualize(type_id_node const& n, int indent)
    -> std::string
{
    auto ret = std::string{};

    for (auto& qual : n.pc_qualifiers) {
        assert(qual);
        ret += qual->as_string_view();
        ret += " ";
    }

    if (n.id.index() == type_id_node::empty) { ret += "_"; }
    ret += try_pretty_print_visualize<type_id_node::qualified  >(n.id, indent);
    ret += try_pretty_print_visualize<type_id_node::unqualified>(n.id, indent);
    ret += try_pretty_print_visualize<type_id_node::keyword    >(n.id, indent);

    return ret;
}


auto pretty_print_visualize(is_as_expression_node const& n, int indent)
    -> std::string
{
    assert (n.expr);

    auto ret = pretty_print_visualize(*n.expr, indent);

    for (auto& op : n.ops) {
        if (op.op)   { ret += " " + op.op->to_string() + " "; }
        if (op.type) { ret += pretty_print_visualize(*op.type, indent); }
        if (op.expr) { ret += pretty_print_visualize(*op.expr, indent); }
    }

    return ret;
}


auto pretty_print_visualize(id_expression_node const& n, int indent)
    -> std::string
{
    auto ret = std::string{};

    ret += try_pretty_print_visualize<id_expression_node::qualified  >(n.id, indent);
    ret += try_pretty_print_visualize<id_expression_node::unqualified>(n.id, indent);

    return ret;
}


auto pretty_print_visualize(compound_statement_node const& n, int indent)
    -> std::string
{
    auto ret = std::string{"\n"} + pre(indent) + "{";

    for (auto& stmt : n.statements) {
        assert (stmt);
        ret += pretty_print_visualize(*stmt, indent+1);
    }

    ret += std::string{"\n"} + pre(indent) + "}";

    return ret;
}


auto pretty_print_visualize(selection_statement_node const& n, int indent)
    -> std::string
{
    assert (n.identifier && n.expression && n.true_branch && n.false_branch);

    auto ret = std::string{};

    ret += std::string{"\n"} + pre(indent) + n.identifier->as_string_view() + " ";

    if (n.is_constexpr) {
        ret += "constexpr ";
    }

    ret += pretty_print_visualize(*n.expression, indent)
        + pretty_print_visualize(*n.true_branch, indent);

    if (n.has_source_false_branch) {
        ret += std::string{"\n"} + pre(indent) + "else "
            + pretty_print_visualize(*n.false_branch, indent);
    }

    return ret;
}


auto pretty_print_visualize(iteration_statement_node const& n, int indent)
    -> std::string
{
    //  First compute the common parts

    auto next_expr = std::string{};
    if (n.next_expression) {
        next_expr += std::string{"\n"} + pre(indent+1) + "next " + pretty_print_visualize(*n.next_expression, indent);
    }

    auto stmts = std::string{};
    if (n.statements) {
        stmts += pretty_print_visualize(*n.statements, indent+1);
    }

    //  Then slot them in where appropriate

    auto ret = std::string{};
    assert (n.identifier);

    ret += std::string{"\n"} + pre(indent);
    if (n.label) {
        ret += n.label->to_string()
            + ": ";
    }

    if (*n.identifier == "while") {
        assert (n.condition);
        ret += "while "
            + pretty_print_visualize(*n.condition, indent) + next_expr + stmts;
    }
    else if (*n.identifier == "do") {
        assert (n.condition);
        ret += "do "
            + stmts
            + "\n" + pre(indent) + "while "
            + pretty_print_visualize(*n.condition, indent)
            + next_expr + ";";
    }
    else {
        assert (n.range && n.parameter && n.body);
        ret += "for "
            + pretty_print_visualize(*n.range, indent)
            + next_expr
            + "\n" + pre(indent) + "do (" + pretty_print_visualize(*n.parameter, indent + 1) + ")"
            + pretty_print_visualize(*n.body, indent+1);
    }

    return ret;
}


auto pretty_print_visualize(return_statement_node const& n, int indent)
    -> std::string
{
    auto ret = std::string{"\n"} + pre(indent) + "return";

    if (n.expression) {
        ret += " " + pretty_print_visualize(*n.expression, indent);
    }

    ret += ";";

    return ret;
}


auto pretty_print_visualize(alternative_node const& n, int indent)
    -> std::string
{
    auto ret = std::string{};
    assert (n.is_as_keyword);
    ret += std::string{"\n"} + pre(indent);
    if (n.name) {
        ret += pretty_print_visualize(*n.name, indent) + ": ";
    }
    ret += n.is_as_keyword->as_string_view();
    if (n.type_id) {
        ret += " " + pretty_print_visualize(*n.type_id, indent);
    }
    if (n.value) {
        ret += " " + pretty_print_visualize(*n.value, indent);
    }
    ret += " = " + pretty_print_visualize(*n.statement, indent+1);
    return ret;
}


auto pretty_print_visualize(inspect_expression_node const& n, int indent)
    -> std::string
{
    assert (n.expression);

    auto ret = std::string{"inspect"};

    if (n.is_constexpr) {
        ret += " constexpr";
    }

    ret += " " + pretty_print_visualize(*n.expression, indent);

    if (n.result_type) {
        ret += " -> " + pretty_print_visualize(*n.result_type, indent);
    }

    ret += " {";

    for (auto& alt : n.alternatives) {
        assert(alt);
        ret += pretty_print_visualize(*alt, indent+1);
    }

    ret += std::string{"\n"} + pre(indent) + "}";

    return ret;
}


auto pretty_print_visualize(contract_node const& n, int indent)
    -> std::string
{
    assert (n.kind && n.condition);

    auto ret = std::string{"\n"} + pre(indent) + "[[" + n.kind->as_string_view();

    if (n.group) {
        ret += " " + pretty_print_visualize(*n.group, indent);
    }

    ret += ": " + pretty_print_visualize(*n.condition, indent);

    if (n.message) {
        ret += " " + n.message->to_string();
    }

    ret += "]]";

    return ret;
}


auto pretty_print_visualize(jump_statement_node const& n, int indent)
    -> std::string
{
    assert (n.keyword);

    auto ret = std::string{"\n"} + pre(indent) + n.keyword->as_string_view();

    if (n.label) {
        ret += " " + n.label->to_string();
    }

    ret += ";";

    return ret;
}


auto pretty_print_visualize(statement_node const& n, int indent)
    -> std::string
{
    auto ret = std::string{};

    if (n.is_expression())
    {
        if (n.compound_parent) {
            ret += std::string{"\n"} + pre(indent);
        }
        auto& expr = std::get<statement_node::expression>(n.statement);
        assert (expr);
        ret += pretty_print_visualize(*expr, indent);
    }
    else
    {
        if (n.parameters) {
            ret += std::string{"\n"} + pre(indent) + pretty_print_visualize(*n.parameters, indent);
        }

        ret += try_pretty_print_visualize<statement_node::compound   >(n.statement, indent);
        ret += try_pretty_print_visualize<statement_node::selection  >(n.statement, indent);
        ret += try_pretty_print_visualize<statement_node::declaration>(n.statement, indent);
        ret += try_pretty_print_visualize<statement_node::return_    >(n.statement, indent);
        ret += try_pretty_print_visualize<statement_node::iteration  >(n.statement, indent);
        ret += try_pretty_print_visualize<statement_node::contract   >(n.statement, indent);
        ret += try_pretty_print_visualize<statement_node::inspect    >(n.statement, indent);
        ret += try_pretty_print_visualize<statement_node::jump       >(n.statement, indent);
    }

    return ret;
}


auto pretty_print_visualize(parameter_declaration_node const& n, int indent, bool is_template_param_list /* = false */ )
    -> std::string
{
    assert (n.declaration);

    auto ret = std::string{};

    if (!is_template_param_list) {
        switch (n.mod) {
        break;case parameter_declaration_node::modifier::implicit : ret += "implicit ";
        break;case parameter_declaration_node::modifier::virtual_ : ret += "virtual ";
        break;case parameter_declaration_node::modifier::override_: ret += "override ";
        break;case parameter_declaration_node::modifier::final_   : ret += "final ";
        break;default: ; // none
        }

        ret += to_string_view(n.pass);
        ret += " ";
    }

    ret += pretty_print_visualize(*n.declaration, indent);

    return ret;
}


auto pretty_print_visualize(parameter_declaration_list_node const& n, int indent, bool is_template_param_list /* = false */)
    -> std::string
{
    assert(n.open_paren && n.close_paren);

    auto ret = n.open_paren->to_string();

    auto space = std::string{};
    if (std::ssize(n.parameters) > 1) {
        space += std::string{"\n"} + pre(indent+1);
    }

    for (auto i = 0; auto& param : n.parameters) {
        ret += space + pretty_print_visualize(*param, indent+1, is_template_param_list);
        if (++i < std::ssize(n.parameters)) {
            ret += ", ";
        }
    }

    if (std::ssize(n.parameters) > 1) {
        ret += std::string{"\n"} + pre(indent);
    }
    ret += n.close_paren->to_string();

    return ret;
}


auto pretty_print_visualize(function_type_node const& n, int indent)
    -> std::string
{
    assert (n.parameters);

    auto ret = pretty_print_visualize(*n.parameters, indent);

    if (n.throws) {
        ret += " throws";
    }

    if (n.has_non_void_return_type()) {
        ret += " -> ";
        ret += try_pretty_print_visualize<function_type_node::list>(n.returns, indent+1);
        if (n.returns.index() == function_type_node::id) {
            auto& single = std::get<function_type_node::id>(n.returns);
            ret += to_string_view(single.pass)
                + std::string{" "} + pretty_print_visualize(*single.type, indent+1);
        }
    }

    for (auto& contract: n.contracts) {
        assert(contract);
        ret += pretty_print_visualize(*contract, indent+1);
    }

    return ret;
}


auto pretty_print_visualize(type_node const& n)
    -> std::string
{
    assert (n.type);

    auto ret = std::string{};

    if (n.final) {
        ret += "final ";
    }

    ret += "type";

    return ret;
}


auto pretty_print_visualize(namespace_node const&)
    -> std::string
{
    return "namespace";
}


auto pretty_print_visualize(declaration_node const& n, int indent, bool include_metafunctions_list /* = false */ )
    -> std::string
{
    indent_spaces = 4;

    //  First compute the common parts

    auto metafunctions = std::string{};
    if (include_metafunctions_list) {
        for (auto& meta : n.metafunctions) {
            metafunctions += " @" + pretty_print_visualize(*meta, indent);
        }
    }

    auto template_params = std::string{};
    if (n.template_parameters) {
        template_params += " " + pretty_print_visualize(*n.template_parameters, indent + 1, true);
    }

    auto requires_clause = std::string{};
    if (n.requires_clause_expression) {
        requires_clause += " requires (" + pretty_print_visualize(*n.requires_clause_expression, indent) + ")";
    }

    auto initializer = std::string{};
    if (n.initializer) {
        auto adjusted_indent = indent;
        if (!n.name()) {
            ++adjusted_indent;
        }
        initializer = " =";
        if (n.is_function() && n.is_constexpr) {
            initializer += "=";
        }
        initializer += " " + pretty_print_visualize(*n.initializer, adjusted_indent);
    }
    else if (!n.is_parameter) {
        initializer = ";";
    }

    //  Then slot them in where appropriate

    auto ret = std::string{""};

    //  Add an extra newline for spacing, unless this declaration
    //  is within a function body or is the first member of a type
    if (
        !n.parent_is_function()
        && !n.parent_is_object()
        && !n.is_parameter
        )
    {
        static declaration_node const* last_parent_type = {};
        if (n.parent_is_type()) {
            if (last_parent_type != n.get_parent()) {
                last_parent_type = n.get_parent();
            }
            else {
                ret += "\n";
            }
        }
        else {
            ret += "\n";
        }
    }
    if (!n.is_parameter && n.name()) {
        ret += std::string{"\n"} + pre(indent);
    }

    switch (n.access) {
    break;case accessibility::public_    : ret += "public ";
    break;case accessibility::protected_ : ret += "protected ";
    break;case accessibility::private_   : ret += "private ";
    break;default: ; // default accessibility
    }

    if (n.identifier) {
        ret += pretty_print_visualize(*n.identifier, indent);
    }

    if (n.is_parameter && (n.has_name("this") || n.has_name("that"))) {
        return ret;
    }

    if (n.is_variadic) {
        ret += "...";
    }

    ret += ":";

    if (n.is_function()) {
        auto& func = std::get<declaration_node::a_function>(n.type);
        assert(func);
        ret += metafunctions
            + template_params
            + pretty_print_visualize(*func, indent)
            + requires_clause
            + initializer;
    }
    else if (n.is_object()) {
        auto& type_id = std::get<declaration_node::an_object>(n.type);
        assert(type_id);
        ret += metafunctions
            + template_params;
        if (!n.has_wildcard_type()) {
            ret += " " + pretty_print_visualize(*type_id, indent);
        }
        ret += requires_clause
            + initializer;
    }
    else if (n.is_type()) {
        auto& t = std::get<declaration_node::a_type>(n.type);
        assert(t);
        ret += metafunctions
            + template_params
            + " " + pretty_print_visualize(*t)
            + initializer;
    }
    else if (n.is_namespace()) {
        auto& t = std::get<declaration_node::a_type>(n.type);
        assert(t);
        ret += "namespace = "
            + initializer;
    }
    else if (n.is_alias()) {
        auto& a = std::get<declaration_node::an_alias>(n.type);
        assert(a);

        auto object_type_id = std::string{};
        if (a->type_id) {
            object_type_id += " " + pretty_print_visualize(*a->type_id, indent);
        }

        ret += template_params;
        if (a->is_type_alias()) {
            auto& t = std::get<alias_node::a_type>(a->initializer);
            ret += " type"
                + requires_clause
                + " == "
                + pretty_print_visualize(*t, indent)
                + ";";
        }
        else if (a->is_namespace_alias()) {
            auto& id = std::get<alias_node::a_namespace>(a->initializer);
            assert(id);
            ret += " namespace == "
                + pretty_print_visualize(*id, indent)
                + ";";
        }
        else if (a->is_object_alias()) {
            auto& expr = std::get<alias_node::an_object>(a->initializer);
            assert(expr);
            ret += object_type_id
                + requires_clause
                + " == "
                + pretty_print_visualize(*expr, indent)
                + ";";
        }
    }

    return ret;
}


auto pretty_print_visualize(translation_unit_node const& n)
    -> std::string
{
    auto ret = std::string{};

    for (auto& decl : n.declarations) {
        assert(decl);
        ret += pretty_print_visualize(*decl, 0);
    }

    return ret;
}

}
