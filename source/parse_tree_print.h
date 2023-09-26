#ifndef __CPP2_PARSE_TREE_PRINTER
#define __CPP2_PARSE_TREE_PRINTER

#include "lex.h"
#include "parse_tree.h"

#include <memory>
#include <variant>
#include <iostream>

namespace cpp2 {

//-----------------------------------------------------------------------
//
//  pretty_print_visualize: pretty-prints Cpp2 ASTs
//
//-----------------------------------------------------------------------
//
auto pretty_print_visualize(token const& n, int indent)
    -> std::string;
auto pretty_print_visualize(primary_expression_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(literal_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(prefix_expression_node const& n, int indent)
    -> std::string;
template<
    String   Name,
    typename Term
>
auto pretty_print_visualize(binary_expression_node<Name,Term> const& n, int indent)
    -> std::string;
auto pretty_print_visualize(expression_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(expression_list_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(expression_statement_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(postfix_expression_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(unqualified_id_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(qualified_id_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(type_id_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(is_as_expression_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(id_expression_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(compound_statement_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(selection_statement_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(iteration_statement_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(return_statement_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(alternative_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(inspect_expression_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(contract_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(jump_statement_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(statement_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(parameter_declaration_node const& n, int indent, bool is_template_param = false)
    -> std::string;
auto pretty_print_visualize(parameter_declaration_list_node const& n, int indent, bool is_template_param_list = false)
    -> std::string;
auto pretty_print_visualize(function_type_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(type_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(namespace_node const& n, int indent)
    -> std::string;
auto pretty_print_visualize(declaration_node const& n, int indent, bool include_metafunctions_list = false)
    -> std::string;



//-----------------------------------------------------------------------
//  pre: Get an indentation prefix
//
inline static int         indent_spaces  = 2;
inline static std::string indent_str     = std::string( 1024, ' ' );    // "1K should be enough for everyone"

auto pre(int indent)
    -> std::string_view;

//-----------------------------------------------------------------------
//  try_pretty_print_visualize
//
//  Helper to emit whatever is in a variant where each
//  alternative is a smart pointer
//
template <int I>
auto try_pretty_print_visualize(
    auto&     v,
    auto&&... more
)
    -> std::string
{
    if (v.index() == I) {
        auto const& alt = std::get<I>(v);
        assert (alt);
        return pretty_print_visualize (*alt, CPP2_FORWARD(more)...);
    }
    return "";
}

//-----------------------------------------------------------------------
//
//  Common parts for printing visitors
//
//-----------------------------------------------------------------------
//
struct printing_visitor
{
    //-----------------------------------------------------------------------
    //  Constructor: remember a stream to write to
    //
    std::ostream& o;

    printing_visitor(std::ostream& out) : o{out} { indent_spaces = 2; }
};

//-----------------------------------------------------------------------
//
//  Visitor for printing a parse tree
//
//-----------------------------------------------------------------------
//
class parse_tree_printer : printing_visitor
{
    using printing_visitor::printing_visitor;

public:
    auto start(token const& n, int indent) -> void
    {
        o << pre(indent) << __as<std::string>(n.type()) << ": " << n.to_string() << "\n";
    }

    auto start(literal_node const&, int indent) -> void
    {
        o << pre(indent) << "literal" << "\n";
    }

    auto start(expression_node const& n, int indent) -> void
    {
        o << pre(indent) << "expression - "
            << n.num_subexpressions << " subexpressions, my_statement ["
            << static_cast<void const*>(n.my_statement) << "]\n";
    }

    auto start(expression_list_node::term const&n, int indent) -> void
    {
        o << pre(indent) << "expression-list term\n";
        if (n.pass == passing_style::out) {
            o << pre(indent+1) << "out\n";
        }
    }

    auto start(expression_list_node const&, int indent) -> void
    {
        o << pre(indent) << "expression-list\n";
    }

    auto start(primary_expression_node const&, int indent) -> void
    {
        o << pre(indent) << "primary-expression\n";
    }

    auto start(prefix_expression_node const&, int indent) -> void
    {
        o << pre(indent) << "prefix-expression\n";
    }

    auto start(is_as_expression_node const&, int indent) -> void
    {
        o << pre(indent) << "is-as-expression\n";
    }

    template<String Name, typename Term>
    auto start(binary_expression_node<Name, Term> const&, int indent) -> void
    {
        o << pre(indent) << Name.value << "-expression\n";
    }

    auto start(expression_statement_node const& n, int indent) -> void
    {
        o << pre(indent) << "expression-statement - [" << static_cast<void const*>(&n) << "]\n";
    }

    auto start(postfix_expression_node const&, int indent) -> void
    {
        o << pre(indent) << "postfix-expression\n";
    }

    auto start(unqualified_id_node const&, int indent) -> void
    {
        o << pre(indent) << "unqualified-id\n";
    }

    auto start(qualified_id_node const&, int indent) -> void
    {
        o << pre(indent) << "qualified-id\n";
    }

    auto start(type_id_node const&, int indent) -> void
    {
        o << pre(indent) << "type-id\n";
    }

    auto start(id_expression_node const&, int indent) -> void
    {
        o << pre(indent) << "id-expression\n";
    }

    auto start(statement_node const&, int indent) -> void
    {
        o << pre(indent) << "statement\n";
    }

    auto start(compound_statement_node const&, int indent) -> void
    {
        o << pre(indent) << "compound-statement\n";
    }

    auto start(selection_statement_node const& n, int indent) -> void
    {
        o << pre(indent) << "selection-statement\n";
        o << pre(indent+1) << "is_constexpr: " << __as<std::string>(n.is_constexpr) << "\n";
    }

    auto start(alternative_node const&, int indent) -> void
    {
        o << pre(indent) << "alternative\n";
    }

    auto start(jump_statement_node const&, int indent) -> void
    {
        o << pre(indent) << "jump\n";
    }

    auto start(inspect_expression_node const& n, int indent) -> void
    {
        o << pre(indent) << "inspect-expression\n";
        o << pre(indent+1) << "is_constexpr: " << __as<std::string>(n.is_constexpr) << "\n";
    }

    auto start(return_statement_node const&, int indent) -> void
    {
        o << pre(indent) << "return-statement\n";
    }

    auto start(iteration_statement_node const& n, int indent) -> void
    {
        o << pre(indent) << "iteration-statement\n";
        assert(n.identifier);
        o << pre(indent+1) << "identifier: " << std::string_view(*n.identifier) << "\n";
    }

    auto start(contract_node const& n, int indent) -> void
    {
        o << pre(indent) << "contract\n";
        assert(n.kind);
        o << pre(indent+1) << "kind: " << std::string_view(*n.kind) << "\n";
        if (n.message) {
            o << pre(indent+1) << "message: " << std::string_view(*n.message) << "\n";
        }
        if (!n.captures.members.empty()) {
            o << pre(indent+1) << "captures: " << n.captures.members.size() << "\n";
        }
    }

    auto start(type_node const&, int indent) -> void
    {
        o << pre(indent) << "user-defined type\n";
    }

    auto start(namespace_node const&, int indent) -> void
    {
        o << pre(indent) << "namespace\n";
    }

    auto start(function_type_node const& n, int indent) -> void
    {
        o << pre(indent) << "function\n";
        o << pre(indent+1) << "throws: " << __as<std::string>(n.throws) << "\n";
        if (n.returns.index() == function_type_node::id) {
            auto& r = std::get<function_type_node::id>(n.returns);
            if (r.pass != passing_style::invalid) {
                o << pre(indent+1) << "returns by: " << to_string_view(r.pass) << "\n";
            }
        }
    }

    auto start(function_returns_tag const&, int indent) -> void
    {
        o << pre(indent) << "function returns\n";
    }

    auto start(template_args_tag const&, int indent) -> void
    {
        o << pre(indent) << "template arguments\n";
    }

    auto start(declaration_identifier_tag const&, int indent) -> void
    {
        o << pre(indent) << "declaration identifier\n";
    }

    auto start(next_expression_tag const&, int indent) -> void
    {
        o << pre(indent) << "next expression\n";
    }

    auto start(alias_node const& n, int indent) -> void
    {
        o << pre(indent) << "alias\n";
        switch (n.initializer.index()) {
        break;case alias_node::a_type:
            o << pre(indent+1) << "type\n";
        break;case alias_node::a_namespace:
            o << pre(indent+1) << "namespace\n";
        break;case alias_node::an_object:
            o << pre(indent+1) << "object\n";
        break;default:
            o << pre(indent+1) << "ICE - invalid variant state\n";
        }
    }

    auto start(declaration_node const& n, int indent) -> void
    {
        o << pre(indent) << "declaration [" << &n << "]\n";
        o << pre(indent+1) << "parent: [" << n.parent_declaration << "]\n";
        o << pre(indent+1) << "is_variadic: [" << std::boolalpha << n.is_variadic << "]\n";
        o << pre(indent+1) << "is_constexpr: " << __as<std::string>(n.is_constexpr) << "\n";
        switch (n.type.index()) {
        break;case declaration_node::a_function:
            o << pre(indent+1) << "function\n";
        break;case declaration_node::an_object:
            o << pre(indent+1) << "object\n";
        break;case declaration_node::a_type:
            o << pre(indent+1) << "type\n";
        break;case declaration_node::a_namespace:
            o << pre(indent+1) << "namespace\n";
        break;case declaration_node::an_alias:
            o << pre(indent+1) << "alias\n";
        break;default:
            o << pre(indent+1) << "ICE - invalid variant state\n";
        }
        if (!n.is_default_access()) {
            o << pre(indent+1) << "access: " << to_string(n.access) << "\n";
        }
        if (!n.captures.members.empty()) {
            o << pre(indent+1) << "captures: " << n.captures.members.size() << "\n";
        }
    }

    auto start(parameter_declaration_node const& n, int indent) -> void
    {
        o << pre(indent) << "parameter-declaration\n";

        o << pre(indent+1);
        switch (n.pass) {
        break;case passing_style::in     : o << "in";
        break;case passing_style::copy   : o << "copy";
        break;case passing_style::inout  : o << "inout";
        break;case passing_style::out    : o << "out";
        break;case passing_style::move   : o << "move";
        break;case passing_style::forward: o << "forward";
        break;default: ;
        }

        o << pre(indent+1);
        switch (n.mod) {
        break;case parameter_declaration_node::modifier::implicit  : o << "implicit";
        break;case parameter_declaration_node::modifier::virtual_  : o << "virtual";
        break;case parameter_declaration_node::modifier::override_ : o << "override";
        break;case parameter_declaration_node::modifier::final_    : o << "final";
        break;default: ;
        }
        o << "\n";

        assert( n.declaration );
    }

    auto start(parameter_declaration_list_node const&, int indent) -> void
    {
        o << pre(indent) << "parameter-declaration-list\n";
    }

    auto start(translation_unit_node const&, int indent) -> void
    {
        o << pre(indent) << "translation-unit\n";
    }

    auto start(auto const&, int indent) -> void
    {
        o << pre(indent) << "UNRECOGNIZED -- FIXME\n";
    }

    auto end(auto const&, int) -> void
    {
        //  Ignore other node types
    }
};


}

#endif
