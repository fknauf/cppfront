
//  Copyright (c) Herb Sutter
//  SPDX-License-Identifier: CC-BY-NC-ND-4.0

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.


//===========================================================================
//  Parser
//===========================================================================

#ifndef __CPP2_PARSE
#define __CPP2_PARSE

#include "parse_tree.h"
#include "parse_tree_print.h"
#include <memory>
#include <variant>
#include <iostream>

namespace cpp2 {

extern bool violates_lifetime_safety;

//-----------------------------------------------------------------------
//  Operator categorization
//

//G prefix-operator:
//G     one of  '!' '-' '+'
//GT     parameter-direction
//G
auto is_prefix_operator(token const& tok)
    -> bool;

//G postfix-operator:
//G     one of  '++' '--' '*' '&' '~' '$' '...'
//G
auto is_postfix_operator(lexeme l)
    -> bool;

//G assignment-operator:
//G     one of  '=' '*=' '/=' '%=' '+=' '-=' '>>=' '<<=' '&=' '^=' '|='
//G
auto is_assignment_operator(lexeme l)
    -> bool;

//-----------------------------------------------------------------------
//
//  parser: parses a section of Cpp2 code
//
//-----------------------------------------------------------------------
//
class parser
{
    std::vector<error_entry>& errors;

    std::unique_ptr<translation_unit_node> parse_tree = {};

    //  Keep a stack of current capture groups (contracts/decls still being parsed)
    std::vector<capture_group*> current_capture_groups = {};

    struct capture_groups_stack_guard
    {
        parser* pars;

        capture_groups_stack_guard(parser* p, capture_group* cg)
            : pars{ p }
        {
            assert(p);
            assert(cg);
            pars->current_capture_groups.push_back(cg);
        }

        ~capture_groups_stack_guard()
        {
            pars->current_capture_groups.pop_back();
        }
    };

    //  Keep a stack of currently active declarations (still being parsed)
    std::vector<declaration_node*> current_declarations = { nullptr };

    struct current_declarations_stack_guard
    {
        parser* pars;

        current_declarations_stack_guard(parser* p, declaration_node* decl)
            : pars{ p }
        {
            assert(p);
            assert(decl);
            pars->current_declarations.push_back(decl);
        }

        ~current_declarations_stack_guard()
        {
            pars->current_declarations.pop_back();
        }
    };

    std::vector<token> const* tokens = {};
    std::deque<token>* generated_tokens = {};
    int pos = 0;
    std::string parse_kind = {};

    //  Keep track of the function bodies' locations - used to emit comments
    //  in the right pass (decide whether it's a comment that belongs with
    //  the declaration or is part of the definition)
    struct function_body_extent {
        lineno_t first;
        lineno_t last;
        auto operator<=>(function_body_extent const&) const = default;
        auto operator<=>(int i) const { return first <=> i; }

        function_body_extent( lineno_t f, lineno_t l ): first{f}, last{l} { }
    };
    mutable std::vector<function_body_extent> function_body_extents;
    mutable bool                              is_function_body_extents_sorted = false;

public:
    auto is_within_function_body(source_position p) const -> bool;

public:
    //-----------------------------------------------------------------------
    //  Constructors - the copy constructor constructs a new instance with
    //                 the same errors reference but otherwise a clean slate
    //
    //  errors      error list
    //
    parser( std::vector<error_entry>& errors_ );
    parser( parser const& that );

    //-----------------------------------------------------------------------
    //  parse
    //
    //  tokens              input tokens for this section of Cpp2 source code
    //  generated_tokens    a shared place to store generated tokens
    //
    //  Each call parses this section's worth of tokens and adds the
    //  result to the stored parse tree. Call this repeatedly for the Cpp2
    //  sections in a TU to build the whole TU's parse tree
    //
    auto parse(
        std::vector<token> const& tokens_,
        std::deque<token>&        generated_tokens_
    )
        -> bool;

    //-----------------------------------------------------------------------
    //  parse_one_statement
    //
    //  tokens              input tokens for this section of Cpp2 source code
    //  generated_tokens    a shared place to store generated tokens
    //
    //  Each call parses one statement and returns its parse tree.
    //
    auto parse_one_declaration(
        std::vector<token> const& tokens_,
        std::deque<token>&        generated_tokens_
    )
        -> std::unique_ptr<statement_node>;


    //-----------------------------------------------------------------------
    //  Get a set of pointers to just the declarations in the given token map section
    //
    auto get_parse_tree_declarations_in_range(std::vector<token> const& token_range) const
        -> std::vector< declaration_node const* >;

    //-----------------------------------------------------------------------
    //  visit
    //
    auto visit(auto& v) -> void
    {
        parse_tree->visit(v, 0);
    }

private:
    //-----------------------------------------------------------------------
    //  Error reporting: Fed into the supplied this->errors object
    //
    //  msg                 message to be printed
    //
    //  include_curr_token  in this file (during parsing), we normally want
    //                      to show the current token as the unexpected text
    //                      we encountered, but some sema rules are applied
    //                      early during parsing and for those it doesn't
    //                      make sense to show the next token (e.g., when
    //                      we detect and reject a "std::move" qualified-id,
    //                      it's not relevant to add "at LeftParen: ("
    //                      just because ( happens to be the next token)
    //
    auto error(
        char const*     msg,
        bool            include_curr_token = true,
        source_position err_pos            = {},
        bool            fallback           = false
    ) const
        -> void;

    auto error(
        std::string const& msg,
        bool               include_curr_token = true,
        source_position    err_pos            = {},
        bool               fallback           = false
    ) const
        -> void;

    bool has_error() const;


    //-----------------------------------------------------------------------
    //  Token navigation: Only these functions should access this->token_
    //
    auto curr() const
        -> token const&;

    auto peek(int num) const
        -> token const*;

    auto done() const
        -> bool;

    auto next(int num = 1)
        -> void;


    //-----------------------------------------------------------------------
    //  Parsers for unary expressions
    //

    //G primary-expression:
    //G     inspect-expression
    //G     id-expression
    //G     literal
    //G     '(' expression-list ')'
    //GT     '{' expression-list '}'
    //G     unnamed-declaration
    //G
    auto primary_expression()
        -> std::unique_ptr<primary_expression_node>;

    //G postfix-expression:
    //G     primary-expression
    //G     postfix-expression postfix-operator     [Note: without whitespace before the operator]
    //G     postfix-expression '[' expression-list? ']'
    //G     postfix-expression '(' expression-list? ')'
    //G     postfix-expression '.' id-expression
    //G
    auto postfix_expression()
        -> std::unique_ptr<postfix_expression_node>;

    //G prefix-expression:
    //G     postfix-expression
    //G     prefix-operator prefix-expression
    //GTODO     await-expression
    //GTODO     'sizeof' '(' type-id ')'
    //GTODO     'sizeof' '...' ( identifier ')'
    //GTODO     'alignof' '(' type-id ')'
    //GTODO     throws-expression
    //G
    auto prefix_expression()
        -> std::unique_ptr<prefix_expression_node>;


    //-----------------------------------------------------------------------
    //  Parsers for binary expressions
    //

    //  The general /*binary*/-expression:
    //     /*term*/-expression { { /* operators at this precedence level */ } /*term*/-expression }*
    //
    template<
        typename Binary,
        typename ValidateOp,
        typename TermFunc
    >
    auto binary_expression(
        ValidateOp validate_op,
        TermFunc   term
    )
        -> std::unique_ptr<Binary>;

    //G multiplicative-expression:
    //G     is-as-expression
    //G     multiplicative-expression '*' is-as-expression
    //G     multiplicative-expression '/' is-as-expression
    //G     multiplicative-expression '%' is-as-expression
    //G
    auto multiplicative_expression()
        -> std::unique_ptr<multiplicative_expression_node>;

    //G additive-expression:
    //G     multiplicative-expression
    //G     additive-expression '+' multiplicative-expression
    //G     additive-expression '-' multiplicative-expression
    //G
    auto additive_expression()
        -> std::unique_ptr<additive_expression_node>;

    //G shift-expression:
    //G     additive-expression
    //G     shift-expression '<<' additive-expression
    //G     shift-expression '>>' additive-expression
    //G
    auto shift_expression(bool allow_angle_operators = true)
        -> std::unique_ptr<shift_expression_node>;

    //G compare-expression:
    //G     shift-expression
    //G     compare-expression '<=>' shift-expression
    //G
    auto compare_expression(bool allow_angle_operators = true)
        -> std::unique_ptr<compare_expression_node>;

    //G relational-expression:
    //G     compare-expression
    //G     relational-expression '<'  compare-expression
    //G     relational-expression '>'  compare-expression
    //G     relational-expression '<=' compare-expression
    //G     relational-expression '>=' compare-expression
    //G
    auto relational_expression(bool allow_angle_operators = true)
        -> std::unique_ptr<relational_expression_node>;

    //G equality-expression:
    //G     relational-expression
    //G     equality-expression '==' relational-expression
    //G     equality-expression '!=' relational-expression
    //G
    auto equality_expression(bool allow_angle_operators = true, bool allow_equality = true)
        -> std::unique_ptr<equality_expression_node>;

    //G bit-and-expression:
    //G     equality-expression
    //G     bit-and-expression '&' equality-expression
    //G
    auto bit_and_expression(bool allow_angle_operators = true, bool allow_equality = true)
        -> std::unique_ptr<bit_and_expression_node>;

    //G bit-xor-expression:
    //G     bit-and-expression
    //G     bit-xor-expression '^' bit-and-expression
    //G
    auto bit_xor_expression(bool allow_angle_operators = true, bool allow_equality = true)
        -> std::unique_ptr<bit_xor_expression_node>;

    //G bit-or-expression:
    //G     bit-xor-expression
    //G     bit-or-expression '|' bit-xor-expression
    //G
    auto bit_or_expression(bool allow_angle_operators = true, bool allow_equality = true)
        -> std::unique_ptr<bit_or_expression_node>;

    //G logical-and-expression:
    //G     bit-or-expression
    //G     logical-and-expression '&&' bit-or-expression
    //G
    auto logical_and_expression(bool allow_angle_operators = true, bool allow_equality = true)
        -> std::unique_ptr<logical_and_expression_node>;

    //  constant-expression:    // don't need intermediate production, just use:
    //  conditional-expression: // don't need intermediate production, just use:
    //G logical-or-expression:
    //G     logical-and-expression
    //G     logical-or-expression '||' logical-and-expression
    //G
    auto logical_or_expression(bool allow_angle_operators = true, bool allow_equality = true)
        -> std::unique_ptr<logical_or_expression_node>;

    //G assignment-expression:
    //G     logical-or-expression
    //G     assignment-expression assignment-operator logical-or-expression
    //G
    auto assignment_expression(bool allow_angle_operators = true)
        -> std::unique_ptr<assignment_expression_node>;

    //G expression:               // eliminated 'condition:' - just use 'expression:'
    //G     assignment-expression
    //GTODO    try expression
    //G
    auto expression(bool allow_angle_operators = true, bool check_arrow = true)
        -> std::unique_ptr<expression_node>;

    //G expression-list:
    //G     parameter-direction? expression
    //G     expression-list ',' parameter-direction? expression
    //G
    auto expression_list(
        token const* open_paren,
        bool inside_initializer = false
    )
        -> std::unique_ptr<expression_list_node>;

    //G type-id:
    //G     type-qualifier-seq? qualified-id
    //G     type-qualifier-seq? unqualified-id
    //G
    //G type-qualifier-seq:
    //G     type-qualifier
    //G     type-qualifier-seq type-qualifier
    //G
    //G type-qualifier:
    //G     'const'
    //G     '*'
    //G
    auto type_id()
        -> std::unique_ptr<type_id_node>;

    //G is-as-expression:
    //G     prefix-expression
    //G     is-as-expression is-type-constraint
    //G     is-as-expression is-value-constraint
    //G     is-as-expression as-type-cast
    //GTODO     type-id is-type-constraint
    //G
    //G is-type-constraint
    //G     'is' type-id
    //G
    //G is-value-constraint
    //G     'is' expression
    //G
    //G as-type-cast
    //G     'as' type-id
    //G
    auto is_as_expression()
        -> std::unique_ptr<is_as_expression_node>;

    //G unqualified-id:
    //G     identifier
    //G     keyword
    //G     template-id
    //GTODO     operator-function-id
    //G     ...
    //G
    //G template-id:
    //G     identifier '<' template-argument-list? '>'
    //G
    //G template-argument-list:
    //G     template-argument-list ',' template-argument
    //G
    //G template-argument:
    //G     # note: < > << >> are not allowed in expressions until new ( is opened
    //G     'const' type-id
    //G     expression
    //G     type-id
    //G
    auto unqualified_id()
        -> std::unique_ptr<unqualified_id_node>;

    //G qualified-id:
    //G     nested-name-specifier unqualified-id
    //G     member-name-specifier unqualified-id
    //G
    //G nested-name-specifier:
    //G     '::'
    //G     unqualified-id '::'
    //G
    //G member-name-specifier:
    //G     unqualified-id '.'
    //G
    auto qualified_id()
        -> std::unique_ptr<qualified_id_node>;

    //G id-expression:
    //G     qualified-id
    //G     unqualified-id
    //G
    auto id_expression()
        -> std::unique_ptr<id_expression_node>;

    //G literal:
    //G     integer-literal ud-suffix?
    //G     character-literal ud-suffix?
    //G     floating-point-literal ud-suffix?
    //G     string-literal ud-suffix?
    //G     boolean-literal ud-suffix?
    //G     pointer-literal ud-suffix?
    //G     user-defined-literal ud-suffix?
    //G
    auto literal()
        -> std::unique_ptr<literal_node>;

    //G expression-statement:
    //G     expression ';'
    //G     expression
    //G
    auto expression_statement(bool semicolon_required)
        -> std::unique_ptr<expression_statement_node>;

    //G selection-statement:
    //G     'if' 'constexpr'? logical-or-expression compound-statement
    //G     'if' 'constexpr'? logical-or-expression compound-statement 'else' compound-statement
    //G
    auto selection_statement()
        -> std::unique_ptr<selection_statement_node>;

    //G return-statement:
    //G     return expression? ';'
    //G
    auto return_statement()
        -> std::unique_ptr<return_statement_node>;

    //G iteration-statement:
    //G     label? 'while' logical-or-expression next-clause? compound-statement
    //G     label? 'do' compound-statement 'while' logical-or-expression next-clause? ';'
    //G     label? 'for' expression next-clause? 'do' unnamed-declaration
    //G
    //G label:
    //G     identifier ':'
    //G
    //G next-clause:
    //G     'next' assignment-expression
    //G
    auto iteration_statement()
        -> std::unique_ptr<iteration_statement_node>;

    //G alternative:
    //G     alt-name? is-type-constraint '=' statement
    //G     alt-name? is-value-constraint '=' statement
    //G     alt-name? as-type-cast '=' statement
    //G
    //G alt-name:
    //G     unqualified-id ':'
    //G
    auto alternative()
        -> std::unique_ptr<alternative_node>;

    //G inspect-expression:
    //G     'inspect' 'constexpr'? expression '{' alternative-seq? '}'
    //G     'inspect' 'constexpr'? expression '->' type-id '{' alternative-seq? '}'
    //G
    //G alternative-seq:
    //G     alternative
    //G     alternative-seq alternative
    //G
    auto inspect_expression(bool is_expression)
        -> std::unique_ptr<inspect_expression_node>;

    //G jump-statement:
    //G     'break' identifier? ';'
    //G     'continue' identifier? ';'
    //G
    auto jump_statement()
        -> std::unique_ptr<jump_statement_node>;

    //G statement:
    //G     selection-statement
    //G     inspect-expression
    //G     return-statement
    //G     jump-statement
    //G     iteration-statement
    //G     compound-statement
    //G     declaration
    //G     expression-statement
    //G     contract
    //
    //GTODO     try-block
    //G
    auto statement(
        bool                     semicolon_required = true,
        source_position          equal_sign         = source_position{},
        bool                     parameters_allowed = false,
        compound_statement_node* compound_parent    = nullptr
    )
        -> std::unique_ptr<statement_node>;

    //G compound-statement:
    //G     '{' statement-seq? '}'
    //G
    //G statement-seq:
    //G     statement
    //G     statement-seq statement
    //G
    auto compound_statement(
        source_position equal_sign                      = source_position{},
        bool            allow_single_unbraced_statement = false
    )
        -> std::unique_ptr<compound_statement_node>;

    //G parameter-declaration:
    //G     this-specifier? parameter-direction? declaration
    //G
    //G parameter-direction: one of
    //G     'in' 'copy' 'inout' 'out' 'move' 'forward'
    //G
    //G this-specifier:
    //G     'implicit'
    //G     'virtual'
    //G     'override'
    //G     'final'
    //G
    auto parameter_declaration(
        bool is_returns   = false,
        bool is_named     = true,
        bool is_template  = true,
        bool is_statement = false
    )
        -> std::unique_ptr<parameter_declaration_node>;


    //G parameter-declaration-list
    //G     '(' parameter-declaration-seq? ')'
    //G
    //G parameter-declaration-seq:
    //G     parameter-declaration
    //G     parameter-declaration-seq ',' parameter-declaration
    //G
    auto parameter_declaration_list(
        bool is_returns    = false,
        bool is_named      = true,
        bool is_template   = false,
        bool is_statement  = false
    )
        -> std::unique_ptr<parameter_declaration_list_node>;

    //G contract:
    //G     '[' '[' contract-kind id-expression? ':' logical-or-expression ']' ']'
    //G     '[' '[' contract-kind id-expression? ':' logical-or-expression ',' string-literal ']' ']'
    //G
    //G contract-kind: one of
    //G     'pre' 'post' 'assert'
    //G
    auto contract()
        -> std::unique_ptr<contract_node>;

    //G function-type:
    //G     parameter-declaration-list throws-specifier? return-list? contract-seq?
    //G
    //G throws-specifier:
    //G     'throws'
    //G
    //G return-list:
    //G     '->' type-id
    //G     '->' parameter_declaration_list
    //G
    //G contract-seq:
    //G     contract
    //G     contract-seq contract
    //G
    auto function_type(
        declaration_node* my_decl,
        bool              is_named = true
        )
        -> std::unique_ptr<function_type_node>;


    auto apply_type_metafunctions( declaration_node& decl )
        -> bool;


    //G unnamed-declaration:
    //G     ':' meta-functions-list? template-parameter-declaration-list? function-type requires-clause? '=' statement
    //G     ':' meta-functions-list? template-parameter-declaration-list? type-id? requires-clause? '=' statement
    //G     ':' meta-functions-list? template-parameter-declaration-list? type-id
    //G     ':' meta-functions-list? template-parameter-declaration-list? 'final'? 'type' requires-clause? '=' statement
    //G     ':' 'namespace' '=' statement
    //G
    //G meta-functions-list:
    //G     '@' id-expression
    //G     meta-functions-list '@' id-expression
    //G
    //G requires-clause:
    //G      # note: for aliases, == is not allowed in expressions until new ( is opened
    //G      'requires' logical-or-expression
    //G
    //G template-parameter-declaration-list
    //G     '<' parameter-declaration-seq '>'
    //G
    auto unnamed_declaration(
        source_position                      start,
        bool                                 semicolon_required    = true,
        bool                                 captures_allowed      = false,
        bool                                 named                 = false,
        bool                                 is_parameter          = false,
        bool                                 is_template_parameter = false,
        std::unique_ptr<unqualified_id_node> id                    = {},
        accessibility                        access                = {},
        bool                                 is_variadic           = false,
        statement_node*                      my_stmt               = {}
    )
        -> std::unique_ptr<declaration_node>;

    //G alias:
    //G     ':' template-parameter-declaration-list? 'type' requires-clause? '==' type-id ';'
    //G     ':' 'namespace' '==' id-expression ';'
    //G     ':' template-parameter-declaration-list? type-id? requires-clause? '==' expression ';'
    //G
    //GT     ':' function-type '==' expression ';'
    //GT        # See commit 63efa6ed21c4d4f4f136a7a73e9f6b2c110c81d7 comment
    //GT        # for why I don't see a need to enable this yet
    //
    auto alias()
        -> std::unique_ptr<declaration_node>;

    //G declaration:
    //G     access-specifier? identifier '...'? unnamed-declaration
    //G     access-specifier? identifier alias
    //G
    //G access-specifier:
    //G     public
    //G     protected
    //G     private
    //G
    auto declaration(
        bool            semicolon_required    = true,
        bool            is_parameter          = false,
        bool            is_template_parameter = false,
        statement_node* my_stmt               = {}
    )
        -> std::unique_ptr<declaration_node>;

    //G declaration-seq:
    //G     declaration
    //G     declaration-seq declaration
    //G
    //G translation-unit:
    //G     declaration-seq?
    //
    auto translation_unit()
        -> std::unique_ptr<translation_unit_node>;

public:
    //-----------------------------------------------------------------------
    //  debug_print
    //
    auto debug_print(std::ostream& o)
        -> void;
};

}

#endif
