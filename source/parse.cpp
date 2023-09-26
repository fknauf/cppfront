#include "parse.h"
#include "parse_tree_print.h"

namespace cpp2 {
bool violates_lifetime_safety = false;

//G prefix-operator:
//G     one of  '!' '-' '+'
//GT     parameter-direction
//G
auto is_prefix_operator(token const& tok)
    -> bool
{
    //if (to_passing_style(tok) != passing_style::invalid) {
    //    return true;
    //}

    switch (tok.type()) {
    break;case lexeme::Not:
          case lexeme::Minus:
          case lexeme::Plus:
        return true;
    break;default:
        return false;
    }
}


//G postfix-operator:
//G     one of  '++' '--' '*' '&' '~' '$' '...'
//G
auto is_postfix_operator(lexeme l)
    -> bool
{
    switch (l) {
    break;case lexeme::PlusPlus:
          case lexeme::MinusMinus:
          case lexeme::Multiply:
          case lexeme::Ampersand:
          case lexeme::Tilde:
          case lexeme::Dollar:
          case lexeme::Ellipsis:
        return true;
    break;default:
        return false;
    }
}


//G assignment-operator:
//G     one of  '=' '*=' '/=' '%=' '+=' '-=' '>>=' '<<=' '&=' '^=' '|='
//G
auto is_assignment_operator(lexeme l)
    -> bool
{
    switch (l) {
    break;case lexeme::Assignment:
          case lexeme::MultiplyEq:
          case lexeme::SlashEq:
          case lexeme::ModuloEq:
          case lexeme::PlusEq:
          case lexeme::MinusEq:
          case lexeme::RightShiftEq:
          case lexeme::LeftShiftEq:
          case lexeme::AmpersandEq:
          case lexeme::CaretEq:
          case lexeme::PipeEq:
        return true;
    break;default:
        return false;
    }
}

auto pre(int indent)
    -> std::string_view
{
    assert (indent >= 0);
    return {
        indent_str.c_str(),
        as<size_t>( std::min( indent*indent_spaces, __as<int>(std::ssize(indent_str))) )
    };
}

auto parser::is_within_function_body(source_position p) const -> bool 
{
    //  Short circuit the empty case, so that the rest of the function
    //  can unconditionally decrement any non-.begin() iterator once
    if (function_body_extents.empty()) {
        return false;
    }

    //  Ensure we are sorted
    if (!is_function_body_extents_sorted) {
        std::sort(
            function_body_extents.begin(),
            function_body_extents.end()
        );
        is_function_body_extents_sorted = true;
    }

    //  Find the first entry that is beyond pos, and back up one to
    //  the last that could be a match; this also ensures iter is
    //  dereferenceable, not .end()
    auto iter = std::lower_bound(
        function_body_extents.begin(),
        function_body_extents.end(),
        p.lineno+1
    );
    if (iter != function_body_extents.begin()) {
        --iter;
    }

    //  Now go backwards through the preceding entries until
    //  one includes pos or we move before pos
    while (
        iter->first <= p.lineno
        )
    {
        if (
            iter->first <= p.lineno
            && p.lineno <= iter->last
            )
        {
            return true;
        }
        if (iter == function_body_extents.begin()) {
            break;
        }
        --iter;
    }
    return false;
}


//-----------------------------------------------------------------------
//  Constructors - the copy constructor constructs a new instance with
//                 the same errors reference but otherwise a clean slate
//
//  errors      error list
//
parser::parser( std::vector<error_entry>& errors_ )
    : errors{ errors_ }
    , parse_tree{std::make_unique<translation_unit_node>()}
{ }

parser::parser( parser const& that )
    : errors{ that.errors }
    , parse_tree{std::make_unique<translation_unit_node>()}
{ }


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
auto parser::parse(
    std::vector<token> const& tokens_,
    std::deque<token>&        generated_tokens_
)
    -> bool
{
    parse_kind = "source file";

    //  Set per-parse state for the duration of this call
    tokens           = &tokens_;
    generated_tokens = &generated_tokens_;

    //  Generate parse tree for this section as if a standalone TU
    pos     = 0;
    auto tu = translation_unit();

    //  Then add it to the complete parse tree
    parse_tree->declarations.insert(
        parse_tree->declarations.end(),
        std::make_move_iterator(tu->declarations.begin()),
        std::make_move_iterator(tu->declarations.end())
    );
    if (!done()) {
        error("unexpected text at end of Cpp2 code section", true, {}, true);
        return false;
    }
    return true;
}


//-----------------------------------------------------------------------
//  parse_one_statement
//
//  tokens              input tokens for this section of Cpp2 source code
//  generated_tokens    a shared place to store generated tokens
//
//  Each call parses one statement and returns its parse tree.
//
auto parser::parse_one_declaration(
    std::vector<token> const& tokens_,
    std::deque<token>&        generated_tokens_
)
    -> std::unique_ptr<statement_node>
{
    parse_kind = "source string during code generation";

    //  Set per-parse state for the duration of this call
    tokens           = &tokens_;
    generated_tokens = &generated_tokens_;

    //  Parse one declaration - we succeed if the parse succeeded,
    //  and there were no new errors, and all tokens were consumed
    auto errors_size = std::ssize(errors);
    pos = 0;
    if (auto d = statement();
        d
        && std::ssize(errors) == errors_size
        && done()
        )
    {
        return d;
    }

    return {};
}


//-----------------------------------------------------------------------
//  Get a set of pointers to just the declarations in the given token map section
//
auto parser::get_parse_tree_declarations_in_range(std::vector<token> const& token_range) const
    -> std::vector< declaration_node const* >
{
    assert (parse_tree);
    assert (!token_range.empty());
    auto first_line = token_range.front().position().lineno;
    auto last_line  = token_range.back().position().lineno;

    auto ret = std::vector< declaration_node const* >{};
    for (auto& decl : parse_tree->declarations)
    {
        assert(decl);

        //  The grammar and the tokens are in lineno order, so we don't
        //  need to look further once we pass the last lineno
        if (decl->position().lineno > last_line) {
            break;
        }
        if (decl->position().lineno >= first_line) {
            ret.push_back( decl.get() );
        }
    }

    return ret;
}

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
auto parser::error(
    char const*     msg,
    bool            include_curr_token,
    source_position err_pos,
    bool            fallback
) const
    -> void
{
    auto m = std::string{msg};
    auto i = done() ? -1 : 0;
    assert (peek(i));
    if (include_curr_token) {
        m += std::string(" (at '") + peek(i)->to_string() + "')";
    }
    if (
        err_pos == source_position{}
    ) {
        err_pos = peek(i)->position();
    }
    errors.emplace_back( err_pos, m, false, fallback );
}

auto parser::error(
    std::string const& msg,
    bool               include_curr_token,
    source_position    err_pos,
    bool               fallback
) const
    -> void
{
    error( msg.c_str(), include_curr_token, err_pos, fallback );
}

bool parser::has_error() const {
    return !errors.empty();
}


//-----------------------------------------------------------------------
//  Token navigation: Only these functions should access this->token_
//
auto parser::curr() const
    -> token const&
{
    if (done()) {
        throw std::runtime_error("unexpected end of " + parse_kind);
    }

    return (*tokens)[pos];
}

auto parser::peek(int num) const
    -> token const*
{
    assert (tokens);
    if (
        pos + num >= 0
        && pos + num < std::ssize(*tokens)
        )
    {
        return &(*tokens)[pos + num];
    }
    return {};
}

auto parser::done() const
    -> bool
{
    assert (tokens);
    assert (pos <= std::ssize(*tokens));
    return pos == std::ssize(*tokens);
}

auto parser::next(int num)
    -> void
{
    assert (tokens);
    pos = std::min( pos+num, __as<int>(std::ssize(*tokens)) );
}


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
auto parser::primary_expression()
    -> std::unique_ptr<primary_expression_node>
{
    auto n = std::make_unique<primary_expression_node>();

    if (auto inspect = inspect_expression(true))
    {
        n->expr = std::move(inspect);
        return n;
    }

    if (auto id = id_expression()) {
        n->expr = std::move(id);
        return n;
    }

    if (auto lit = literal()) {
        n->expr = std::move(lit);
        return n;
    }

    if (curr().type() == lexeme::LeftParen
        //  If in the future (not now) we decide to allow braced-expressions
        //      || curr().type() == lexeme::LeftBrace
        )
    {
        bool inside_initializer = (
            peek(-1) && peek(-1)->type() == lexeme::Assignment
        );
        auto open_paren = &curr();
        auto close = close_paren_type(open_paren->type());
        auto close_text = [&] () -> std::string { if (close == lexeme::RightParen) { return ")"; } return "}"; }();
        next();
        auto expr_list = expression_list(open_paren, inside_initializer);
        if (!expr_list) {
            error("unexpected text - ( is not followed by an expression-list");
            next();
            return {};
        }
        if (curr().type() != close_paren_type(open_paren->type())) {
            error("unexpected text - expression-list is not terminated by " + close_text);
            next();
            return {};
        }
        expr_list->close_paren = &curr();
        next();
        if (
               curr().type() != lexeme::Semicolon
            && curr().type() != lexeme::RightParen
            && curr().type() != lexeme::RightBracket
            && curr().type() != lexeme::Greater
            && curr().type() != lexeme::Comma
        ) {
            expr_list->inside_initializer = false;
        }
        n->expr = std::move(expr_list);
        return n;
    }

    if (auto decl = unnamed_declaration(curr().position(), false, true)) // captures are allowed
    {
        assert (
            !decl->has_name()
            && "ICE: declaration should have been unnamed"
        );
        if (auto obj = std::get_if<declaration_node::an_object>(&decl->type)) {
            if ((*obj)->is_wildcard()) {
                error("an unnamed object at expression scope currently cannot have a deduced type (the reason to create an unnamed object is typically to create a temporary of a named type)");
                next();
                return {};
            }
        }
        else if (auto func = std::get_if<declaration_node::a_function>(&decl->type)) {
            if ((*func)->returns.index() == function_type_node::list) {
                error("an unnamed function at expression scope currently cannot return multiple values");
                next();
                return {};
            }
            if ( // check if a single-expression function is followed by an extra second semicolon
                decl->initializer && decl->initializer->is_expression()
                && !done() && curr().type() == lexeme::Semicolon
            ) {
                error("a single-expression function should end with a single semicolon");
            }
            if (!(*func)->contracts.empty()) {
                error("an unnamed function at expression scope currently cannot have contracts");
                next();
                return {};
            }
            if (
                peek(-1) && peek(-1)->type() != lexeme::RightBrace  // it is short function syntax
                && curr().type() != lexeme::LeftParen               // not imediatelly called
                && curr().type() != lexeme::RightParen              // not as a last argument to function
                && curr().type() != lexeme::Comma                   // not as first or in-the-middle, function argument
            ) {
                // this is a fix for a short function syntax that should have double semicolon used
                // (check comment in expression_statement(bool semicolon_required))
                // We simulate double semicolon by moving back to single semicolon.
                next(-1);
            }
        }
        else {
            error("(temporary alpha limitation) an unnamed declaration at expression scope must be a function or an object");
            next();
            return {};
        }

        n->expr = std::move(decl);
        return n;
    }

    return {};
}


//G postfix-expression:
//G     primary-expression
//G     postfix-expression postfix-operator     [Note: without whitespace before the operator]
//G     postfix-expression '[' expression-list? ']'
//G     postfix-expression '(' expression-list? ')'
//G     postfix-expression '.' id-expression
//G
auto parser::postfix_expression()
    -> std::unique_ptr<postfix_expression_node>
{
    auto n = std::make_unique<postfix_expression_node>();
    n->expr = primary_expression();
    if (!(n->expr)) {
        return {};
    }

    while (
        !done()
        && (
            (is_postfix_operator(curr().type())
                //  Postfix operators must be lexically adjacent
                && curr().position().lineno == peek(-1)->position().lineno
                && curr().position().colno == peek(-1)->position().colno + peek(-1)->length()
            )
            || curr().type() == lexeme::LeftBracket
            || curr().type() == lexeme::LeftParen
            || curr().type() == lexeme::Dot
        )
        )
    {
        //  these can't be unary operators if followed by a (, identifier, or literal
        if (
            (
                curr().type() == lexeme::Multiply
                || curr().type() == lexeme::Ampersand
                || curr().type() == lexeme::Tilde
                )
            && peek(1)
            && (
                peek(1)->type() == lexeme::LeftParen
                || peek(1)->type() == lexeme::Identifier
                || is_literal(peek(1)->type())
                )
            )
        {
            auto op  = curr().to_string();
            auto msg = "postfix unary " + op;
            if      (curr().type() == lexeme::Multiply ) { msg += " (dereference)"          ; }
            else if (curr().type() == lexeme::Ampersand) { msg += " (address-of)"           ; }
            else if (curr().type() == lexeme::Tilde    ) { msg += " (unary bit-complement)" ; }
            msg += " cannot be immediately followed by a (, identifier, or literal - add whitespace before "
                + op + " here if you meant binary " + op;
            if      (curr().type() == lexeme::Multiply ) { msg += " (multiplication)"       ; }
            else if (curr().type() == lexeme::Ampersand) { msg += " (bitwise and)"          ; }
            else if (curr().type() == lexeme::Tilde    ) { msg += " (binary bit-complement)"; }

            error(msg, false);
            break;
        }

        if (curr().type() == lexeme::Dollar) {
            //  cap_grp must not already be set, or this is a multi-$ postfix-expression
            if (n->cap_grp) {
                error("$ (capture) can appear at most once in a single postfix-expression");
                return {};
            }
            if (current_capture_groups.empty()) {
                error("$ (capture) cannot appear here - it must appear in an anonymous expression function, a postcondition, or an interpolated string literal");
                return {};
            }
            n->cap_grp = current_capture_groups.back();
            n->cap_grp->add(n.get());
        }

        auto term = postfix_expression_node::term{&curr()};
        next();

        if (term.op->type() == lexeme::LeftBracket)
        {
            term.expr_list = expression_list(term.op);
            if (!term.expr_list)
            {
                error("[ is not followed by a valid expression list");
                return {};
            }
            if (curr().type() != lexeme::RightBracket)
            {
                error("unexpected text - [ is not properly matched by ]", true, {}, true);
                return {};
            }
            term.expr_list->close_paren = &curr();
            term.op_close = &curr();
            next();
        }
        else if (term.op->type() == lexeme::LeftParen)
        {
            term.expr_list = expression_list(term.op);
            if (!term.expr_list) {
                error("( is not followed by a valid expression list");
                return {};
            }
            if (curr().type() != lexeme::RightParen) {
                error("unexpected text - ( is not properly matched by )", true, {}, true);
                return {};
            }
            term.expr_list->close_paren = &curr();
            term.op_close = &curr();
            next();
        }
        else if (term.op->type() == lexeme::Dot)
        {
            term.id_expr = id_expression();
            if (!term.id_expr) {
                error("'.' must be followed by a valid member name");
                return {};
            }
        }

        n->ops.push_back( std::move(term) );
    }

    if (auto tok = n->expr->get_token();
        tok
        && *tok == "this"
        && curr().type() == lexeme::Arrow
        )
    {
        auto next_word = std::string{};
        if (peek(1)) {
            next_word = peek(1)->to_string();
        }
        error("'this' is not a pointer - write 'this." + next_word + "' instead of 'this->" + next_word + "'");
        return {};
    }

    for (auto& e : expression_node::current_expressions) {
        e->num_subexpressions += std::ssize(n->ops);
    }

    return n;
}


//G prefix-expression:
//G     postfix-expression
//G     prefix-operator prefix-expression
//GTODO     await-expression
//GTODO     'sizeof' '(' type-id ')'
//GTODO     'sizeof' '...' ( identifier ')'
//GTODO     'alignof' '(' type-id ')'
//GTODO     throws-expression
//G
auto parser::prefix_expression()
    -> std::unique_ptr<prefix_expression_node>
{
    auto n = std::make_unique<prefix_expression_node>();
    for ( ;
        is_prefix_operator(curr());
        next()
        )
    {
        n->ops.push_back(&curr());
    }
    if ((n->expr = postfix_expression())) {
        return n;
    }
    switch (curr().type())
    {
    break; case lexeme::PlusPlus:
        error("prefix '++var' is not valid Cpp2; use postfix 'var++' instead", false);
    break; case lexeme::MinusMinus:
        error("prefix '--var' is not valid Cpp2; use postfix 'var--' instead", false);
    break; case lexeme::Multiply:
        error("prefix '*ptr' dereference is not valid Cpp2; use postfix 'ptr*' instead", false);
    break; case lexeme::Ampersand:
        error("prefix '&var' address-of is not valid Cpp2; use postfix 'var&' instead", false);
    break; case lexeme::Tilde:
        error("prefix '~var' is not valid Cpp2; use postfix 'var~' instead", false);
    break; default: ;
    }
    return {};
}


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
auto parser::binary_expression(
    ValidateOp validate_op,
    TermFunc   term
)
    -> std::unique_ptr<Binary>
{
    auto n = std::make_unique<Binary>();
    if ( (n->expr = term()) )
    {
        while (!done())
        {
            typename Binary::term t{};

            //  Remember current position, because we may need to backtrack if this next
            //  t.op might be valid but isn't followed by a valid term and so isn't for us
            auto term_pos = pos;

            //  Most of these predicates only look at the current token and return
            //  true/false == whether this is a valid operator for this production
            if constexpr( requires{ bool{ validate_op(curr()) }; } ) {
                if (!validate_op(curr())) {
                    break;
                }
                t.op = &curr();
                next();
            }

            //  But for shift-expression we may synthesize >> from > >
            //  which will return a token* == a valid operator for this production
            //  (possibly a synthesized new token) or nullptr otherwise
            else if constexpr( requires{ validate_op(curr(), *peek(1)); } ) {
                if (
                    peek(1) == nullptr
                    || (t.op = validate_op(curr(), *peek(1))) == nullptr
                    )
                {
                    break;
                }
                //  If we didn't consume the next token, we consumed the next two
                if (t.op != &curr()) {
                    next();
                }
                next();
            }

            //  And it shouldn't be anything else
            else {
                assert (!"ICE: validate_op should take one token and return bool, or two tokens and return token const* ");
            }

            //  At this point we may have a valid t.op, so try to parse the next term...
            //  If it's not a valid term, then this t.op wasn't for us, pop it and return
            //  what we found (e.g., with "requires expression = {...}" the = is a grammar
            //  element and not an operator, it isn't and can't be part of the expression)
            if ( !(t.expr = term()) ) {
                pos = term_pos;    // backtrack
                return n;
            }

            //  We got a term, so this op + term was for us
            n->terms.push_back( std::move(t) );
        }
        return n;
    }
    return {};
}

//G multiplicative-expression:
//G     is-as-expression
//G     multiplicative-expression '*' is-as-expression
//G     multiplicative-expression '/' is-as-expression
//G     multiplicative-expression '%' is-as-expression
//G
auto parser::multiplicative_expression()
    -> std::unique_ptr<multiplicative_expression_node>
{
    return binary_expression<multiplicative_expression_node> (
        [](token const& t){ return t.type() == lexeme::Multiply || t.type() == lexeme::Slash || t.type() == lexeme::Modulo; },
        [this]{ return is_as_expression(); }
        );
}

//G additive-expression:
//G     multiplicative-expression
//G     additive-expression '+' multiplicative-expression
//G     additive-expression '-' multiplicative-expression
//G
auto parser::additive_expression()
    -> std::unique_ptr<additive_expression_node>
{
    return binary_expression<additive_expression_node> (
        [](token const& t){ return t.type() == lexeme::Plus || t.type() == lexeme::Minus; },
        [this]{ return multiplicative_expression(); }
    );
}

//G shift-expression:
//G     additive-expression
//G     shift-expression '<<' additive-expression
//G     shift-expression '>>' additive-expression
//G
auto parser::shift_expression(bool allow_angle_operators)
    -> std::unique_ptr<shift_expression_node>
{
    if (allow_angle_operators) {
        return binary_expression<shift_expression_node> (
            [this](token const& t, token const& next) -> token const* {
                if (t.type() == lexeme::LeftShift) {
                    return &t;
                }
                if (
                    t.type() == lexeme::Greater
                    && next.type() == lexeme::Greater
                    && t.position() == source_position{ next.position().lineno, next.position().colno-1 }
                    )
                {
                    generated_tokens->emplace_back( ">>", t.position(), lexeme::RightShift);
                    return &generated_tokens->back();
                }
                return nullptr;
            },
            [this]{ return additive_expression(); }
        );
    }
    else {
        return binary_expression<shift_expression_node> (
            [](token const&, token const&) -> token const* { return nullptr; },
            [this]{ return additive_expression(); }
        );
    }
}

//G compare-expression:
//G     shift-expression
//G     compare-expression '<=>' shift-expression
//G
auto parser::compare_expression(bool allow_angle_operators)
    -> std::unique_ptr<compare_expression_node>
{
    return binary_expression<compare_expression_node> (
        [](token const& t){ return t.type() == lexeme::Spaceship; },
        [=,this]{ return shift_expression(allow_angle_operators); }
    );
}

//G relational-expression:
//G     compare-expression
//G     relational-expression '<'  compare-expression
//G     relational-expression '>'  compare-expression
//G     relational-expression '<=' compare-expression
//G     relational-expression '>=' compare-expression
//G
auto parser::relational_expression(bool allow_angle_operators)
    -> std::unique_ptr<relational_expression_node>
{
    if (allow_angle_operators) {
        return binary_expression<relational_expression_node> (
            [](token const& t, token const& next) -> token const* {
                if (
                    t.type() == lexeme::Less
                    || t.type() == lexeme::LessEq
                    || (t.type() == lexeme::Greater && next.type() != lexeme::GreaterEq)
                    || t.type() == lexeme::GreaterEq
                    ) {
                    return &t;
                }
                return nullptr;
            },
            [=,this]{ return compare_expression(allow_angle_operators); }
        );
    }
    else {
        return binary_expression<relational_expression_node> (
            [](token const&, token const&) -> token const* { return nullptr; },
            [=,this]{ return compare_expression(allow_angle_operators); }
        );
    }
}

//G equality-expression:
//G     relational-expression
//G     equality-expression '==' relational-expression
//G     equality-expression '!=' relational-expression
//G
auto parser::equality_expression(bool allow_angle_operators, bool allow_equality)
    -> std::unique_ptr<equality_expression_node>
{
    if (allow_equality) {
        return binary_expression<equality_expression_node> (
            [](token const& t){ return t.type() == lexeme::EqualComparison || t.type() == lexeme::NotEqualComparison; },
            [=,this]{ return relational_expression(allow_angle_operators); }
        );
    }
    else {
        return binary_expression<equality_expression_node> (
            [](token const& t){ return t.type() == lexeme::NotEqualComparison; },
            [=,this]{ return relational_expression(allow_angle_operators); }
        );
    }
}

//G bit-and-expression:
//G     equality-expression
//G     bit-and-expression '&' equality-expression
//G
auto parser::bit_and_expression(bool allow_angle_operators, bool allow_equality)
    -> std::unique_ptr<bit_and_expression_node>
{
    return binary_expression<bit_and_expression_node> (
        [](token const& t){ return t.type() == lexeme::Ampersand; },
        [=,this]{ return equality_expression(allow_angle_operators, allow_equality); }
    );
}

//G bit-xor-expression:
//G     bit-and-expression
//G     bit-xor-expression '^' bit-and-expression
//G
auto parser::bit_xor_expression(bool allow_angle_operators, bool allow_equality)
    -> std::unique_ptr<bit_xor_expression_node>
{
    return binary_expression<bit_xor_expression_node> (
        [](token const& t){ return t.type() == lexeme::Caret; },
        [=,this]{ return bit_and_expression(allow_angle_operators, allow_equality); }
    );
}

//G bit-or-expression:
//G     bit-xor-expression
//G     bit-or-expression '|' bit-xor-expression
//G
auto parser::bit_or_expression(bool allow_angle_operators, bool allow_equality)
    -> std::unique_ptr<bit_or_expression_node>
{
    return binary_expression<bit_or_expression_node> (
        [](token const& t){ return t.type() == lexeme::Pipe; },
        [=,this]{ return bit_xor_expression(allow_angle_operators, allow_equality); }
    );
}

//G logical-and-expression:
//G     bit-or-expression
//G     logical-and-expression '&&' bit-or-expression
//G
auto parser::logical_and_expression(bool allow_angle_operators, bool allow_equality)
    -> std::unique_ptr<logical_and_expression_node>
{
    return binary_expression<logical_and_expression_node> (
        [](token const& t){ return t.type() == lexeme::LogicalAnd; },
        [=,this]{ return bit_or_expression(allow_angle_operators, allow_equality); }
    );
}

//  constant-expression:    // don't need intermediate production, just use:
//  conditional-expression: // don't need intermediate production, just use:
//G logical-or-expression:
//G     logical-and-expression
//G     logical-or-expression '||' logical-and-expression
//G
auto parser::logical_or_expression(bool allow_angle_operators, bool allow_equality)
    -> std::unique_ptr<logical_or_expression_node>
{
    return binary_expression<logical_or_expression_node> (
        [](token const& t){ return t.type() == lexeme::LogicalOr; },
        [=,this]{ return logical_and_expression(allow_angle_operators, allow_equality); }
    );
}

//G assignment-expression:
//G     logical-or-expression
//G     assignment-expression assignment-operator logical-or-expression
//G
auto parser::assignment_expression(bool allow_angle_operators)
    -> std::unique_ptr<assignment_expression_node>
{
    if (allow_angle_operators)
    {
        auto ret = binary_expression<assignment_expression_node> (
            [this](token const& t, token const& next) -> token const* {
                if (is_assignment_operator(t.type())) {
                    return &t;
                }
                if (
                    t.type() == lexeme::Greater
                    && next.type() == lexeme::GreaterEq
                    && t.position() == source_position{ next.position().lineno, next.position().colno-1 }
                    )
                {
                    generated_tokens->emplace_back( ">>=", t.position(), lexeme::RightShiftEq);
                    return &generated_tokens->back();
                }
                return nullptr;
            },
            [=,this]{ return logical_or_expression(allow_angle_operators); }
        );

        if (ret && ret->terms_size() > 1) {
            error("assignment cannot be chained - instead of 'c = b = a;', write 'b = a; c = b;'", false);
            return {};
        }

        return ret;
    }
    else
    {
        auto ret = binary_expression<assignment_expression_node> (
            [](token const&, token const&) -> token const* { return nullptr; },
            [=,this]{ return logical_or_expression(allow_angle_operators); }
        );

        if (ret && ret->terms_size() > 1) {
            error("assignment cannot be chained - instead of 'c = b = a;', write 'b = a; c = b;'", false);
            return {};
        }

        return ret;
    }
}

//G expression:               // eliminated 'condition:' - just use 'expression:'
//G     assignment-expression
//GTODO    try expression
//G
auto parser::expression(bool allow_angle_operators, bool check_arrow)
    -> std::unique_ptr<expression_node>
{
    auto n = std::make_unique<expression_node>();

    {
    expression_node::current_expressions.push_back(n.get());
    auto guard = finally([&]{ expression_node::current_expressions.pop_back(); });

    if (!(n->expr = assignment_expression(allow_angle_operators))) {
        return {};
    }

    if (
        check_arrow
        && curr().type() == lexeme::Arrow
        )
    {
        error("'->' is not Cpp2 deference syntax - write '*.' instead");
        return {};
    }
    }

    for (auto& e : expression_node::current_expressions) {
        ++e->num_subexpressions;
    }
    return n;
}

//G expression-list:
//G     parameter-direction? expression
//G     expression-list ',' parameter-direction? expression
//G
auto parser::expression_list(
    token const* open_paren,
    bool inside_initializer
)
    -> std::unique_ptr<expression_list_node>
{
    auto pass = passing_style::in;
    auto n = std::make_unique<expression_list_node>();
    n->open_paren = open_paren;
    n->inside_initializer = inside_initializer;

    if (auto dir = to_passing_style(curr());
        (
            dir == passing_style::out
            || dir == passing_style::move
            || dir == passing_style::forward
            )
        && peek(1)
        && peek(1)->type() == lexeme::Identifier
        )
    {
        pass = dir;
        next();
    }
    auto x = expression();

    //  If this is an empty expression_list, we're done
    if (!x) {
        return n;
    }

    //  Otherwise remember the first expression
    n->expressions.push_back( { pass, std::move(x) } );
    //  and see if there are more...
    while (curr().type() == lexeme::Comma) {
        next();
        pass = passing_style::in;
        if (auto dir = to_passing_style(curr());
            dir == passing_style::out
            || dir == passing_style::move
            || dir == passing_style::forward
            )
        {
            pass = dir;
            next();
        }
        auto expr = expression();
        if (!expr) {
            error("invalid text in expression list", true, {}, true);
            return {};
        }
        n->expressions.push_back( { pass, std::move(expr) } );
    }
    return n;
}


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
auto parser::type_id()
    -> std::unique_ptr<type_id_node>
{
    auto n = std::make_unique<type_id_node>();

    while (
        (curr().type() == lexeme::Keyword && curr() == "const")
        || curr().type() == lexeme::Multiply
        )
    {
        if (
            curr() == "const"
            && !n->pc_qualifiers.empty()
            && *n->pc_qualifiers.back() == "const"
            )
        {
            error("consecutive 'const' not allowed");
            return {};
        }
        n->pc_qualifiers.push_back( &curr() );
        next();
    }

    if (auto id = qualified_id()) {
        n->pos = id->position();
        n->id  = std::move(id);
        assert (n->id.index() == type_id_node::qualified);
    }
    else if (auto id = unqualified_id()) {
        n->pos = id->position();
        n->id  = std::move(id);
        assert (n->id.index() == type_id_node::unqualified);
    }
    else {
        if (!n->pc_qualifiers.empty()) {
        error("'*'/'const' type qualifiers must be followed by a type name or '_' wildcard");
        }
        return {};
    }

    return n;
}


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
auto parser::is_as_expression()
    -> std::unique_ptr<is_as_expression_node>
{
    auto n = std::make_unique<is_as_expression_node>();
    n->expr = prefix_expression();
    if (!(n->expr)) {
        return {};
    }

    auto is_found = false;
    auto as_found = false;

    while (
        !done()
        && (curr() == "is" || curr() == "as")
        )
    {
        if (curr() == "is") {
            if (is_found) {
                error("repeated 'is' are not allowed");
                return {};
            }
            is_found = true;
        }
        else {
            as_found = true;
        }

        if (is_found && as_found) {
            error("mixed 'is' and 'as' are not allowed");
            return {};
        }

        auto term = is_as_expression_node::term{};
        term.op = &curr();
        next();

        if ((term.type = type_id()) != nullptr) {
            ;
        }
        else if ((term.expr = expression()) != nullptr) {
            ;
        }

        if (
            *term.op == "as"
            && term.expr
            )
        {
            error("'as' must be followed by a type-id, not an expression", false);
            return {};
        }
        if (
            !term.type
            && !term.expr
            )
        {
            if (*term.op == "is") {
                error( "'is' must be followed by a type-id or an expression", false);
            }
            else {
                error( "'as' must be followed by a type-id", false);
            }
            return {};
        }

        n->ops.push_back( std::move(term) );
    }

    return n;
}


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
auto parser::unqualified_id()
    -> std::unique_ptr<unqualified_id_node>
{
    //  Handle the identifier
    if (
        curr().type() != lexeme::Identifier
        && curr().type() != lexeme::Keyword
        && curr().type() != lexeme::Cpp2FixedType
        && curr().type() != lexeme::Ellipsis
        )
    {
        return {};
    }

    auto n = std::make_unique<unqualified_id_node>();

    n->identifier = &curr();
    next();

    //  Handle the template-argument-list if there is one
    if (curr().type() == lexeme::Less)
    {
        //  Remember current position, in case this < is isn't a template argument list
        auto start_pos = pos;

        n->open_angle = curr().position();
        next();

        auto term = template_argument{};

        do {
            //  If it doesn't start with * or const (which can only be a type id),
            //  try parsing it as an expression
            if (auto e = [&]{
                    if (
                        curr().type() == lexeme::Multiply // '*'
                        || curr() == "const"              // 'const'
                    )
                    {
                        return decltype(expression()){};
                    }
                    return expression(false);   // false == disallow unparenthesized relational comparisons in template args
                }()
            )
            {
                term.arg = std::move(e);
            }

            //  Else try parsing it as a type id
            else if (auto i = type_id()) {
                term.arg = std::move(i);
            }

            else {
                break;
            }

            n->template_args.push_back( std::move(term) );
        }
        //  Use the lambda trick to jam in a "next" clause
        while (
            curr().type() == lexeme::Comma
            && [&]{term.comma = curr().position(); next(); return true;}()
        );
            //  When this is rewritten in Cpp2, it will be:
            //      while curr().type() == lexeme::Comma
            //      next  term.comma = curr().position();

        if (curr().type() != lexeme::Greater) {
            //  Aha, this wasn't a template argument list after all,
            //  so back out just that part and return the identifier
            n->open_angle = source_position{};
            n->template_args.clear();
            pos = start_pos;
            return n;
        }
        n->close_angle = curr().position();
        next();
    }

    else {
        if (*n->identifier == "new") {
            error( "use 'new<" + curr().to_string() + ">', not 'new " + curr().to_string() + "'", false);
            return {};
        }
        if (*n->identifier == "co_await" || *n->identifier == "co_yield") {
            error( "(temporary alpha limitation) coroutines are not yet supported in Cpp2", false);
            return {};
        }
    }

    return n;
}


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
auto parser::qualified_id()
    -> std::unique_ptr<qualified_id_node>
{
    auto n = std::make_unique<qualified_id_node>();

    auto term = qualified_id_node::term{nullptr};

    //  Handle initial :: if present, else the first scope_op will be null
    if (curr().type() == lexeme::Scope) {
        term.scope_op = &curr();
        next();
    }

    //  Remember current position, because we need to look ahead to the next ::
    auto start_pos = pos;

    //  If we don't get a first id, or if we didn't have a leading :: and
    //  the next thing isn't :: or ., back out and report unsuccessful
    term.id = unqualified_id();
    if (
        !term.id
        || (!term.scope_op && curr().type() != lexeme::Scope)
        )
    {
        pos = start_pos;    // backtrack
        return {};
    }

    //  Reject "std" :: "move" / "forward"
    assert (term.id->identifier);
    auto first_uid_was_std = (*term.id->identifier == "std");
    auto first_time_through_loop = true;

    n->ids.push_back( std::move(term) );

    while (curr().type() == lexeme::Scope)
    {
        auto term = qualified_id_node::term{ &curr() };
        next();
        term.id = unqualified_id();
        if (!term.id) {
            error("invalid text in qualified name", true, {}, true);
            return {};
        }
        assert (term.id->identifier);
        if (
            first_time_through_loop
            && first_uid_was_std
            && term.scope_op->type() == lexeme::Scope
            )
        {
            if (*term.id->identifier == "move") {
                error("std::move is not needed in Cpp2 - use 'move' parameters/arguments instead", false);
                return {};
            }
            else if (*term.id->identifier == "forward") {
                error("std::forward is not needed in Cpp2 - use 'forward' parameters/arguments instead", false);
                return {};
            }
            first_time_through_loop = false;
        }
        n->ids.push_back( std::move(term) );
    }

    return n;
}


//G id-expression:
//G     qualified-id
//G     unqualified-id
//G
auto parser::id_expression()
    -> std::unique_ptr<id_expression_node>
{
    auto n = std::make_unique<id_expression_node>();
    if (auto id = qualified_id()) {
        n->pos = id->position();
        n->id  = std::move(id);
        assert (n->id.index() == id_expression_node::qualified);
        return n;
    }
    if (auto id = unqualified_id()) {
        n->pos = id->position();
        n->id  = std::move(id);
        assert (n->id.index() == id_expression_node::unqualified);
        return n;
    }
    return {};
}

//G literal:
//G     integer-literal ud-suffix?
//G     character-literal ud-suffix?
//G     floating-point-literal ud-suffix?
//G     string-literal ud-suffix?
//G     boolean-literal ud-suffix?
//G     pointer-literal ud-suffix?
//G     user-defined-literal ud-suffix?
//G
auto parser::literal()
    -> std::unique_ptr<literal_node>
{
    if (is_literal(curr().type())) {
        auto n = std::make_unique<literal_node>();
        n->literal = &curr();
        next();
        if (curr().type() == lexeme::UserDefinedLiteralSuffix) {
            n->user_defined_suffix = &curr();
            next();
        }
        return n;
    }
    return {};
}

//G expression-statement:
//G     expression ';'
//G     expression
//G
auto parser::expression_statement(bool semicolon_required)
    -> std::unique_ptr<expression_statement_node>
{
    auto n = std::make_unique<expression_statement_node>();

    expression_statement_node::current_expression_statements.push_back(n.get());
    auto guard = finally([&]{ expression_statement_node::current_expression_statements.pop_back(); });

    if (!(n->expr = expression())) {
        return {};
    }

    if (
        semicolon_required
        && (done() || curr().type() != lexeme::Semicolon)
        && peek(-1)->type() != lexeme::Semicolon
            //  this last peek(-1)-condition is a hack (? or is it just
            //  maybe elegant? I'm torn) so that code like
            //
            //      callback := :(inout x:_) = x += "suffix"; ;
            //
            //  doesn't need the redundant semicolon at the end of a decl...
            //  there's probably a cleaner way to do it, but this works and
            //  it doesn't destabilize any regression tests
        )
    {
        return {};
    }
    if (
        !done()
        && curr().type() == lexeme::Semicolon
        )
    {
        n->has_semicolon = true;
        next();
    }
    return n;
}


//G selection-statement:
//G     'if' 'constexpr'? logical-or-expression compound-statement
//G     'if' 'constexpr'? logical-or-expression compound-statement 'else' compound-statement
//G
auto parser::selection_statement()
    -> std::unique_ptr<selection_statement_node>
{
    if (
        curr().type() != lexeme::Keyword
        || curr() != "if"
        )
    {
        return {};
    }
    auto n = std::make_unique<selection_statement_node>();
    n->identifier = &curr();
    next();

    if (
        curr().type() == lexeme::Keyword
        && curr() == "constexpr"
        )
    {
        n->is_constexpr = true;
        next();
    }

    if (auto e = logical_or_expression()) {
        n->expression = std::move(e);
    }
    else {
        error("invalid if condition", true, {}, true);
        return {};
    }

    if (curr().type() != lexeme::LeftBrace) {
        error("an if branch body must be enclosed with { }");
        return {};
    }

    if (auto s = compound_statement()) {
        n->true_branch = std::move(s);
    }
    else {
        error("invalid if branch body", true, {}, true);
        return {};
    }

    if (
        curr().type() != lexeme::Keyword
        || curr() != "else"
        )
    {
        //  Add empty else branch to simplify processing elsewhere
        //  Note: Position (0,0) signifies it's implicit (no source location)
        n->false_branch =
            std::make_unique<compound_statement_node>( source_position(0,0) );
    }
    else {
        n->else_pos = curr().position();
        next();

        if (
            curr().type() != lexeme::LeftBrace
            && curr() != "if"
            )
        {
            error("an else branch body must be enclosed with { }");
            return {};
        }

        if (auto s = compound_statement( source_position{}, true )) {
            n->false_branch = std::move(s);
            n->has_source_false_branch = true;
        }
        else {
            error("invalid else branch body", true, {}, true);
            return {};
        }
    }

    return n;
}


//G return-statement:
//G     return expression? ';'
//G
auto parser::return_statement()
    -> std::unique_ptr<return_statement_node>
{
    if (
        curr().type() != lexeme::Keyword
        || curr() != "return"
        )
    {
        return {};
    }

    auto n = std::make_unique<return_statement_node>();
    n->identifier = &curr();
    next();

    //  If there's no optional return expression, we're done
    if (curr().type() == lexeme::Semicolon) {
        next();
        return n;
    }

    //  Handle the return expression
    auto x = expression();
    if (!x) {
        error("invalid return expression", true, {}, true);
        return {};
    }
    n->expression = std::move(x);

    //  Final semicolon
    if (curr().type() != lexeme::Semicolon) {
        error("missing ; after return");
        next();
        return {};
    }

    next();
    return n;
}


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
auto parser::iteration_statement()
    -> std::unique_ptr<iteration_statement_node>
{
    auto n = std::make_unique<iteration_statement_node>();

    //  If the next three tokens are:
    //      identifier ':' 'for/while/do'
    //  then it's a labeled iteration statement
    if (
        curr().type() == lexeme::Identifier
        && peek(1)
        && peek(1)->type() == lexeme::Colon
        && peek(2)
        && peek(2)->type() == lexeme::Keyword
        && (*peek(2) == "while" || *peek(2) == "do" || *peek(2) == "for")
        )
    {
        n->label = &curr();
        next();
        next();
    }

    if (
        curr().type() != lexeme::Keyword
        || (curr() != "while" && curr() != "do" && curr() != "for")
        )
    {
        return {};
    }

    n->identifier = &curr();
    next();

    //-----------------------------------------------------------------
    //  We'll do these same things in different orders,
    //  so extract them into local functions...
    auto handle_optional_next_clause = [&]() -> bool {
        if (curr() != "next") {
            return true; // absent next clause is okay
        }
        next(); // don't bother remembering "next" token, shouldn't need its position info
        auto next = assignment_expression();
        if (!next) {
            error("invalid expression after 'next'", true, {}, true);
            return false;
        }
        n->next_expression = std::move(next);
        return true;
    };

    auto handle_logical_expression = [&]() -> bool {
        auto x = logical_or_expression();
        if (!x) {
            error("a loop must have a valid conditional expression");
            return false;
        }
        n->condition = std::move(x);
        return true;
    };

    auto handle_compound_statement = [&]() -> bool {
        auto s = compound_statement();
        if (!s) {
            error("invalid while loop body", true, {}, true);
            return false;
        }
        n->statements = std::move(s);
        return true;
    };
    //-----------------------------------------------------------------

    //  Handle "while"
    //
    if (*n->identifier == "while")
    {
        if (!handle_logical_expression  ()) { return {}; }
        if (!handle_optional_next_clause()) { return {}; }
        if (!handle_compound_statement  ()) { return {}; }
        if (!done() && curr().type() == lexeme::Semicolon) {
            error("a loop body may not be followed by a semicolon (empty statements are not allowed)");
            return {};
        }
        return n;
    }

    //  Handle "do"
    //
    else if (*n->identifier == "do")
    {
        if (!handle_compound_statement  ()) { return {}; }
        if (curr() != "while") {
            error("do loop body must be followed by 'while'");
            return {};
        }
        next();
        if (!handle_logical_expression  ()) { return {}; }
        if (!handle_optional_next_clause()) { return {}; }
        if (curr().type() != lexeme::Semicolon) {
            error("missing ; after do..while loop condition");
            next();
            return {};
        }
        next();
        return n;
    }

    //  Handle "for"
    //
    else if (*n->identifier == "for")
    {
        n->range = expression();
        if (!n->range) {
            error("expected valid range expression after 'for'", true, {}, true);
            return {};
        }

        if (!handle_optional_next_clause()) { return {}; }

        if (
            curr() != "do"
            || !peek(1)
            || peek(1)->type() != lexeme::LeftParen
            )
        {
            next();
            if (curr().type() == lexeme::Colon) {
                error("alpha design change note: 'for range' syntax has changed - please remove ':' and '=', for example: for args do (arg) std::cout << arg;");
            }
            else {
                error("'for range' must be followed by 'do ( parameter )'");
            }
            return {};
        }
        next(2);    // eat 'do' and '('

        n->parameter = parameter_declaration(false, false, false);
        if (!n->parameter) {
            error("'for range do (' must be followed by a parameter declaration", false, source_position{}, true);
            return {};
        }

        if (curr().type() != lexeme::RightParen) {
            error("expected ')' after 'for' parameter");
            return {};
        }
        next();     // eat ')'

        n->body = statement();
        if (!n->body) {
            error("invalid for..do loop body", false, source_position{}, true);
            return {};
        }
        //  else
        if (n->parameter->pass == passing_style::in) {
            n->for_with_in = true;
        }

        if (!done() && curr().type() == lexeme::Semicolon) {
            error("a loop body may not be followed by a semicolon (empty statements are not allowed)");
            return {};
        }

        return n;
    }

    assert(!"compiler bug: unexpected case");
    return {};
}


//G alternative:
//G     alt-name? is-type-constraint '=' statement
//G     alt-name? is-value-constraint '=' statement
//G     alt-name? as-type-cast '=' statement
//G
//G alt-name:
//G     unqualified-id ':'
//G
auto parser::alternative()
    -> std::unique_ptr<alternative_node>
{
    auto n = std::make_unique<alternative_node>();

    //  Now we should be as "is" or "as"
    //  (initial partial implementation, just "is/as id-expression")
    if (
        curr() != "is"
        && curr() != "as"
        )
    {
        return {};
    }

    n->is_as_keyword = &curr();
    next();

    if (auto id = type_id()) {
        n->type_id = std::move(id);
    }
    else if (auto e = postfix_expression()) {
        n->value = std::move(e);
    }
    else {
        error("expected type-id or value after 'is' in inspect alternative", true, {}, true);
        return {};
    }

    if (curr().type() != lexeme::Assignment) {
        error("expected = at start of inspect alternative body", true, {}, true);
        return {};
    }
    n->equal_sign = curr().position();
    next();

    if (auto s = statement(true, n->equal_sign)) {
        n->statement = std::move(s);
    }
    else {
        error("expected statement after = in inspect alternative", true, {}, true);
        return {};
    }

    return n;
}


//G inspect-expression:
//G     'inspect' 'constexpr'? expression '{' alternative-seq? '}'
//G     'inspect' 'constexpr'? expression '->' type-id '{' alternative-seq? '}'
//G
//G alternative-seq:
//G     alternative
//G     alternative-seq alternative
//G
auto parser::inspect_expression(bool is_expression)
    -> std::unique_ptr<inspect_expression_node>
{
    if (curr() != "inspect") {
        return {};
    }

    if (!is_expression) {
        errors.emplace_back(
            curr().position(),
            "(temporary alpha limitation) cppfront is still learning 'inspect' - only inspect expressions are currently supported"
        );
        return {};
    }

    auto n = std::make_unique<inspect_expression_node>();
    n->identifier = &curr();
    next();

    if (curr() == "constexpr") {
        n->is_constexpr = true;
        next();
    }

    if (auto e = expression(true, false)) {
        n->expression = std::move(e);
    }
    else {
        error("invalid inspect expression", true, {}, true);
        return {};
    }

    //  Handle the optional explicit return type
    if (curr().type() == lexeme::Arrow)
    {
        if (!is_expression) {
            error("an inspect statement cannot have an explicit return type (whereas an inspect expression must have one)");
            return {};
        }
        next();
        if (curr().type() == lexeme::LeftParen) {
            error("multiple/named returns are not currently allowed for inspect");
            return {};
        }

        auto type = type_id();
        if (!type) {
            error("expected a valid inspect return type after ->");
            return {};
        }
        n->result_type = std::move(type);
    }
    else if (is_expression) {
        error("an inspect expression must have an explicit '-> result_type'");
        return {};
    }

    //  Now do the inspect body
    if (curr().type() != lexeme::LeftBrace) {
        error("expected { at start of inspect body");
        return {};
    }
    n->open_brace = curr().position();
    next();

    while (curr().type() != lexeme::RightBrace)
    {
        auto a = alternative();
        if (!a) {
            error("invalid alternative in inspect", true, {}, true);
            return {};
        }
        if (
            is_expression
            && !a->statement->is_expression()
            )
        {
            error("an inspect expression alternative must be just an expression "
                "(not a braced block) that will be used as the value of the inspect expression");
            return {};
        }
        n->alternatives.push_back( std::move(a) );
    }

    n->close_brace = curr().position();
    next();

    if (n->alternatives.empty()) {
        error("inspect body cannot be empty - add at least one alternative");
        return {};
    }

    return n;
}


//G jump-statement:
//G     'break' identifier? ';'
//G     'continue' identifier? ';'
//G
auto parser::jump_statement()
    -> std::unique_ptr<jump_statement_node>
{
    auto n = std::make_unique<jump_statement_node>();

    if (
        curr() != "break"
        && curr() != "continue"
        )
    {
        return {};
    }

    n->keyword = &curr();
    next();

    if (curr().type() == lexeme::Identifier) {
        n->label = &curr();
        next();
    }

    if (curr().type() != lexeme::Semicolon) {
        error("expected ; at end of jump-statement");
        return {};
    }
    next();

    return n;
}


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
auto parser::statement(
    bool                     semicolon_required,
    source_position          equal_sign,
    bool                     parameters_allowed,
    compound_statement_node* compound_parent
)
    -> std::unique_ptr<statement_node>
{
    if (!done() && curr().type() == lexeme::Semicolon) {
        error("empty statement is not allowed - remove extra semicolon");
        return {};
    }

    auto n = std::make_unique<statement_node>(compound_parent);

    //  If a parameter list is allowed here, try to parse one
    if (parameters_allowed) {
        n->parameters = parameter_declaration_list(false, true, false, true);
        if (n->parameters) {
            for (auto& param : n->parameters->parameters) {
                if (
                    param->direction() != passing_style::in
                    && param->direction() != passing_style::inout
                    && param->direction() != passing_style::copy
                    )
                {
                    error("(temporary alpha limitation) parameters scoped to a block/statement must be 'in' (the default), 'copy', or 'inout'", false);
                    return {};
                }
            }
        }
    }

    //  Now handle the rest of the statement

    if (auto s = selection_statement()) {
        n->statement = std::move(s);
        assert (n->is_selection());
        return n;
    }

    else if (auto i = inspect_expression(false)) {
        n->statement = std::move(i);
        assert (n->is_inspect());
        return n;
    }

    else if (auto s = return_statement()) {
        n->statement = std::move(s);
        assert (n->is_return());
        return n;
    }

    else if (auto s = jump_statement()) {
        n->statement = std::move(s);
        assert (n->is_jump());
        return n;
    }

    else if (auto s = iteration_statement()) {
        n->statement = std::move(s);
        assert (n->is_iteration());
        return n;
    }

    else if (auto s = compound_statement(equal_sign)) {
        n->statement = std::move(s);
        assert (n->is_compound());
        return n;
    }

    else if (auto s = declaration(true, false, false, n.get())) {
        n->statement = std::move(s);
        assert (n->is_declaration());
        return n;
    }

    else if (auto s = expression_statement(semicolon_required)) {
        n->statement = std::move(s);
        assert (n->is_expression());
        return n;
    }

    else if (auto s = contract()) {
        if (*s->kind != "assert") {
            error("only 'assert' contracts are allowed at statement scope");
            return {};
        }
        n->statement = std::move(s);
        assert (n->is_contract());
        return n;
    }

    else {
        return {};
    }
}


//G compound-statement:
//G     '{' statement-seq? '}'
//G
//G statement-seq:
//G     statement
//G     statement-seq statement
//G
auto parser::compound_statement(
    source_position equal_sign,
    bool            allow_single_unbraced_statement
)
    -> std::unique_ptr<compound_statement_node>
{
    bool is_braced = curr().type() == lexeme::LeftBrace;
    if (
        !is_braced
        && !allow_single_unbraced_statement
        )
    {
        return {};
    }

    auto n = std::make_unique<compound_statement_node>();
    if (!is_braced) {
        n->body_indent = curr().position().colno-1;
    }
    else if (peek(1)) {
        n->body_indent = peek(1)->position().colno-1;
    }

    //  Remember current position, in case this isn't a valid statement
    auto start_pos = pos;

    //  In the case where this is a declaration initializer with
    //      = {
    //  on the same line, we want to remember our start position
    //  as where the = was, not where the { was
    if (equal_sign.lineno == curr().position().lineno) {
        n->open_brace = equal_sign;
    }
    else {
        n->open_brace = curr().position();
    }

    if (is_braced) {
        next();
    }

    while (
        curr().type() != lexeme::RightBrace
        && (
            is_braced
            || std::ssize(n->statements) < 1
            )
        )
    {
        //  Only inside a compound-statement, a
        //  contained statement() may have parameters
        auto s = statement(true, source_position{}, true, n.get());
        if (!s) {

            // Only add error when no specific one already exist
            if(!has_error()) {
                error("invalid statement encountered inside a compound-statement", true);
            }
            pos = start_pos;    // backtrack
            return {};
        }
        n->statements.push_back( std::move(s) );
    }

    if (is_braced) {
        assert(curr().type() == lexeme::RightBrace);
        n->close_brace = curr().position();
        next();
    }
    return n;
}


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
auto parser::parameter_declaration(
    bool is_returns,
    bool is_named,
    bool is_template,
    bool is_statement
)
    -> std::unique_ptr<parameter_declaration_node>
{
    //  Remember current position, because we may need to backtrack if this is just
    //  a parenthesized expression statement, not a statement parameter list
    auto start_pos = pos;

    auto n = std::make_unique<parameter_declaration_node>();
    n->pass =
        is_returns   ? passing_style::out  :
        is_statement ? passing_style::copy :
        passing_style::in;
    n->pos  = curr().position();

    //  Handle optional this-specifier
    //
    if (curr() == "implicit") {
        n->mod = parameter_declaration_node::modifier::implicit;
        next();
    }
    else if (curr() == "virtual") {
        n->mod = parameter_declaration_node::modifier::virtual_;
        next();
    }
    else if (curr() == "override") {
        n->mod = parameter_declaration_node::modifier::override_;
        next();
    }
    else if (curr() == "final") {
        n->mod = parameter_declaration_node::modifier::final_;
        next();
    }

    //  Handle optional parameter-direction
    //
    if (auto dir = to_passing_style(curr());
        dir != passing_style::invalid
        )
    {
        if (is_template) {
            error("a template parameter cannot have a passing style (it is always implicitly 'in')");
            return {};
        }

        if (is_returns)
        {
            if (dir == passing_style::in) {
                error("a return value cannot be 'in'");
                return {};
            }
            if (dir == passing_style::copy) {
                error("a return value cannot be 'copy'");
                return {};
            }
            if (dir == passing_style::inout) {
                error("a return value cannot be 'inout'");
                return {};
            }
            if (dir == passing_style::move) {
                error("a return value cannot be 'move' (it is implicitly 'move'-out)");
                return {};
            }
        }
        if (
            !is_named
            && dir == passing_style::out
            )
        {
            error("(temporary alpha limitation) an unnamed function cannot have an 'out' parameter");
            return {};
        }
        n->pass = dir;
        next();
    }

    //  Now the main declaration
    //
    if (!(n->declaration = declaration(false, true, is_template))) {
        pos = start_pos;    // backtrack
        return {};
    }

    //  And some error checks
    //
    if (
        n->mod != parameter_declaration_node::modifier::none
        && !n->declaration->has_name("this")
        )
    {
        error( "only a 'this' parameter may be declared implicit, virtual, override, or final", false );
    }

    if (
        n->declaration->has_name("this")
        && n->pass != passing_style::in
        && n->pass != passing_style::inout
        && n->pass != passing_style::out
        && n->pass != passing_style::move
        )
    {
        error( "a 'this' parameter must be in, inout, out, or move", false );
    }

    if (
        n->declaration->has_name("that")
        && n->pass != passing_style::in
        && n->pass != passing_style::move
        )
    {
        error( "a 'that' parameter must be in or move", false );
    }

    //  The only parameter type that could be const-qualified is a 'copy' parameter, because
    //  only it is always truly its own variable, so it makes sense to let the user qualify it;
    //  all the other parameter types are conceptually (usually actually) bound to their args
    if (
        !is_returns
        && n->declaration->is_const()
        && n->pass != passing_style::copy
        && n->pass != passing_style::inout
        )
    {
        switch (n->pass) {
        break;case passing_style::in:
            error( "an 'in' parameter is always const, 'const' isn't needed and isn't allowed", false );
        break;case passing_style::inout:
            // error( "an 'inout' parameter can't be const, if you do want it to be const then use 'in' instead", false );
        break;case passing_style::out:
            error( "an 'out' parameter can't be const, otherwise it can't be initialized in the function body", false );
        break;case passing_style::move:
            error( "a 'move' parameter can't be const, otherwise it can't be moved from in the function body", false );
        break;case passing_style::forward:
            error( "a 'forward' parameter shouldn't be const, because it passes along the argument's actual const-ness (and actual value category)", false );
        break;default:
            assert (!"ICE: missing case");
        }
        return {};
    }

    if (
        !is_returns
        && !is_statement
        && n->declaration->initializer
        )
    {
        error("Cpp2 is currently exploring the path of not allowing default arguments - use overloading instead", false);
        return {};
    }
    if (is_named && is_returns) {
        auto tok = n->name();
        assert(tok);
        if (tok->type() != lexeme::Identifier) {
            error("expected identifier, not '" + tok->to_string() + "'",
                false, tok->position());
        }
        else if (n->declaration->has_wildcard_type()) {
            error("return parameter '" + tok->to_string() + "' must have a type",
                false, tok->position());
        }
    }
    return n;
}


//G parameter-declaration-list
//G     '(' parameter-declaration-seq? ')'
//G
//G parameter-declaration-seq:
//G     parameter-declaration
//G     parameter-declaration-seq ',' parameter-declaration
//G
auto parser::parameter_declaration_list(
    bool is_returns,
    bool is_named,
    bool is_template,
    bool is_statement
)
    -> std::unique_ptr<parameter_declaration_list_node>
{
    //  Remember current position, because we need to look ahead in
    //  the case of seeing whether a local statement starts with a
    //  parameter list, since finding that it doesn't (it's some other
    //  parenthesized expression) is not an error, just backtrack
    auto start_pos = pos;

    auto opener = lexeme::LeftParen;
    auto closer = lexeme::RightParen;
    if (is_template) {
        opener = lexeme::Less;
        closer = lexeme::Greater;
    }

    if (curr().type() != opener) {
        return {};
    }

    auto n = std::make_unique<parameter_declaration_list_node>();
    n->open_paren = &curr();
    next();

    auto param = std::make_unique<parameter_declaration_node>();

    auto count = 1;
    auto expect_another_param_decl = false;

    while ((param = parameter_declaration(is_returns, is_named, is_template, is_statement)) != nullptr)
    {
        expect_another_param_decl = false;
        param->ordinal = count;
        ++count;

        if (
            std::ssize(n->parameters) > 1
            && n->parameters.back()->has_name("that")
            )
        {
            error("'that' may not be followed by any additional parameters", false);
        }

        n->parameters.push_back( std::move(param) );

        if (curr().type() == closer) {
            break;
        }
        else if (curr().type() != lexeme::Comma) {
            if (is_statement) {
                pos = start_pos;    // backtrack
            }
            else {
                error("expected ',' in parameter list", true, {}, true);
            }
            return {};
        }

        expect_another_param_decl = true;
        next();
    }

    if (expect_another_param_decl) {
        error("invalid parameter list: a comma must be followed by another parameter", true, {}, true);
    }

    if (curr().type() != closer) {
        if (is_statement) {
            pos = start_pos;    // backtrack
        }
        else {
            error("invalid parameter list", true, {}, true);
        }
        return {};
    }

    n->close_paren = &curr();
    next();
    return n;
}


//G contract:
//G     '[' '[' contract-kind id-expression? ':' logical-or-expression ']' ']'
//G     '[' '[' contract-kind id-expression? ':' logical-or-expression ',' string-literal ']' ']'
//G
//G contract-kind: one of
//G     'pre' 'post' 'assert'
//G
auto parser::contract()
    -> std::unique_ptr<contract_node>
{
    //  Note: For now I'm using [[ ]] mainly so that existing Cpp1 syntax highlighters
    //        don't get confused... I initially implemented single [ ], but then
    //        my editor's default Cpp1 highlighter didn't colorize the following
    //        multiline // comment correctly as a comment

    //  If there's no [ [ then this isn't a contract
    if (
        done()
        || curr().type() != lexeme::LeftBracket
        || !peek(1)
        || peek(1)->type() != lexeme::LeftBracket
        )
    {
        return {};
    }

    auto n = std::make_unique<contract_node>(curr().position());
    auto guard = capture_groups_stack_guard(this, &n->captures);
    next();
    next();

    if (
        curr() != "pre"
        && curr() != "post"
        && curr() != "assert"
        )
    {
        error("[ begins a contract and must be followed by 'pre', 'post', or 'assert'");
        return {};
    }
    n->kind = &curr();
    next();

    if (auto id = id_expression()) {
        n->group = std::move(id);
    }

    if (curr().type() != lexeme::Colon) {
        error("expected : before the contract condition");
        return {};
    }
    next();

    auto condition = logical_or_expression();
    if (!condition) {
        error("invalid contract condition", true, {}, true);
        return {};
    }
    n->condition = std::move(condition);

    //  Now check for the optional string message
    if (curr().type() == lexeme::Comma) {
        next();
        if (curr().type() != lexeme::StringLiteral) {
            error("expected contract message string");
            return {};
        }
        n->message = &curr();
        next();
    }

    if (
        curr().type() != lexeme::RightBracket
        || !peek(1)
        || peek(1)->type() != lexeme::RightBracket
        )
    {
        error("expected ]] at the end of the contract");
        return {};
    }
    next();
    next();

    return n;
}


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
auto parser::function_type(
    declaration_node* my_decl,
    bool              is_named
    )
    -> std::unique_ptr<function_type_node>
{
    auto n = std::make_unique<function_type_node>( my_decl );

    //  Parameters
    auto parameters = parameter_declaration_list(false, is_named, false);
    if (!parameters) {
        return {};
    }
    n->parameters = std::move(parameters);

    //  Optional "throws"
    if (
        curr().type() == lexeme::Keyword
        && curr() == "throws"
        )
    {
        if (
            n->is_move()
            || n->is_swap()
            || n->is_destructor()
            )
        {
            error( "(experimental restriction) Cpp2 currently does not allow a move, swap, or destructor function to be designated 'throws'" );
            return {};
        }

        n->throws = true;
        next();
    }

    //  Optional returns
    if (curr().type() == lexeme::Arrow)
    {
        next();

        if (auto pass = to_passing_style(curr());
            pass != passing_style::invalid
            )
        {
            if (
                pass != passing_style::forward
                && pass != passing_style::move
                )
            {
                error("only 'forward' and 'move' return passing style are allowed from functions");
            }
            next();
            if (auto t = type_id()) {
                n->returns = function_type_node::single_type_id{ std::move(t), pass };
                assert(n->returns.index() == function_type_node::id);
            }
            else {
                auto msg = std::string("'");
                msg += to_string_view(pass);
                error(msg + "' must be followed by a type-id");
            }
        }
        else if (auto t = type_id()) {
            if (
                t->get_token()
                && t->get_token()->to_string() == "auto"
                )
            {
                auto name = std::string{"v"};
                if (my_decl && my_decl->name()) {
                    name = my_decl->name()->to_string();
                }
                errors.emplace_back(
                    curr().position(),
                    "to define a function " + name + " with deduced return type, write '" + name + ": ( /* arguments */ ) -> _ = { /* function body */ }'"
                );
                return {};
            }
            n->returns = function_type_node::single_type_id{ std::move(t), passing_style::move };
            assert(n->returns.index() == function_type_node::id);
        }
        else if (auto returns_list = parameter_declaration_list(true, is_named)) {
            if (std::ssize(returns_list->parameters) < 1) {
                error("an explicit return value list cannot be empty");
                return {};
            }
            n->returns = std::move(returns_list);
            assert(n->returns.index() == function_type_node::list);
        }
        else {
            error("missing function return after ->");
            return {};
        }
    }

    //  Pre/post conditions
    while (auto c = contract())
    {
        if (
            *c->kind != "pre"
            && *c->kind != "post"
            )
        {
            error("only 'pre' and 'post' contracts are allowed on functions");
            return {};
        }
        n->contracts.push_back( std::move(c) );
    }

    return n;
}


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
auto parser::unnamed_declaration(
    source_position                      start,
    bool                                 semicolon_required,
    bool                                 captures_allowed,
    bool                                 named,
    bool                                 is_parameter,
    bool                                 is_template_parameter,
    std::unique_ptr<unqualified_id_node> id,
    accessibility                        access,
    bool                                 is_variadic,
    statement_node*                      my_stmt
)
    -> std::unique_ptr<declaration_node>
{
    auto n = std::make_unique<declaration_node>( current_declarations.back() );
    n->pos = start;

    n->identifier   = std::move(id);
    n->access       = access;
    n->is_variadic  = is_variadic;
    n->my_statement = my_stmt;

    //  If we're in a type scope and the next token is ';', treat this as if
    //  ': _;' without an initializer.
    //  This is for type metafunctions that want to use the incomplete name-only
    //  declaration, and transform it to something else. If unchanged the
    //  incomplete declaration will be rejected later by sema.check rule.
    if (
        n->parent_is_type()
        && curr().type() == lexeme::Semicolon
        )
    {
        n->type = std::make_unique<type_id_node>();
        assert (n->is_object());
        next();
        return n;
    }

    //  For a template parameter, ':' is not required and
    //  we default to ': type'
    if (
        is_template_parameter
        && curr().type() != lexeme::Colon
        )
    {
        //  So invent the "type" token
        generated_text.push_back("type");
        generated_tokens->push_back({
            generated_text.back().c_str(),
            std::ssize(generated_text.back()),
            start,
            lexeme::Identifier
        });

        //  So we can create the type_node

        auto t = std::make_unique<type_node>( &generated_tokens->back() );

        n->type = std::move(t);
        assert (n->is_type());

        //  That's it, we're done here
        return n;
    }

    //  For 'this' and 'that' parameters ':' is not allowed and we'll use the default ': _'
    if (
        n->identifier
        && is_parameter
        && (
            *n->identifier->identifier == "this"
            || *n->identifier->identifier == "that"
            )
        && curr().type() == lexeme::Colon
        )
    {
        error("a 'this' or 'that' parameter knows its type, no ':' is allowed here", false);
        return {};
    }

    //  For an ordinary parameter, ':' is not required and
    //  we default to ': _' - i.e., deduced with no initializer
    if (
        is_parameter
        && curr().type() != lexeme::Colon
        )
    {
        //  So invent the "_" token
        generated_text.push_back("_");
        generated_tokens->push_back({
            generated_text.back().c_str(),
            std::ssize(generated_text.back()),
            start,
            lexeme::Identifier
        });

        //  So we can create the typeid_id_node and its unqualified_id_node

        auto gen_id = std::make_unique<unqualified_id_node>();
        gen_id->identifier = &generated_tokens->back();

        auto type = std::make_unique<type_id_node>();
        type->pos = start;
        type->id = std::move(gen_id);

        n->type = std::move(type);
        assert (n->is_object());

        //  That's it, we're done here
        return n;
    }

    //  Otherwise, the next token must be ':'
    if (curr().type() != lexeme::Colon) {
        return {};
    }
    next();

    if (curr() == "union") {
        error("unsafe 'union' is not supported in Cpp2 - write '@union' to apply Cpp2's safe 'union' type metafunction instead, or use std::variant");
    }

    //  Next is an optional metafunctions clause
    while (curr() == "@") {
        next();
        auto idx = id_expression();
        if (!idx) {
            error("'@' must be followed by a a metafunction name", false);
            return {};
        }
        n->metafunctions.push_back( std::move(idx) );
    }

    //  Next is an optional template parameter list
    if (curr().type() == lexeme::Less) {
        auto template_parameters = parameter_declaration_list(false, false, true);
        if (!template_parameters) {
            error("invalid template parameter list");
            return {};
        }
        n->template_parameters = std::move(template_parameters);
    }

    auto guard =
        captures_allowed
        ? std::make_unique<capture_groups_stack_guard>(this, &n->captures)
        : std::unique_ptr<capture_groups_stack_guard>()
        ;

    auto guard2 = current_declarations_stack_guard(this, n.get());

    //  Next is an an optional type

    auto deduced_type = false;

    //  It could be "type", declaring a user-defined type
    if (
        curr() == "type"
        || (
            curr() == "final"
            && peek(1) && *peek(1) == "type"
            )
        )
    {
        n->type = std::make_unique<type_node>( &curr(), curr() == "final" );

        if (curr() == "final") {
            next();
        }
        next();

        if (
            is_parameter
            && !is_template_parameter
            )
        {
            error("a normal parameter cannot be a 'type' - did you mean to put this in a < > template parameter list?");
            return {};
        }
        assert (n->is_type());
    }

    //  Or a function type, declaring a function - and tell the function whether it's in a user-defined type
    else if (auto t = function_type(n.get(), named))
    {
        n->type = std::move(t);
        assert (n->is_function());

        if (!n->metafunctions.empty()) {
            errors.emplace_back(
                n->metafunctions.front()->position(),
                "(temporary alpha limitation) metafunctions are currently not supported on functions, only on types"
            );
            return {};
        }
    }

    //  Or a namespace
    else if (curr() == "namespace")
    {
        n->type = std::make_unique<namespace_node>( &curr() );
        assert (n->type.index() == declaration_node::a_namespace);
        next();

        if (!n->metafunctions.empty()) {
            errors.emplace_back(
                n->metafunctions.front()->position(),
                "(temporary alpha limitation) metafunctions are currently not supported on namespaces, only on types"
            );
            return {};
        }
    }

    //  Or just a type-id, declaring a non-pointer object
    else if (auto t = type_id())
    {
        if (
            t->get_token()
            && t->get_token()->to_string() == "auto"
            )
        {
            auto name = std::string{"v"};
            if (n->name()) {
                name = n->name()->to_string();
            }
            errors.emplace_back(
                curr().position(),
                "to define a variable " + name + " with deduced type, write '" + name + " := /* initializer */;'"
            );
            return {};
        }

        n->type = std::move(t);
        assert (n->is_object());

        if (!n->metafunctions.empty()) {
            errors.emplace_back(
                n->metafunctions.front()->position(),
                "(temporary alpha limitation) metafunctions are currently not supported on objects, only on types"
            );
            return {};
        }

        if (curr().type() == lexeme::LeftBracket) {
            error("C-style array types are not allowed, use std::array instead");
            return {};
        }
    }

    //  Or nothing, declaring an object of deduced type,
    //  which we'll represent using an empty type-id
    else {
        n->type = std::make_unique<type_id_node>();
        assert (n->is_object());
        deduced_type = true;
    }

    //  Next is optionally a requires clause
    if (curr() == "requires")
    {
        if (
            n->is_type()
            && !n->template_parameters
            )
        {
            error("'requires' is not allowed on a type that does not have a template parameter list");
            return {};
        }

        if (n->is_namespace())
        {
            error("'requires' is not allowed on a namespace");
            return {};
        }

        n->requires_pos = curr().position();
        next();
        auto e = logical_or_expression();
        if (!e) {
            error("'requires' must be followed by an expression");
            return {};
        }
        n->requires_clause_expression = std::move(e);
    }

    //  Next is optionally = or == followed by an initializer

    //  If there is no = or ==
    if (
        !done()
        && curr().type() != lexeme::Assignment
        && curr().type() != lexeme::EqualComparison
        )
    {
        if (
            n->is_type()
            && !is_template_parameter
            )
        {
            error("a user-defined type must have an = initializer");
            return {};
        }

        //  Then there may be a semicolon
        //  If there is a semicolon, eat it
        if (!done() && curr().type() == lexeme::Semicolon) {
            next();
        }
        // But if there isn't one and it was required, diagnose an error
        else if (semicolon_required) {
            if (curr().type() == lexeme::LeftBrace) {
                error("expected '=' before '{' - did you mean '= {' ?", true, {}, true);
            }
            else {
                error("missing ';' at end of declaration or '=' at start of initializer", true, {}, true);
            }
            return {};
        }
    }

    //  There was an = or ==, so eat it and continue
    else {
        n->equal_sign = curr().position();

        if (curr().type() == lexeme::EqualComparison) {
            if (!n->is_function()) {
                error("syntax error at '==' - did you mean '='?");
            }
            n->is_constexpr = true;
        }

        next();

        if (auto t = std::get_if<declaration_node::an_object>(&n->type);
            t
            && (*t)->is_pointer_qualified()
            )
        {
            if (
                curr() == "nullptr"
                || isdigit(std::string_view(curr())[0])
                || (
                    curr() == "("
                    && peek(1)
                    && *peek(1) == ")"
                    )
                )
            {
                error("pointer cannot be initialized to null or int - leave it uninitialized and then set it to a non-null value when you have one");
                violates_lifetime_safety = true;
                throw std::runtime_error("null initialization detected");
            }
        }

        //  deduced_type == true means that the type will be deduced,
        //  represented using an empty type-id
        if (
            deduced_type
            && peek(1)
            )
        {
            auto& type = std::get<declaration_node::an_object>(n->type);
            // object initialized by the address of the curr() object
            if (peek(1)->type() == lexeme::Ampersand) {
                type->address_of = &curr();
            }
            // object initialized by (potentially multiple) dereference of the curr() object
            else if (peek(1)->type() == lexeme::Multiply) {
                type->dereference_of = &curr();
                for (int i = 1; peek(i)->type() == lexeme::Multiply; ++i)
                    type->dereference_cnt += 1;
            }
            else if (
                // object initialized by the result of the function call (and it is not unnamed function)
                (peek(1)->type() == lexeme::LeftParen && curr().type() != lexeme::Colon)
                || curr().type() == lexeme::Identifier // or by the object (variable that the type need to be checked)
            ) {
                type->suspicious_initialization = &curr();
            }
        }

        if (!(n->initializer = statement(semicolon_required, n->equal_sign))) {
            error(
                "ill-formed initializer",
                true, {}, true
            );
            next();
            return {};
        }
    }

    //  If this is a type with metafunctions, apply those
    if (n->is_type()) {
        if (!apply_type_metafunctions(*n)) {
            error(
                "error encountered while applying type metafunctions",
                false, {}, true
            );
            return {};
        }
    }

    if (
        n->is_function()
        && n->initializer
        && !done() && curr().type() == lexeme::Semicolon
        )
    {
        if (n->initializer->is_compound() && n->has_name()) {
            error("a braced function body may not be followed by a semicolon (empty statements are not allowed)");
            return {};
        } else if (n->initializer->is_expression()) {
            error("a single-expression function should end with a single semicolon");
            return {};
        }
    }

    //  If this is a function with a list of multiple/named return values,
    //  and the function body's end doesn't already have "return" as the
    //  last statement, then generate "return;" as the last statement
    if (auto func = std::get_if<declaration_node::a_function>(&n->type);
        func
        && n->initializer
        && (*func)->returns.index() == function_type_node::list
        )
    {
        if (!n->initializer->is_compound()) {
            error(
                "a function with named return value(s) must have a full { } body",
                false,
                {},
                true
            );
            return {};
        }

        auto& body = std::get<statement_node::compound>(n->initializer->statement);

        if (
            body->statements.empty()
            || !body->statements.back()->is_return()
            )
        {
            auto last_pos = n->position();
            if (!body->statements.empty()) {
                last_pos = body->statements.back()->position();
            }
            ++last_pos.lineno;
            generated_tokens->emplace_back( "return", last_pos, lexeme::Keyword);

            auto ret = std::make_unique<return_statement_node>();
            ret->identifier = &generated_tokens->back();

            auto stmt = std::make_unique<statement_node>();
            stmt->statement = std::move(ret);

            body->statements.push_back(std::move(stmt));
        }
    }

    //  If this is a function, record its extents
    if (n->is_function()) {
        function_body_extents.emplace_back(
            n->equal_sign.lineno,
            peek(-1)->position().lineno
        );
    }

    return n;
}


//G alias:
//G     ':' template-parameter-declaration-list? 'type' requires-clause? '==' type-id ';'
//G     ':' 'namespace' '==' id-expression ';'
//G     ':' template-parameter-declaration-list? type-id? requires-clause? '==' expression ';'
//G
//GT     ':' function-type '==' expression ';'
//GT        # See commit 63efa6ed21c4d4f4f136a7a73e9f6b2c110c81d7 comment
//GT        # for why I don't see a need to enable this yet
//
auto parser::alias()
    -> std::unique_ptr<declaration_node>
{
    //  Remember current position, because we need to look ahead
    auto start_pos = pos;

    auto n = std::make_unique<declaration_node>( current_declarations.back() );

    if (curr().type() != lexeme::Colon) {
        return {};
    }
    next();

    //  Next is an optional template parameter list
    if (curr().type() == lexeme::Less) {
        auto template_parameters = parameter_declaration_list(false, false, true);
        if (!template_parameters) {
            pos = start_pos;    // backtrack
            return {};
        }
        n->template_parameters = std::move(template_parameters);
    }

    auto a = std::make_unique<alias_node>( &curr() );

    //  Next must be 'type', 'namespace', a type-id, or we're at the 'requires' or '=='
    if (curr() == "type")
    {
        next();
    }
    else if (curr() == "namespace")
    {
        next();
        if (n->template_parameters) {
            errors.emplace_back(
                curr().position(),
                "a namespace or namespace alias cannot have template parameters"
            );
            return {};
        }
    }
    else if (curr().type() != lexeme::EqualComparison && curr() != "requires")
    {
        a->type_id = type_id();
        if (!a->type_id) {
            pos = start_pos;    // backtrack
            return {};
        }
    }

    //  Next is optionally a requires clause
    if (curr() == "requires")
    {
        if (
            n->is_type_alias()
            && !n->template_parameters
            )
        {
            error("'requires' is not allowed on a type alias that does not have a template parameter list");
            return {};
        }

        if (n->is_namespace_alias())
        {
            error("'requires' is not allowed on a namespace alias");
            return {};
        }

        n->requires_pos = curr().position();
        next();
        auto e = logical_or_expression(true, false);
        if (!e) {
            error("'requires' must be followed by an expression");
            return {};
        }
        n->requires_clause_expression = std::move(e);
    }

    //  Now we should be at the '==' if this is an alias

    if (curr().type() == lexeme::EqualComparison) {
        next();
    }
    else {
        if (a->type->type() != lexeme::EqualComparison) {
            pos = start_pos;    // backtrack
            return {};
        }
    }
    assert(peek(-1)->type() == lexeme::EqualComparison);

    if (
        n->parent_is_type()
        && *a->type == "namespace"
        )
    {
        errors.emplace_back(
            curr().position(),
            "a namespace alias cannot appear in a type scope"
        );
        return {};
    }

    //  Finally, pick up the initializer

    //  Type alias
    if (*a->type == "type")
    {
        auto t = type_id();
        if (!t) {
            errors.emplace_back(
                curr().position(),
                "a 'type ==' alias declaration must be followed by a type name"
            );
            return {};
        }
        if (
            t->is_wildcard()
            || ( t->get_token() && t->get_token()->to_string() == "auto" )
        ) {
            errors.emplace_back(
                curr().position(),
                "a 'type ==' alias declaration must be followed by a type name (not a wildcard _ nor auto)"
            );
            return {};
        }
        a->initializer = std::move(t);
    }

    //  Namespace alias
    else if (*a->type == "namespace")
    {
        if (auto qid = id_expression()) {
            a->initializer = std::move(qid);
        }
        else {
            errors.emplace_back(
                curr().position(),
                "a 'namespace ==' alias declaration must be followed by a namespace name (id-expression)"
            );
            return {};
        }
    }

    //  Object alias
    else if (
        a->type_id
        || a->type->type() == lexeme::EqualComparison
        )
    {
        auto e = expression();
        if (!e) {
            errors.emplace_back(
                curr().position(),
                "an object '==' alias declaration must be followed by an expression"
            );
            return {};
        }
        a->initializer = std::move(e);
    }

    //  Anything else shouldn't be possible
    else {
        assert(!"ICE: should be unreachable - invalid alias declaration");
        return {};
    }

    //  And the final ceremonial semicolon
    if (curr() != ";") {
        errors.emplace_back(
            curr().position(),
            "';' expected at end of alias declaration"
        );
        return {};
    }
    next();

    n->type = std::move(a);

    return n;
}


//G declaration:
//G     access-specifier? identifier '...'? unnamed-declaration
//G     access-specifier? identifier alias
//G
//G access-specifier:
//G     public
//G     protected
//G     private
//G
auto parser::declaration(
    bool            semicolon_required,
    bool            is_parameter,
    bool            is_template_parameter,
    statement_node* my_stmt
)
    -> std::unique_ptr<declaration_node>
{
    if (done()) { return {}; }

    //  Remember current position, because we need to look ahead
    auto start_pos = pos;

    auto n = std::unique_ptr<declaration_node>{};

    //  This scope is to ensure that once we've moved 'id' into the
    //  declaration_node, we don't access the moved-from local name
    //  (and similar hygiene for 'access' though that one doesn't matter as much)
    //  The reason to move 'id' into unnamed_declaration() is so that
    //  it can conveniently perform some checks that refer to the name
    {
        auto access = accessibility::default_;
        if (curr() == "public") {
            access = accessibility::public_;
            next();
        }
        else if (curr() == "protected") {
            access = accessibility::protected_;
            next();
        }
        else if (curr() == "private") {
            access = accessibility::private_;
            next();
        }

        //  If they wrote an access-specifier, see if they put a ':'
        //  after it out of Cpp1 habit (there's no colon in Cpp2)
        if (
            access != accessibility::default_
            && curr().type() == lexeme::Colon
            )
        {
            errors.emplace_back(
                curr().position(),
                "':' is not allowed after an access-specifier"
            );
            return {};
        }

        auto id = unqualified_id();
        if (!id) {
            return {};
        }

        auto is_variadic = false;
        if (curr().type() == lexeme::Ellipsis) {
            is_variadic = true;
            next();
        }

        //  Provide some useful Cpp1->Cpp2 migration diagnostics for common mistakes
        //
        if (
            id->get_token()
            && *id->get_token() == "auto"
            && curr().type() != lexeme::Colon
            )
        {
            auto name = std::string{"v"};
            if (peek(0) && peek(0)->type() == lexeme::Identifier) {
                name = peek(0)->to_string();
            }
            errors.emplace_back(
                curr().position(),
                "to define a variable " + name + " of type T, write '" + name + ": T = /* initializer */'"
            );
            return {};
        }
        if (
            id->get_token()
            && *id->get_token() == "namespace"
            && curr().type() != lexeme::Colon
            )
        {
            auto name = std::string{"N"};
            if (peek(0)) {
                name = peek(0)->to_string();
            }
            errors.emplace_back(
                curr().position(),
                "to define a namespace " + name + ", write '" + name + " : namespace = { /*contents*/ }'"
            );
            return {};
        }
        if (
            id->get_token()
            && (
                *id->get_token() == "class"
                || *id->get_token() == "struct"
                )
            && curr().type() != lexeme::Colon
            )
        {
            auto name = std::string{"C"};
            if (peek(0)) {
                name = peek(0)->to_string();
            }
            errors.emplace_back(
                curr().position(),
                "to define a type " + name + ", write '" + name + " : type = { /*body*/ }'"
            );
            return {};
        }

        //  Now proceed...
        //

        //  First see if it's an alias declaration
        n = alias();
        if (n) {
            if (is_parameter) {
                errors.emplace_back(
                    curr().position(),
                    "a parameter declaration may not be an alias declaration"
                );
                return {};
            }

            if (is_variadic) {
                errors.emplace_back(
                    curr().position(),
                    "an alias declaration may not be variadic"
                );
                return {};
            }

            n->pos        = start_pos;
            n->identifier = std::move(id);
            n->access     = access;
            return n;
        }

        //  Otherwise, this is a normal declaration
        n = unnamed_declaration(
            start_pos,
            semicolon_required,
            false,
            true,
            is_parameter,
            is_template_parameter,
            std::move(id),
            access,
            is_variadic,
            my_stmt
        );
        if (!n) {
            pos = start_pos;    // backtrack
            return {};
        }
    }

    //  Note: Do this after trying to parse this as a declaration, for parse backtracking

    if (
        *n->identifier->identifier == "that"
        && (
            !is_parameter
            || is_template_parameter
            )
        )
    {
        errors.emplace_back(
            n->identifier->position(),
            "'that' may only be declared as an ordinary function parameter"
        );
        return {};
    }

    //  Cache some context
    n->is_template_parameter = is_template_parameter;
    n->is_parameter          = is_parameter;

    return n;
}


//G declaration-seq:
//G     declaration
//G     declaration-seq declaration
//G
//G translation-unit:
//G     declaration-seq?
//
auto parser::translation_unit()
    -> std::unique_ptr<translation_unit_node>
{
    auto n = std::make_unique<translation_unit_node>();
    for (auto d = declaration(); d; d = declaration()) {
        n->declarations.push_back( std::move(d) );
    }
    return n;
}

auto parser::debug_print(std::ostream& o)
    -> void
{
    o << "\n\n--- Parse tree\n";

    auto tree_printer = parse_tree_printer{o};
    visit( tree_printer );

    o << "\n\n--- Function body extents\n";

    for (auto const& f : function_body_extents) {
        o << "    " << f.first << "-" << f.last << "\n";
    }
}    
}

