#include "io.h"

namespace cpp2 {

//---------------------------------------------------------------------------
//  move_next: advances i as long as p(line[i]) is true or the end of line
//
//  line    current line being processed
//  i       current index
//  p       predicate to apply
//
auto move_next(
    std::string const& line,
    int&               i,
    auto               p
)
    -> bool
{
    while (
        i < ssize(line)
        && line[i]
        && p(line[i])
        )
    {
        ++i;
    }
    return
        i < ssize(line)
        && line[i]
        ;
}


//---------------------------------------------------------------------------
//  peek_first_non_whitespace: returns the first non-whitespace char in line
//
//  line    current line being processed
//
auto peek_first_non_whitespace(std::string const& line)
    -> char
{
    auto i = 0;

    //  find first non-whitespace character
    if (!move_next(line, i, isspace)) {
        return '\0';
    }

    return line[i];
}


//---------------------------------------------------------------------------
//  is_preprocessor: returns whether this is a preprocessor line starting
//  with #, and whether it will be followed by another preprocessor line
//
//  line        current line being processed
//  first_line  whether this is supposed to be the first line (start with #)
//
auto is_preprocessor(
    std::string const& line,
    bool               first_line
)
    -> is_preprocessor_ret
{
    //  see if the first non-whitespace is #
    if (
        first_line
        && peek_first_non_whitespace(line) != '#'
        )
    {
        return { false, false };
    }

    //  return true iff last character is a \ continuation
    return { true, line.back() == '\\' };
}


//---------------------------------------------------------------------------
//  starts_with_import: returns whether the line starts with "import"
//
//  line    current line being processed
//
auto starts_with_import(std::string const& line)
    -> bool
{
    auto i = 0;

    //  find first non-whitespace character
    if (!move_next(line, i, isspace)) {
        return false;
    }

    static constexpr auto import_keyword = std::string_view{"import"};

    // the first token must begin with 'import'
    if (!std::string_view(line).substr(i).starts_with(import_keyword)) {
        return false;
    }

    // and not be immediately followed by an _identifier-continue_
    return !is_identifier_continue(line[i + import_keyword.size()]);
}


//---------------------------------------------------------------------------
//  starts_with_whitespace_slash_slash: is this a "// comment" line
//
//  line    current line being processed
//
auto starts_with_whitespace_slash_slash(std::string const& line)
    -> bool
{
    auto i = 0;

    //  find first non-whitespace character
    if (!move_next(line, i, isspace)) {
        return false;
    }

    return
        i < ssize(line)-1
        && line[i] == '/'
        && line[i+1] == '/'
        ;
}


//---------------------------------------------------------------------------
//  starts_with_whitespace_slash_star_and_no_star_slash: is this a "/* comment" line
//
//  line    current line being processed
//
auto starts_with_whitespace_slash_star_and_no_star_slash(std::string const& line)
    -> bool
{
    auto i = 0;

    //  find first non-whitespace character
    if (!move_next(line, i, isspace)) {
        return false;
    }

    if (
        i < ssize(line) - 1
        && line[i] == '/'
        && line[i + 1] == '*'
        )
    {
        return line.find("*/", i) == std::string::npos;
    }
    else {
        return false;
    }
}


//---------------------------------------------------------------------------
//  starts_with_operator: returns whether the line starts with the string "operator"
//  followed by the symbols of an operator
//
//  line    current line being processed
//
auto starts_with_operator(std::string_view s)
    -> int
{
    if (s.starts_with("operator"))
    {
        auto j = 8;

        //  skip any spaces
        while (
            j < std::ssize(s)
            && isspace(s[j])
            )
        {
            ++j;
        }
        if (j >= std::ssize(s)) {
            return 0;
        }

        auto c1 = [&]{ if (j   < std::ssize(s)) { return s[j  ]; } return '\0'; }();
        auto c2 = [&]{ if (j+1 < std::ssize(s)) { return s[j+1]; } return '\0'; }();
        auto c3 = [&]{ if (j+2 < std::ssize(s)) { return s[j+2]; } return '\0'; }();

        switch (c1)
        {
            //  /= /
            //  == =
            //  ! !=
            //  *= *
            //  %= %
            //  ^= ^
            //  ~= ~
        break;case '/':
              case '=':
              case '!':
              case '*':
              case '%':
              case '^':
              case '~':
            if (c2 == '=') { return j+2; }
            return j+1;

            //  ++ += +
        break;case '+':
            if (c2 == '=' || c2 == '+') { return j+2; }
            return j+1;

            //  -- -= -> -
        break;case '-':
            if (c2 == '=' || c2 == '-' || c2 == '>') { return j+2; }
            return j+1;

            //  ||= || |= |
            //  &&= && &= &
        break;case '|':
              case '&':
            if (c2 == c1 && c3 == '=') { return j+3; }
            if (c2 == c1 || c2 == '=') { return j+2; }
            return j+1;

            //  >>= >> >= >
        break;case '>':
            if (c2 == '>' && c3 == '=') { return j + 3; }
            if (c2 == '>' || c2 == '=') { return j + 2; }
            return j+1;

            //  <<= << <=> <= <
        break;case '<':
            if (c2 == '<' && c3 == '=') { return j + 3; }
            if (c2 == '=' && c3 == '>') { return j + 3; }
            if (c2 == '<' || c2 == '=') { return j + 2; }
            return j+1;

        break;default:
            ;
        }
    }

    return 0;
}

//---------------------------------------------------------------------------
//  starts_with_identifier_colon: returns whether the line starts with an
//  identifier followed by one colon (not ::) (possibly preceded by an access specifier)
//
//  line    current line being processed
//
auto starts_with_identifier_colon(std::string const& line)
    -> bool
{
    auto i = 0;

    //  find first non-whitespace character
    if (!move_next(line, i, isspace)) {
        return false;
    }

    //  see if it's an access-specifier
    auto s = std::string_view( &line[i], std::ssize(line) - i );
    auto j = 0;
    assert (!isspace(s[j]));
    if (s.starts_with("public")) {
        j += 6;
    }
    else if (s.starts_with("protected")) {
        j += 9;
    }
    else if (s.starts_with("private")) {
        j += 7;
    }
    while (
        j < std::ssize(s)
        && isspace(s[j])
        )
    {
        ++j;
    }
    s.remove_prefix(j);
    i += j;

    //  see if it's an "operator @" name
    j = starts_with_operator(s);
    //  else see if it's a single identifier
    if (j == 0) {
        j = starts_with_identifier(s);
    }
    //  if it's neither, bail
    if (j == 0) {
        return false;
    }
    i += j;

    if (!move_next(line, i, isalnum)) {
        return false;
    }

    //  find first non-whitespace character
    if (!move_next(line, i, isspace)) {
        return false;
    }

    //  it's Cpp2 iff what's here is : not followed by another :
    //  (e.g., not a Cpp1 "using ::something")
    assert (i < ssize(line));
    return
        line[i] == ':'
        && (i == ssize(line)-1 || line[i+1] != ':')
        ;
}

//---------------------------------------------------------------------------
//  starts_with_preprocessor_if_else_endif: the line starts with a preprocessor conditional
//
//  line    current line being processed
//
auto starts_with_preprocessor_if_else_endif(
    std::string const& line
)
    -> preprocessor_conditional
{
    auto i = 0;

    //  find first non-whitespace character
    if (!move_next(line, i, isspace)) {
        return preprocessor_conditional::none;
    }

    //  if it's not #, this isn't an #if/#else/#endif
    if (line[i] != '#') {
        return preprocessor_conditional::none;
    }

    //  find next non-whitespace character
    ++i;
    if (!move_next(line, i, isspace)) {
        return preprocessor_conditional::none;
    }

    if (line.substr(i).starts_with("if")) {
        return preprocessor_conditional::pre_if;
    }
    else if (line.substr(i).starts_with("else")) {
        return preprocessor_conditional::pre_else;
    }
    else if (line.substr(i).starts_with("endif")) {
        return preprocessor_conditional::pre_endif;
    }
    else {
        return preprocessor_conditional::none;
    }
}


//---------------------------------------------------------------------------
//  process_cpp_line: just enough to know what to skip over
//
//  line                current line being processed
//  in_comment          track whether we're in a comment
//  in_string_literal   track whether we're in a string literal
//
auto process_cpp_line(
    std::string const&  line,
    bool&               in_comment,
    bool&               in_string_literal,
    bool&               in_raw_string_literal,
    std::string&        raw_string_closing_seq,
    braces_tracker&     braces,
    lineno_t            lineno
)
    -> process_line_ret
{
    if (
        !in_comment
        && !in_string_literal
        && !in_raw_string_literal
        )
    {
        if (starts_with_whitespace_slash_slash(line)) {
            return { true, false, false };
        }
        else if (starts_with_whitespace_slash_star_and_no_star_slash(line)) {
            in_comment = true;
            return { true, false, false };
        }
    }

    struct process_line_ret r { in_comment, true , in_raw_string_literal};

    auto prev = ' ';
    for (auto i = colno_t{0}; i < ssize(line); ++i)
    {
        //  Local helper functions for readability
        //  Note: in_literal is for { and } and so doesn't have to work for escaped ' characters
        //
        auto peek       = [&](int num) {  return (i+num < std::ssize(line)) ? line[i+num] : '\0';  };
        auto in_literal = [&]          {  return in_string_literal || in_raw_string_literal || (prev == '\'' && peek(1) == '\'');  };

        //  Process this source character
        //
        if (!isspace(line[i])) {
            r.empty_line = false;
        }

        if (
            in_comment
            && !in_string_literal
            && !in_raw_string_literal
            )
        {
            switch (line[i]) {
                break;case '/': if (prev == '*') { in_comment = false; }
                break;default: ;
            }
        }
        else if (in_raw_string_literal) {
            auto end_pos = line.find(raw_string_closing_seq, i);
            if (end_pos == std::string::npos) {
                return r;
            }
            in_raw_string_literal = false;
            i = end_pos+raw_string_closing_seq.size()-1;
        }
        else {
            r.all_comment_line = false;
            r.all_rawstring_line = false;
            switch (line[i]) {
                break;case 'R':
                    if (
                        !in_comment
                        && !in_string_literal
                        && !in_raw_string_literal
                        && peek(1) == '"'
                        )
                    {
                        i+=2;
                        if (i < ssize(line) - 1)
                        {
                            if (auto paren_pos = line.find("(", i);
                                paren_pos != std::string::npos
                                )
                            {
                                raw_string_closing_seq = ")"+line.substr(i, paren_pos-i)+"\"";
                                in_raw_string_literal = true;
                            }
                        }
                    }

                break;case '\"':
                    //  If this isn't an escaped quote, toggle string literal state
                    if (
                        !in_comment
                        && prev != '\\'
                        && (in_string_literal || prev != '\'')
                        && !in_raw_string_literal
                        )
                    {
                        in_string_literal = !in_string_literal;
                    }

                break;case '{':
                    if (!in_literal()) {
                        braces.found_open_brace(lineno);
                    }

                break;case '}':
                    if (!in_literal()) {
                        braces.found_close_brace(source_position(lineno, i));
                    }

                break;case '*':
                    if (
                        !in_string_literal
                        && !in_raw_string_literal
                        && prev == '/'
                        )
                    {
                        in_comment = true;
                    }

                break;case '/':
                    if (
                        !in_string_literal
                        && !in_raw_string_literal
                        && prev == '/'
                        )
                    {
                        in_comment = false;
                        return r;
                    }

                break;default: ;
            }
        }

        prev = line[i];
    }

    return r;
}


//---------------------------------------------------------------------------
//  process_cpp2_line: to find the end of a Cpp2 definition
//      - find first of ; and {
//          - if ; we're done
//          - if { find matching }
//      - then there must be nothing else on the last line
//
//  line        current line being processed
//  in_comment  whether this line begins inside a multi-line comment
//
//  Returns:    whether additional lines should be inspected
//
auto process_cpp2_line(
    std::string const&        line,
    bool&                     in_comment,
    braces_tracker&           braces,
    lineno_t                  lineno,
    std::vector<error_entry>& errors
)
    -> bool
{
    auto found_end = false;

    auto prev = ' ';
    auto in_string_literal = false;

    for (auto i = colno_t{0}; i < ssize(line); ++i) {

        if (in_comment) {
            switch (line[i]) {
            break;case '/': if (prev == '*') { in_comment = false; }
            break;default: ;
            }
        } else if (in_string_literal) {
            switch (line[i]) {
            break;case '"': if (prev != '\\') { in_string_literal = false; }
            break;default: ;
            }
        }

        else {
            switch (line[i]) {
            break;case '{':
                braces.found_open_brace(lineno);

            break;case '}':
                braces.found_close_brace( source_position(lineno, i) );
                if (braces.current_depth() < 1) {
                    found_end = true;
                }

            break;case ';':
                if (braces.current_depth() < 1) { found_end = true; }

            break;case '*':
                if (prev == '/') {
                    in_comment = true;
                    if (found_end) {
                        errors.emplace_back(
                            source_position(lineno, i),
                            std::string("alpha limitation:"
                                " after the closing ; or } of a definition, the rest"
                                " of the line cannot begin a /*...*/ comment")
                        );
                    }
                }

            break;case '/':
                if (prev == '/') { in_comment = false; return found_end; }

            break;case '"':
                if (prev != '\\') { in_string_literal = true; }

            break;default: ;
            }
        }

        prev = line[i];
    }

    return found_end;
}

}
