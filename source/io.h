
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
//  Source loader
//===========================================================================

#ifndef __CPP2_LOAD
#define __CPP2_LOAD

#include "common.h"
#include <fstream>
#include <ostream>
#include <iterator>
#include <cctype>


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
    -> bool;

//---------------------------------------------------------------------------
//  peek_first_non_whitespace: returns the first non-whitespace char in line
//
//  line    current line being processed
//
auto peek_first_non_whitespace(std::string const& line)
    -> char;

//---------------------------------------------------------------------------
//  is_preprocessor: returns whether this is a preprocessor line starting
//  with #, and whether it will be followed by another preprocessor line
//
//  line        current line being processed
//  first_line  whether this is supposed to be the first line (start with #)
//
struct is_preprocessor_ret {
    bool is_preprocessor;
    bool has_continuation;
};
auto is_preprocessor(
    std::string const& line,
    bool               first_line
)
    -> is_preprocessor_ret;

//---------------------------------------------------------------------------
//  starts_with_import: returns whether the line starts with "import"
//
//  line    current line being processed
//
auto starts_with_import(std::string const& line)
    -> bool;

//---------------------------------------------------------------------------
//  starts_with_whitespace_slash_slash: is this a "// comment" line
//
//  line    current line being processed
//
auto starts_with_whitespace_slash_slash(std::string const& line)
    -> bool;

//---------------------------------------------------------------------------
//  starts_with_whitespace_slash_star_and_no_star_slash: is this a "/* comment" line
//
//  line    current line being processed
//
auto starts_with_whitespace_slash_star_and_no_star_slash(std::string const& line)
    -> bool;

//---------------------------------------------------------------------------
//  starts_with_operator: returns whether the line starts with the string "operator"
//  followed by the symbols of an operator
//
//  line    current line being processed
//
auto starts_with_operator(std::string_view s)
    -> int;

//---------------------------------------------------------------------------
//  starts_with_identifier_colon: returns whether the line starts with an
//  identifier followed by one colon (not ::) (possibly preceded by an access specifier)
//
//  line    current line being processed
//
auto starts_with_identifier_colon(std::string const& line)
    -> bool;

//---------------------------------------------------------------------------
//  braces_tracker: to track brace depth
//
//  Normally we don't emit diagnostics for Cpp1 code, but we do for a
//  brace mismatch since we're relying on balanced { } to find Cpp2 code
//
class braces_tracker
{
    //  to track preprocessor #if brace depth and brace counts
    //
    class pre_if_depth_info
    {
        int  if_net_braces   = 0;
        bool found_else      = false;
        int  else_net_braces = 0;

    public:
        auto found_open_brace() -> void {
            if (!found_else) { ++if_net_braces;   }
            else             { ++else_net_braces; }
        }

        auto found_close_brace() -> void {
            if (!found_else) { --if_net_braces;   }
            else             { --else_net_braces; }
        }

        auto found_preprocessor_else() -> void {
            assert (!found_else);
            found_else = true;
        }

        //  If the "if" and "else" branches opened/closed the same net number
        //  of unbalanced braces, they were double-counted in the brace
        //  matching and to try to keep going we can apply this adjustment
        auto braces_to_ignore() -> int {
            if (
                if_net_braces >= 0
                && if_net_braces == else_net_braces
                )
            {
                return if_net_braces;
            }
            else {
                return 0;
            }
        }
    };
    std::vector<pre_if_depth_info> preprocessor = { {} };  // sentinel
    std::vector<lineno_t>          open_braces;
    std::vector<error_entry>&      errors;

public:
    braces_tracker( std::vector<error_entry>& errors )
        : errors{errors}
    { }

    //  --- Brace matching functions - { and }

    auto found_open_brace(lineno_t lineno) -> void {
        assert(std::ssize(preprocessor) > 0);
        open_braces.push_back(lineno);
        preprocessor.back().found_open_brace();
    }

    auto found_close_brace(source_position pos) -> void {
        assert(std::ssize(preprocessor) > 0);

        if (std::ssize(open_braces) < 1) {
            errors.emplace_back(
                pos,
                "closing } does not match a prior {"
            );
        }
        else {
            open_braces.pop_back();
        }

        preprocessor.back().found_close_brace();
    }

    auto found_eof(source_position pos) const -> void {
        //  Emit diagnostic if braces didn't match
        //
        if (current_depth() != 0) {
            std::string unmatched_brace_lines;
            for (auto i = 0; i < std::ssize(open_braces); ++i) {
                if (i > 0 && std::size(open_braces)>2)       { unmatched_brace_lines += ","; };
                if (i > 0 && i == std::ssize(open_braces)-1) { unmatched_brace_lines += " and"; };
                unmatched_brace_lines += " " + std::to_string(open_braces[i]);
            }
            errors.emplace_back(
                pos,
                std::string("end of file reached with ")
                + std::to_string(current_depth())
                + " missing } to match earlier { on line"
                + (current_depth() > 1 ? "s" : "")
                + unmatched_brace_lines
            );
        }
    }

    auto current_depth() const -> int {
        return std::ssize(open_braces);
    }

    //  --- Preprocessor matching functions - #if/#else/#endif

    //  Entering an #if
    auto found_pre_if() -> void {
        assert(std::ssize(preprocessor) > 0);
        preprocessor.push_back({});
    }

    //  Encountered an #else
    auto found_pre_else() -> void {
        assert(std::ssize(preprocessor) > 1);
        preprocessor.back().found_preprocessor_else();
    }

    //  Exiting an #endif
    auto found_pre_endif() -> void {
        assert(std::ssize(preprocessor) > 1);

        //  If the #if/#else/#endif introduced the same net number of braces,
        //  then we will have recorded that number too many open braces, and
        //  braces_to_ignore() will be the positive number of those net open braces
        //  that this loop will now throw away
        for (auto i = 0; i < preprocessor.back().braces_to_ignore(); ++i) {
            found_close_brace( source_position{} );
        }

        preprocessor.pop_back();
    }
};


//---------------------------------------------------------------------------
//  starts_with_preprocessor_if_else_endif: the line starts with a preprocessor conditional
//
//  line    current line being processed
//
enum class preprocessor_conditional {
    none = 0, pre_if, pre_else, pre_endif
};
auto starts_with_preprocessor_if_else_endif(
    std::string const& line
)
    -> preprocessor_conditional;

//---------------------------------------------------------------------------
//  process_cpp_line: just enough to know what to skip over
//
//  line                current line being processed
//  in_comment          track whether we're in a comment
//  in_string_literal   track whether we're in a string literal
//
struct process_line_ret {
    bool all_comment_line;
    bool empty_line;
    bool all_rawstring_line;
};
auto process_cpp_line(
    std::string const&  line,
    bool&               in_comment,
    bool&               in_string_literal,
    bool&               in_raw_string_literal,
    std::string&        raw_string_closing_seq,
    braces_tracker&     braces,
    lineno_t            lineno
)
    -> process_line_ret;

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
    -> bool;

//-----------------------------------------------------------------------
//
//  source: Represents a program source file
//
//-----------------------------------------------------------------------
//
class source
{
    std::vector<error_entry>& errors;
    std::vector<source_line>  lines;
    bool                      cpp1_found = false;
    bool                      cpp2_found = false;

    static const int max_line_len = 90'000;
        //  do not reduce this - I encountered an 80,556-char
        //  line in real world code during testing
    char buf[max_line_len];

public:
    //-----------------------------------------------------------------------
    //  Constructor
    //
    //  errors      error list
    //
    source(
        std::vector<error_entry>& errors_
    )
        : errors{ errors_ }
        , lines( 1 )        // extra blank to avoid off-by-one everywhere
        , buf{0}
    {
    }


    //-----------------------------------------------------------------------
    //  has_cpp1: Returns true if this file has some Cpp1/preprocessor lines
    //            (note: import lines don't count toward Cpp1 or Cpp2)
    //
    auto has_cpp1() const -> bool {
        return cpp1_found;
    }


    //-----------------------------------------------------------------------
    //  has_cpp2: Returns true if this file has some Cpp2 lines
    //            (note: import lines don't count toward Cpp1 or Cpp2)
    //
    auto has_cpp2() const -> bool {
        return cpp2_found;
    }


    //-----------------------------------------------------------------------
    //  load: Read a line-by-line view of 'filename', preserving line breaks
    //
    //  filename                the source file to be loaded
    //  source                  program textual representation
    //
    auto load(
        std::string const&  filename
    )
        -> bool
    {
        std::ifstream in{ filename };
        if (!in.is_open()) {
            return false;
        }

        auto in_comment            = false;
        auto in_string_literal     = false;
        auto in_raw_string_literal = false;
        std::string raw_string_closing_seq;

        auto braces = braces_tracker(errors);

        auto add_preprocessor_line = [&] {
            lines.push_back({ &buf[0], source_line::category::preprocessor });
            if (auto pre = starts_with_preprocessor_if_else_endif(lines.back().text);
                pre != preprocessor_conditional::none
                )
            {
                switch (pre) {
                break;case preprocessor_conditional::pre_if:
                    braces.found_pre_if();
                break;case preprocessor_conditional::pre_else:
                    braces.found_pre_else();
                break;case preprocessor_conditional::pre_endif:
                    braces.found_pre_endif();
                break;default:
                    assert(false);
                }
            }
        };

        while (in.getline(&buf[0], max_line_len)) {

            //  Handle preprocessor source separately, they're outside the language
            //
            if (auto pre = is_preprocessor(buf, true);
                pre.is_preprocessor
                && !in_comment
                && !in_raw_string_literal
                )
            {
                cpp1_found = true;
                add_preprocessor_line();
                while (
                    pre.has_continuation
                    && in.getline(&buf[0], max_line_len)
                    )
                {
                    add_preprocessor_line();
                    pre = is_preprocessor(buf, false);
                }
            }

            else
            {
                lines.push_back({ &buf[0], source_line::category::cpp1 });

                //  Switch to cpp2 mode if we're not in a comment, not inside nested { },
                //  and the line starts with "nonwhitespace :" but not "::"
                //
                if (!in_comment
                    && !in_raw_string_literal
                    && braces.current_depth() < 1
                    && starts_with_identifier_colon(lines.back().text)
                    )
                {
                    cpp2_found= true;

                    //  Mark this line, and preceding comment/blank source, as cpp2
                    lines.back().cat = source_line::category::cpp2;
                    if (std::ssize(lines) > 1) {
                        auto prev = --std::end(lines);
                        while (
                            --prev != std::begin(lines)
                            && (prev->cat == source_line::category::empty
                                || prev->cat == source_line::category::comment)
                            )
                        {
                            prev->cat = source_line::category::cpp2;
                        }
                    }

                    //  Find the end of the definition:
                    while (
                        !process_cpp2_line(
                            lines.back().text,
                            in_comment,
                            braces,
                            std::ssize(lines)-1,
                            errors
                        )
                        && in.getline(&buf[0], max_line_len)
                        )
                    {
                        lines.push_back({ &buf[0], source_line::category::cpp2 });
                    }
                }

                //  Else still in Cpp1 code, but could be a comment, empty, or import
                //
                else
                {
                    if (starts_with_import(lines.back().text)) {
                        lines.back().cat = source_line::category::import;
                    }
                    else {
                        auto stats = process_cpp_line(
                            lines.back().text,
                            in_comment,
                            in_string_literal,
                            in_raw_string_literal,
                            raw_string_closing_seq,
                            braces,
                            std::ssize(lines) - 1
                        );
                        if (stats.all_comment_line) {
                            lines.back().cat = source_line::category::comment;
                        }
                        else if (stats.all_rawstring_line) {
                            lines.back().cat = source_line::category::rawstring;
                        }
                        else if (stats.empty_line) {
                            lines.back().cat = source_line::category::empty;
                        }
                        else {
                            cpp1_found = true;
                        }
                    }
                }

            }
        }

        //  Because I encountered very long lines in real-world code during testing
        //
        if (in.gcount() >= max_line_len-1)
        {
            errors.emplace_back(
                source_position(lineno_t(std::ssize(lines)), 0),
                std::string("source line too long - length must be less than ")
                    + std::to_string(max_line_len)
            );
            return false;
        }

        //  This shouldn't be possible, so check it anyway
        //
        if (!in.eof())
        {
            errors.emplace_back(
                source_position(lineno_t(std::ssize(lines)), 0),
                std::string("unexpected error reading source lines - did not reach EOF"),
                false,
                true    // a noisy fallback error
            );
            return false;
        }

        braces.found_eof( source_position(lineno_t(std::ssize(lines)), 0) );

        return true;
    }


    //-----------------------------------------------------------------------
    //  get_lines: Access the source lines
    //
    auto get_lines() -> std::vector<source_line>&
    {
        return lines;
    }

    auto get_lines() const -> std::vector<source_line> const&
    {
        return lines;
    }

    //-----------------------------------------------------------------------
    //  debug_print
    //
    auto debug_print(std::ostream& o) const -> void
    {
        for (auto lineno = 0; auto const& line : lines) {
            //  Skip dummy first entry
            if (lineno > 0) {
                if (line.all_tokens_are_densely_spaced) {
                    o << "+";
                }
                else {
                    o << " ";
                }
                o << line.prefix() << line.text << '\n';
            }
            ++lineno;
        }
    }

    //  No copying
    //
    source(source const&)            = delete;
    source& operator=(source const&) = delete;
    source(source&&)                 = delete;
    source& operator=(source&&)      = delete;
};

}

#endif
