
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
//  Lexer
//===========================================================================

#ifndef __CPP2_LEX
#define __CPP2_LEX

#include "io.h"
#include <map>
#include <climits>
#include <deque>
#include <cstring>


namespace cpp2 {

//-----------------------------------------------------------------------
//
//  lexeme: represents the type of a token
//
//-----------------------------------------------------------------------
//

enum class lexeme : std::int8_t {
    SlashEq,
    Slash,
    LeftShiftEq,
    LeftShift,
    Spaceship,
    LessEq,
    Less,
    RightShiftEq,
    RightShift,
    GreaterEq,
    Greater,
    PlusPlus,
    PlusEq,
    Plus,
    MinusMinus,
    MinusEq,
    Arrow,
    Minus,
    LogicalOrEq,
    LogicalOr,
    PipeEq,
    Pipe,
    LogicalAndEq,
    LogicalAnd,
    MultiplyEq,
    Multiply,
    ModuloEq,
    Modulo,
    AmpersandEq,
    Ampersand,
    CaretEq,
    Caret,
    TildeEq,
    Tilde,
    EqualComparison,
    Assignment,
    NotEqualComparison,
    Not,
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Scope,
    Colon,
    Semicolon,
    Comma,
    Dot,
    Ellipsis,
    QuestionMark,
    At,
    Dollar,
    FloatLiteral,
    BinaryLiteral,
    DecimalLiteral,
    HexadecimalLiteral,
    StringLiteral,
    CharacterLiteral,
    UserDefinedLiteralSuffix,
    Keyword,
    Cpp1MultiKeyword,
    Cpp2FixedType,
    Identifier,
    None = 127
};

auto is_literal(lexeme l) -> bool;

auto close_paren_type(lexeme l)
    -> lexeme;

template<typename T>
    requires std::is_same_v<T, std::string>
auto __as(lexeme l)
    -> std::string
{
    switch (l) {
    break;case lexeme::SlashEq:             return "SlashEq";
    break;case lexeme::Slash:               return "Slash";
    break;case lexeme::LeftShiftEq:         return "LeftShiftEq";
    break;case lexeme::LeftShift:           return "LeftShift";
    break;case lexeme::Spaceship:           return "Spaceship";
    break;case lexeme::LessEq:              return "LessEq";
    break;case lexeme::Less:                return "Less";
    break;case lexeme::RightShiftEq:        return "RightShiftEq";
    break;case lexeme::RightShift:          return "RightShift";
    break;case lexeme::GreaterEq:           return "GreaterEq";
    break;case lexeme::Greater:             return "Greater";
    break;case lexeme::PlusPlus:            return "PlusPlus";
    break;case lexeme::PlusEq:              return "PlusEq";
    break;case lexeme::Plus:                return "Plus";
    break;case lexeme::MinusMinus:          return "MinusMinus";
    break;case lexeme::MinusEq:             return "MinusEq";
    break;case lexeme::Arrow:               return "Arrow";
    break;case lexeme::Minus:               return "Minus";
    break;case lexeme::LogicalOrEq:         return "LogicalOrEq";
    break;case lexeme::LogicalOr:           return "LogicalOr";
    break;case lexeme::PipeEq:              return "PipeEq";
    break;case lexeme::Pipe:                return "Pipe";
    break;case lexeme::LogicalAndEq:        return "LogicalAndEq";
    break;case lexeme::LogicalAnd:          return "LogicalAnd";
    break;case lexeme::MultiplyEq:          return "MultiplyEq";
    break;case lexeme::Multiply:            return "Multiply";
    break;case lexeme::ModuloEq:            return "ModuloEq";
    break;case lexeme::Modulo:              return "Modulo";
    break;case lexeme::AmpersandEq:         return "AmpersandEq";
    break;case lexeme::Ampersand:           return "Ampersand";
    break;case lexeme::CaretEq:             return "CaretEq";
    break;case lexeme::Caret:               return "Caret";
    break;case lexeme::TildeEq:             return "TildeEq";
    break;case lexeme::Tilde:               return "Tilde";
    break;case lexeme::EqualComparison:     return "EqualComparison";
    break;case lexeme::Assignment:          return "Assignment";
    break;case lexeme::NotEqualComparison:  return "NotEqualComparison";
    break;case lexeme::Not:                 return "Not";
    break;case lexeme::LeftBrace:           return "LeftBrace";
    break;case lexeme::RightBrace:          return "RightBrace";
    break;case lexeme::LeftParen:           return "LeftParen";
    break;case lexeme::RightParen:          return "RightParen";
    break;case lexeme::LeftBracket:         return "LeftBracket";
    break;case lexeme::RightBracket:        return "RightBracket";
    break;case lexeme::Scope:               return "Scope";
    break;case lexeme::Colon:               return "Colon";
    break;case lexeme::Semicolon:           return "Semicolon";
    break;case lexeme::Comma:               return "Comma";
    break;case lexeme::Dot:                 return "Dot";
    break;case lexeme::Ellipsis:            return "Ellipsis";
    break;case lexeme::QuestionMark:        return "QuestionMark";
    break;case lexeme::At:                  return "At";
    break;case lexeme::Dollar:              return "Dollar";
    break;case lexeme::FloatLiteral:        return "FloatLiteral";
    break;case lexeme::BinaryLiteral:       return "BinaryLiteral";
    break;case lexeme::DecimalLiteral:      return "DecimalLiteral";
    break;case lexeme::HexadecimalLiteral:  return "HexadecimalLiteral";
    break;case lexeme::StringLiteral:       return "StringLiteral";
    break;case lexeme::CharacterLiteral:    return "CharacterLiteral";
    break;case lexeme::UserDefinedLiteralSuffix:    return "UserDefinedLiteralSuffix";
    break;case lexeme::Keyword:             return "Keyword";
    break;case lexeme::Cpp1MultiKeyword:    return "Cpp1MultiKeyword";
    break;case lexeme::Cpp2FixedType:       return "Cpp2FixedType";
    break;case lexeme::Identifier:          return "Identifier";
    break;case lexeme::None:                return "(NONE)";
    break;default:                          return "INTERNAL-ERROR";
    }
};


auto is_operator(lexeme l)
    -> bool;


//-----------------------------------------------------------------------
//
//  token: represents a single token
//
//     Note: by reference, thge test into the program's source lines
//
//-----------------------------------------------------------------------
//
class token
{
public:
    token(
        char const*     start,
        auto            count,
        source_position pos,
        lexeme          type
    )
      : sv      {start, unsafe_narrow<ulong>(count)}
      , pos     {pos}
      , lex_type{type}
    {
    }

    token(
        char const*     sz,
        source_position pos,
        lexeme          type
    )
      : sv      {sz}
      , pos     {pos}
      , lex_type{type}
    {
    }

    auto as_string_view() const
        -> std::string_view
    {
        assert (sv.data());
        return sv;
    }

    operator std::string_view() const
    {
        return as_string_view();
    }

    auto operator== (token const& t) const
        -> bool
    {
        return operator std::string_view() == t.operator std::string_view();
    }

    auto operator== (std::string_view s) const
        -> bool
    {
        return s == this->operator std::string_view();
    }

    auto to_string() const
        -> std::string
    {
        return std::string{sv};
    }

    friend auto operator<< (auto& o, token const& t)
        -> auto&
    {
        return o << std::string_view(t);
    }

    auto position_col_shift( colno_t offset )
        -> void
    {
        assert (pos.colno + offset > 0);
        pos.colno += offset;
    }

    auto position() const -> source_position { return pos;       }

    auto length  () const -> int             { return sv.size(); }

    auto type    () const -> lexeme          { return lex_type;  }

    auto set_type(lexeme l) -> void          { lex_type = l;     }

    auto visit(auto& v, int depth) const
        -> void
    {
        v.start(*this, depth);
    }

    auto remove_prefix_if(std::string_view prefix) {
        if (
            sv.size() > prefix.size()
            && sv.starts_with(prefix)
            )
        {
            sv.remove_prefix(prefix.size());
        }
    }

private:
    std::string_view sv;
    source_position  pos;
    lexeme           lex_type;
};

static_assert (CHAR_BIT == 8);


auto labelized_position(token const* t)
    -> std::string;


//-----------------------------------------------------------------------
//
//  A StringLiteral could include captures
//
auto expand_string_literal(
    std::string_view          text,
    std::vector<error_entry>& errors,
    source_position           src_pos
)
    -> std::string;

auto expand_raw_string_literal(
    const std::string&           opening_seq,
    const std::string&           closing_seq,
    string_parts::adds_sequences closing_strategy,
    std::string_view             text,
    std::vector<error_entry>&    errors,
    source_position src_pos
)
    -> string_parts;

//-----------------------------------------------------------------------
//  lex: Tokenize a single line while maintaining inter-line state
//
//  mutable_line            the line to be tokenized
//  lineno                  the current line number
//  in_comment              are we currently in a comment
//  current_comment         the current partial comment
//  current_comment_start   the current comment's start position
//  tokens                  the token list to add to
//  comments                the comment token list to add to
//  errors                  the error message list to use for reporting problems
//  raw_string_multiline    the current optional raw_string state
//

extern std::deque<std::string> generated_text;
extern std::deque<std::vector<source_line>> generated_lines;

auto lex_line(
    std::string&               mutable_line,
    int const                  lineno,
    bool&                      in_comment,
    std::string&               current_comment,
    source_position&           current_comment_start,
    std::vector<token>&        tokens,
    std::vector<comment>&      comments,
    std::vector<error_entry>&  errors,
    std::optional<raw_string>& raw_string_multiline
)
    -> bool;

//-----------------------------------------------------------------------
//
//  tokens: a map of the tokens of a source file
//
//-----------------------------------------------------------------------
//

class tokens
{
    std::vector<error_entry>& errors;

    //  All non-comment source tokens go here, which will be parsed in the parser
    std::map<lineno_t, std::vector<token>> grammar_map;

    //  All comment source tokens go here, which are applied in the lexer
    //
    //  We could put all the tokens in the same map, but that would mean the
    //  parsing logic would have to remember to skip comments everywhere...
    //  simpler to keep comments separate, at the smaller cost of traversing
    //  a second token stream when lowering to Cpp1 to re-interleave comments
    std::vector<comment> comments;

    //  A stable place to store additional tokens that are synthesized later
    std::deque<token> generated_tokens;

public:
    //-----------------------------------------------------------------------
    //  Constructor
    //
    //  errors      error list
    //
    tokens(
        std::vector<error_entry>& errors_
    )
        : errors{ errors_ }
    {
    }


    //-----------------------------------------------------------------------
    //  lex: Tokenize the Cpp2 lines
    //
    //  lines           tagged source lines
    //  is_generated    is this generated code
    //
    auto lex(
        std::vector<source_line>& lines,
        bool                      is_generated = false
    )
        -> void
    {
        auto in_comment           = false;
        auto raw_string_multiline = std::optional<raw_string>();

        assert (std::ssize(lines) > 0);
        auto line = std::begin(lines);
        while (line != std::end(lines)) {

            //  Skip over non-Cpp2 lines
            if (line->cat != source_line::category::cpp2) {
                ++line;
                continue;
            }

            //  At this point, we're at the first line of a Cpp2 code section

            //  Create new map entry for the section starting at this line,
            //  and populate its tokens with the tokens in this section
            auto lineno = std::distance(std::begin(lines), line);

            //  If this is generated code, use negative line numbers to
            //  inform and assist the printer
            if (is_generated) {
                lineno -= 10'000;
            }

            auto& entry = grammar_map[lineno];
            auto current_comment = std::string{};
            auto current_comment_start = source_position{};

            for (
                ;
                line != std::end(lines) && line->cat == source_line::category::cpp2;
                ++line, ++lineno
                )
            {
                lex_line(
                    line->text, lineno,
                    in_comment, current_comment, current_comment_start,
                    entry, comments, errors,
                    raw_string_multiline
                );

                //  Check whether all the tokens on this line were consecutive
                //  w/o extra whitespace (separated by 0 or 1 whitespace chars)
                if (!entry.empty()) {
                    for (auto i = std::ssize(entry) - 1;
                        i > 0;
                        --i
                        )
                    {
                        if (entry[i-1].position().lineno != lineno) {
                            break;
                        }

                        if (
                            entry[i].position().lineno == lineno
                            && entry[i-1].position().colno + entry[i-1].length() + 1
                                < entry[i].position().colno
                            )
                        {
                            line->all_tokens_are_densely_spaced = false;
                            break;
                        }
                    }
                }
            }
        }
    }


    //-----------------------------------------------------------------------
    //  get_map: Access the token map
    //
    auto get_map() const
        -> auto const&
    {
        return grammar_map;
    }


    //-----------------------------------------------------------------------
    //  get_comments: Access the comment list
    //
    auto get_comments() const
        -> auto const&
    {
        return comments;
    }


    //-----------------------------------------------------------------------
    //  get_generated: Access the generated tokens
    //
    auto get_generated()
        -> auto&
    {
        return generated_tokens;
    }


    //-----------------------------------------------------------------------
    //  num_unprinted_comments: The number of not-yet-printed comments
    //
    auto num_unprinted_comments()
        -> int
    {
        auto ret = 0;
        for (auto const& c : comments) {
            if (!c.dbg_was_printed) {
                ++ret;
            }
        }
        return ret;
    }

    //-----------------------------------------------------------------------
    //  debug_print
    //
    auto debug_print(std::ostream& o) const
        -> void;
};

extern std::deque<tokens> generated_lexers;

}

#endif
