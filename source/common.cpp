#include "common.h"

#ifdef _MSC_VER
#pragma warning(disable: 4456)
#endif

namespace cpp2 {

//-----------------------------------------------------------------------
//
//  Digit classification, with '\'' digit separators
//
//-----------------------------------------------------------------------
//

//G binary-digit:
//G     one of '0' '1'
//G
auto is_binary_digit(char c)
    -> bool
{
    return
        c == '0'
        || c == '1'
        ;
}

//G digit: one of
//G     binary-digit
//G     one of '2' '3' '4' '5' '6' '7' '8' '9'
//G
auto is_digit(char c)
    -> bool
{
    return isdigit(c);
}

//G hexadecimal-digit:
//G     digit
//G     one of 'A' 'B' 'C' 'D' 'E' 'F'
//G
auto is_hexadecimal_digit(char c)
    -> bool
{
    return isxdigit(c);
}

//G nondigit:
//G     one of 'a'..'z'
//G     one of 'A'..'Z'
//G     _
//G
auto is_nondigit(char c)
    -> bool
{
    return
        isalpha(c)
        || c == '_'
        ;
};

//G identifier-start:
//G     nondigit
//G
auto is_identifier_start(char c)
    -> bool
{
    return is_nondigit(c);
}

//G identifier-continue:
//G     digit
//G     nondigit
//G
auto is_identifier_continue(char c)
    -> bool
{
    return
        is_digit(c)
        || is_nondigit(c)
        ;
}

//G identifier:
//G     '__identifier__' keyword    [Note: without whitespace before the keyword]
//G     identifier-start
//G     identifier identifier-continue
//G     'operator' operator
//G
auto starts_with_identifier(std::string_view s)
    -> int
{
    if (is_identifier_start(s[0])) {
        auto j = 1;
        while (
            j < std::ssize(s)
            && is_identifier_continue(s[j])
            )
        {
            ++j;
        }
        return j;
    }
    return 0;
};

//  String path prefix from filename
//
auto strip_path(std::string const& file)
    -> std::string
{
    auto i = std::ssize(file)-1;
    while (
        i >= 0
        && file[i] != '\\'
        && file[i] != '/'
        )
    {
        --i;
    }
    return {file, __as<size_t>(i+1)};
}


//-----------------------------------------------------------------------
//
//  Misc helpers
//
//-----------------------------------------------------------------------
//
auto replace_all(std::string& s, std::string_view what, std::string_view with)
    -> std::string
{
    for (
        std::string::size_type pos{};
        s.npos != (pos = s.find(what.data(), pos, what.length()));
        pos += with.length()
        )
    {
        s.replace(pos, what.length(), with.data(), with.length());
    }
    return s;
}


auto to_upper(char c)
    -> char
{
    //  C toupper is only not-UB in [0,127] and returns the wrong type,
    //  so wrap the range check and the type cast here in one place...
    //  note the 126 (not 127) is intentional to avoid a GCC warning
    if (0 <= c && c <= 126) { return static_cast<char>(std::toupper(c)); }
    //  else
    return c;
}


auto to_upper_and_underbar(std::string_view s)
    -> std::string
{
    auto ret = std::string{s};
    for (char& c : ret) {
        if (std::isalnum(c)) { c = to_upper(c); }
        else                 { c = '_'; }
    }
    return ret;
}


auto is_empty_or_a_decimal_number(std::string_view s)
    -> bool
{
    auto size = std::ssize(s);
    if (size == 0) { return true; }

    auto i = 0;
    while (i < size && isspace(s[i]) ) { ++i; }
    while (i < size && isdigit(s[i]) ) { ++i; }
    while (i < size && isspace(s[i]) ) { ++i; }
    return i == size;
}


auto starts_with(
    std::string const& s,
    std::string_view   sv
)
    -> bool
{
    return std::string_view(s).starts_with(sv);
}

cmdline_processor cmdline;

cmdline_processor::register_flag::register_flag(
    int              group,
    std::string_view name,
    std::string_view description,
    callback0        handler0,
    callback1        handler1,
    std::string_view synonym,
    bool             opt_out
)
{
    cmdline.add_flag( group, name, description, handler0, handler1, synonym, opt_out );
}

static cmdline_processor::register_flag cmd_help   (
    0,
    "help",
    "Print help",
    []{ cmdline.print_help(); },
    nullptr,
    "?"
);

static cmdline_processor::register_flag cmd_version(
    0,
    "version",
    "Print version information",
    []{ cmdline.print_version(); }
);

static cmdline_processor::register_flag cmd_gen_version(
    0,
    "_gen_version",
    "Generate version information",
    []{ cmdline.gen_version(); }
);
}
