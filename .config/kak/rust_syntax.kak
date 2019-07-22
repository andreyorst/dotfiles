# ╭─────────────╥──────────────────╮
# │ Author:     ║ File:            │
# │ Andrey Orst ║ kakrc            │
# ╞═════════════╩══════════════════╡
# │ Rust highlighting overrides.   │
# │ Based om builtin script and    │
# │ http://rust_syntax-lang.org    │
# ╞════════════════════════════════╡
# │ Rest of .dotfiles:             │
# │ GitHub.com/andreyorst/dotfiles │
# ╰────────────────────────────────╯

hook -group rust-syntax-highlight global WinSetOption filetype=rust %{
    require-module rust_syntax
    add-highlighter window/rust_syntax ref rust_syntax
}

provide-module rust_syntax %§

add-highlighter shared/rust_syntax regions
add-highlighter shared/rust_syntax/code default-region group
add-highlighter shared/rust_syntax/string           region %{(?<!')"} (?<!\\)(\\\\)*"              fill string
add-highlighter shared/rust_syntax/raw_string       region -match-capture %{(?<!')r(#*)"} %{"(#*)} fill string
add-highlighter shared/rust_syntax/comment          region -recurse "/\*" "/\*" "\*/"              fill comment
add-highlighter shared/rust_syntax/line_comment     region "//" "$"                                fill comment
add-highlighter shared/rust_syntax/macro_attributes region "#!?\[" "\]"                            fill meta

add-highlighter shared/rust_syntax/code/byte_literal         regex "'\\\\?.'" 0:value
add-highlighter shared/rust_syntax/code/long_quoted          regex "('\w+)[^']" 1:meta
add-highlighter shared/rust_syntax/code/field_or_parameter   regex (_?\w+)(?::)(?!:) 1:variable
add-highlighter shared/rust_syntax/code/namespace            regex [a-zA-Z](\w+)?(\h+)?(?=::) 0:module
# the number literals syntax is defined here:
# https://doc.rust-lang.org/reference.html#number-literals
add-highlighter shared/rust_syntax/code/values regex \b(?:self|true|false|[0-9][_0-9]*(?:\.[0-9][_0-9]*|(?:\.[0-9][_0-9]*)?E[\+\-][_0-9]+)(?:f(?:32|64))?|(?:0x[_0-9a-fA-F]+|0o[_0-7]+|0b[_01]+|[0-9][_0-9]*)(?:(?:i|u|f)(?:8|16|32|64|128|size))?)\b 0:value
add-highlighter shared/rust_syntax/code/attributes regex \b(?:trait|struct|enum|type|mut|ref|static|const)\b 0:attribute
# the language keywords are defined here, but many of them are reserved and unused yet:
# https://doc.rust-lang.org/grammar.html#keywords
add-highlighter shared/rust_syntax/code/keywords             regex \b(?:let|as|fn|return|match|if|else|loop|for|in|while|break|continue|move|box|where|impl|dyn|pub|unsafe|async|await|mod|crate|use|extern)\b 0:keyword
add-highlighter shared/rust_syntax/code/builtin_types        regex \b(?:u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize|f32|f64|bool|char|str|Self)\b 0:type
add-highlighter shared/rust_syntax/code/user_defined_type    regex \b[A-Z]\w*\b 0:type
add-highlighter shared/rust_syntax/code/function_declaration regex (?:fn\h+)(_?\w+)(?:<[^>]+?>)?\( 1:function
add-highlighter shared/rust_syntax/code/variable_declaration regex (?:let\h+(?:mut\h+)?)(_?\w+) 1:variable
add-highlighter shared/rust_syntax/code/macro                regex \b[A-z0-9_]+! 0:meta

§
