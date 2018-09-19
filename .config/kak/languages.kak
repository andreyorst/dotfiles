# Custom faces
    set-face global child rgb:fb4934,default+b

# C/Cpp/Rust syntax fixes
    hook global WinSetOption filetype=(c|cpp|rust) %{
        add-highlighter buffer/ regex \w+(\h+)?(?=\() 0:function
        add-highlighter buffer/ regex ((?<=\.)|(?<=->))\w+\b(?![>"\(]) 0:child
        add-highlighter buffer/ regex ((?<=\.)|(?<=->))\w+(\h+)?(?=\() 0:function
    }

# Expandtab hooks
    hook global WinSetOption filetype=(rust|kak) %{
        hook -group expandtab global InsertChar \t %{
            exec -draft h@
        }
        hook -group expandtab global InsertDelete ' ' %{ try %{
          execute-keys -draft 'h<a-h><a-k>\A\h+\z<ret>i<space><esc><lt>'
        }}
    }
    hook global WinSetOption filetype=makefile %{
        remove-hooks global expandtab
    }

# C/Cpp
    hook global WinSetOption filetype=(c|cpp) %{
        # Custom C/Cpp types highlighing
        add-highlighter buffer/ regex \b(v|u|vu)\w+(8|16|32|64)(_t)?\b 0:type
        add-highlighter buffer/ regex \b(v|u|vu)?(_|__)?(s|u)(8|16|32|64)(_t)?\b 0:type
        add-highlighter buffer/ regex \b(v|u|vu)?(_|__)?(int|short|char|long)(_t)?\b 0:type
        add-highlighter buffer/ regex \b\w+_t\b 0:type
        clang-enable-autocomplete
    }

# Rust
    hook global WinSetOption filetype=rust %{
        set window formatcmd 'rustfmt'
    }
