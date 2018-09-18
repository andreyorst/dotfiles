# Custom faces
set-face global child rgb:fb4934,default+b

hook global WinSetOption filetype=(c|cpp|rust) %{
    add-highlighter buffer/ regex \w+(\h+)?(?=\() 0:function
    add-highlighter buffer/ regex (?<=\.)\w+(?!\()(?!>)(?!") 0:child
    add-highlighter buffer/ regex (?<=->)\w+(?!\()(?!>)(?!") 0:child
}

hook global WinSetOption filetype=(c|cpp) %{
    add-highlighter buffer/ regex \b(v|u|vu)\w+(8|16|32|64)(_t)?\b 0:type
    add-highlighter buffer/ regex \b(v|u|vu)?(_|__)?(s|u)(8|16|32|64)(_t)?\b 0:type
    add-highlighter buffer/ regex \b(v|u|vu)?(_|__)?(int|short|char|long)(_t)?\b 0:type
    add-highlighter buffer/ regex \b\w+_t\b 0:type
}

hook global WinSetOption filetype=(rust) %{
    hook global InsertChar \t %{
        exec -draft h@
    }
    hook global InsertDelete ' ' %{ try %{
      execute-keys -draft 'h<a-h><a-k>\A\h+\z<ret>i<space><esc><lt>'
    }}
}
