set-option global snippets \
'snippet' 'snip' %{
    snippets-insert '''$1'' ''$2'' %{
    ${indent}snippets-insert ''$0''
    }'
}

hook global WinSetOption filetype=(c|cpp) %{
    unset-option window snippets

    set-option -add window snippets \
    'for (i) {}' 'for' %{
        snippets-insert 'for (int $1 = 0; $1 < $2; $1++) {
                         ${indent}$0
                         }'
    } \
    'if {}' 'if' %{
        snippets-insert 'if ($1) {
                         ${indent}$0
                         }'
    } \
    'if {} else {}' 'ifel' %{
        snippets-insert 'if ($1) {
                         ${indent}$2
                         } else {
                         ${indent}$0
                         }'
    } \
    'else if {}' 'elif' %{
        snippets-insert 'else if ($1) {
                         ${indent}$0
                         }'
    } \
    'tos ton tos' 'ttt' %{
        snippets-insert 'tos("$1"); ton($2); tos(${3:endl});$0'
    } \
    'Terminal out string' 'ton' %{
        snippets-insert 'tos("${0:text}");'
    } \
    'Terminal out byte' 'tob' %{
        snippets-insert 'tob(''$0'');'
    } \
    'Terminal out decimal' 'ton10' %{
        snippets-insert 'ton10($0);'
    } \
    'Terminal out hexadecimal' 'ton' %{
        snippets-insert 'ton($0);'
    } \
    'Terminal out formatted' 'tonf' %{
        snippets-insert 'tonf($1, $0);'
    } \
    'Main function' 'main' %{
        snippets-insert 'int main()
                         {
                         ${indent}$0
                         }'
    } \
    'Case block' 'case' %{
        snippets-insert 'case $1:
                         ${indent}$0
                         ${indent}break;'
    } \
    'Switch block' 'switch' %{
        snippets-insert 'switch ($1) {
                         ${indent}case $2:
                         ${indent}${indent}$3
                         ${indent}${indent}break;
                         ${indent}default:
                         ${indent}${indent}$0
                         ${indent}${indent}break;
                         }'
    } \
    'Enum' 'enum' %{
        snippets-insert 'enum ${1:name} { ${2:constant} };'
    } \
    'Typedef enum' 'tenum' %{
        snippets-insert 'typedef enum ${1:name} { ${2:constant} } $1;'
    } \
    '#ifdef __cplusplus' 'nocxx' %{
        snippets-insert '#ifdef __cplusplus
                         extern "C" {
                         #endif

                         $0

                         #ifdef __cplusplus
                         } /* extern "C" */
                         #endif'
     } \
     'include' 'inc' %{
         snippets-insert '#include <${0:stdio}.h>'
     } \
     'ifndef directive' 'ifndef' %{
         snippets-insert '#ifndef $1
                          #define NAME $0
                          #endif /* ifndef $1 */'
     } \
     'define' 'def' %{
         snippets-insert '#define $1 $0'
     }
}

hook global WinSetOption filetype=(rust) %{
    unset-option window snippets

    set-option -add window snippets 'Main function' 'main' %{
    snippets-insert 'fn main() {
                     ${indent}$0
                     }'
    }
}
