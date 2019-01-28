hook global WinSetOption filetype=(c|cpp) %{
    unset-option window snippets

    set-option -add window snippets \
    'for (i) {}' 'for' %{
        snippets-insert 'for (int ${1:i}; $1 < $2; $1++) {
                         ${indent}$0
                         }'
    } \
    'if {}' 'if' %{
        snippets-insert 'if ($1) {
                         ${indent}$0
                         }'
    } \
    'if {} else {}' 'elif' %{
        snippets-insert 'if ($1) {
                         ${indent}$2
                         } else {
                         ${indent}$0
                         }'
    } \
    'tos ton tos' 'ttt' %{
        snippets-insert 'tos("$1"); ton($2); tos("${3:\n\r}");$0'
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
