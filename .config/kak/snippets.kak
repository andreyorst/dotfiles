hook global WinSetOption filetype=(c|cpp) %{
    unset-option window snippets

    set-option -add window snippets 'For loop' 'for' %{
        snippets-insert 'for (int ${1:i}; $1 < $2; $1++) {
                         ${indent}$0
                         }'
    }

    set-option -add window snippets 'Main function' 'main' %{
    snippets-insert %{int main() {
                     ${indent}$0
                     }}
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
