hook global WinSetOption filetype=(c|cpp) %{
    set-option window snippets
    set-option window snippets_triggers

    set-option -add window snippets "For" %{
        snippets-insert "for (int ${1:i}; $1 < $2; $1++) {
                         ${indent}$0
                         }"
    }
    set-option -add window snippets_triggers "for" "For"

    set-option -add window snippets "Main" %{
    snippets-insert "int main() {
                     ${indent}$0
                     }"
    }
    set-option -add window snippets_triggers "main" "Main"
}

hook global WinSetOption filetype=(rust) %{
    set-option window snippets
    set-option window snippets_triggers

    set-option -add window snippets "Main" %{
    snippets-insert "fn main() {
                     ${indent}$0
                     }"
    }
    set-option -add window snippets_triggers "main" "Main"
}
