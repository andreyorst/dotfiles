hook global WinSetOption filetype=(c|cpp) %{
    %sh{kak-lsp --kakoune -s $kak_session}
}
