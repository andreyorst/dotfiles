hook global WinSetOption filetype=(c|cpp) %{
    %sh{kak-lsp --kakoune -s $kak_session}
}

hook global WinCreate .* %{
	auto-pairs-enable
}

map global normal <a-s> :auto-pairs-surround<ret>
