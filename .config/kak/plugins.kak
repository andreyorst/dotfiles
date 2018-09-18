plug andreyorst/plug.kak
plug Delapouite/kakoune-text-objects
plug occivink/kakoune-gdb

plug ul/kak-lsp
hook global WinSetOption filetype=(c|cpp) %{
    %sh{kak-lsp --kakoune -s $kak_session}
}

plug alexherbo2/auto-pairs.kak
hook global WinCreate .* %{
	auto-pairs-enable
}

map global normal <a-s> :auto-pairs-surround<ret>
