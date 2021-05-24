dark_mode() {
    emacsclient -e '(progn
                      (disable-theme aorst--light-theme)
                      (load-theme aorst--dark-theme))'
}

light_mode() {
    emacsclient -e '(progn
                      (disable-theme aorst--dark-theme)
                      (load-theme aorst--light-theme))'
}
