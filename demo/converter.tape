Output demo/converter.gif

# https://github.com/charmbracelet/vhs/blob/main/THEMES.md
Set Theme "OneDark"

Set FontSize 14
Set Width 1200
Set Height 800
Set Padding 0
Set Margin 1

# start
Hide
# --- start by ignoring previous state ---
Type "$(cabal list-bin tick-tock-tui) -i"
Enter
Sleep 1
# --- enter converter ---
Type "a"
Type "3"
Sleep 3
Show
Sleep 500ms
Ctrl+E
Sleep 500ms
Backspace 16
Type@50ms "0.123 BTC"
Enter
Sleep 600ms
Type@300ms "ttttttt"
Ctrl+E
Sleep 500ms
Tab 1
Backspace 13
Type@50ms "999 USD"
Enter
Sleep 600ms
Type@300ms "ttttttt"
Sleep 300ms
Type "s"
Sleep 600ms
Ctrl+E
Sleep 500ms
Backspace 13
Type@50ms "98765 sat"
Enter
Sleep 2s
