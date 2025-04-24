Output demo/menu.gif

# https://github.com/charmbracelet/vhs/blob/main/THEMES.md
Set Theme "matrix"

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
Type "1"
Sleep 3
Show
Sleep 1
Type "m"
Sleep 0.5
Type@400ms "234"
Sleep 0.5
Ctrl+E
Sleep 0.5
Escape
Sleep 0.5
Type "m"
Sleep 0.5
