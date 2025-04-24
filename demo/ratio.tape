Output demo/ratio.gif

# https://github.com/charmbracelet/vhs/blob/main/THEMES.md
# Set Theme "LiquidCarbon"
Set Theme "iTerm2 Solarized Light"

Set FontSize 14
Set Width 800
Set Height 600
Set Padding 0
Set Margin 1

# start
Hide
# --- start by ignoring previous state ---
Type "$(cabal list-bin tick-tock-tui) -i"
Enter
Sleep 1
# --- enter converter ---
Type "4"
Sleep 3
Show
Sleep 1
# --- toggle extra info ---
Type@0.5s "ee"
Sleep 0.5
Ctrl+E
Sleep 500ms
Backspace 3
Type@50ms "DOGE"
Enter
Sleep 1.5
Type@0.5s "ee"
Sleep 1
