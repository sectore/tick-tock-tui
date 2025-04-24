Output demo/dashboard.gif

# https://github.com/charmbracelet/vhs/blob/main/THEMES.md
Set Theme "AtomOneLight"

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
Sleep 0.2
Type"a"
Type "0"
Sleep 3
Show
Sleep 1
# --- toggle extra info ---
Type@1s "ee"
# --- reload ---
Type "r"
Sleep 1
