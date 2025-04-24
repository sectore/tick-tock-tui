Output demo/block.gif

# https://github.com/charmbracelet/vhs/blob/main/THEMES.md
Set Theme "Belafonte Day"

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
Type "2"
Sleep 3.0
Show
Sleep 0.5
# --- toggle extra info ---
Type@0.5s "e"
# --- toggle fiat ---
Type@300ms "ttttttt"
Type@0.3s "e"
# --- reload ---
Type "r"
Sleep 1
