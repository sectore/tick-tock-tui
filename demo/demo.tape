Output demo/output.gif

# https://github.com/charmbracelet/vhs/tree/main?tab=readme-ov-file#set-theme
Set Theme "Whimsy"
Set FontSize 14
Set Width 800
Set Height 600
Set Padding 0
Set Margin 1

# start
Type "$(cabal list-bin tick-tock-tui) -i"
Enter
Sleep 1s
Type "a"
# --- FEES ---
Sleep 2s
Type@500ms "ee"
Type "m"
Sleep 500ms
# --- BLOCK ---
Type "b"
Sleep 500ms
Type "m"
Sleep 500ms
Type@500ms "ee"
Type "m"
Sleep 500ms
# --- CONVERTER ---
Type "c"
Sleep 500ms
Type "m"
Sleep 500ms
Backspace 16
Type@50ms "0.123 BTC"
Enter
Sleep 600ms
Type@300ms "ttttttt"
Tab 1
Backspace 13
Type "999 USD"
Enter
Sleep 600ms
Type@300ms "ttttttt"
Sleep 300ms
Type "s"
Sleep 600ms
Backspace 13
Type@50ms "98765 sat"
Enter
Sleep 2s
