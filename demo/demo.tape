Output demo/output.gif

# https://github.com/charmbracelet/vhs/tree/main?tab=readme-ov-file#set-theme
Set Theme { "name": "Whimsy", "black": "#535178", "red": "#ef6487", "green": "#5eca89", "yellow": "#fdd877", "blue": "#65aef7", "magenta": "#aa7ff0", "cyan": "#43c1be", "white": "#ffffff", "brightBlack": "#535178", "brightRed": "#ef6487", "brightGreen": "#5eca89", "brightYellow": "#fdd877", "brightBlue": "#65aef7", "brightMagenta": "#aa7ff0", "brightCyan": "#43c1be", "brightWhite": "#ffffff", "background": "#29283b", "foreground": "#b3b0d6", "selection": "#3d3c58", "cursor": "#b3b0d6" }
# Set Theme {"name":"Monokai Pro (Filter Ristretto)","background":"#2C2525","black":"#403838","blue":"#F38D70","brightBlack":"#72696A","brightBlue":"#F38D70","brightCyan":"#85DACC","brightGreen":"#ADDA78","brightMagenta":"#A8A9EB","brightRed":"#FD6883","brightWhite":"#FFF1F3","brightYellow":"#F9CC6C","cursor":"#FFF1F3","cyan":"#85DACC","foreground":"#FFF1F3","green":"#ADDA78","magenta":"#A8A9EB","red":"#FD6883","selection":"#C3B7B8","white":"#FFF1F3","yellow":"#F9CC6C"}
Set FontSize 14
Set Width 800
Set Height 600
Set Padding 0
Set Margin 1

# start
Type "$(cabal list-bin tick-tock-tui) -i"
Enter
# --- FEES ---
Sleep 3s
Type "e"
Sleep 500ms
Type "e"
Sleep 500ms
Type "m"
Sleep 500ms
# --- BLOCK ---
Type "b"
Sleep 500ms
Type "m"
Sleep 500ms
Type "e"
Sleep 500ms
Type "e"
Sleep 500ms
Type "m"
Sleep 500ms
# --- CONVERTER ---
Type "c"
Sleep 500ms
Type "m"
Sleep 500ms
Backspace 16
Type "0.123 BTC"
Enter
Sleep 600ms
Type "t"
Sleep 300ms
Type "t"
Sleep 300ms
Type "t"
Sleep 300ms
Type "t"
Sleep 300ms
Type "t"
Sleep 300ms
Type "t"
Sleep 300ms
Type "t"
Sleep 300ms
Tab 1
Backspace 13
Type "999 USD"
Enter
Sleep 600ms
Type "t"
Sleep 300ms
Type "t"
Sleep 300ms
Type "t"
Sleep 300ms
Type "t"
Sleep 300ms
Type "t"
Sleep 300ms
Type "t"
Sleep 300ms
Type "t"
Sleep 300ms
Type "s"
Sleep 600ms
Backspace 13
Type "98765 sat"
Enter
Sleep 2s
