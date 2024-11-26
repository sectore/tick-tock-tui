Output demo/output.gif

# https://github.com/charmbracelet/vhs/tree/main?tab=readme-ov-file#set-theme
# Set Theme { "name": "Whimsy", "black": "#535178", "red": "#ef6487", "green": "#5eca89", "yellow": "#fdd877", "blue": "#65aef7", "magenta": "#aa7ff0", "cyan": "#43c1be", "white": "#ffffff", "brightBlack": "#535178", "brightRed": "#ef6487", "brightGreen": "#5eca89", "brightYellow": "#fdd877", "brightBlue": "#65aef7", "brightMagenta": "#aa7ff0", "brightCyan": "#43c1be", "brightWhite": "#ffffff", "background": "#29283b", "foreground": "#b3b0d6", "selection": "#3d3c58", "cursor": "#b3b0d6" }
# Set Theme {"name": "AtomOneLight","black": "#000000","red": "#de3e35","green": "#3f953a","yellow": "#d2b67c","blue": "#2f5af3","magenta": "#950095","cyan": "#3f953a","white": "#bbbbbb","brightBlack": "#000000","brightRed": "#de3e35","brightGreen": "#3f953a","brightYellow": "#d2b67c","brightBlue": "#2f5af3","brightMagenta": "#a00095","brightCyan": "#3f953a","brightWhite": "#ffffff","background": "#f9f9f9","foreground": "#2a2c33","cursor": "#bbbbbb","selection": "#ededed"}
# Set Theme {"name": "Atom","black": "#000000","red": "#fd5ff1","green": "#87c38a","yellow": "#ffd7b1","blue": "#85befd","magenta": "#b9b6fc","cyan": "#85befd","white": "#e0e0e0","brightBlack": "#000000","brightRed": "#fd5ff1","brightGreen": "#94fa36","brightYellow": "#f5ffa8","brightBlue": "#96cbfe","brightMagenta": "#b9b6fc","brightCyan": "#85befd","brightWhite": "#e0e0e0","background": "#161719","foreground": "#c5c8c6","cursor": "#d0d0d0","selection": "#444444"}
# Set Theme {"name":"Afterglow","black":"#151515","red":"#ac4142","green":"#7e8e50","yellow":"#e5b567","blue":"#6c99bb","magenta":"#9f4e85","cyan":"#7dd6cf","white":"#d0d0d0","brightBlack":"#505050","brightRed":"#ac4142","brightGreen":"#7e8e50","brightYellow":"#e5b567","brightBlue":"#6c99bb","brightMagenta":"#9f4e85","brightCyan":"#7dd6cf","brightWhite":"#f5f5f5","background":"#212121","foreground":"#d0d0d0","cursor":"#d0d0d0","selection":"#303030"}
# Set Theme {"name":"Monokai Pro (Filter Ristretto)","background":"#2C2525","black":"#403838","blue":"#F38D70","brightBlack":"#72696A","brightBlue":"#F38D70","brightCyan":"#85DACC","brightGreen":"#ADDA78","brightMagenta":"#A8A9EB","brightRed":"#FD6883","brightWhite":"#FFF1F3","brightYellow":"#F9CC6C","cursor":"#FFF1F3","cyan":"#85DACC","foreground":"#FFF1F3","green":"#ADDA78","magenta":"#A8A9EB","red":"#FD6883","selection":"#C3B7B8","white":"#FFF1F3","yellow":"#F9CC6C"}
Set Theme {"name":"Monokai Soda","black":"#1a1a1a","red":"#f4005f","green":"#98e024","yellow":"#fa8419","blue":"#9d65ff","magenta":"#f4005f","cyan":"#58d1eb","white":"#c4c5b5","brightBlack":"#625e4c","brightRed":"#f4005f","brightGreen":"#98e024","brightYellow":"#e0d561","brightBlue":"#9d65ff","brightMagenta":"#f4005f","brightCyan":"#58d1eb","brightWhite":"#f6f6ef","background":"#1a1a1a","foreground":"#c4c5b5","cursor":"#f6f7ec","selection":"#343434"}
# Set FontSize 19
# Set Width 640
# Set Height 80
Set FontSize 14
Set Width 520
Set Height 60
Set Padding 0
# Set BorderRadius 20

Set Margin 0
# Set Margin 5
# Set MarginFill "#F7931A"

Type "$(cabal list-bin tick-tock-tui) -i"
Enter
Sleep 1s
Type "a"
Sleep 4s
Type "r"
Sleep 5s

# cut
# ffmpeg -i tick-tock-price.gif -ss 4 -t 5 tick-tock-price-cut.gif
# ffmpeg -i tick-tock-price.gif -ss 4 -t 5 -vf "split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" tick-tock-price-cut.gif
# ffmpeg -i tick-tock-price.mp4 -ss 4 -t 5 tick-tock-price-cut.mp4
