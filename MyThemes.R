# my themes for ggplot2
# using information from:
# https://github.com/wch/ggplot2/wiki/New-theme-system

require(ggplot2)

# usage
# p <- p + theme_my - no "()" needed!

theme_my <- theme_bw() + theme(
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = "white"),
            legend.key       = element_blank())

# further theme updates via eg
# p <- p + theme_my + theme(strip.text.y = element_text(size = rel(0.8),
#                                          face = "italic", angle = -90))
# relative sizes:
# theme(axis.text = element_text(size=rel(0.5)))
