theme_white <- function() {
     theme_update(panel.background = theme_blank(),
         panel.grid.major = theme_blank())
 }

theme_set(theme_bw())
theme_white()
