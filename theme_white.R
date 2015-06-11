theme_white <- function() {
     theme_update(panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())
 }

theme_set(theme_bw())
theme_white()
