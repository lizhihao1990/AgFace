# Yitpi helper script

# Boxplots
MyBoxplot <- function(dataframe, 
                      label, 
                      xaxis, 
                      yaxis, 
                      treatment_sep_a,
                      treatment_sep_b,
                      facet_var_a,
                      facet_var_b,
                      two_separators) {

  require(ggplot2)
  
  axis_label <- unique(label)
  missing_data <- length(dataframe$value[is.na(dataframe$value)])
  # workaround for specifying facets from strings in a function from Hadley Wickham:
  # http://r.789695.n4.nabble.com/ggplot2-proper-use-of-facet-grid-inside-a-function-td906018.html
  # facets <- facet_grid(paste(facet_var_a, "~ ."))
  #facets <- facet_grid(paste(facet_var_a, facet_var_b, sep = " ~ "))
  facets <- facet_grid(paste(facet_var_a, facet_var_b, sep = " ~ "))
  
  if (nrow(dataframe) != missing_data ) {
  p <- ggplot(dataframe, aes_string(x = xaxis, y = yaxis))
       
       if (two_separators == TRUE) {
       p <- p + geom_boxplot(aes_string(fill   = treatment_sep_a, 
                                        linetype = treatment_sep_b), 
                                  outlier.colour = NA)
       } else {
       p <- p + geom_boxplot(aes_string(fill   = treatment_sep_a), 
                                  outlier.colour = NA)}
                                  
       p <- p + ylab(axis_label)
       # p <- p + facet_wrap(facet_var ~ .)
       p <- p + facets
       # p <- p + facet_grid(scales = "free_y")
       p <- p + labs(x = "Environment cases ordered by increasing yield")
       p <- p + theme_bw()
       p <- p + theme(axis.text.x = element_text(angle = 90))
       
  return(p) } else {
  # create a dummy plot for paramters with 0 samples
  p <- ggplot()
        p <- p + geom_text(data = NULL, aes(x = 1, y = 1), label = "missing data")
        p <- p + ylab(axis_label)
  return(p)
  }
  
}

# Barplots
MyBarplot <- function(dataframe, 
                      label, 
                      xaxis, 
                      yaxis, 
                      treatment_sep_a,
                      facet_var_a,
                      facet_var_b) {

  require(ggplot2)

  axis_label <- unique(label)
  print(axis_label) 
  # workaround for specifying facets from strings in a function from Hadley Wickham:
  # http://r.789695.n4.nabble.com/ggplot2-proper-use-of-facet-grid-inside-a-function-td906018.html
  # facets <- facet_grid(paste(facet_var_a, "~ ."))
  #facets <- facet_grid(paste(facet_var_a, facet_var_b, sep = " ~ "))
  facets <- facet_grid(paste(facet_var_a, facet_var_b, sep = " ~ "))
  
  p <- ggplot(dataframe, aes_string(x = xaxis, y = yaxis))
  
        p <- p + stat_summary(aes_string(fill = treatment_sep_a,
                                       colour = treatment_sep_a), 
                                  fun.y = mean,
                                  mult = 1,
                                  geom = "bar",
                                  position = "dodge")
        p <- p + stat_summary(aes_string(colour = treatment_sep_a), 
                                  fun.data = mean_sdl,
                                  mult = 1,
                                  geom = "linerange",
                                  position = position_dodge(width = 0.9))
       p <- p + ylab(axis_label)
       p <- p + scale_colour_manual(values = c("black", "black"), guide=FALSE)
       p <- p + scale_fill_manual(values = c("white", "grey"))
       p <- p + facets
       p <- p + labs(x = "Nitrogen treatment")
       p <- p + theme_bw()
       
  return(p)
  
}
