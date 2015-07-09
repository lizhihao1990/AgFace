# regular expression in R

nightres$Comment <- gsub("[[:digit:]]+\\:[[:digit:]]+\\:[[:digit:]]+", "", 
                              nightres$Comment)
nightres$Comment <- gsub("([[:alpha:]])([[:digit:]])", "\\1 \\2", 
                              nightres$Comment)
# working with backreference: 
# place the element to be found in (). 
# The first of these "()" can then be referenced via "\\1"                              
gsub("([[:alpha:]])", "_\\1", df$RingPlot)
