# create the formulae
form.A <- formula(value ~ 1 + CO2 + Cultivar + Environment)
form.B <- formula(value ~ CO2 + Cultivar + Environment)
form.C <- formula(value ~ CO2 * Cultivar + Environment)
form.D <- formula(value ~ CO2 * Cultivar * Environment)

# combine all formulae as a list
my.models <- c(form.A, form.B, form.C, form.D)

# apply a function to each element of the list (lapply)
# in this case return the anova results for each model
# return in the from of a list
# using plyr would add some additional options
# this is not so much different than a for-loop, but much faster!

my.outputs <- lapply(my.models,
                   # apply runs over all objects in the list
                   # we apply a custom function to each element 
                   # of the list (=x)
                   function(x) {
                   anova(lme(x,
                      random = ~ 1 | my.HalfringID/Cultivar,
                      data = df.melt,
                      na.action = na.omit))
                   })

# the output list has as many elements as the original list
# comparison between the individual model results can be accessed from
the list
my.outputs[2]
my.outputs[3]
