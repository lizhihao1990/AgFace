# --------------------------------------------------
# collecting figures from the environment
# --------------------------------------------------

# all objects with fig at the start are assembled into a list

my.figures <- ls()[grep("^fig", ls())]
my.figures.list <- llply(my.figures,
              function(x) get(x))
names(my.figures.list) <- my.figures

my.width  <- 17
my.height <- my.width

pdf(file = "Tillering_figures.pdf",
    width = my.width/2.54, height = my.height/2.54)
    print(my.figures.list)
dev.off()


