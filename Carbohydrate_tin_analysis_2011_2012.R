# Analyse the water soluble carbohydrate data 2011/2012 from SB and Silverstar lines

# set working directory, open workspace, clean up workspace
setwd("~/AgFace/Topics/Tillering_2011_2012/WSC")
load(file = "Carbohydrate_tillering_workspace.RData")

to_keep <- c("Carbs")
rm(list = ls()[!(ls() %in% to_keep)])

# libraries
require(ggplot2)
require(plyr)
require(reshape)
require(nlme)

# rename Conc..mg.mg.
names(Carbs) <- gsub("Conc\\.\\.mg\\.mg\\.", "WSC_conc_mg_per_mg", names(Carbs))

# re-order data frame to have all descriptors in front
my.descriptors <- c("Year", "Ring", "Plot", "Trait", "Cultivar", "CO2", "Stage", "Organ", "RingPos",
                    "my.HalfringID", "Irrigation", "Environment", "Ord.Environment")

Carbs <- Carbs[, c(my.descriptors, "WSC_conc_mg_per_mg")]

# reshape the data into long format
Carbs.melt <- melt(Carbs)

Carbs.plot <- dlply(Carbs.melt,
          .(Organ),
          function(x) {
          my.title <- unique(x$Organ)
          p <- ggplot(x, aes(x = Environment, y = value))
            p <- p + geom_boxplot(aes(linetype = CO2, fill = Cultivar))
            p <- p + labs(title = my.title,
                          y = "Carbohydrate concentration [mg mg-1]")
            #p <- p + facet_grid(Stage ~ Trait, scales = "free_y")
            p <- p + facet_grid(Stage ~ Trait, scales = "free_y")
            p <- p + theme_bw()
            p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 0))
          return(p)
          })

pdf(file = "Carbohydrate_boxplots.pdf", width = 7, height = 7)
   print(Carbs.plot)
dev.off()

# plot SB only
GRDC.carbs <- Carbs.melt[Carbs.melt$Trait == "SB" &
                         Carbs.melt$Organ == "Stem",]
p <- ggplot(GRDC.carbs, aes(x = Environment, y = value))
            p <- p + geom_boxplot(aes(linetype = CO2, fill = Cultivar))
            p <- p + labs(x = "Environment",
                          y = "Stem carbohydrate concentration [mg mg-1]")
#            p <- p + facet_grid(Stage ~ ., scales = "free_y")
            p <- p + facet_grid(Stage ~ .)
            p <- p + theme_bw()
            p <- p + theme(legend.position = c(0.28, 0.88))
#            p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 0))
p

ggsave(file = "GRDC_report_stem_carbs_boxplot.pdf",
       width = 7, height = 14)

lme.out <- dlply(Carbs.melt,
                 .(Year, Trait, Stage, Organ, variable),
                 function(x) {
# old model      try(anova(lme(value ~ CO2 * Cultivar * Environment,
#                    random = ~ 1 | my.HalfringID/Cultivar,            
                 out <- try(anova(lme(value ~ CO2 * Irrigation * Cultivar,
                      random = ~ 1 | Ring/Cultivar,
                      data = x,
                      na.action = na.omit)))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      lme.out <- "not testable"} 
                 else {
                      print("successful fit")
                      lme.out <- out}
                 })
sink("Carbohydrates_tillering_lme.txt")
        print(lme.out)
sink()
# keep a copy of the lme results
Carbs.lme.out <- lme.out

factor_names <- rownames(lme.out[[1]])
# x <- lme.out[[1]]
# y <- lme.out[[3]]

lme.res <- ldply(lme.out,
             function (x) {
             if (x == "not testable") {
                my.p <- c(rep(NA, 8)) 
             } else {
                my.p <- x$`p-value`
             }
             names(my.p) <- factor_names
             return(my.p)
             }
)

# get rid of samples that result in missing values
lme.res <- na.omit(lme.res)

# melt the data down for the graph
lme.res.melt <- melt(lme.res)
names(lme.res.melt)[6] <- "AOV_factor"

#head(lme.res.melt[is.na(lme.res.melt$variable), ])

write.table(lme.res.melt,
            file = "SB_Silverstar_WSC_nested_AOV_pvalues.csv",
            row.names = FALSE, sep = ",")

relev.p.values <- lme.res.melt[lme.res.melt$AOV_factor != "(Intercept)" &
                               !is.na(lme.res.melt$AOV_factor) & 
                               lme.res.melt$value <= 0.05 &
                               !is.na(lme.res.melt$Stage), ]
relev.p.values$variable <- factor(relev.p.values$variable,
                                  levels = sort(unique(as.character(relev.p.values$variable))))
p <- ggplot(relev.p.values,
            aes(x = AOV_factor, y = variable))
  p <- p + geom_point(aes(colour = value, shape = Organ),
                      position = position_dodge(width = 0.5))
  p <- p + scale_colour_gradient(low = "green", high = "red", name = "p-value")
  p <- p + facet_grid(Stage ~ Trait * Year)
  p <- p + theme_bw()
  p <- p + labs(x = "ANOVA Factor")
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 0),
                 axis.text.y = element_text(size = rel(0.6)))
p

ggsave(file = "Silverstar_Tin_Carbohydrates_lme_results.pdf",
       width = 7, height = 7)

# export data
README <- c("Water soluble carbohydrates, Agface, years 2011/2012. Markus Löw, May 2014. WSC_conc_mg_per_mg is carbohydrate concentration in mg/mg.")
save(list = c("Carbs", "README"), file = "WSC_Agface_2011_2012.RData", compress = TRUE)

# for Maryse
Carbs.SB.Stem <- Carbs[Carbs$Trait == "SB" &
                       Carbs$Organ == "Stem", ]

save(list = c("Carbs.SB.Stem", "README"), 
     file = "WSC_Agface_2011_2012_SB_Stem.RData", 
     compress = TRUE)

# export as csv file
write.table(Carbs[, -c(grep("Environment", names(Carbs)))], 
            file = "WSC_Agface_Stem_Leaf_2011_2012.csv",
            row.names = FALSE, sep = ",")
            
# export p-value table
relev.p.values.Carbs <- relev.p.values
save(relev.p.values.Carbs, file = "WSC_lme_p_values.RData", compress = TRUE)
