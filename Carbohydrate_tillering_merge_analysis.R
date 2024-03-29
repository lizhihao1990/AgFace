# Carbohydrate tillering merge and analysis

# libraries
require(ggplot2)
require(plyr)
require(reshape)
require(nlme)

# set working directory
setwd("~/AgFace/Topics/Tillering_2011_2012")

# import the data from
load("../Carbohydrates_DC65_2009_2010_2011_2012/DC65_Carbohydrates_2012.RData")
load("../Carbohydrates_DC31_2011_2012/CarbohydratesDC31_2011_2012.RData")
load("../Carbohydrates_DC65_2009_2010_2011_2012/DC65_Carbohydrates_2011.RData")

CarbohydratesDC31$Year <- as.numeric(as.character(CarbohydratesDC31$Year))
DC65_2012$Year <- as.numeric(as.character(DC65_2012$Year))
Carb.DC65.2011.tin$Year <- as.numeric(as.character(Carb.DC65.2011.tin$Year))

# assemble the data
Carbs <- rbind(CarbohydratesDC31, DC65_2012)

Carb.DC65.2011.tin$sample.ID <- NULL

Carbs <- rbind(Carbs, Carb.DC65.2011.tin)

# the traits
Silverstars <- c("Silverstar", "SSR T65")
SBs <- c("SB062", "SB003")

# assign traits to cultivars
Carbs$Trait <- NA
Carbs$Trait[Carbs$Cultivar %in% Silverstars] <- "Silverstar"
Carbs$Trait[Carbs$Cultivar %in% SBs] <- "SB"
Carbs$Trait <- as.factor(Carbs$Trait)

# re-order cultivars
Carbs$Cultivar <- factor(Carbs$Cultivar, 
                         levels = c("SB003", "SB062", "Silverstar", "SSR T65"))

# re-name Environments to match other workspaces
Carbs$Environment <- gsub("Supp", "Sup", Carbs$Environment)

# format Carbs
Carbs$Ring <- as.factor(Carbs$Ring)
Carbs$Year <- as.factor(Carbs$Year)
Carbs$Environment     <- as.factor(Carbs$Environment)
Carbs$Ord.Environment <- as.factor(Carbs$Environment)

# Factor Order from tillering plant production data
Carbs$Ord.Environment <- factor(Carbs$Ord.Environment, 
               levels = c("2011.Rain", "2012.Rain", "2012.Sup", "2011.Sup"))

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
                 .(Trait, Stage, Organ, variable),
                 function(x) {
                 
                 out <- try(anova(lme(value ~ CO2 * Cultivar * Environment,
                      random = ~ 1 | my.HalfringID/Cultivar,
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
names(lme.res.melt)[5] <- "AOV_factor"

#head(lme.res.melt[is.na(lme.res.melt$variable), ])

#write.table(lme.res.melt,
#            file = "lme_pvalues.csv",
#            row.names = FALSE, sep = ",")

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
  p <- p + facet_grid(Stage ~ Trait)
  p <- p + theme_bw()
  p <- p + labs(x = "ANOVA Factor")
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 0),
                 axis.text.y = element_text(size = rel(0.6)))
p

ggsave(file = "Silverstar_Tin_Carbohydrates_lme_results.pdf",
       width = 7, height = 7)
save.image(file = "Carbohydrate_tillering_workspace.RData", compress = TRUE)



write.table(Carbs, file = "Carbohydrate_Stem_Leaf_2011_2012.csv",
            row.names = FALSE, sep = ",")
