# Analysis of Antioxidants, Chlorophyll_fluorescence, SLA, and N+S data 2010

# imports .RData files created by the individual scripts for the data

# load libraries
require(reshape)
require(ggplot2)
require(gridExtra)

# set working directory
setwd("~/AgFace/2010/Antioxidants_ChlFL_SLA_2010")

# load the individual data sets
load("../Data_files_for_Markus/Antioxidants/Antioxidants_2010.RData")
load("../chlorophyll_fluorescence_summary_Oct-Nov_2010/Chlorophyll_fluorescence_2010.RData")
load("../Data_files_for_Markus/Zebu-Janz_dry_weight_width/Dry_weight_and_width_2010.RData")
load("../Data_files_for_Markus/Nitrogen_and_Sulfur/Nitrogen_and_Sulfur_2010.RData")

# objects and data to keep in the workspace
to_keep <- c("Antioxidants_data", 
             "Chlorophyll_fluorescence_data",
             "Dry_weight_and_width_data",
             "Nitrogen_Sulfur_data")

# remove elements that we do not need
rm(list = ls()[!(ls() %in% to_keep)])

# load AgFACE helper script
source("~/AgFace/R_scripts/AgFace_helper_scripts.R")

# the different experiments (Antioxidants, Dry weight, and Chlorophyllfluorescence) sampled on different dates. However, the data is compared to each other in this analysis - the data are still comparable, the time difference is small enough to allow this.

# overview on dates in each experiment
sort(unique(Antioxidants_data$Date))
sort(unique(Chlorophyll_fluorescence_data$Date))
sort(unique(Dry_weight_and_width_data$Date))
sort(unique(Nitrogen_Sulfur_data$Date))

# sampling dates
# Antioxidants "2010-10-06 EST" "2010-10-28 EST" "2010-11-19 EST" "2010-11-30 EST"
# ChlFl "2010-10-06 EST" "2010-10-29 EST" "2010-11-08 EST" "2010-11-19 EST" "2010-11-30 EST"
# Dry weight   "2010-10-06 EST" "2010-10-28 EST" "2010-11-19 EST" "2010-12-09 EST"
#
# Date table:
# --------------------------------------------------------------------------
# Antioxidants     | Chlorophyll_fluo | Dry weight       | Nitrogen Sulfur
# --------------------------------------------------------------------------
# "2010-10-06 EST" | "2010-10-06 EST" | "2010-10-06 EST" | "2010-10-06 EST"
# "2010-10-28 EST" | "2010-10-29 EST" | "2010-10-28 EST" | "2010-10-28 EST"
#                  | "2010-11-08 EST" |                  | 
# "2010-11-19 EST" | "2010-11-19 EST" | "2010-11-19 EST" | "2010-11-19 EST"
# "2010-11-30 EST" | "2010-11-30 EST" |                  | 
#                  |                  | "2010-12-09 EST" | "2010-12-09 EST"
# --------------------------------------------------------------------------

# For date-compatibility, moving chlorophyll fluorescence date "2010-10-29 EST" to the 28th
# No changes for other dates
Chlorophyll_fluorescence_data$Date[Chlorophyll_fluorescence_data$Date == as.POSIXct("2010-10-29", tz = "Australia/Melbourne")] <- as.POSIXct("2010-10-28", tz = "Australia/Melbourne")

# getting rid of Sample_ID and Plot columns - not needed and cause confusion when merging data
Antioxidants_data$Sample_ID      <- NULL
Dry_weight_and_width_data$Plot   <- NULL
Nitrogen_Sulfur_data$Sample_ID   <- NULL

# merge Antioxidants and SLA data frames
df <- merge(Dry_weight_and_width_data, Antioxidants_data,
            all = TRUE)

# merge df with Nitrogen and Sulfur data
df <- merge(df, Nitrogen_Sulfur_data,
            all = TRUE)

# add the "is growth CO2" column to the data to match fluorescence data
df$is_growth_CO2 <- "At growth CO2"
df$is_growth_CO2 <- as.factor(df$is_growth_CO2)

# merge df with Chlorophyll_fluorescence
df <- merge(df, Chlorophyll_fluorescence_data,
            all = TRUE)

# looking at a small data set for quality control
small_set <- df[!is.na(df$PhiPS2) & !is.na(df$SLA_m2_per_kg), ]

# removing non-interesting paramters from the file
columns_to_keep <- c("Date", "Ring", "Cultivar", "CO2_treatment", "Organ", "is_growth_CO2", "Leaf_dry_weight_g", "Three_cm_leaf_dry_weight_g", "Stem_dry_weight_g", "Three_cm_stem_dry_weight_g", "Head_dry_weight_g", "Three_cm_head_dry_weight_g", "Leaf_width_mm", "Stem_smallest_diameter_mm", "Stem_largest_diameter_mm", "SLA_m2_per_kg", "GSH_nmol_per_g", "GSH_umol_per_m2", "ASC_umol_per_g", "ASC_mmol_per_m2", "Percent_N", "Percent_S", "N_per_S_ratio", "Photo", "Cond", "Ci", "Fo.", "Fs", "Fv..Fm.", "PhiPS2", "qP", "qN", "ETR")

# keep a copy of the data frame with all columns
df.orig <- df

# create a small data frame with the relevant parameters only
df <- df[, columns_to_keep]

# export the table for inspection
write.table(df, file = "Combined_data.csv",
            row.names = FALSE,
            sep = ",",
            na = "")
            
# Chaff and seeds Antioxidant data together could be compared to "head" data.
# Head

# data in "long" format
df.melt <- melt(df,
                id = c("Date", 
                       "Ring",
                       "Cultivar", 
                       "CO2_treatment", 
                       "Organ",
                       "is_growth_CO2"))

# df.wide <- cast(df.melt,
#                 Date + Ring + Cultivar + CO2_treatment + Organ + is_growth_CO2 ~ variable)

# for quality control, looking at one sample            
Dry_weight_and_width_data[Dry_weight_and_width_data$Date     == as.POSIXct("2010-10-28") &
                          Dry_weight_and_width_data$Organ    == "Leaf" &
                          Dry_weight_and_width_data$Cultivar == "Janz", ]

# for quality control, looking at one sample            
df[df$Date     == as.POSIXct("2010-10-28") &
   df$Organ    == "Leaf" &
   df$Cultivar == "Janz" &
   df$Ring     == 9, ]

# plot everyting
# create one plot per parameter
my_plots <- dlply(df.melt[df.melt$is_growth_CO2 == "At growth CO2", ],
                  .(variable),
                  function(x) {
                  MyMultPlot(x, x$variable, "Date", "value", "CO2_treatment", "Organ", "Cultivar")})

#pdf(file = "All_graphs.pdf")
#        print(my_plots)
#dev.off()

# some graphs
p <- ggplot(df[df$is_growth_CO2 == "At growth CO2", ], 
            aes(x = GSH_nmol_per_g , y = PhiPS2))
        p <- p + geom_point( aes(colour = CO2_treatment))
        p <- p + geom_smooth(aes(colour = CO2_treatment), method = "lm")
        p <- p + facet_grid(Organ ~ Cultivar)
        p <- p + theme_bw()
        #p <- p + ylim(0, 1)
p

p <- ggplot(df, aes(x = Percent_N, y = PhiPS2))
        p <- p + geom_point(aes(colour =  CO2_treatment))
        p <- p + geom_smooth(aes(colour = CO2_treatment), method = "lm")
        p <- p + facet_grid(Organ ~ Cultivar)
        p <- p + theme_bw()
p

p <- ggplot(df, aes(x = SLA_m2_per_kg, y = Percent_N))
        p <- p + geom_point(aes(colour =  CO2_treatment))
        p <- p + geom_smooth(aes(colour = CO2_treatment), method = "lm")
        p <- p + facet_grid(Organ ~ Cultivar)
        p <- p + theme_bw()
p

p <- ggplot(df, aes(x = Date, y = ETR))
        p <- p + stat_summary(aes(colour =  CO2_treatment), 
                              fun.data = mean_sdl, mult = 1)
        p <- p + stat_summary(aes(colour =  CO2_treatment), 
                              fun.data = mean_sdl, mult = 1, geom = "line")
        p <- p + facet_grid(Organ ~ Cultivar)
        p <- p + theme_bw()
p

p <- ggplot(df[df$Organ == "Leaf", ], 
                 aes(x = Cond, y = ETR))
        p <- p + geom_point(aes(colour =  CO2_treatment))
        p <- p + geom_smooth(aes(colour = CO2_treatment), 
                             method  = "nls", 
                             formula = y ~ (a * x) / (b + x),
                             se      = FALSE,
                             start   = list(a = 327, b = 0.3))
        p <- p + facet_grid(Cultivar ~ is_growth_CO2)
        p <- p + theme_bw()
p



my_lm <- lm(PhiPS2 ~ Percent_N, data = df)

out <- dlply(df[df$Organ == "Leaf" | df$Organ == "Stem", ],
            .(CO2_treatment, Organ, Cultivar),
            function(x) 
            summary(lm(x$PhiPS2 ~ x$Percent_N)))

# common plotmath expressions for graph labels
label.CO2 <- expression(bold(CO[2]~~treatment))

# more graphs
p <- ggplot(df[df$is_growth_CO2 == "At growth CO2" & 
               df$Organ == "Leaf", ], 
            aes(x = GSH_nmol_per_g, y = PhiPS2))
        p <- p + geom_point(aes(colour = CO2_treatment, 
                                 shape = as.factor(Date)))
        p <- p + geom_smooth(aes(colour = CO2_treatment, 
                               linetype = CO2_treatment), 
                               method = "lm")
        p <- p + facet_grid(Cultivar ~ is_growth_CO2)
        p <- p + scale_linetype(expression(bold(CO[2]~~treatment)))
        p <- p + scale_colour_manual(expression(bold(CO[2]~~treatment)), 
                               values = c("black", "black"))
        p <- p + scale_shape(name = "Date")
        p <- p + labs(x = expression(Glutathion~(GSH)~"["~nmol~g^-1~"]"), 
                      y = expression(phi~PS[2]))
        p <- p + theme_bw()
p

# Photosynthesis over time
p <- ggplot(df[df$Organ == "Leaf", ], 
                 aes(x = Date, y = Photo))
        p <- p + stat_summary(aes(colour =  CO2_treatment, shape = CO2_treatment), 
                                fun.data = mean_sdl, mult = 1)
        p <- p + stat_summary(aes(colour =  CO2_treatment, linetype = CO2_treatment), 
                                fun.data = mean_sdl, mult = 1, geom = "line")
        p <- p + scale_colour_manual(label.CO2, values = c("black", "black"))
        p <- p + scale_linetype(label.CO2)
        p <- p + scale_shape(label.CO2)
        p <- p + facet_grid(Cultivar ~ is_growth_CO2)
        p <- p + labs(y = expression(Assimilation~rate~"["~mu*mol~CO[2]*m^-2*s^-1~"]"))
        p <- p + theme_bw()
        p <- p + theme(legend.position = c(.90, .35))
p
photo_plot <- p

# Conductance over time
p <- ggplot(df[df$Organ == "Leaf", ], 
                 aes(x = Date, y = Cond))
        p <- p + stat_summary(aes(colour =  CO2_treatment, shape = CO2_treatment), 
                                fun.data = mean_sdl, mult = 1)
        p <- p + stat_summary(aes(colour =  CO2_treatment, linetype = CO2_treatment), 
                                fun.data = mean_sdl, mult = 1, geom = "line")
        p <- p + scale_colour_manual(label.CO2, values = c("black", "black"))
        p <- p + scale_linetype(label.CO2)
        p <- p + scale_shape(label.CO2)
        p <- p + facet_grid(Cultivar ~ is_growth_CO2)
        p <- p + labs(y = expression(Stomatal~conductance~"["~mol~H[2]*O~m^-2*s^-1~"]"))
        p <- p + theme_bw()
        p <- p + theme(legend.position = c(.90, .35))
p
cond_plot <- p

# re-organise the data for repeated measures with "car" package
tmp.df <- df[df$is_growth_CO2 == "At growth CO2" &
             df$Organ == "Leaf", ]

write.table(tmp.df,
            file = "Leaves_at_growth_CO2.csv",
            sep = ",", na = "", row.names = F)
# risky approach, assuming all samples on all dates have the same order!!
re.photo <- cast(tmp.df,
                 Ring + Cultivar + CO2_treatment + Organ + is_growth_CO2 ~ Date,
                 value = "Photo")

# PhiPS2 over time
p <- ggplot(df[df$Organ == "Leaf", ], 
                 aes(x = Date, y = PhiPS2))
        p <- p + stat_summary(aes(colour =  CO2_treatment, shape = CO2_treatment), 
                                fun.data = mean_sdl, mult = 1)
        p <- p + stat_summary(aes(colour =  CO2_treatment, linetype = CO2_treatment), 
                                fun.data = mean_sdl, mult = 1, geom = "line")
        p <- p + scale_colour_manual(label.CO2, values = c("black", "black"))
        p <- p + scale_linetype(label.CO2)
        p <- p + scale_shape(label.CO2)
        p <- p + facet_grid(Cultivar ~ is_growth_CO2)
        p <- p + labs(y = expression(phi~PS[2]))
        p <- p + theme_bw()
        p <- p + theme(legend.position = c(.90, .35))
p
phips2_plot <- p

# qP over time
p <- ggplot(df[df$Organ == "Leaf", ], 
                 aes(x = Date, y = qP))
        p <- p + stat_summary(aes(colour =  CO2_treatment, shape = CO2_treatment), 
                                fun.data = mean_sdl, mult = 1)
        p <- p + stat_summary(aes(colour =  CO2_treatment, linetype = CO2_treatment), 
                                fun.data = mean_sdl, mult = 1, geom = "line")
        p <- p + scale_colour_manual(label.CO2, values = c("black", "black"))
        p <- p + scale_linetype(label.CO2)
        p <- p + scale_shape(label.CO2)
        p <- p + facet_grid(Cultivar ~ is_growth_CO2)
        p <- p + labs(y = expression(q[P]))
        p <- p + scale_y_continuous(limits = c(0, 1))
        p <- p + theme_bw()
        p <- p + theme(legend.position = c(.90, .35))
p
qp_plot <- p 

# 1 - qP over time
p <- ggplot(df[df$Organ == "Leaf", ], 
                 aes(x = Date, y = 1 - qP))
        p <- p + stat_summary(aes(colour =  CO2_treatment, shape = CO2_treatment), 
                                fun.data = mean_sdl, mult = 1)
        p <- p + stat_summary(aes(colour =  CO2_treatment, linetype = CO2_treatment), 
                                fun.data = mean_sdl, mult = 1, geom = "line")
        p <- p + scale_colour_manual(label.CO2, values = c("black", "black"))
        p <- p + scale_linetype(label.CO2)
        p <- p + scale_shape(label.CO2)
        p <- p + scale_y_continuous(limits = c(0, 1))
        p <- p + facet_grid(Cultivar ~ is_growth_CO2)
        p <- p + labs(y = expression(1-q[p]))
        p <- p + theme_bw()
        p <- p + theme(legend.position = c(.90, .35))
p
one_minus_qp_plot <- p

# arrange the plots
pdf(file = "Gas_ex_chl_graphs.pdf", width = 15, height = 10)
        grid.arrange(photo_plot, cond_plot, phips2_plot, one_minus_qp_plot, 
                  nrow = 2, ncol = 2 )
dev.off()

# some testing
# with(mydata,cbind(dv[treatment==1], dv[treatment==2], dv[treatment==3], dv[treatment==4]))
# repeated measures statistic for Photosynthesis
photo.aov <- aov(Photo ~ CO2_treatment * Cultivar * Date + Error(Date/CO2_treatment),
                 data = df[df$is_growth_CO2 == "At growth CO2",])
summary(photo.aov)

cond.aov <- aov(Cond ~ CO2_treatment * Cultivar * Date + Error(Date/CO2_treatment),
                 data = df[df$is_growth_CO2 == "At growth CO2",])
summary(cond.aov)

phips2.aov <- aov(PhiPS2 ~ CO2_treatment * Cultivar * Date + Error(Date/CO2_treatment),
                 data = df[df$is_growth_CO2 == "At growth CO2",])
summary(phips2.aov)

qp.aov <- aov(qP ~ CO2_treatment * Cultivar * Date + Error(Date/CO2_treatment),
                 data = df[df$is_growth_CO2 == "At growth CO2",])
summary(qp.aov)

# No major effects found from repeated measures

# Antooxidant graphs
# GSH_nmol_per_g over time
p <- ggplot(df[df$Organ == "Leaf", ], 
                 aes(x = Date, y = GSH_nmol_per_g))
        p <- p + stat_summary(aes(colour =  CO2_treatment, shape = CO2_treatment), 
                                fun.data = mean_sdl, mult = 1)
        p <- p + stat_summary(aes(colour =  CO2_treatment, linetype = CO2_treatment), 
                                fun.data = mean_sdl, mult = 1, geom = "line")
        p <- p + scale_colour_manual(label.CO2, values = c("black", "black"))
        p <- p + scale_linetype(label.CO2)
        p <- p + scale_shape(label.CO2)
        p <- p + facet_grid(Cultivar ~ .)
        p <- p + labs(y = expression(Glutathion~(GSH)~"["~nmol~g^-1~"]"))
        p <- p + theme_bw()
        p <- p + theme(legend.position = c(.90, .35))
p
glutathion_plot <- p

# Ascorbat plot
p <- ggplot(df[df$Organ == "Leaf", ], 
                 aes(x = Date, y = ASC_umol_per_g))
        p <- p + stat_summary(aes(colour =  CO2_treatment, shape = CO2_treatment), 
                                fun.data = mean_sdl, mult = 1)
        p <- p + stat_summary(aes(colour =  CO2_treatment, linetype = CO2_treatment), 
                                fun.data = mean_sdl, mult = 1, geom = "line")
        p <- p + scale_colour_manual(label.CO2, values = c("black", "black"))
        p <- p + scale_linetype(label.CO2)
        p <- p + scale_shape(label.CO2)
        p <- p + facet_grid(Cultivar ~ .)
        p <- p + labs(y = expression(Ascorbat~(ASC)~"["~mu*mol~g^-1~"]"))
        p <- p + theme_bw()
        p <- p + theme(legend.position = c(.90, .35))
p
ascorbat_plot <- p

pdf(file = "Antioxidant_graphs.pdf", height = 15, width = 10)
        grid.arrange(glutathion_plot, ascorbat_plot, ncol = 1)
dev.off()
# repeated measures statistic for Photosynthesis
gsh.aov <- aov(GSH_nmol_per_g ~ CO2_treatment * Cultivar * Date + Error(Date/CO2_treatment),
                 data = df[df$is_growth_CO2 == "At growth CO2",])
summary(gsh.aov)

asc.aov <- aov(ASC_umol_per_g ~ CO2_treatment * Cultivar * Date + Error(Date/CO2_treatment),
                 data = df[df$is_growth_CO2 == "At growth CO2",])
summary(asc.aov)

# Anovas in a loop
treat_columns <- df[, 1:6]
to_test <- names(df)[7:length(names(df))]
sink(file="Anovas.txt")
for(i in 1:length(to_test)) {
	column_of_interest <-  df[, to_test[i]]
	testframe <- data.frame(cbind(treat_columns, column_of_interest))
	print(to_test[i])	
	fit <- aov(testframe[, 7] ~ CO2_treatment * Cultivar * Date , data = testframe)
	print(summary(fit))
	}
sink()

df.melt.leaf <- df.melt[df.melt$Organ == "Leaf" , ]
#                       & df.melt$variable == "PhiPS2", ]

indi.tests <- dlply(df.melt.leaf,
                    .(variable, Date, Cultivar, is_growth_CO2),
                    function(x) {try(summary(aov(x$value ~ x$CO2_treatment)))})

sink("Individual_date_tests.txt")
        print(indi.tests)
sink()
my.aov <- aov(value ~ CO2_treatment)

# Create a data frame for graphing
df.graph <- df[!(df$CO2_treatment == "ambient" &
                 df$is_growth_CO2 == "Not at growth CO2"),]

# repeated measures via "car"
require(car)

# grab the responses out of re.photo
#response <- re.photo[, c(6:10)]

#my.model <- lm(response ~ 1)
#rfactor <- factor(c("Date1", "Date2", "Date3", "Date4", "Date5"))

#my.aov <- Anova(my.model,
#                idata = data.frame(rfactor), 
#                idesign = ~rfactor, type="III")
#summary(my.aov, multivariate = FALSE)

# building a mixed model
require(nlme)
m1.nlme = lme(Photo ~ Cultivar * Organ * CO2_treatment,
                      random = ~ 1 | Date,
                      data = df.graph,
                      na.action = na.exclude)

summary(m1.nlme)
anova(m1.nlme)

my.simple.aov <- aov(Photo ~ Cultivar * Organ * CO2_treatment + Error(Date),
                     data = df.graph)
summary(my.simple.aov)

# run the analysis on all cases
lme.out <- dlply(df.melt[df.melt$is_growth_CO2 == "At growth CO2", ],
                 .(variable),
                 function(x) {
                 out <- try(anova(lme(value ~ Cultivar * Organ * CO2_treatment,
                      random = ~ 1 | Date,
                      data = x,
                      na.action = na.omit)))
                 if (inherits(out, "try-error")) {
                      lme.out <- "not testable"} 
                 else {lme.out <- out}
                 })
sink("nlme_results.txt")
        print(lme.out)
sink()

# but check homogeneity of variances!
leveneTest(Photo^-1 ~ Cultivar * Organ * CO2_treatment, data = df.graph)
