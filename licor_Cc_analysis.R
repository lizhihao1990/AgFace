# Analysis of Licor files
# for A-Ci 


library(Deducer) # helpful gui
library(ggplot2) # Grammar of Graphics
library(plyr) # package for split-apply-combine procedures
library(Hmisc) 


# rm(list=ls()) # delete all current R objects

setwd("/home/loewi/Analysis/AgFace/Cc_Oct_2011/analysis")
# import the data
df.orig <- read.csv("triple_ACi_Oct2011_AgFACE.csv", header=TRUE)

# import the treatment information
treat_info <- read.csv("AgFace_treatment_info_Oct_2011.csv", header=TRUE)

# ACi -curve information is stored in the "organ" prompt
# rename "organ" to "ACi-light-level"
# 
names(df.orig)[which(names(df.orig)=="organ")]="ACi_light_level"


# rename some variables to R standards
# Fo. Fo_dash
# Fm. Fm_dash
# Fv.Fm Fv_over_Fm
# Fv..Fm. Fv_dash_over_Fm_dash
# ParIn.Fs ParInatFs
# PS2.1 PS2_over_1

names(df.orig)[which(names(df.orig)=="Fo.")]="Fo_dash"
names(df.orig)[which(names(df.orig)=="Fm.")]="Fm_dash"
names(df.orig)[which(names(df.orig)=="Fv.Fm")]="Fv_over_Fm"
names(df.orig)[which(names(df.orig)=="Fv..Fm.")]="Fv_dash_over_Fm_dash"
names(df.orig)[which(names(df.orig)=="ParIn.Fs")]="ParInatFs"
names(df.orig)[which(names(df.orig)=="PS2.1")]="PS2_over_1"


# add CO2 treatment information
# merge with info from treatment table

df <- merge(treat_info, df.orig, by.x= "Ring", by.y= "ring")

# a unique identifier for each plant
df$sample_name <- paste(df$Ring, df$Species, df$variety, df$CO2_treatment, sep="_")
df$measurement_part <- paste(df$Ring, df$Species, df$variety, df$CO2_treatment, df$ACi_light_level, sep="_")

# clean data table
# each rows has an indicator if the row holds usable data"
clean_data <- df[df$use_row == "yes", ]

# calculate mean and standard deviation for each CO2-level, and curve
gas_exchange_table <- clean_data[clean_data$fluorescence_flash == "no", ]
mean_gas_ex_table <- aggregate(gas_exchange_table, by=data.frame(
	gas_exchange_table$sample_name,
	gas_exchange_table$measurement_part,
	gas_exchange_table$Ring, 
	gas_exchange_table$Species, 
	gas_exchange_table$CO2_treatment, 
	gas_exchange_table$variety, 
	gas_exchange_table$ACi_light_level, 
	gas_exchange_table$CO2_target,
	gas_exchange_table$use_row,
	gas_exchange_table$fluorescence_flash
		), 
	FUN=mean
	)

sd_gas_ex_table <- aggregate(gas_exchange_table, by=data.frame(
	gas_exchange_table$sample_name,
	gas_exchange_table$measurement_part,
	gas_exchange_table$Ring, 
	gas_exchange_table$Species, 
	gas_exchange_table$CO2_treatment, 
	gas_exchange_table$variety, 
	gas_exchange_table$ACi_light_level, 
	gas_exchange_table$CO2_target,
	gas_exchange_table$use_row,
	gas_exchange_table$fluorescence_flash
		), 
	FUN=sd
	)

# remove the now empty columns by name
mean_gas_ex_table[which(names(mean_gas_ex_table)=="Ring")] <- NULL
mean_gas_ex_table[which(names(mean_gas_ex_table)=="Species")] <- NULL
mean_gas_ex_table[which(names(mean_gas_ex_table)=="CO2_treatment")] <- NULL
mean_gas_ex_table[which(names(mean_gas_ex_table)=="variety")] <- NULL
mean_gas_ex_table[which(names(mean_gas_ex_table)=="ACi_light_level")] <- NULL
mean_gas_ex_table[which(names(mean_gas_ex_table)=="use_row")] <- NULL
mean_gas_ex_table[which(names(mean_gas_ex_table)=="CO2_target")] <- NULL
mean_gas_ex_table[which(names(mean_gas_ex_table)=="fluorescence_flash")] <- NULL
mean_gas_ex_table[which(names(mean_gas_ex_table)=="sample_name")] <- NULL
mean_gas_ex_table[which(names(mean_gas_ex_table)=="measurement_part")] <- NULL

sd_gas_ex_table[which(names(sd_gas_ex_table)=="Ring")] <- NULL
sd_gas_ex_table[which(names(sd_gas_ex_table)=="Species")] <- NULL
sd_gas_ex_table[which(names(sd_gas_ex_table)=="CO2_treatment")] <- NULL
sd_gas_ex_table[which(names(sd_gas_ex_table)=="variety")] <- NULL
sd_gas_ex_table[which(names(sd_gas_ex_table)=="ACi_light_level")] <- NULL
sd_gas_ex_table[which(names(sd_gas_ex_table)=="use_row")] <- NULL
sd_gas_ex_table[which(names(sd_gas_ex_table)=="CO2_target")] <- NULL
sd_gas_ex_table[which(names(sd_gas_ex_table)=="fluorescence_flash")] <- NULL
sd_gas_ex_table[which(names(sd_gas_ex_table)=="sample_name")] <- NULL
sd_gas_ex_table[which(names(sd_gas_ex_table)=="measurement_part")] <- NULL

# Remove "gas_exchange.xy" from names that got introduced during aggregation
names(mean_gas_ex_table) <- gsub("gas_exchange_table.", "", names(mean_gas_ex_table) )
names(sd_gas_ex_table) <- gsub("gas_exchange_table.", "", names(sd_gas_ex_table) )


full_light_data <- mean_gas_ex_table[mean_gas_ex_table$ACi_light_level == "full_light", ]
full_light_data_sd <- sd_gas_ex_table[sd_gas_ex_table$ACi_light_level == "full_light", ]

# full_light_data <- clean_data[clean_data$ACi_light_level == "full_light" & clean_data$Ring == 16 , ] # ACi in Ring 16, ambient is limited by low conductance compared to other measurements

# calculate Photosynthesis error bars for the plots
limits_Photo <- aes(ymax = full_light_data$Photo + full_light_data_sd$Photo, ymin=full_light_data$Photo - full_light_data_sd$Photo) 

# calculate Ci error bars for the plots
limits_Ci <- aes(xmax = full_light_data$Ci + full_light_data_sd$Ci, xmin=full_light_data$Ci - full_light_data_sd$Ci) 
# create some overview plots

p <- ggplot(full_light_data, aes(x=Ci, y=Photo))
p <- p + geom_errorbar(limits_Photo) + geom_errorbarh(limits_Ci)
p <- p + geom_point(aes(colour=sample_name), alpha=0.66)
p <- p + geom_smooth(aes(colour=sample_name))
p <- p + facet_grid(. ~ CO2_treatment)
p <- p + theme_bw()
p <- p + labs(
	x = expression(leaf-internal~CO[2]~concentration~(μmol~mol^-1)), 
	y = expression(CO[2]~uptake~rate~(μmol~m^-2*s^-1))
	)
p <- p + opts(legend.position = c(0.85,0.25))
p
#ggsave(file="01_general_ACi_overview.png", width=9, height=9)
ggsave(file="01_general_ACi_overview.pdf", width=9, height=9)



# linear fits

linear_part_ambient <- mean_gas_ex_table[
	mean_gas_ex_table$Ci < 180 & mean_gas_ex_table$CO2_treatment == "ambient", ]

ambient_linear_coeffs <- lapply(
	split(linear_part_ambient, linear_part_ambient$ACi_light_level), 
		function(x)lm(Photo ~ Ci, data = x)
	)

linear_part_elevated <- mean_gas_ex_table[
	mean_gas_ex_table$Ci < 180 & mean_gas_ex_table$CO2_treatment == "elevated", ]

elevated_linear_coeffs <- lapply(
	split(linear_part_elevated, linear_part_elevated$ACi_light_level), 
	function(x)lm(Photo ~ Ci, data = x)
	)

# access the individual coefficientes via:
# elevated_linear_coeffs$full_light$coefficients



# detailed view of the intersection area
# separate plots for ambient and elevated CO2

# ambient
mean_gas_ex_table_ambient <- mean_gas_ex_table[mean_gas_ex_table$CO2_treatment == "ambient", ]

p <- ggplot(mean_gas_ex_table_ambient, aes(x=Ci, y=Photo))
p <- p + geom_point(aes(colour = ACi_light_level))
p <- p + geom_abline(
	intercept = ambient_linear_coeffs$full_light$coefficients[1], 
	slope = ambient_linear_coeffs$full_light$coefficients[2] )
p <- p + geom_abline(
	intercept = ambient_linear_coeffs$medium_light$coefficients[1], 
	slope = ambient_linear_coeffs$medium_light$coefficients[2] )
p <- p + geom_abline(
	intercept = ambient_linear_coeffs$low_light$coefficients[1], 
	slope = ambient_linear_coeffs$low_light$coefficients[2] )
p <- p + scale_y_continuous(limits=c(-1.5, 25))
p <- p + theme_bw()
p <- p + opts(legend.position = c(0.85,0.3),  title = "ambient")
p
ggsave(file="03_ambient_ACi_with_linear_regression_lines.pdf", width=9, height=9)

# detailed look at intersection area
p <- ggplot(mean_gas_ex_table_ambient, aes(x=Ci, y=Photo))
p <- p + geom_point(aes(colour = ACi_light_level))
p <- p + geom_abline(
	intercept = ambient_linear_coeffs$full_light$coefficients[1], 
	slope = ambient_linear_coeffs$full_light$coefficients[2] )
p <- p + geom_abline(
	intercept = ambient_linear_coeffs$medium_light$coefficients[1], 
	slope = ambient_linear_coeffs$medium_light$coefficients[2] )
p <- p + geom_abline(
	intercept = ambient_linear_coeffs$low_light$coefficients[1], 
	slope = ambient_linear_coeffs$low_light$coefficients[2] )
p <- p + coord_cartesian(xlim = c(20, 70), ylim = c(-2,3) ) # does not exclude any data
p <- p + scale_x_continuous(breaks=20:80) + scale_y_continuous(breaks=-1:5)
p <- p + theme_bw()
p <- p + opts(legend.position = c(0.3,0.7),  title = "ambient")
p
ggsave(file="04_ambient_ACi_with_linear_regression_lines_detailed.pdf", width=9, height=9)


# elevated
mean_gas_ex_table_elevated <- mean_gas_ex_table[mean_gas_ex_table$CO2_treatment == "elevated", ]
p <- ggplot(mean_gas_ex_table_elevated, aes(x=Ci, y=Photo))
p <- p + geom_point(aes(colour = ACi_light_level))
p <- p + geom_abline(
	intercept = elevated_linear_coeffs$full_light$coefficients[1], 
	slope = elevated_linear_coeffs$full_light$coefficients[2] )
p <- p + geom_abline(
	intercept = elevated_linear_coeffs$medium_light$coefficients[1], 
	slope = elevated_linear_coeffs$medium_light$coefficients[2] )
p <- p + geom_abline(
	intercept = elevated_linear_coeffs$low_light$coefficients[1], 
	slope = elevated_linear_coeffs$low_light$coefficients[2] )
p <- p + scale_y_continuous(limits=c(-1.5, 25))
p <- p + theme_bw()
p <- p + opts(legend.position = c(0.85,0.3),  title = "elevated")
p
ggsave(file="05_elevated_ACi_with_linear_regression_lines.pdf", width=9, height=9)

# detailed look at intersection area
mean_gas_ex_table_elevated <- mean_gas_ex_table[mean_gas_ex_table$CO2_treatment == "elevated", ]
p <- ggplot(mean_gas_ex_table_elevated, aes(x=Ci, y=Photo))
p <- p + geom_point(aes(colour = ACi_light_level))
p <- p + geom_abline(
	intercept = elevated_linear_coeffs$full_light$coefficients[1], 
	slope = elevated_linear_coeffs$full_light$coefficients[2] )
p <- p + geom_abline(
	intercept = elevated_linear_coeffs$medium_light$coefficients[1], 
	slope = elevated_linear_coeffs$medium_light$coefficients[2] )
p <- p + geom_abline(
	intercept = elevated_linear_coeffs$low_light$coefficients[1], 
	slope = elevated_linear_coeffs$low_light$coefficients[2] )
p <- p + coord_cartesian(xlim = c(20, 70), ylim = c(-2,3) ) # does not exclude any data
p <- p + scale_x_continuous(breaks=20:80) + scale_y_continuous(breaks=-1:5)
p <- p + theme_bw()
p <- p + opts(legend.position = c(0.3,0.7),  title = "elevated")
p
ggsave(file="06_elevated_ACi_with_linear_regression_lines_detailed.pdf", width=9, height=9)



# xxxxxxxxxxxxxxxxxxxxx
# Do the linear fit for each measured ACi-curve individually

linear_part <- mean_gas_ex_table[
	mean_gas_ex_table$Ci < 180, ]

# using plyr to fit a linear model to each measurement
# output is a list
lm.out <- dlply(linear_part, c("sample_name", "CO2_treatment", "ACi_light_level", "measurement_part"), 
	function(linear_part)lm(Photo ~ Ci, data = linear_part)
	)

# extract coefficients from the list and put them in a data frame
coeffs.out <- ldply(lm.out, function(x) c(coef(x) )
	)

# remove the brackets from the name of the intercept column
names(coeffs.out) <- gsub("\\(Intercept\\)", "Intercept", names(coeffs.out) )

# mean values for Intercept and slope
mean_slopes_and_intercepts <- dlply(coeffs.out, c("CO2_treatment"),
	function(x) c( mean(x$Intercept), mean(x$Ci) )
	)

# ambient_mean_intercept <- mean(coeffs.out$Intercept)
# ambient_sd_intercept <- sd(coeffs.out$Intercept)
# ambient_mean_slope <- mean(coeffs.out$Ci)


# ================Begin plot =================
# plot the linear regressions
par(mfrow=c(1,2)) # one row of plots, with two panels
#first ambient CO2
plot(Photo ~ Ci, data = linear_part[linear_part$CO2_treatment == "ambient", ], 
	xlim = c(20,100), ylim = c(-2, 8), 
	col = measurement_part,
	main = "ambient"
	)

# add a horizontal and vertical line for mean Intercept and Ci
abline(h = mean_slopes_and_intercepts$ambient[1], lty=3)
abline(mean_slopes_and_intercepts$ambient[1], mean_slopes_and_intercepts$ambient[2], lty=1, lwd=3)

# add the regression lines for each curve
for (i in 1:length(coeffs.out[coeffs.out$CO2_treatment == "ambient", ]$Intercept) ) {
	abline(coeffs.out$Intercept[i], coeffs.out$Ci[i], col = i )
	}

# now elevated CO2
plot(Photo ~ Ci, data = linear_part[linear_part$CO2_treatment == "elevated", ], 
	xlim = c(20,100), ylim = c(-2, 8), 
	col = measurement_part,
	main = "elevated"
	)

# add a horizontal and vertical line for mean Intercept and Ci
abline(h = mean_slopes_and_intercepts$elevated[1], lty=3)
abline(mean_slopes_and_intercepts$elevated[1], mean_slopes_and_intercepts$elevated[2], lty=1, lwd=3)

# add the regression lines for each curve
for (i in 1:length(coeffs.out[coeffs.out$CO2_treatment == "elevated", ]$Intercept) ) {
	abline(coeffs.out$Intercept[i], coeffs.out$Ci[i], col = i )
	}

# ================End plot =================

# ================plot the whole curve with linear regressions =================


# plot the linear regressions
par(mfrow=c(1,2)) # one row of plots, with two panels
#first ambient CO2
plot(Photo ~ Ci, data = mean_gas_ex_table[mean_gas_ex_table$CO2_treatment == "ambient", ], 
	ylim = c(-2, 26), 
	col = measurement_part,
	main = "ambient"
	)

# add a horizontal and vertical line for mean Intercept and Ci
abline(h = mean_slopes_and_intercepts$ambient[1], lty=3)
abline(mean_slopes_and_intercepts$ambient[1], mean_slopes_and_intercepts$ambient[2], lty=1, lwd=3)

# add the regression lines for each curve
for (i in 1:length(coeffs.out[coeffs.out$CO2_treatment == "ambient", ]$Intercept) ) {
	abline(coeffs.out$Intercept[i], coeffs.out$Ci[i], col = i )
	}

# now elevated CO2
plot(Photo ~ Ci, data = mean_gas_ex_table[mean_gas_ex_table$CO2_treatment == "elevated", ], 
	ylim = c(-2, 26),  
	col = measurement_part,
	main = "elevated"
	)

# add a horizontal and vertical line for mean Intercept and Ci
abline(h = mean_slopes_and_intercepts$elevated[1], lty=3)
abline(mean_slopes_and_intercepts$elevated[1], mean_slopes_and_intercepts$elevated[2], lty=1, lwd=3)

# add the regression lines for each curve
for (i in 1:length(coeffs.out[coeffs.out$CO2_treatment == "elevated", ]$Intercept) ) {
	abline(coeffs.out$Intercept[i], coeffs.out$Ci[i], col = i )
	}

# ================End of plot the whole curve with linear regressions =================

par(mfrow=c(1,1)) # one row of plots, with one panel

# Calculate intersection of two regression lines
#first line: 
#y = b0 + b1*x

#second line:
#y = c0 + c1*x


#b0 + b1*x = c0 + c1*x

# for y-coordinate
b0 <- coeffs.out$Intercept[1]
b1 <- coeffs.out$Ci[1]
c0 <- coeffs.out$Intercept[2]
c1 <- coeffs.out$Ci[2]


#Solve for c0:
# c0 + c1*x <- b0 + b1*x
# c0 + (-b1+c1*x) <- b0
# (-b1+c1*x) <- b0 - c0
# x <- (b0 - c0)/(-b1+c1)
#x <- (b0 - c0)/(-b1+c1)
# y <- b0 + b1*x

# plot the lines
plot(Photo ~ Ci,xlim = c(0, 180), ylim = c(-5, 10), data=linear_part)
abline(coeffs.out$Intercept[1], coeffs.out$Ci[1])
abline(coeffs.out$Intercept[2], coeffs.out$Ci[2])
#abline(v=x)
# abline(h=y)


intersections <- data.frame() # create an empty data frame

# loop
#for (i in 1:length(coeffs.out$sample_name)) {
# using seq() to generate the step-width of three curves per plant

for (i in seq(1, length(coeffs.out$sample_name), 3) ) {
	j <- i + 1
	k <- i + 2
	
	# intersection of first and second regression
	# get the name by combining the information of the light_level for each curve
	light <- paste(coeffs.out$ACi_light_level[i], coeffs.out$ACi_light_level[j], sep = "_")
	CO2_info <- paste(coeffs.out$CO2_treatment[i])
	sample_info <- paste(coeffs.out$sample_name[i])
	
	# calculate the intersection via the two linear regression coeffcients
	b0 <- coeffs.out$Intercept[i]
	b1 <- coeffs.out$Ci[i]
	c0 <- coeffs.out$Intercept[j]
	c1 <- coeffs.out$Ci[j]
	x <- (b0 - c0)/(-b1+c1)
	y <- b0 + b1*x

	# put the results in a format suitable for inclusion in a data frame
	bla <- cbind(sample_info, CO2_info, light, x, y)
	
	# print info in the console
	print(i)
	print(j)
	print(k)
	print(x)
	print(y)
	intersections <- rbind(intersections, bla)
	

	# intersection of second and third regression
	# increase the counter by one each to get to the second pair of lines using the same code
	i <- i + 1
	j <- j + 1
	k <- k + 1
	
	# get the name by combining the information of the light_level for each curve
	light <- paste(coeffs.out$ACi_light_level[i], coeffs.out$ACi_light_level[j], sep = "_")
	CO2_info <- paste(coeffs.out$CO2_treatment[i])
	sample_info <- paste(coeffs.out$sample_name[i])
	
	# calculate the intersection via the two linear regression coeffcients
	b0 <- coeffs.out$Intercept[i]
	b1 <- coeffs.out$Ci[i]
	c0 <- coeffs.out$Intercept[j]
	c1 <- coeffs.out$Ci[j]
	x <- (b0 - c0)/(-b1+c1)
	y <- b0 + b1*x

	# put the results in a format suitable for inclusionin a data frame
	bla <- cbind(sample_info, CO2_info, light, x, y)
	
	# print info in the console
	print(i)
	print(j)
	print(k)
	print(x)
	print(y)
	intersections <- rbind(intersections, bla)

	# intersection of first and third regression
	# increase the counter by one each to get to the second pair of lines using the same code
	i <- i - 1 #goes back to original "i", "j" stays at its current high value
			
	# k <- k + 1
	
	# get the name by combining the information of the light_level for each curve
	light <- paste(coeffs.out$ACi_light_level[i], coeffs.out$ACi_light_level[j], sep = "_")
	CO2_info <- paste(coeffs.out$CO2_treatment[i])
	sample_info <- paste(coeffs.out$sample_name[i])
	
	# calculate the intersection via the two linear regression coeffcients
	b0 <- coeffs.out$Intercept[i]
	b1 <- coeffs.out$Ci[i]
	c0 <- coeffs.out$Intercept[j]
	c1 <- coeffs.out$Ci[j]
	x <- (b0 - c0)/(-b1+c1)
	y <- b0 + b1*x

	# put the results in a format suitable for inclusionin a data frame
	bla <- cbind(sample_info, CO2_info, light, x, y)
	
	# print info in the console
	print(i)
	print(j)
	#print(k)
	print(x)
	print(y)
	intersections <- rbind(intersections, bla)
	
	}

# get rid of intermediate objects
rm(list = c("bla", "light", "sample_info", "CO2_info") )

# format the intersection correctly as values, not text
intersections$x <- as.double(as.character(intersections$x))
intersections$y <- as.double(as.character(intersections$y))

# calculate mean and stadard deviation for each plant based on the three curves
intersections_means_per_plant <- ddply(intersections, c("sample_info", "CO2_info"),
	function(x) c( mean(x$x), sd(x$x), mean(x$y), sd(x$y) )
	)
names(intersections_means_per_plant) <- c("sample_info", "CO2_info", "x", "sd_x", "y", "sd_y")

# t-test testing for differences in coordinates 
x.test <- t.test(x ~ CO2_info, data = intersections_means_per_plant)
y.test <- t.test(y ~ CO2_info, data = intersections_means_per_plant)
pvaluex <- paste("p value from t-test: Ci (Ci*) =", round(x.test$p.value, digits = 3), sep=" ")
pvaluey <- paste("p value form t-test:  A  (Rd) =", round(y.test$p.value, digits = 3), sep=" ")


# calculate the mean coordinates for ambient and elevated treatments
mean_coordinates <- dlply(intersections_means_per_plant, c("CO2_info"),
	function(x) c( mean(x$x), sd(x$x), mean(x$y), sd(x$y) )
	)

mean_coordinates_df <- ldply(mean_coordinates, function(x) x )
names(mean_coordinates_df)[2] <- "x"
names(mean_coordinates_df)[3] <- "sd_x"
names(mean_coordinates_df)[4] <- "y"
names(mean_coordinates_df)[5] <- "sd_y"

# t-test testing for differences in coordinates based on all three curves per plant
# x.test <- t.test(x ~ CO2_info, data = intersections)
# y.test <- t.test(y ~ CO2_info, data = intersections)
# pvaluex <- paste("p value (t-test) Ci <", round(x.test$p.value, digits = 3), sep=" ")
# pvaluey <- paste("p value (t-test) A  <", round(y.test$p.value, digits = 3), sep=" ")

# calculate error bars
x_limits <- aes(xmax = mean_coordinates_df$x + mean_coordinates_df$sd_x, xmin = mean_coordinates_df$x - mean_coordinates_df$sd_x) 
y_limits <- aes(ymax = mean_coordinates_df$y + mean_coordinates_df$sd_y, ymin = mean_coordinates_df$y - mean_coordinates_df$sd_y)

# errorbars per plant
x_limits_pp <- aes(xmax = intersections_means_per_plant$x + intersections_means_per_plant$sd_x, xmin = intersections_means_per_plant$x - intersections_means_per_plant$sd_x) 
y_limits_pp <- aes(ymax = intersections_means_per_plant$y + intersections_means_per_plant$sd_y, ymin = intersections_means_per_plant$y - intersections_means_per_plant$sd_y)

# plot the intersections
p <- ggplot(mean_coordinates_df, aes(x=x, y=y))
p <- p + geom_point(aes(x) )
p <- p + geom_errorbar(y_limits)
p <- p + geom_errorbarh(x_limits)
p <- p + geom_point(aes(x, y, colour = CO2_info), data = intersections_means_per_plant) #overlay raw data
p <- p + geom_errorbar(y_limits_pp)
p <- p + geom_errorbarh(x_limits_pp)
p <- p + geom_text(aes(40,-1),label=pvaluex, colour="red")
p <- p + geom_text(aes(40,-1.05),label=pvaluey, colour="red")
p <- p + theme_bw()
p <- p + labs(
	x = expression(leaf-internal~CO[2]~concentration~(mu~mol~mol^-1)), 
	y = expression(CO[2]~uptake~rate~(mu~mol~m^-2*s^-1)))
p <- p + opts(legend.position = c(0.3, 0.85) )
p



p <- ggplot(intersections_means_per_plant, aes(x=x, y=y))
p <- p + geom_errorbar(y_limits_pp, colour = "grey")
p <- p + geom_errorbarh(x_limits_pp, colour = "grey")
p <- p + geom_point(aes(colour = CO2_info))
# p <- p + geom_point(aes(x, y, colour = CO2_info), data = mean_coordinates_df) #overlay raw data
#p <- p + geom_errorbar(y_limits_pp, colour = "red", data = mean_coordinates_df)
#p <- p + geom_errorbarh(x_limits_pp, colour = "green", data = mean_coordinates_df)
p <- p + geom_text(aes(40,-0.95),label="3 ACi curves per plant (mean and sd shown)", colour="red")
p <- p + geom_text(aes(40,-0.91),label="points indicate individual plants (n = 4 per treatment)", colour="red")
p <- p + geom_text(aes(40,-1),label=pvaluex, colour="red")
p <- p + geom_text(aes(40,-1.05),label=pvaluey, colour="red")
p <- p + theme_bw()
p <- p + labs(
	x = expression(leaf-internal~CO[2]~concentration~(mu~mol~mol^-1)), 
	y = expression(CO[2]~uptake~rate~(mu~mol~m^-2*s^-1)))
p <- p + opts(legend.position = c(0.3, 0.85) )
p
ggsave(file="07_intersection_overview.pdf", width=9, height=9)

# +++++++++++++ Curve Fit ++++++++++++++++++++++++++++

# equation (1) and (2) from Warren et al. (2007)
# J:= (A + Rd)*((4*((Ci-(A/gi))+2*Gamma))/((Ci-(A/gi))-Gamma)) (1)
# Ci_star:= Gamma -(Rd/gi) (2)
# Ci_star + (Rd/gi) := Gamma
# Gamma := Ci_star + (Rd/gi)

# substitute equation (2) in (1) to get

# J <- (A + Rd)*((4*((Ci-(A/gi))+2*Gammastar))/((Ci-(A/gi))-Gammastar))
 
# solve for gi and Gammastar that minimise electron transport J
# Ci* and Rd taken from intersections
# this solves equation 2

# loop over all plants in the mean_gas_ex_table
# create result table with Ci*, Rd, gi, and gammastar

# allfits.out <- data.frame()

mean_gas_ex_table_stable_J <- mean_gas_ex_table[
		mean_gas_ex_table$Ci >= 500 & 
		mean_gas_ex_table$ACi_light_level == "full_light" #&
		#mean_gas_ex_table$sample_name == "7_Wheat_h45_ambient" # only using the plant with stable J
		, ]

# merge the gas_exchange table with the Ci* and Rd information
names(intersections_means_per_plant)[which(names(intersections_means_per_plant)=="x")]="Ci_star"
names(intersections_means_per_plant)[which(names(intersections_means_per_plant)=="y")]="Rd"

combi <- merge(mean_gas_ex_table_stable_J,intersections_means_per_plant,by.x = "sample_name", by.y ="sample_info")

#intersections_means_per_plant$x[intersections_means_per_plant$sample_name == "16_Wheat_h45_ambient"]

fitit <- function(ETR, Photo, Ci, Ci_star, Rd, sample_name
		){
	#Ci_star <- unique(combi$Ci_star[1])
	#Ci_star <- intersections_means_per_plant$x[intersections_means_per_plant$sample_info == x$sample_name]
	#Rd 	<- combi$Rd[1]
	print("Now processing sample :")
	print(paste(unique(sample_name)))
	print("Ci*: ")	
	Ci_star <- mean(Ci_star)
	print(Ci_star)
	print("Rd is: ")
	Rd <- mean(Rd) * (-1)
	print(Rd)
	# gammastar = (Ci_star + ( Rd / gi ))
	
	nlsfit <- try(nls(	
		ETR ~ (Photo + Rd ) * (
		 (4 * 
			( ( Ci - (Photo/gi) ) + 2 * (Ci_star + (Rd / gi)  ) )  )
		/ 
			( ( Ci - (Photo/gi) ) -     (Ci_star + (Rd / gi)  ) ) 
		),

	# data = mean_gas_ex_table_stable_J,
	start = list(gi = 1), # original starting value was 0.24
	control = nls.control(maxiter=2500, warnOnly=TRUE, tol=1e-12, minFactor = 1/2048),
	algorithm = "port", trace = TRUE,
	) )
	print("Fit complete")
	print(nlsfit$message)
	gi <- coef(nlsfit)
	out <- cbind(gi, Ci_star, Rd)
	return(out)
	}

# call the above function for each plant
allfits <- lapply(split(combi, combi$sample_name), function(x)fitit(x$ETR, x$Photo, x$Ci, x$Ci_star, x$Rd, x$sample_name) )



# summary(allfits$'10_Wheat_h45_elevated')


# access elements in the list via the names for example
# allfits$"10_Wheat_h45_elevated"

#out <- do.call("rbind", allfits)
#out.df <- as.data.frame(out)

# convert the output of the curve fits to a table
gi.out <- ldply(allfits)
names(gi.out) <- c("sample_name", "gi", "Ci_star", "Rd")

# calculate gammastar from known Ci*, Rd, and gi
gi.out$gammastar <- gi.out$Ci_star + (gi.out$Rd/gi.out$gi)

# merge with the treatment information based on plant name
# sample_name and treatment info are in the first two columns
gi_df <- merge(gi.out, intersections_means_per_plant[, 1:2], by.x = "sample_name", by.y = "sample_info")

# t-test for ambient vs elevated gi
gi.ttest <- t.test(gi ~ CO2_info, data = gi_df)
pvalue_gi <- paste("p value from t-test for gi =", round(gi.ttest$p.value, digits = 3), sep=" ")


# t-test for ambient vs elevated gammastar
gammastar.ttest <- t.test(gammastar ~ CO2_info, data = gi_df)
pvalue_gammastar <- paste("p value from t-test: gamma* =", round(gammastar.ttest$p.value, digits = 3), sep=" ")


# boxplot of gi
p <- ggplot(gi_df, aes(x=CO2_info, y=gi))
p <- p + geom_boxplot(aes(colour = CO2_info), outlier.colour = NA)
p <- p + geom_jitter(aes(colour = CO2_info))
p <- p + geom_text(aes(1.5,0.06),label="n = 4 plants per treatment", colour="red")
p <- p + geom_text(aes(1.5,0.059),label="points indicate gi of individual plants", colour="red")
p <- p + geom_text(aes(1.5,0.057),label=pvalue_gi, colour="red")
p <- p + geom_text(aes(1.5,0.056),label="no statistical difference", colour="red")
p <- p + theme_bw()
p <- p + labs(
	x = expression(growth~CO[2]~concentration),
	y = expression(internal~conductance~to~CO[2]~(mol~m^-2*s^-1)))
p <- p + opts(legend.position = c(0.3, 0.85) )
p

ggsave(file="08_boxplot_gi.pdf", width=9, height=9)

# boxplot of gammastar
p <- ggplot(gi_df, aes(x=CO2_info, y=gammastar))
p <- p + geom_boxplot(aes(colour = CO2_info), outlier.colour = "white")
p <- p + geom_jitter(aes(colour = CO2_info))
p <- p + geom_text(aes(2,8),label="n = 4 plants per treatment", colour="red")
p <- p + geom_text(aes(2,6), label="points indicate gammastar per plant", colour="red")
p <- p + geom_text(aes(2,4),label=pvalue_gammastar, colour="red")
p <- p + theme_bw()
p <- p + labs(
	x = expression(growth~CO[2]~concentration),
	y = expression(chloroplastic~CO[2]~compensation~point~Gamma~"*"~(mu~mol~m^-2*s^-1)))
p <- p + opts(legend.position = c(0.3, 0.75) )
p

ggsave(file="09_gammastar.pdf", width=9, height=9)


#+++++++++++++++++ Calculation of Cc ++++++++++++++++++++++

# merge the gi data with the ACi-data measured at full light

mean_gas_ex_table_full_light <- mean_gas_ex_table[
		mean_gas_ex_table$ACi_light_level == "full_light"	
		, ]

for_Cc_calculation <- merge(mean_gas_ex_table_full_light, gi_df, by = "sample_name")

for_Cc_calculation$CO2_info <- NULL #obsolete column

# Cc := Ci - (A/gi) Warren et al. (2007) eq. (3)
for_Cc_calculation$Cc <- for_Cc_calculation$Ci - (for_Cc_calculation$Photo / for_Cc_calculation$gi)


# A-Cc plot

p <- ggplot(for_Cc_calculation, aes(x=Cc, y=Photo))
p <- p + geom_point(aes(colour=sample_name), alpha=0.66)
#p <- p + geom_smooth(aes(colour=sample_name))
p <- p + facet_grid(. ~ CO2_treatment)
p <- p + theme_bw()
p <- p + labs(
	x = expression(chloroplastic~CO[2]~concentration~(μmol~mol^-1)), 
	y = expression(CO[2]~uptake~rate~(μmol~m^-2*s^-1))
	)
p <- p + opts(legend.position = c(0.85,0.25))
p
ggsave(file="10_A-Cc_curving.pdf", width=9, height=9)

# plot Cc vs Ci
p <- ggplot(for_Cc_calculation, aes(x=Ci, y=Cc))
p <- p + geom_abline(slope = 1, colour = "grey")
p <- p + geom_abline(slope = 1, intercept = -250, colour = "grey")
p <- p + geom_abline(slope = 1, intercept = -450, colour = "grey")
p <- p + geom_smooth()
p <- p + geom_point(aes(colour = CO2_treatment))
#p <- p + coord_equal(ratio = 1) 
p <- p + theme_bw()
p <- p + opts(legend.position = c(0.85,0.25))
p

ggsave(file="11_Cc_vs_Ci.pdf", width=9, height=9)

# write a table of the men ACi-data to disk for other programs to use
write.table(for_Cc_calculation, file="mean_ACi_data.csv", row.names = FALSE, sep = ",")



# Curve fit for carboxylation limited part of A/Ci curve after Ethier & Livingston (2004) PCE
#
# following eq(10) with 'a priory' knowlege on the parameter "Kc(1 + O/Ko)" from Cousin et al. (2010) on wheat
#
#
# Parametrisation for Triticum aestivum (wheat)
Kc <- 0.291 #291 #ubar = 0.291 mbar
Ko <- 194 #mbar
O_conc <- 212 # mbar #21.2 kPa = 21% oxygen in atmosphere = 212 mbar to have everything in mbar
Sco <- 114 #Rubisco specificy factor


K_param <- Kc * ( 1 + O_conc / Ko ) # equals 0.609 for wheat ! check units mbar <--> ubar !!

# gamma_star_calculated from the curve and enzyme kinetics
gammastar_calc <- (0.5 * O_conc)/Sco


# get the lower part of A/Ci curve from the table, low is defined by Ci below 500 ppm

carb_lim <- mean_gas_ex_table[
		mean_gas_ex_table$Ci <= 500 & 
		mean_gas_ex_table$ACi_light_level == "full_light" &
		mean_gas_ex_table$sample_name == "7_Wheat_h45_ambient" # only using one plant for now
		, ]
carb_lim <- merge(carb_lim, gi_df, by = "sample_name") # add the Rd and gammastar information for each plant
carb_lim$gi <- NULL # get rid of the previous estimate for gi

# fit eq(10) from Ethier and Livingston to the data

Ac_fit <- function(Photo, Ci, Rd,
	gi_pars = c(0.5, 0.001, 10), # defines starting value, lower limit and upper limit for iteration
	Vcmax_pars = c(10, 1, 50) # defines starting value, lower limit and upper limit for iteration
	){
	# for now static references for Rd and gamma, will refer to individual values per plant later
	Rd <- -0.28 * (-1) # example value from ring 7
	gamma <- 35# example value -- not gammastar! # gi is very, very, very sensitive to gamma!!!!
	
	# do a non-linear least squares fit for eq (10) of Ethier & Livingston, 2004
	nlsfit <- try(nls(
	Photo ~ 
	( 
	-(((Vcmax - Rd)/gi) + Ci + K_param) 
	+ sqrt( ((((Vcmax - Rd)/gi) + Ci + K_param)^2) - 4 * (-1/gi) * ( - ((Vcmax - Rd) * (Ci - gamma)) ) ) 
	)
	/
	(2 * (-1/gi)),
	
	#data = carb_lim, # data source has to provided when lapply and split is not used
	start = list(gi = gi_pars[1], Vcmax = Vcmax_pars[1]), # use first of the above defined values as a start
	control = nls.control(maxiter=2500, warnOnly=TRUE, tol=1e-12, minFactor = 1/4096),
	algorithm = "port", trace = TRUE,
	lower=c(gi_pars[2], Vcmax_pars[2]), # define lower limit as second value from *_pars
	upper=c(gi_pars[3], Vcmax_pars[3]) # define upper limit as third value from *_pars
	))
	#coefs <- coef(nlsfit) # enable this for lapply
	# return(coefs)
	}

# fit eq(10) from Ethier and Livingston to the data, using exisiting gammastar and Rd eq(8)
Ac_fit_b <- function(Photo, Ci, Rd, Ci_star, gammastar,
	gi_pars = c(0.2, 0.1, 1), # defines starting value, lower limit and upper limit for iteration
	Vcmax_pars = c(50, 10, 1000) # defines starting value, lower limit and upper limit for iteration
	){
	
	# do a non-linear least squares fit for eq (10) of Ethier & Livingston, 2004
	nlsfit <- try(nls(
	Photo ~ 
	( -(((Vcmax - Rd)/gi) + Ci + K_param) 
	+ sqrt( ((((Vcmax - Rd)/gi) + Ci + K_param)^2) - 4 * (-1/gi)  * (Rd * (Ci + K_param) ) - (Vcmax * (Ci - gammastar))) )
	/
	(2 * (-1/gi)),
	
	data = carb_lim, # data source has to provided when lapply and split is not used
	start = list(gi = gi_pars[1], Vcmax = Vcmax_pars[1]), # use first of the above defined values as a start
	control = nls.control(maxiter=2500, warnOnly=TRUE, tol=1e-12, minFactor = 1/4096),
	algorithm = "port", trace = TRUE,
	lower=c(gi_pars[2], Vcmax_pars[2]), # define lower limit as second value from *_pars
	upper=c(gi_pars[3], Vcmax_pars[3]) # define upper limit as third value from *_pars
	))
	#coefs <- coef(nlsfit) # enable this for lapply
	# return(coefs)
	}



out <- Ac_fit(Photo, Ci, Rd) # call the above function, store the results in the object "out"
out <- Ac_fit_b(Photo, Ci, Rd, Ci_star, gammastar) # call the above function, store the results in the object "out"
# diagnostic plot
par(mfrow=c(1,1)) # one plot only
# graph A/Ci for the lower part of the A/Ci curve
plot(Photo ~ Ci, data = carb_lim) # plot the A-Ci data
co <- coef(out) # extract the curve fit coefficients for gi and VCmax
zValues <- seq(min(0), max(10000), length.out = 1000)  # create a sequence of from 0 to 10000 with 1000 steps for plot
lines(zValues, predict(out, data.frame(Ci = zValues)), col = "red") # add the predicted line to the plot

# now do the fit for each plant in the table, store the individual results in the list object "out.fits"
carb_lim <- mean_gas_ex_table[
		mean_gas_ex_table$Ci <= 500 & 
		mean_gas_ex_table$ACi_light_level == "full_light"
		, ]

out.fits <- lapply(split(carb_lim, carb_lim$sample_name), function(x)Ac_fit(x$Photo, x$Ci))
Ethier_gi.out <- ldply(out.fits) # convert the list to a data frame (i.e. table)

# +++++++++++++++  end curve fit ++++++++++


# get the example data from Ethier and Livingston (2004)

ethier_data <- read.csv("ethier_livingston_data_2003.csv", header = T)
names(ethier_data)[1] <- "Ci"
ethier_data$sample_name <- "example_data"
ethier_data$Rd <- 0.6 # from ethier $ livingston paper, see Fig. 4 
ethier_data$gammastar <- 45.146 # from ethier $ livingston paper, see Fig. 4 , Table 2
ethier_data$Ci_star <- 25 # guess
# Kc, and Ko were taken from Jordan & Ogren (1984) (at 25 ∞C and 210 mbar O2; see Table 2)
Kc <- 0.0274 # kPa #0.274 mbar = 274 ubar
Ko <- 41.8 # kPa # 418 mbar
O_conc <- 21.2 # kPa # 212 mbar = 21% oxygen in atmosphere


K_param <- Kc * ( 1 + O_conc / Ko ) # equals 0.166 for example data

ethier_carb_lim <- ethier_data[
		ethier_data$Ci <= 500, ]

ethier_example.fits <- lapply(split(ethier_carb_lim, ethier_carb_lim$sample_name), function(x)Ac_fit(x$Photo, x$Ci, x$Rd))

# show the statistics overview on the example fit
summary(ethier_example.fits$example_data)
plot(Photo ~ Ci, ylim = c(-2, 40), data = ethier_data) # plot the A-Ci data to change axes: xlim = c(0, 180), ylim = c(-5, 10)

co <- coef(ethier_example.fits$example_data) # extract the curve fit coefficients for gi and VCmax
zValues <- seq(min(0), max(10000), length.out = 1000)  # create a sequence of from 0 to 10000 with 1000 steps for plot
lines(zValues, predict(ethier_example.fits$example_data, data.frame(Ci = zValues)), col = "red")


# ethier_example.fits <- lapply(split(ethier_carb_lim, ethier_carb_lim$sample_name), function(x)Ac_fit_b(x$Photo, x$Ci, x$Ci_star, x$Rd, x$gammastar))


# ==========================+++++++++++++==========================
# Battle plan
# use fixed gammastar, but correct for Leaf temperature according to Bernacchi 2001/2003
# use enzyme kinetics as given by Cousin et al (2009)
# DeltaHa for Ko and Kc is necessary for temperature conversion after Bernacchi!!
# ==========================+++++++++++++==========================
# temperature correction for gammastar after Bernacchi
# starting values
# Parameter = exp [ c − deltaHa / (R * Tk) ]
# deltaHa = 
# scaling constant c
# universal gas constant R := (8.314 J K−1 mol−1 )
# Tk = leaf temperature in K

# for case where parameter is known at 25 °C:
# Parameter = Parameter25 * exp ( ((Tk - 298) *Ha ) / (R * Tk * 298 ))

# using parameters from Bernachhi et al 2002
R <- 8.314
gammastar <- 37.43 # at 25°C
c_gammastar <- 13.49
deltaHa_for_gammastar <- 49.6 # from Bernacchi, Portis 2002, Plant Physiol
Kelvinzero <- 273.2 # for Celsius -> Kelvin conversion
#Tleaf <- 40
# gammastar_adjusted <- gammastar * exp( ( (Tleaf + Kelvinzero) - 298) * deltaHa_for_gammastar / (R * (Tleaf + Kelvinzero ) * 298) )

# gammastar temperature correction
for_Cc_calculation$gammastar_adjusted <- for_Cc_calculation$gammastar * 
	exp( 
	( (for_Cc_calculation$Tleaf + Kelvinzero) - 298) * deltaHa_for_gammastar 
	/ (R * (for_Cc_calculation$Tleaf + Kelvinzero ) * 298) 
	)

# add Rubisco enzyme kinetics information to table
# from Cousins et al. 2010
# no information on deltaHa
# Parametrisation for Triticum aestivum (wheat)

deltaHa_for_Kc <- 80.99 # from Tobacco! have to find correct value for wheat!
deltaHa_for_Ko <- 23.72 # from Tobacco! have to find correct value for wheat!

# Kc
for_Cc_calculation$Kc <- 0.0291 # kPa
for_Cc_calculation$Kc_adjusted <- for_Cc_calculation$Kc * 
	exp( 
	( (for_Cc_calculation$Tleaf + Kelvinzero) - 298) * deltaHa_for_Kc
	/ (R * (for_Cc_calculation$Tleaf + Kelvinzero ) * 298) 
	)


# for Ko
for_Cc_calculation$Ko <- 19.4 # kPa
for_Cc_calculation$Ko_adjusted <- for_Cc_calculation$Ko * 
	exp( 
	( (for_Cc_calculation$Tleaf + Kelvinzero) - 298) * deltaHa_for_Ko
	/ (R * (for_Cc_calculation$Tleaf + Kelvinzero ) * 298) 
	)

# overview of leaf temperatures
p <- ggplot(for_Cc_calculation, aes(x=sample_name, y=Tleaf))
p <- p + geom_boxplot(aes(colour = CO2_treatment)) + coord_flip()
p <- p + theme_bw()
p <- p + labs(
	y = expression(Leaf~temperature~(degree~C))
	)
p <- p + opts(
	axis.title.x = theme_text(hjust = 0.66),
	legend.position = c(0.85,0.85),
	legend.background = theme_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.1)
	)
p
ggsave(file="12_Leaf_temperatures_per_plant.pdf", width=9, height=9)

# End temperature correction ========================+++++++++++++++++================

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# curve fit using Etherier and Livingston eq.8 with temperature corrected data

O_conc <- 21.2 # kPa # 212 mbar = 21% oxygen in atmosphere

# calculate pressure-corrected in kPa
# Licor gives the atmospheric pressure in kPa
standard_atmosphere <- 101.325 #kPa
for_Cc_calculation$Ci_corrected <- ((for_Cc_calculation$Ci * 10) / standard_atmosphere) # results in Pa * 0.001 # results in kPa
summary(for_Cc_calculation$Ci_corrected)

# revert the Ci conversion for testing purposes
for_Cc_calculation$Ci_corrected <- for_Cc_calculation$Ci
summary(for_Cc_calculation$Ci_corrected)

# fit VCmax and gi using eq 8 to the temperature corrected data!
# starting gm might be in the range of 8 - 10 umol m-2 s-1 Pa-1


temp_corrected_fit <- function(Photo, Ci_corrected, Rd, gammastar_adjusted, Kc_adjusted, Ko_adjusted, sample_name,
	gi_pars = c(0.3, 0.000001, 1), # defines starting value, lower limit and upper limit for iteration
	Vcmax_pars = c(15, 5, 1000) # defines starting value, lower limit and upper limit for iteration
	){
	print("start fit")
	print(unique(sample_name))
	# do a non-linear least squares fit for eq (10) of Ethier & Livingston, 2004
	nlsfit <- try(nls(
	Photo ~ (
		(
		-( ( (Vcmax - Rd) / gi) + Ci_corrected + Kc_adjusted * ( 1 + (O_conc/Ko_adjusted) ) )
		+ sqrt( 
		(( ( (Vcmax - Rd) / gi) + Ci_corrected + Kc_adjusted * ( 1 + (O_conc/Ko_adjusted) ) )^2)
		+ 4 * ( (-1/gi)
		* Rd * (Ci_corrected + Kc_adjusted * (1 + (O_conc/Ko_adjusted) ) ) - Vcmax * (Ci_corrected - gammastar_adjusted)
		)		
		)
		)
		/
		(2 * (-1/gi))
		)
	,
	
	# data = carb_lim, # data source has to provided when lapply and split is not used
	start = list(gi = gi_pars[1], Vcmax = Vcmax_pars[1]), # use first of the above defined values as a start
	control = nls.control(maxiter=2500, warnOnly=TRUE, tol=1e-12, minFactor = 1/4096),
	algorithm = "port", trace = TRUE,
	lower=c(gi_pars[2], Vcmax_pars[2]), # define lower limit as second value from *_pars
	upper=c(gi_pars[3], Vcmax_pars[3]) # define upper limit as third value from *_pars
	))
	#coefs <- coef(nlsfit) # enable this for lapply
	# return(coefs)
	}

carb_lim <- for_Cc_calculation[
		for_Cc_calculation$Ci <= 500 & 
		for_Cc_calculation$ACi_light_level == "full_light" #&
		#for_Cc_calculation$sample_name == "7_Wheat_h45_ambient" # only using one plant for now
		, ]

carb_lim$Rd <- carb_lim$Rd * (-1) # to convert Rd to a positive number

out.fits <- lapply(split(carb_lim, carb_lim$sample_name), function(x)temp_corrected_fit(x$Photo, x$Ci_corrected, x$Rd, x$gammastar_adjusted, x$Kc_adjusted, x$Ko_adjusted, x$sample_name))


summary(out.fits$'10_Wheat_h45_elevated')
plot(Photo ~ Ci_corrected, data = carb_lim[carb_lim$sample_name == "10_Wheat_h45_elevated", ]) # plot the A-Ci data to change axes: xlim = c(0, 180), ylim = c(-5, 10)

co <- coef(out.fits$'10_Wheat_h45_elevated') # extract the curve fit coefficients for gi and VCmax
zValues <- seq(min(0), max(10000), length.out = 1000)  # create a sequence of from 0 to 10000 with 1000 steps for plot
lines(zValues, predict(out.fits$'10_Wheat_h45_elevated', data.frame(Ci_corrected = zValues)), col = "red")

coeffs.out <- ldply(out.fits, function(x) c(coef(x) ))

# rename columnheader
names(coeffs.out)[which(names(coeffs.out)=="gi")]="gi_temp_corrected"
names(coeffs.out)[which(names(coeffs.out)==".id")]="sample_name"

treatments <- unique(for_Cc_calculation[, 1:6])

gi_results_with_treatment_info <- merge(treatments, coeffs.out)

# t-test for gi ~ Co2
gi.test <- t.test(gi_temp_corrected ~ CO2_treatment, data = gi_results_with_treatment_info)
Vcmax.test <- t.test(Vcmax ~ CO2_treatment, data = gi_results_with_treatment_info)
pvaluegi <- paste("p value from t-test for gi =", round(gi.test$p.value, digits = 3), sep=" ")
pvalueVcmax <- paste("p value from t-test for Vcmax =", round(Vcmax.test$p.value, digits = 3), sep=" ")


# plot the gi-results per treatment

p <- ggplot(gi_results_with_treatment_info , aes(y=gi_temp_corrected, x=CO2_treatment))
p <- p + geom_boxplot()
p <- p + geom_jitter()
p <- p + geom_text(aes(1.2,0.23),label=pvaluegi, colour="red")
p <- p + theme_bw()
p
ggsave(file="13_gi_temp_corrected_boxplot.pdf", width=9, height=9)

# plot Vcmax per treatment
p <- ggplot(gi_results_with_treatment_info , aes(y=Vcmax, x=CO2_treatment))
p <- p + geom_boxplot(outlier.colour= NA)
p <- p + geom_jitter()
p <- p + geom_text(aes(1.2,0.23),label=pvalueVcmax, colour="red")
p <- p + theme_bw()
p
ggsave(file="14_Vcmax_boxplot.pdf", width=9, height=9)


# merge the gi results into the Cc_calculation table
gi_results <- merge(for_Cc_calculation, coeffs.out, by= "sample_name")

# calculate Cc
gi_results$Cc <- gi_results$Ci - (gi_results$Photo / gi_results$gi_temp_corrected) 


# plot the A-Cc data
p <- ggplot(gi_results, aes(x=Cc, y=Photo))
p <- p + geom_point(aes(colour = sample_name), legend = FALSE)
p <- p + geom_smooth(aes(colour = sample_name), legend = FALSE)
p <- p + facet_grid(. ~ CO2_treatment )
p <- p + theme_bw()
p <- p + opts(legend.position = 'none',
	legend.background = theme_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.1)
	)
p
ggsave(file="15_A-Cc_temperature_corrected.pdf", width=9, height=9)


# plot Cc vs Ci
p <- ggplot(gi_results, aes(x=Ci, y=Cc))
p <- p + geom_abline(slope = 1, colour = "grey")
p <- p + geom_abline(slope = 1, intercept = -250, colour = "grey")
p <- p + geom_abline(slope = 1, intercept = -450, colour = "grey")
p <- p + geom_smooth()
p <- p + geom_point(aes(colour=CO2_treatment))
p <- p + theme_bw()
p <- p + opts(legend.position = c(0.85,0.2))
p

ggsave(file="16_Cc_vs_Ci_temp_corrected.pdf", width=9, height=9)
