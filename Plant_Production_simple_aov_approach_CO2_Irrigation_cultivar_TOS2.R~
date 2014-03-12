# Anova config CO2xIrrigationxCultivar
# TOS2 only!

# Wheat in different environments
# integration of Horsham and Walpeup data

# looking into responses of Yitpi and Janz in different environments

# based on scripts 
# "Plant_Production_2007_2009.R" and
# "Walpeup_data_shuffle.R"

# now modified for new file form Glenn that integrates Horsham and Walpeup data
# based on script "Plant_Production_import_new_file_from_Glenn_2007-2009.R" only!

# set the working directory within the Plant Production folder
setwd("~/AgFace/Plant_Production/Environment_comparison/CO2xIrrigationxCultivar_TOS2")

# load exisiting workspaces
load("../../Plant_Production_2007_2009_Glenn_Feb14_2014.RData")

# get rid of the objects from the workspace we don't need
to_keep <- c("DCall")
rm(list = ls()[!(ls() %in% to_keep)])

# load libraries
require(car)
require(reshape)

# load Yitpi N-experiment helper script for custom Boxplot function
source("~/AgFace/R_scripts/Yitpi_N_experiment_2007_2009_helper_script.R")


# only keep rainfed Yitpi N0 in this comparison
yj <- DCall[((DCall$Cultivar == "Yitpi" & DCall$Nitrogen == "N0") | 
             DCall$Cultivar == "Janz")  & DCall$TrialID  == "Horsham", ]

# Additionally, only rainfed samples are used!
yj <- yj[yj$TOS == "TOS2", ]

# select parameters to be analysed
#Emergence/m2
#Biomass (g)
#Plant wt (g)
#Plants/m2
#Tillers/m2
#Tillers/plant
#Heads/m2
#Heads/plant
#1000 Grain Wt (g)
#Grains/m2
#Grains/plant
#Grains/tiller
#Grains/head
#Screenings (<2mm) (%)
#Harvest Index
#Yield (g/m2)
#Milling Yield (%)

#Spikelets per head
#Spikelet wt (g)

#Grains per spikelet (= Seeds per floret)

# parameters_to_keep <- c("Emergence.Plants.m2", "AR.Dry.wt.area..g.m2.", "Plant.wt..g.plant.", "Plants.m2..Quadrat.", "Tillers.m2..Quadrat.", "Tillers.plant..Quadrat.", "Crop.Height..cm.", "Tiller.wt..g.tiller.", "Heads.m2..Quadrat.SS.", "Heads.plant..Quadrat.", "1000.Grain.Wt..g.", "Grains.m2", "Grains.plant..quadrat.", "Grains.tiller..quadrat.", "Grains.head..quadrat.", "Screenings...2mm.....", "Harvest.Index..AR.", "Yield..g.m2.", "Milling.Yield....", "Spikelets.head", "Spikelet.wt..g.", "Seeds.floret")

parameters_to_keep <- c("Spikelets.head", "Spikelet.wt..g.", "Crop.Height..cm.", "Emergence.Plants.m2", "AR.Dry.wt.area..g.m2.", "Plant.wt..g.plant.", "Tiller.wt..g.tiller.",  "Plants.m2..Quadrat.", "Tillers.m2..Quadrat.", "Tillers.plant..SS.", "Heads.m2..Quadrat.", "..Fertile.Tillers..Quadrat.SS.", "Heads.plant..SS.", "1000.Grain.Wt..g.", "Grains.m2", "Grains.plant", "Grains.tiller", "Grains.head", "Screenings...2mm.....", "Harvest.Index..AR.", "Milling.Yield....", "Yield..g.m2.", "Seeds.floret")

# For now, we keep all parameters until all are definitively identified
names(yj)[grep("Dry", names(yj))]


# create Environment variable               
yj$Environment <- interaction(yj$TrialID,
                              yj$Year, 
                              yj$Irrigation,
                              yj$TOS,
                              drop = TRUE)

# sort Environment by increasing Grain yield in aCO2
# Michael: I would group the environments according to their grain yield under aCO2 on the x axis. This should show us already lots!

Ord.Env <- ddply(yj[yj$CO2 == "aCO2", ],
              .(Environment),
              summarise,
              mean_yield = mean(Yield..g.m2., na.rm = TRUE))

yj$Ord.Environment <- factor(yj$Environment,
                             levels = Ord.Env$Environment[order(Ord.Env$mean_yield)],
                             ordered = TRUE)

# re-organise the data frame, all descriptors in front
yj <- yj[, c(1:17, 98:99, 18:97)]

# create the long format of the data
yj.melt <- melt(yj,
                id = names(yj)[1:19])

#yj.sum <- cast(yj.melt, 
yj.sum <- cast(yj.melt[yj.melt$variable %in% parameters_to_keep, ], 
             TrialID + TOS + CO2 + Stage + Year + Cultivar ~ variable, 
             c(function(x) mean(x, na.rm = TRUE),
               function(x) sd(x, na.rm = TRUE),
               function(x) sum(!is.na(x))),
               fill = NaN
             )


# names get tangled up when using custom functions with "cast"
names(yj.sum) <- gsub("_function.x..mean.x..na.rm...TRUE.", "_Mean", names(yj.sum))
names(yj.sum) <- gsub("_function.x..sd.x..na.rm...TRUE.", "_SD", names(yj.sum))
names(yj.sum) <- gsub("_function.x..sum..is.na.x..", "_No_samples", names(yj.sum))

write.table(yj.sum,
            file = "Yitpi_Janz_plant_production_summary_per_year.csv",
            row.names = FALSE,
            na = "",
            sep = ",")


# Boxplots in a loop for Janz and YitpiN0 only
my.boxplots <- dlply(yj.melt[yj.melt$variable %in% parameters_to_keep, ],
                  .(Year, variable),
                  function(x) {
                  MyBoxplot(dataframe = x, 
                            label     = x$variable, 
                            xaxis = "Cultivar", 
                            yaxis = "value", 
                            treatment_sep_a = "CO2", 
                            treatment_sep_b = "TrialID", 
                            facet_var_a = "Stage", 
                            facet_var_b = "Irrigation",
                            two_separators = TRUE,
                            the_x_label = "Cultivar",
                            the_main_title = unique(x$Year))})

# orig line: MyBoxplot(x, x$variable, "Ord.Environment", "value", "CO2", "CO2", "Stage", ".")})

pdf("CO2xIrrigationxCultivar_TOS2_boxplots_Yipti_Janz_Feb10_free_y_simple.pdf")
        print(my.boxplots)
dev.off()

## More graphs
##From: Glenn
##To: michael
##Subject: CO2 response Analysis
##Date: Fri, 6 Sep 2013 15:50:08 +1000
##Michael,

##I think it would be useful to plot the data  like Markus has done with aCO2 and eCO2 as %CO2 response. Thus, another set of box plots ordered by %yield response.  Thoughts on how to do stats on response?  Since you have to calculate from rep means you lose the ability to do AOV.  I'd you agree then Marcus could run another set of analyses.

##Cheers,
##Glenn

##From: Michael
##To: Glenn
##Subject: RE: CO2 response Analysis
##Date: Fri, 6 Sep 2013 07:05:24 +0000

##HI Glenn, Markus,

##If we use ratios (i. e. eCO2 as multiple or precentage of aCO2) there are rules on how to combine the SDs for the new ratios:

##Mean value A +- sdA, mean value B +- sdB and we want X = A/B, the new SD

##    SD = X * sqrt( (sdA/A)**2 + (dB/B)**2)
## 
##Need to read up in a trusty basic stats book, as I pulled this off the web. From memory this looks about right (there is a different rule for subtractions - in fact easier: just the sqrt of the sum of SD squares)

##This way we could calculate percentage response with its appropriate SD around it, and if everything was normally distributed (in the population!) we would not give away information.

##I think this might be useful in analysing the relationship of this response to other parameters (e. g. environmental indices?) or analysing interrelationships among responses (e. g. yield response directly correlated to relative N decrease?)

##Pretty much from the top of my head, so please use with caution.


# Calculate %CO2 response
# 100% is set as the aCO2 for each Environment


# create a little data set to test the boxcox function
#myvalue <- sample(-25:25, 20)
#mydata <- as.data.frame(x = myvalue)
#mydata$CO2_treat <- "aCO2"
#mydata$CO2_treat[11:20] <- "eCO2"
#mydata$CO2_treat <- as.factor(mydata$CO2_treat)
#mydata$N <- "N0"
#mydata$N[11:20] <- "N+"
#mydata$N <- as.factor(mydata$N)
## mydata
#rm(myvalue)



CalcPercent <- function(data, separator, value, reference) {
        ## Calculate the %response compared to a mean reference
        ## returns a data frame with the %response-values in place of the original data.
        ## only the 
        
        
        # data: a data frame
        # value: name of the column to be investigated
        # separator: a name that distinguishes between reference and response
        # reference: a name that identifies the reference treatment within the separator.
        
        # calculate the mean of of the reference
        sep_col <- which(names(data) == separator)
        value_col <- which(names(data) == value)

        # there must be an easier way...        
        # all rows of data that match the specified reference
        matches <- which(data[, sep_col] == reference)
        ref <- data[matches, ]
        ref.mean <- mean(ref[, value_col], na.rm = TRUE)
        ref.sd   <-   sd(ref[, value_col], na.rm = TRUE)
        
        # select the data that are not part of the reference
        non.ref <- which(data[, sep_col] != reference)
        response <- data[non.ref, ]
        response.values <- response[, value_col]
        # calculating the relative difference between treatments
        # absolute: ## response.values <- response.values / ref.mean * 100

        # Glenn, Sept 13:
        # I calculate the relative response as:  (eCO2-aCO2)/aCO2 * 100. 
        response.values <- (response.values - ref.mean) / ref.mean * 100
        resp.mean <- mean(response.values, na.rm = TRUE)
        resp.sd   <-   sd(response.values, na.rm = TRUE)
        
        response[, value_col] <- response.values
        ## return(response)
        
        # calculate relative standard deviation - to do?
        ## Mean value A +- sdA, mean value B +- sdB and we want X = A/B, the new SD
        ##    SD = X * sqrt( (sdA/A)**2 + (dB/B)**2)
        ## for the relative difference, the sd calculation is more complex
        ## SD = 100**2 sd((B/A) - 1) = 100**2 SD(B/A)
        ## SD = sqrt(variance)
        #res.sd <- 100^2 * sqrt(var(response.values/ref[, value_col]))

        ##res.sd <- 100^2 * (sd(response.values, na.rm = TRUE)/sd(ref[, value_col], na.rm = TRUE))
        # if there is no difference involved
        res.sd <- resp.mean * sqrt( (resp.sd/resp.mean)^2 + (ref.sd/ref.mean)^2) 
        
        
        current_variable <- unique(data$variable)
        current_Stage    <- unique(data$Stage)
        current_Ord.Env  <- unique(data$Ord.Environment)
#        mean_sd_table <- data.frame(variable = current_variable,
#                                    Stage    = current_Stage,
#                                    Ord.Environment = current_Ord.Env,
#                                    resp_mean = resp.mean,
#                                    resp_sd    = res.sd) 
        # adding the (redundant information into the results dataframe)
        #output <- c(response, mean_sd_table)
        response$mean <- resp.mean
        response$SD <- res.sd
        return(response)
}

## testing the test case
## CalcPercent(mydata, "CO2_treat", "myvalue", "aCO2")

# Calculate the relative response to eCO2 for each variable, stage and environment.
rel.response <- ddply(yj.melt,
                     .(Irrigation, Year, Irrigation, variable, Stage),
                     function(x)
                     CalcPercent(x, "CO2", "value", "aCO2"))

# rename the variable to inidate these are relative differences
rel.response$variable <- gsub("$", "_rel_diff_to_aCO2", rel.response$variable)

# extract the mean data
#yj.rel <- rel.response[, -c(4, 16)]
yj.rel <- rel.response


# checking the yield data
#p <- ggplot(rel.response[rel.response$variable == "Yield..g.m2.", ],
#            aes(x = Ord.Environment, y = value))
#        p <- p + geom_boxplot(aes(colour = CO2))
#p


# re-ordering the Environemnts based on maximum relative difference
# Glenn 13/09/2013: I'd like them ordered by relative yield response rather than absolute yield so we can compare these data sets quickly. 

# This section was taken out on Sept 30. Email Glenn:
#I was wondering if you could plot the relative data in the order of the absolute yield data? 

#----------------------------------------------------------
#Rel.Ord.Env <- ddply(rel.response[rel.response$variable == "Yield..g.m2._rel_diff_to_aCO2",],
#              .(Ord.Environment),
#              summarise,
#              mean_rel_yield = mean(value, na.rm = TRUE))

#rel.response$Ord.Environment <- factor(rel.response$Ord.Environment,
#                             levels = Rel.Ord.Env$Ord.Environment[order(Rel.Ord.Env$mean_rel_yield)],
#                             ordered = TRUE)
#----------------------------------------------------------

## rel response results table
#yitpi.rel <- cast(rel.response, 
#             CO2 + Stage + Ord.Environment ~ variable, 
#             c(function(x) mean(x, na.rm = TRUE),
#               function(x) sd(x, na.rm = TRUE),
#               function(x) sum(!is.na(x))),
#               fill = NaN
#             )


## names get tangled up when using custom functions with "cast"
#names(yitpi.rel) <- gsub("_function.x..mean.x..na.rm...TRUE.", "_Mean", names(yitpi.rel))
#names(yitpi.rel) <- gsub("_function.x..sd.x..na.rm...TRUE.", "_SD", names(yitpi.rel))
#names(yitpi.rel) <- gsub("_function.x..sum..is.na.x..", "_No_samples", names(yitpi.rel))

names(yj.rel)[which(names(yj.rel) == "variable")] <- "parameter"
# melt the table with the relative SD data
yj.rel.melt <- melt(yj.rel,
                    id = names(yj.rel)[1:20])

yj.rel.cast <- cast(yj.rel.melt,
                    Irrigation + Year + CO2 + Stage + Cultivar ~ parameter + variable,
                    fun = mean)

write.table(yj.rel.cast[yj.rel.cast$variable %in% parameters_to_keep, ],
            file = "Yitpi_Janz_plant_production_relative_difference_per_environment_relSD.csv",
            row.names = FALSE,
            na = "",
            sep = ",")


# Boxplots in a loop for relative response data
my.boxplots <- dlply(rel.response[rel.response$variable %in% parameters_to_keep, ],
                  .(TrialID, Year, variable),
                  function(x) {
                  MyBoxplot(dataframe = x, 
                            label     = x$variable, 
                            xaxis = "Cultivar", 
                            yaxis = "value", 
                            treatment_sep_a = "CO2", 
                            treatment_sep_b = "", 
                            facet_var_a = "Stage", 
                            facet_var_b = "Irrigation",
                            two_separators = FALSE,
                            the_x_label = "Cultivar")})

#pdf("Horsham_Walpeup_boxplots_Yipti_Janz_relative_response_free_y_simple_aov.pdf")
#        print(my.boxplots)
#dev.off()


# Statistical tests
# To be true to the analysis we should do the following:
# 1) Perform an AOV on each year*location individually (eg, Horsham 2007, Walpeup 2008) with CO2, TOS, irrigation and cultivar
# 2) Do an exploratory analysis of the results from (1) to determine if TOS is significant and look at interactions.  Based on previous analyses, there are very few interactions and TOS
# appears to be significant in almost all cases for the data analysed here, except probably for HI.
# 3) Most likely, for most variables, we will analyse TOS separately since it will probably be highly significant and in any case fits the view that this is a very different environment and
#should be analysed separately.

# checking assumptions for ANOVA
shap.out <- shapiro.test(yj$Yield..g.m2.)
shap.out$p.value

lev.out <- leveneTest(Yield..g.m2. ~ CO2 * Irrigation * Cultivar, 
                      data = yj)
lev.out$`Pr(>F)`[1]
# Check all parameters for normality
levene.out <- dlply(yj.melt,
                   .(TrialID, Year, variable, Stage),
                   function(x) {
                 
                 out <- try(leveneTest(value ~ CO2 * Irrigation * Cultivar,
                                    data = x))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      lev.out <- "not testable"} 
                 else {
                      print("successful fit")
                      lev.out <- out}
                 })
sink("Complete_Levene_test.txt")
        print(levene.out)
sink()

## When the sample size is small, even big departures from normality are not detected, and when your sample size is large, even the smallest deviation from normality will lead to a rejected null.

# Check the sample size
n_size.out <- ddply(yj.melt,
                   .(TrialID, Year, Irrigation, variable, Stage),
                   summarise,
                   n = sum(!is.na(value)))

# Test for normality of the underlying population

shapiro.out <- ddply(yj.melt,
                   .(TrialID, Year, variable, Stage),
                   function(x) {
                 
                 out <- try(shapiro.test(x$value))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      sha.out <- NA} 
                 else {
                      print("successful fit")
                      sha.out <- out$p.value}
                names(sha.out) <- "Shapiro_p_value"
                return(sha.out)
                 })

sample_and_shapiro <- merge(n_size.out, shapiro.out)

# Extract the levene result for each parameter
levene.out <- ddply(yj.melt,
                   .(TrialID, Year, variable, Stage),
                   function(x) {
                 
                 out <- try(leveneTest(value ~ CO2 * Irrigation * Cultivar,
                                    data = x))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      lev.out <- NA} 
                 else {
                      print("successful fit")
                      lev.out <- out$`Pr(>F)`[1]}
                names(lev.out) <- "Levene_p_value"
                return(lev.out)
                 })

# Assemble a table on normality and homogeneity of variances
pre.tests <- merge(sample_and_shapiro, levene.out)

pre.tests$either_fail <- FALSE
pre.tests$either_fail[pre.tests$Shapiro_p_value < 0.05 |
                      pre.tests$Levene_p_value < 0.05] <- TRUE

pre.tests$Shap_Leve_fail <- FALSE
pre.tests$Shap_Leve_fail[pre.tests$Shapiro_p_value < 0.05 &
                         pre.tests$Levene_p_value < 0.05] <- TRUE

pre.tests$Shap_fail <- FALSE
pre.tests$Shap_fail[pre.tests$Shapiro_p_value < 0.05 ] <- TRUE

pre.tests$Leve_fail <- FALSE
pre.tests$Leve_fail[pre.tests$Levene_p_value < 0.05 ] <- TRUE

write.table(pre.tests,
            file = "Shapiro_wilk_Levene_result_raw_data.csv",
            row.names = FALSE,
            sep = ",",
            na = "")
pre.tests[pre.tests$variable == "1000.Grain.Wt..g.",]

# transform the data that do not meet our pre-requisites for the test

# Fortunately, an anova is not very sensitive to moderate deviations from normality; simulation studies, using a variety of non-normal distributions, have shown that the false positive rate is not affected very much by this violation of the assumption (Glass et al. 1972, Harwell et al. 1992, Lix et al. 1996).
     
#    Glass, G.V., P.D. Peckham, and J.R. Sanders. 1972. Consequences of failure to meet assumptions underlying fixed effects analyses of variance and covariance. Rev. Educ. Res. 42: 237-288.
#    Harwell, M.R., E.N. Rubinstein, W.S. Hayes, and C.C. Olds. 1992. Summarizing Monte Carlo results in methodological research: the one- and two-factor fixed effects ANOVA cases. J. Educ. Stat. 17: 315-339.
#    Lix, L.M., J.C. Keselman, and H.J. Keselman. 1996. Consequences of assumption violations revisited: A quantitative review of alternatives to the one-way analysis of variance F test. Rev. Educ. Res. 66: 579-619.
 
# Failure of non-homogeneous variances are more severe.
# now using the Levene result to decide if transformation is needed.

to_transform <- pre.tests[pre.tests$Leve_fail == TRUE, ]
to_transform <- to_transform[, c("variable", "Year", "Stage")]

nrow(to_transform)
length(sort(unique(interaction(to_transform$variable, 
                               to_transform$Year, 
                               to_transform$Stage, drop = TRUE))))

yj.to_transform <- yj.melt[
                  interaction(yj.melt$variable, yj.melt$Year, yj.melt$Stage) %in% 
                  interaction(to_transform$variable, to_transform$Year, to_transform$Stage), ]

length(sort(unique(interaction(yj.to_transform$variable, yj.to_transform$Year,
                               yj.to_transform$Stage, drop = TRUE))))

# the "good" data that do not need transformation
yj.no_transform <- yj.melt[!
                  interaction(yj.melt$variable, yj.melt$Year, yj.melt$Stage) %in% 
                  interaction(to_transform$variable, to_transform$Year, to_transform$Stage), ]

yj.no_transform[yj.no_transform$variable == "1000.Grain.Wt..g.",]


# calling Myhist function from helper script
#myhist <- MyHist(yj.test, 
#       label = yj.test$variable,
#       yaxis = "value",
#       treatment_sep_a = "CO2",
#       treatment_sep_b = "TOS",
#       facet_var_a = "Stage",
#       facet_var_b = "Cultivar")
#myhist

# Boxplots in a loop for YitpiN0 only
my.hist <- dlply(yj.to_transform,
                  .(variable),
                  function(x) {
                  MyHist(dataframe = x, 
                         label     = x$variable,
                         yaxis = "value", 
                         treatment_sep_a = "CO2", 
                         treatment_sep_b = "Cultivar", 
                         facet_var_a = "Irrigation", 
                         facet_var_b = "Stage"
                         )})

# orig line: MyBoxplot(x, x$variable, "Ord.Environment", "value", "CO2", "CO2", "Stage", ".")})

#pdf("Horsham_Walpeup_Histograms_for_non_homogeneous_paras.pdf")
#        print(my.hist)
#dev.off()



# create a little data set to test the boxcox function
#myvalue <- sample(-25:25, 20)
#mydata <- as.data.frame(x = myvalue)
#mydata$CO2_treat <- "aCO2"
#mydata$CO2_treat[11:20] <- "eCO2"
#mydata$CO2_treat <- as.factor(mydata$CO2_treat)
#mydata$N <- "N0"
#mydata$N[11:20] <- "N+"
#mydata$N <- as.factor(mydata$N)
# mydata

# extract a small test-parameter from yitpi.to_transform to test the boxcoc-Transformation
yj.test <- yj.to_transform[yj.to_transform$variable == "Yield..g.m2.", ]
yj.test <- yj.to_transform[yj.to_transform$variable == "1000.Grain.Wt..g.", ]
# hist(yitpi.test$value)

# Transformation function that uses box-cox to transform and uses leveneTest to check homogeneity of variances

# the boxcox-transformation has been moed to its own script "BoxCox_transformation.R"

source("~/AgFace/R_scripts/BoxCox_transformation.R")

#BoxcoxTrans <- function(data, value, fac1, fac2) {
#        ## Takes a data frame, tests homogeneity of variances,
#        ## transforms the data using bcPower or yjPower,
#        ## and returns a data frame with the original and the transformed values
#           require(car)
#           require(plyr)

#           lambdaseq <- seq(from = -2, to = 2, by = 0.1)
#           
#           min_value <- min(value, na.rm = TRUE)
#               
#           out <- ldply(lambdaseq,
#                  
#                function(x) {
#                if (min_value > 0) {
#                        lev.out <- leveneTest(bcPower(value, x) ~ fac1 * fac2)
#                        }
#                else {
#                        lev.out <- leveneTest(yjPower(value, x) ~ fac1 * fac2)
#                        }
#                
#                lev.p_value <- lev.out$`Pr(>F)`[1]
#                #print(x)
#                #print(lev.p_value)
#                res.table <- data.frame(cbind(lambda = x, p_value = lev.p_value))
#                return(res.table)
#                })
#           
#           #print(out)
#           my_max <- out$lambda[which.max(out$p_value)]
#           #print(my_max)
#           if (max(out$p_value) < 0.05) {
#                print("Not homogeneous")}
#           
#           # Transform the original data to achieve homogeneous variances
#           # with the lambda that provides most "homogeneous" result
#           if (min_value > 0) {
#                   trans_data <- bcPower(value, my_max)}
#           else {
#                   trans_data <- yjPower(value, my_max)
#           }   
#           data$trans_value <- trans_data
#           return(data)
#        }

#out <- BoxcoxTrans(data = mydata, value = mydata$myvalue, fac1 = mydata$CO2_treat, fac2 = mydata$N)

# out <- BoxcoxTrans(yj.test, yj.test$value, yj.test$CO2, yj.test$Irrigation, yj$Cultivar)

# data frame with the transformed data
yj.transformed  <- ddply(yj.to_transform,
                    .(TrialID, Year, variable, Stage),
                    function(x){
                    BoxcoxTrans(x, x$value, x$CO2, x$Irrigation, x$Cultivar)
                    })

yj.transtest <- yj.transformed[yj.transformed$variable == "1000.Grain.Wt..g.", ]

# checking the success of the transformtaion
after.tests <- ddply(yj.transformed,
                   .(TrialID, Year, variable, Stage),
                   function(x) {
                 
                 out <- try(leveneTest(trans_value ~ CO2 * Irrigation * Cultivar,
                                    data = x))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      lev.out <- NA} 
                 else {
                      print("successful fit")
                      lev.out <- out$`Pr(>F)`[1]}
                names(lev.out) <- "Levene_p_value"
                return(lev.out)
                 })

after.tests$Leve_fail <- FALSE
after.tests$Leve_fail[after.tests$Levene_p_value < 0.05 ] <- TRUE

write.table(after.tests,
            file = "After_transformation_Levene_results.csv",
            sep = ",",
            row.names = FALSE)

# Put the transformed and non-transformed data back together
# Add a column to the non-transformed data to match the format of the transformed data frame. This column is a copy of the orginal values.
yj.no_transform$trans_value <- yj.no_transform$value
yj.all_transformed <- rbind(yj.no_transform, yj.transformed)

bad.paras <- interaction(after.tests$variable[after.tests$Leve_fail == TRUE],
                         after.tests$Year[after.tests$Leve_fail == TRUE],
                         after.tests$Stage[after.tests$Leve_fail == TRUE])

# export the "non-homogeneous" parameters
write.table(bad.paras,
            file = "Non-homogeneous_parameters.txt",
            row.names = FALSE,
            col.names = FALSE)
            
yj.all_transformed$inter <- interaction(yj.all_transformed$variable,
                                        yj.all_transformed$Year,
                                        yj.all_transformed$Stage)

# keepimg all parametersa in the analysis, despite failed anov-prerequisites
# as requested by Glenn.
# yj.homogeneous <- yj.all_transformed[!(yj.all_transformed$inter %in% bad.paras), ]
yj.homogeneous <- yj.all_transformed


yj.all_transformed[yj.all_transformed$variable =="1000.Grain.Wt..g.", ]
yj.homtest <- yj.homogeneous[yj.homogeneous$variable == "1000.Grain.Wt..g.", ]

# Run an anova
# run the analysis on all cases for YitpiN0
aov.out <- dlply(yj.homogeneous[yj.homogeneous$variable %in% parameters_to_keep, ],
                 .(TrialID, Year, variable, Stage),
                 function(x) {
                 
                 out <- try(summary(aov(trans_value ~ CO2 * Irrigation * Cultivar,
                                    data = x,
                                    na.action = na.omit)))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      aov.out <- "not testable"} 
                 else {
                      print("successful fit")
                      aov.out <- out}
                 })
sink("CO2xIrrigationxCultivar_TOS2_Anova_results_after_transformation.txt")
        print(aov.out)
sink()

my.aov <- aov(trans_value ~ CO2 * Irrigation * Cultivar,
         data = yj.homogeneous[yj.homogeneous$variable == "Emergence.Plants.m2",])
summary(my.aov)
str(summary(my.aov))
summary(my.aov)[[1]][["Pr(>F)"]]

aov.details.out <- ddply(yj.homogeneous[yj.homogeneous$variable %in% parameters_to_keep, ],
                 .(TrialID, Year, variable, Stage),
                 function(x) {
                 
                 out <- try(aov(
                       trans_value ~ CO2 * Irrigation * Cultivar,
                                    data = x,
                                    na.action = na.omit))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      aov.out <- c(rep(NA, 7))
                      return(aov.out)} 
                 else {
                      print("successful fit")
                      my.terms <- rownames(summary(out)[[1]])[1:7]
                      aov.out <- summary(out)[[1]][["Pr(>F)"]][1:7]
                      names(aov.out) <- my.terms
                      return(aov.out)
                      }
                 })
names(aov.details.out) <- gsub(" ", "", names(aov.details.out))

aov.details.out.melt <- melt(aov.details.out,
                             id = names(aov.details.out)[1:4])

names(aov.details.out.melt)[3] <- "Parameter"
# visualise the anovas
# all parameters
aov.res <- aov.details.out.melt[aov.details.out.melt$value <= 0.05 & 
                                !is.na(aov.details.out.melt$value),]

p <- ggplot(aov.res, aes(x = variable, y = Parameter))
  p <- p + geom_point(aes(colour = value, shape =TrialID), 
                      size = 5)
  p <- p + scale_colour_gradient(low = "green", high = "red", name = "p-value")
  p <- p + facet_grid(Stage ~ Year)
  p <- p + labs(x = "ANOVA Factor")
  p <- p + theme_bw()
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 0))
p

ggsave(file = "CO2xIrrigationxCultivar_TOS2_ANOVA_overview.pdf", height = 25, width = 16)

dim(aov.res[aov.res$variable == "CO2:Cultivar", ])

walpeup <- yj.homogeneous[yj.homogeneous$TrialID == "Walpeup",]

my.aov <- aov(trans_value ~ CO2 * Irrigation * Cultivar,
         data = yj.homogeneous[yj.homogeneous$variable == "Emergence.Plants.m2" &
                               yj.homogeneous$TrialID == "Walpeup",])

