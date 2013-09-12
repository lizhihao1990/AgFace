# Wheat in different environments
# integration of Horsham and Walpeup data

# looking into responses of Yitpi in different environments

# based on scripts 
# "Plant_Production_2007_2009.R" and
# "Walpeup_data_shuffle.R"

# now modified for new file form Glenn that integrates Horsham and Walpeup data
# based on script "Plant_Production_import_new_file_from_Glenn_2007-2009.R" only!

# set the working directory within the Plant Production folder
setwd("~/AgFace/Plant_Production/Environment_comparison")

# load exisiting workspaces
load("../Plant_Production_2007_2009_Glenn_Aug23_2013.RData")

# get rid of the objects from the workspace we don't need
to_keep <- c("DCall")
rm(list = ls()[!(ls() %in% to_keep)])

# load libraries
require(car)
require(reshape)

# load Yitpi N-experiment helper script for custom Boxplot function
source("~/AgFace/R_scripts/Yitpi_N_experiment_2007_2009_helper_script.R")


# only keep rainfed Yitpi N0 in this comparison
yitpi <- DCall[DCall$Cultivar == "Yitpi" &
               DCall$Nitrogen == "N0", ]

# create Environment variable               
yitpi$Environment <- interaction(yitpi$TrialID,
                                 yitpi$Year, 
                                 yitpi$Irrigation,
                                 yitpi$TOS,
                                 drop = TRUE)

# sort Environment by increasing Grain yield in aCO2
# Michael: I would group the environments according to their grain yield under aCO2 on the x axis. This should show us already lots!

Ord.Env <- ddply(yitpi[yitpi$CO2 == "aCO2", ],
              .(Environment),
              summarise,
              mean_yield = mean(Yield..g.m2., na.rm = TRUE))

yitpi$Ord.Environment <- factor(yitpi$Environment,
                             levels = Ord.Env$Environment[order(Ord.Env$mean_yield)],
                             ordered = TRUE)

# re-organise the data frame, all descriptors in front
yitpi <- yitpi[, c(1:10, 87:88, 11:86)]


# create the long format of the data
yitpi.melt <- melt(yitpi,
                id = names(yitpi)[1:12])

yitpi.sum <- cast(yitpi.melt, 
             CO2 + Stage + Ord.Environment ~ variable, 
             c(function(x) mean(x, na.rm = TRUE),
               function(x) sd(x, na.rm = TRUE),
               function(x) sum(!is.na(x))),
               fill = NaN
             )


# names get tangled up when using custom functions with "cast"
names(yitpi.sum) <- gsub("_function.x..mean.x..na.rm...TRUE.", "_Mean", names(yitpi.sum))
names(yitpi.sum) <- gsub("_function.x..sd.x..na.rm...TRUE.", "_SD", names(yitpi.sum))
names(yitpi.sum) <- gsub("_function.x..sum..is.na.x..", "_No_samples", names(yitpi.sum))

write.table(yitpi.sum,
            file = "Yitpi_plant_production_summary_per_environment.csv",
            row.names = FALSE,
            na = "",
            sep = ",")


# Boxplots in a loop for YitpiN0 only
my.boxplots <- dlply(yitpi.melt,
                  .(variable),
                  function(x) {
                  MyBoxplot(dataframe = x, 
                            label     = x$variable, 
                            xaxis = "Ord.Environment", 
                            yaxis = "value", 
                            treatment_sep_a = "CO2", 
                            treatment_sep_b = NA, 
                            facet_var_a = "Stage", 
                            facet_var_b = ".",
                            two_separators = FALSE)})

# orig line: MyBoxplot(x, x$variable, "Ord.Environment", "value", "CO2", "CO2", "Stage", ".")})

pdf("Horsham_Walpeup_Environments_boxplots_new_file_from_Glenn_free_y.pdf")
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
        
        # select the data that are not part of the reference
        non.ref <- which(data[, sep_col] != reference)
        response <- data[non.ref, ]
        response.values <- response[, value_col]
        response.values <- response.values / ref.mean * 100
        
        response[, value_col] <- response.values
        
        # calculate relative standard deviation - to do?
        ## Mean value A +- sdA, mean value B +- sdB and we want X = A/B, the new SD
        ##    SD = X * sqrt( (sdA/A)**2 + (dB/B)**2)
        
        return(response)
}

## testing the test case
## CalcPercent(mydata, "CO2_treat", "myvalue", "aCO2")

# Calculate the relative response to eCO2 for each variable, stage and environment.
rel.response <- ddply(yitpi.melt,
                     .(variable, Stage, Ord.Environment),
                     function(x)
                     CalcPercent(x, "CO2", "value", "aCO2"))

# checking the yield data
#p <- ggplot(rel.response[rel.response$variable == "Yield..g.m2.", ],
#            aes(x = Ord.Environment, y = value))
#        p <- p + geom_boxplot(aes(colour = CO2))
#p


# Boxplots in a loop for relative response data
my.boxplots <- dlply(rel.response,
                  .(variable),
                  function(x) {
                  MyBoxplot(dataframe = x, 
                            label     = x$variable, 
                            xaxis = "Ord.Environment", 
                            yaxis = "value", 
                            treatment_sep_a = "CO2", 
                            treatment_sep_b = NA, 
                            facet_var_a = "Stage", 
                            facet_var_b = ".",
                            two_separators = FALSE)})

pdf("Horsham_Walpeup_Environments_boxplots_relative_response_free_y.pdf")
        print(my.boxplots)
dev.off()


# Statistical tests

# checking assumptions for ANOVA
shap.out <- shapiro.test(yitpi$Yield..g.m2.)
shap.out$p.value

lev.out <- leveneTest(Yield..g.m2. ~ CO2 * Environment, 
                      data = yitpi)
lev.out$`Pr(>F)`[1]
# Check all parameters for normality
levene.out <- dlply(yitpi.melt,
                   .(variable, Stage),
                   function(x) {
                 
                 out <- try(leveneTest(value ~ CO2 * Environment,
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
n_size.out <- ddply(yitpi.melt,
                   .(variable, Stage),
                   summarise,
                   n = sum(!is.na(value)))

# Test for normality of the underlying population

shapiro.out <- ddply(yitpi.melt,
                   .(variable, Stage),
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
levene.out <- ddply(yitpi.melt,
                   .(variable, Stage),
                   function(x) {
                 
                 out <- try(leveneTest(value ~ CO2 * Environment,
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


# transform the data that do not meet our pre-requisites for the test

# Fortunately, an anova is not very sensitive to moderate deviations from normality; simulation studies, using a variety of non-normal distributions, have shown that the false positive rate is not affected very much by this violation of the assumption (Glass et al. 1972, Harwell et al. 1992, Lix et al. 1996).
     
#    Glass, G.V., P.D. Peckham, and J.R. Sanders. 1972. Consequences of failure to meet assumptions underlying fixed effects analyses of variance and covariance. Rev. Educ. Res. 42: 237-288.
#    Harwell, M.R., E.N. Rubinstein, W.S. Hayes, and C.C. Olds. 1992. Summarizing Monte Carlo results in methodological research: the one- and two-factor fixed effects ANOVA cases. J. Educ. Stat. 17: 315-339.
#    Lix, L.M., J.C. Keselman, and H.J. Keselman. 1996. Consequences of assumption violations revisited: A quantitative review of alternatives to the one-way analysis of variance F test. Rev. Educ. Res. 66: 579-619.
 
# Failure of non-homogeneous variances are more severe.
# now using the Levene result to decide if transformation is needed.

to_transform <- pre.tests[pre.tests$Leve_fail == TRUE, ]
to_transform <- to_transform[, c("variable", "Stage")]

nrow(to_transform)
length(sort(unique(interaction(to_transform$variable, to_transform$Stage, drop = TRUE))))

yitpi.to_transform <- yitpi.melt[
                  interaction(yitpi.melt$variable, yitpi.melt$Stage) %in% 
                  interaction(to_transform$variable, to_transform$Stage), ]

length(sort(unique(interaction(yitpi.to_transfrom$variable, 
                               yitpi.to_transfrom$Stage, drop = TRUE))))

# the "good" data that do not need transformation
yitpi.no_transform <- yitpi.melt[!
                  interaction(yitpi.melt$variable, yitpi.melt$Stage) %in% 
                  interaction(to_transform$variable, to_transform$Stage), ]


# calling Myhist function from helper script
myhist <- MyHist(yitpi.test, 
       label = yitpi.test$variable,
       yaxis = "value",
       treatment_sep_a = "CO2",
       treatment_sep_b = NA,
       facet_var_a = "Stage",
       facet_var_b = ".")
myhist

# Boxplots in a loop for YitpiN0 only
my.hist <- dlply(yitpi.to_transform,
                  .(variable),
                  function(x) {
                  MyHist(dataframe = x, 
                         label     = x$variable,
                         yaxis = "value", 
                         treatment_sep_a = "CO2", 
                         treatment_sep_b = NA, 
                         facet_var_a = "Ord.Environment", 
                         facet_var_b = "Stage"
                         )})

# orig line: MyBoxplot(x, x$variable, "Ord.Environment", "value", "CO2", "CO2", "Stage", ".")})

pdf("Horsham_Walpeup_Histograms_for_non_homogeneous_paras.pdf")
        print(my.hist)
dev.off()



# create a little data set to test the boxcox function
myvalue <- sample(-25:25, 20)
mydata <- as.data.frame(x = myvalue)
mydata$CO2_treat <- "aCO2"
mydata$CO2_treat[11:20] <- "eCO2"
mydata$CO2_treat <- as.factor(mydata$CO2_treat)
mydata$N <- "N0"
mydata$N[11:20] <- "N+"
mydata$N <- as.factor(mydata$N)
# mydata

# extract a small test-parameter from yitpi.to_transform to test the boxcoc-Transformation
yitpi.test <- yitpi.to_transform[yitpi.to_transform$variable == "Yield..g.m2.", ]
yitpi.test <- yitpi.to_transform[yitpi.to_transform$variable == "Plant.wt..g.plant.", ]
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

out <- BoxcoxTrans(yitpi.test, yitpi.test$value, yitpi.test$CO2, yitpi.test$Environment)

# data frame with the transformed data
yitpi.transformed  <- ddply(yitpi.to_transform,
                    .(variable, Stage),
                    function(x){
                    BoxcoxTrans(x, x$value, x$CO2, x$Environment)
                    })

# checking the success of the transformtaion
after.tests <- ddply(yitpi.transformed,
                   .(variable, Stage),
                   function(x) {
                 
                 out <- try(leveneTest(trans_value ~ CO2 * Environment,
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
yitpi.no_transform$trans_value <- yitpi.no_transform$value
yitpi.all_transformed <- rbind(yitpi.no_transform, yitpi.transformed)

bad.paras <- interaction(after.tests$variable[after.tests$Leve_fail == TRUE],
                         after.tests$Stage[after.tests$Leve_fail == TRUE])

# export the "non-homogeneous" parameters
write.table(bad.paras,
            file = "Non-homogeneous_parameters.txt",
            row.names = FALSE,
            col.names = FALSE)
            
yitpi.all_transformed$inter <- interaction(yitpi.all_transformed$variable,
                                           yitpi.all_transformed$Stage)

yitpi.homogeneous <- yitpi.all_transformed[!(yitpi.all_transformed$inter %in% bad.paras), ]

# Run an anova
# run the analysis on all cases for YitpiN0
aov.out <- dlply(yitpi.homogeneous,
                 .(variable, Stage),
                 function(x) {
                 
                 out <- try(summary(aov(trans_value ~ CO2 * Environment,
                                    data = x,
                                    na.action = na.omit)))
                    
                 if (inherits(out, "try-error")) {
                      print("problem with fit")
                      aov.out <- "not testable"} 
                 else {
                      print("successful fit")
                      aov.out <- out}
                 })
sink("Horsham_Walpeup_Anova_results_after_transformation.txt")
        print(aov.out)
sink()
