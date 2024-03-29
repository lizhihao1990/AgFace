# BoxCox Transformation
# returns the original dataframe with an additional column with boxcox-transfromed values.
# The lambda parameter of the box-cox transformatin gets chosen dependent on a test of homogeneity of variances

# Test for homogeneity of variances is using at least two variables!!

BoxcoxTrans <- function(data, value, fac1, fac2, fac3 = NA, fac4 = NA) {
        ## Takes a data frame, tests homogeneity of variances,
        ## transforms the data using bcPower or yjPower,
        ## and returns a data frame with the original and the transformed values
           require(car)
           require(plyr)

           lambdaseq <- seq(from = -2, to = 2, by = 0.1)
           
                     
        ## check if data is completely missing
           value_length <- length(value)
           value_missing_length <- sum(is.na(value))
           
           if (value_missing_length < value_length) {
           
           min_value <- min(value, na.rm = TRUE)
           
           out <- ldply(lambdaseq,
                function(x) {
                                
                if(is.na(fac3)) {
                if (min_value > 0) {
                        lev.out <- leveneTest(bcPower(value, x) ~ fac1 * fac2)
                        }
                
                else {
                        lev.out <- leveneTest(yjPower(value, x) ~ fac1 * fac2)
                        }
               }
               
              else {
                if (is.na(fac4)) {
                if (min_value > 0) {
                        lev.out <- leveneTest(bcPower(value, x) ~ fac1 * fac2 * fac3)
                        }
                else {
                        lev.out <- leveneTest(yjPower(value, x) ~ fac1 * fac2 * fac3)
                        }
               
              } else {
                 if (min_value > 0) {
                        lev.out <- leveneTest(bcPower(value, x) ~ fac1 * fac2 * fac3 * fac4)
                        }
                else {
                        lev.out <- leveneTest(yjPower(value, x) ~ fac1 * fac2 * fac3 * fac4)
                        }
              }
              }
              
                lev.p_value <- lev.out$`Pr(>F)`[1]
                #print(x)
                #print(lev.p_value)
                res.table <- data.frame(cbind(lambda = x, p_value = lev.p_value))
                return(res.table)
                })
           
           
           my_max <- out$lambda[which.max(out$p_value)]
           
           if (max(out$p_value) < 0.05) {
                print("Not homogeneous")}
           
           # Transform the original data to achieve homogeneous variances
           # with the lambda that provides most "homogeneous" result
           if (min_value > 0) {
                   trans_data <- bcPower(value, my_max)}
           else {
                   trans_data <- yjPower(value, my_max)
           }   
           data$trans_value <- trans_data
           return(data)
           }
        
        # else case for all cases missing
        else {
           data$trans_value <- NA
           return(data) 
           }   
        }
# Example usage:

# create a little data set to test the boxcox function
#myvalue <- sample(-25:25, 20)
#mydata <- as.data.frame(x = myvalue)
#mydata$CO2_treat <- "aCO2"
#mydata$CO2_treat[11:20] <- "eCO2"
#mydata$CO2_treat <- as.factor(mydata$CO2_treat)
#mydata$N <- "N0"
#mydata$N[11:20] <- "N+"
#mydata$N <- as.factor(mydata$N)
#mydata$Irri <- "Rain"
#mydata$Irri[11:20] <- "Supp"
#mydata$Irri <- as.factor(mydata$Irri)
#mydata$Loc  <- "Melbourne"
#mydata$Loc[11:20] <- "Sydney"
#mydata$Loc <- as.factor(mydata$Loc)
#mydata
 
# add a missing value
# mydata$myvalue[2] <- NA

# only missing values
# mydata$myvalue <- NA

#out <- BoxcoxTrans(mydata, mydata$myvalue, mydata$CO2_treat, mydata$N)
#out <- BoxcoxTrans(mydata, mydata$myvalue, mydata$CO2_treat, mydata$N, NA)
#out <- BoxcoxTrans(mydata, mydata$myvalue, mydata$CO2_treat, mydata$N, mydata$Irri)
#out <- BoxcoxTrans(mydata, mydata$myvalue, mydata$CO2_treat, mydata$N, mydata$Irri, mydata$Loc)
#out <- BoxcoxTrans(yitpi.test, yitpi.test$value, yitpi.test$CO2, yitpi.test$Environment)

# data frame with the transformed data
# yitpi.transformed  <- ddply(yitpi.to_transform,
#                    .(variable, Stage),
#                    function(x){
#                    BoxcoxTrans(x, x$value, x$CO2, x$Environment)
#                    })
