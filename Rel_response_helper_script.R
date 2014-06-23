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
        resp.mean <- mean(response.values, na.rm = TRUE)
        resp.sd   <-   sd(response.values, na.rm = TRUE)
        
        # Glenn changed his mind regarding the calculation of the relative response
        # in April 2014
        # response.values <- (resp.mean - ref.mean) / ref.mean * 100
        # now just a ratio
        response.values <- resp.mean / ref.mean
        
        # calculate relative standard deviation - to do?
        ## Mean value A +- sdA, mean value B +- sdB and we want X = A/B, the new SD
        ##    SD = X * sqrt( (sdA/A)**2 + (dB/B)**2)
        ## for the relative difference, the sd calculation is more complex
        ## SD = 100**2 sd((B/A) - 1) = 100**2 SD(B/A)
        ## SD = sqrt(variance)
        #res.sd <- 100^2 * sqrt(var(response.values/ref[, value_col]))

        ##res.sd <- 100^2 * (sd(response.values, na.rm = TRUE)/sd(ref[, value_col], na.rm = TRUE))
        # if there is no difference involved
        res.sd <- resp.mean / ref.mean * sqrt( (resp.sd/resp.mean)^2 + (ref.sd/ref.mean)^2) 
        
        
        # prepare data frame to return 
        out <- data.frame(reference_mean_abs     = ref.mean,
                          reference_sd_abs       = ref.sd, 
                          response_mean_abs      = resp.mean,
                          response_sd_abs        = resp.sd,
                          response_mean_relative = response.values,
                          response_sd_relative   = res.sd)
        
        return(out)
}


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

## testing the test case
## CalcPercent(mydata, "CO2_treat", "myvalue", "aCO2")

# Calculate the relative response to eCO2 for each variable, stage and environment.
#rel.response <- ddply(yj.melt,
#                     .(Irrigation, Year, Irrigation, variable, Stage),
#                     function(x)
#                     CalcPercent(x, "CO2", "value", "aCO2"))

# rename the variable to inidate these are relative differences
#rel.response$variable <- gsub("$", "_rel_diff_to_aCO2", rel.response$variable)
