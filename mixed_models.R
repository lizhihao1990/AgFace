# example mixed models
## http://www.r-bloggers.com/linear-mixed-models-in-r/

library(MASS)
data(oats)
names(oats) = c('block', 'variety', 'nitrogen', 'yield')
oats$mainplot = oats$variety
oats$subplot = oats$nitrogen

## The nlme code for this analysis is fairly simple: response on the left-hand side of the tilde, followed by the fixed effects (variety, nitrogen and their interaction). Then there is the specification of the random effects (which also uses a tilde) and the data set containing all the data. Notice that 1|block/mainplot is fitting block and mainplot within block. There is no reference to subplot as there is a single assessment for each subplot, which ends up being used at the residual level.

library(nlme)
m1.nlme = lme(yield ~ variety*nitrogen,
                      random = ~ 1|block/mainplot,
                      data = oats)

summary(m1.nlme)
anova(m1.nlme)

# The syntax for lme4 is not that dissimilar, with random effects specified using a (1|something here) syntax. One difference between the two packages is that nlme reports standard deviations instead of variances for the random effects.

library(lme4)
m1.lme4 = lmer(yield ~ variety*nitrogen + (1|block/mainplot),
                       data = oats)
 
summary(m1.lme4)
anova(m1.lme4)

# regarding p-values
# Douglas Bates statement
# https://stat.ethz.ch/pipermail/r-help/2006-May/094765.html

# See lmerTest package
# http://cran.r-project.org/web/packages/lmerTest/lmerTest.pdf

# Option via markov chain monte carlo simulations:
# from  http://www.u.arizona.edu/~ljchang/NewSite/papers/LME4_HO.pdf

# You may have noticed that there are no p-values associated with the parameter es-
#timates from the model output. While the lme4 package does provide t values, the
#authors have admitted to not knowing how to calculate exact values and are perplexed
#as to how to best approximate the degrees of freedom in a mixed model framework,
#particularly with unbalanced designs and correlated random factors. In SAS there
#are apparently 6 different df approximations, which lead to different p-values. For
#example, how does one even go about calculating the number of parameters for a
#mixed model? In the simple model we used in the example, there are 6, fixed ef-
#fects values 1 random effect, and 1 value for the variance of the error term. But
#what about the 18 parameters that were calculated for each participant's intercept?
#The authors here have chosen to abstain from providing p-values, until they have
#developed a more accurate method with which they are more comfortable. Unfortu-
#nately, most of us work in areas where providing p-values is still customary, which
#makes this particularly frustrating. However, do not be discouraged there are two
#approaches which can be taken, both of which, unfortunately, suffer from their own
#respective problems. First, you can use the t value reported and approximate the de-
#grees of freedom by subtracting the number of observations - the number of fixed effects parameters - 1. This is the approach typically taken in standard
#linear models and happens to be the strategy adopted by SPSS, however, it is likely
#anticonservative, particularly when the sample size is small. An alternative approach
#is to use markov chain monte carlo simulations on the parameter estimates and cal-
#culate the p-values based on the confidence intervals of the empirically observed
#distributions. This can be accomplished using the mcmcsamp() function included in
#the lme4 package. This approach has been made easier with the pval.fnc()
#from the languageR package. We can use this approach to examine the p-values on our best 
#fitting model.

library(languageR)
pvals.fnc(m1.lme4)

# This is a promising approach to calculating p-values for mixed models. However, it is important to note that at this time the method only works on models with varying intercepts. There is no current implementation for models with correlated random effects, such as those with varying intercepts and slopes.

# Interpreting lmer results
# https://stat.ethz.ch/pipermail/r-sig-mixed-models/2008q2/000904.html

# Statsexchange:
# http://stats.stackexchange.com/questions/22988/significant-effect-in-lme4-mixed-model

# mixed-models FAQ r-sig-mixed-models 
# http://glmm.wikidot.com/faq

# difference between aov and lme
# http://stats.stackexchange.com/questions/14088/why-do-lme-and-aov-return-different-results-for-repeated-measures-anova-in-r/14185#14185
# Now, you should know that using aov for repeated measures is only appropriate if you believe that the correlation between all pairs of repeated measures is identical; this is called compound symmetry. (Technically, sphericity is required but this is sufficient for now.) One reason to use lme over aov is that it can handle different kinds of correlation structures.

# Book ressources:
# Andrzej Gałecki • Tomasz Burzykowski
# Linear Mixed-Effects Models Using R - A Step-by-Step Approach
# http://link.springer.com.ezp.lib.unimelb.edu.au/content/pdf/10.1007%2F978-1-4614-3900-4.pdf


