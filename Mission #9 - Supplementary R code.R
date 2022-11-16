# STAT E-100
# Mission 9

##########################################

# Reference:
# http://homepages.gac.edu/~anienow2/MCS_142/R/R-groupstats.html

# Average income as a function of Sex
with(Employed_ACS_recoded, aggregate(Employed_ACS_recoded$Income ~ Employed_ACS_recoded$Sex, FUN = mean))
# Median income as a function of Sex
with(Employed_ACS_recoded, aggregate(Employed_ACS_recoded$Income ~ Employed_ACS_recoded$Sex, FUN = median))
# Standard deviation of income as a function of Sex
with(Employed_ACS_recoded, aggregate(Employed_ACS_recoded$Income ~ Employed_ACS_recoded$Sex, FUN = sd))

library(mosaic) # Loads the mosaic package
favstats(Employed_ACS_recoded$Income ~ Employed_ACS_recoded$Sex)

t.test(Income ~ Sex, data = Employed_ACS_recoded)

##########################################

table(GSS2018$WRKSTAT)

# Full-time employment rate is approximately 83%:
# https://www.advisorperspectives.com/dshort/updates/2022/11/09/full-time-and-part-time-employment-a-deeper-look
# Part-time employment was approximately 17%:
# https://www.zippia.com/advice/part-time-job-statistics/

observed_counts <- c(123, # Group A
                     456, # Group B
                     789, # Group C
                     102) # Group D
res <- chisq.test(observed_counts, p = c(0.25, # Proportion for Group A
                                         0.25, # Proportion for Group B
                                         0.25, # Proportion for Group C
                                         0.25)) # Proportion for Group D
res
res$expected

