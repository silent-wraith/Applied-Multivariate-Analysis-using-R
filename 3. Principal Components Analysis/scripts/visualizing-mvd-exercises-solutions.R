###############################################
#####    VISUALIZING MULTIVARIATE DATA    #####
#####       EXERCISES AND SOLUTIONS       #####
###############################################
library("MVA")
# install.packages("grid")
# library("grid")
library("lattice")

################# QUESTION #1

#1 Use the bivariate boxplot on the scatterplot 
# of pairs of variables ((temp, wind), (temp, precip), 
# (temp, predays)) in the air pollution data to 
# identify any outliers. Calculate the correlation 
# between each pair of variables using all the data 
# and the data with any identified outliers removed. 
# Comment on the results.

# ANSWER: Correlation does not always seem 
# to change when outliers are trimmed.
# It may increase or decrease depending
# on orientation of bivariate pairs and
# relative locations of the outliers.

# load the data
data("USairpollution", package = "HSAUR2")

# exclude SO2, manu, popul variables
names(USairpollution[,-c(1,3,4)])

# call a scatterplot matrix
pairs(USairpollution[,-c(1,3,4)])

##### TEMP and WIND

# extract variables from df
x <- USairpollution[, c("temp", "wind")]
# draw bivariate boxplot (must load MVA package)
bvbox(x, mtitle = "", 
      xlab ="temperature", 
      ylab="wind")

# add text, abbreviated city names
with(USairpollution, 
     text(temp,wind,
          cex = 0.6,
          labels=abbreviate(row.names(USairpollution))))

# Miami and Phoenix are outliers: correlation
# reduced from -0.35 to -0.26

# cor() computes pearson product corr...all data
with(USairpollution, 
     cor(temp, wind))
# [1] -0.3497396
# with Miami and Phoenix removed
with(USairpollution[-c(23,31),], 
     cor(temp, wind))
# [1] -0.2587808

rownames(USairpollution)

##### TEMP and PRECIP
names(USairpollution)

# extract variables from df
x <- USairpollution[, c("temp", "precip")]

# draw bivariate boxplot (must load MVA package)
bvbox(x, mtitle = "", 
      xlab ="temperature", 
      ylab="precipitation")

# add text, abbreviated city names
with(USairpollution, 
     text(temp,precip,
          cex = 0.6,
          labels=abbreviate(row.names(USairpollution))))

# find out what is what
rownames(USairpollution)

# Miami[23], Phoenix[31], Albuquerque[2],
# maybe Denver[12]
# are outliers: correlation
# increased from 0.39 to 0.657

# cor() computes pearson product corr...all data
with(USairpollution, 
     cor(temp, precip))
# [1] 0.386
# remove Miami, Phoenix, Albuquerque, Denver
with(USairpollution[-c(2,12,23,31),], 
     cor(temp, precip))

# corr increase to [1] 0.657

##### TEMP and PREDAYS
names(USairpollution)

# extract variables from df
x <- USairpollution[, c("temp", "predays")]

# draw bivariate boxplot (must load MVA package)
bvbox(x, mtitle = "", 
      xlab ="temperature", 
      ylab="predays")

# add text, abbreviated city names
with(USairpollution, 
     text(temp,predays,
          cex = 0.6,
          labels=abbreviate(row.names(USairpollution))))

# Miami[23], Phoenix[31]
# are outliers: correlation
# changes little

# cor() computes pearson product corr...all data
with(USairpollution, 
     cor(temp, predays))
# [1] -0.4302421 note is negative

# with Miami, Phoenix removed
with(USairpollution[-c(23,31),], 
     cor(temp, predays))
# [1] -0.4239283

######################### QUESTION #2

#2 Compare the chi-plots with the corresponding 
# scatterplots for the above pairs of variables in 
# the air pollution data. Do you think that there is 
# any advantage in the former?

## ANSWER: Former shows likely deviation from lack
## of independence as well as the correlation, so
## you retrieve more information from former.

##### TEMP and WIND

# scatterplot:
par(mfrow=c(1,2))
with(USairpollution, 
     plot(temp, wind, 
          cex.lab = 0.9))

# chi-plot:
with(USairpollution, 
     chiplot(temp, wind))

##### TEMP and PRECIP

# scatterplot:
par(mfrow=c(1,2))
with(USairpollution, 
     plot(temp, precip, 
          cex.lab = 0.9))

# chi-plot:
with(USairpollution, 
     chiplot(temp, precip))

##### TEMP and PREDAYS

# scatterplot:
par(mfrow=c(1,2))
with(USairpollution, 
     plot(temp, predays, 
          cex.lab = 0.9))

# chi-plot:
with(USairpollution, 
     chiplot(temp, predays))
