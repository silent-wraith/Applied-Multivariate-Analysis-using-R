###############################################
#####    VISUALIZING MULTIVARIATE DATA    #####
###############################################
library("MVA")

# Need Multivariate graphics lattice package.
# You will need to install if have not yet

# install.packages("lattice")

# Also make sure you have grid package installed
# and loaded:

# install.packages("grid")
# library("grid")

library("lattice")

##### Datasets

# USairpollution Dataset
# air pollution in US cities
data("USairpollution", package = "HSAUR2")
?USairpollution
##### Scatterplots are standard for for representing
# continuous bivariate data, but can be used to
# accomodate other variables

# look at basic scatterplot of manu and popul

# character variables for labels:
mlab <- "Manufacturing enterprises with 20 or more workers"
plab <- "Population size (1970 census) in thousands"

# call plot() high-level function, note formula
# describing variables to be plotted
plot(popul ~ manu, data = USairpollution, 
     xlab = mlab, ylab = plab)

# Can see outliers, will come back to this

# Let's look at marginal distributions
# with rug plots, they show where actual
# points lie

plot(popul ~ manu, data = USairpollution, 
     xlab = mlab, ylab = plab)
# put rug symbols on horizontal
rug(USairpollution$manu, side = 1)
# puts rug symbols on vertical
rug(USairpollution$popul, side = 2)

# Now we show marginal distributions
# another way

# first divide up the plotting area into
# three areas so can draw a scatterplot,
# histogram and boxplot

?layout

# matrix shows locations of plots
layout(matrix(c(2, 0, 1, 3), 
              nrow = 2, byrow = TRUE),
       # widths of columns (plots)
       # heights of rows (plots)
       widths = c(2, 1), heights = c(1, 2), 
       # relates to dimensions
       respect = TRUE)

# stretches range on x-axis
xlim <- with(USairpollution, 
             range(manu)) * 1.1

# sets up plot but does not draw anything
plot(popul ~ manu, data = USairpollution, 
     cex.lab = 0.9, xlab = mlab, ylab = plab, 
     type = "n", xlim = xlim)

# adds text to plot, we use abbreviated names
# of city as plotting symbols
with(USairpollution, text(manu, popul, cex = 0.6,
     labels=abbreviate(row.names(USairpollution))))

# draws histogram as marginal distribution
# of manufacturing; with() function allows us
# to extract variables from dataframe...hist()
# and boxplot() are evaluated INSIDE of the 
# dataframe USairpollution
with(USairpollution, 
     hist(manu, main = "", xlim = xlim))

# draws boxplot as marginal distribution
# population size of city
with(USairpollution, boxplot(popul))

# We see that outlying points show themselves
# in both the scatterplot of the variables AND
# in each marginal distribution. Most extreme
# outlier is Chicago, also Philly and Detroit

# So big cities have big manufacturing facil.

### The Bivariate Boxplot

# Let's look for more formal and objective
# method for labelling observations as outliers

# Bivariate boxplot is a 2D analogue of boxplot
# for univariate data, can show distributional
# properties of the data in identifying 
# possible outliers.

# Is based on calculating 'robust' measures of
# location, scale and correlation. It consists
# of pair of concentric ellipses, on is the
# 'hinge' and the other the 'fence' which
# delineates outliers

# Regression lines of both y on x and x on y
# with intersection showing bivariate location
# estimator; Acute angle between regression
# lines will be small for a large absolute
# value of correlations and large for a small
# one.

# Create scatterplot of manu and popul including
# bivariate boxplot

# going to see if these cities are outliers:
outcity <- match(lab <- c("Chicago", "Detroit", 
    "Cleveland", "Philadelphia", "Houston"), 
                 rownames(USairpollution))
# extract manu and popul variables from df
x <- USairpollution[, c("manu", "popul")]
# draw bivariate boxplot (must load MVA package)
# middle circle contains 50% of points; lines
# are inverted regression lines of man and pop
bvbox(x, mtitle = "", xlab = mlab, ylab = plab)
# lable those five cities
text(x$manu[outcity], x$popul[outcity], 
     labels = lab, cex = 0.7, 
     pos = c(2, 2, 4, 2, 2))

# Chicago, Philly, Detroit and Cleveland are
# outliers, but not Houston since is on the
# 'fence'

# Suppose now we want to calculate correlation
# between manu and popul, should always look
# at scatterplot first because outliers can
# distort correlation coefficient.

# So we exclude those four cities and calculate

# cor() computes pearson product corr
with(USairpollution, cor(manu, popul))

# four outlier cities matched with dataframe
outcity <- match(c("Chicago", "Detroit", 
                   "Cleveland", "Philadelphia"),
                 rownames(USairpollution))

# compute the correlations without them
with(USairpollution, cor(manu[-outcity], 
                         popul[-outcity]))

# correlation goes from 0.96 to 0.80

### The Convex Hull of Bivariate Data

# alternative to using scatterplot with
# bivariate boxplot is to calculate corr without
# outliers is 'convex hull trimming' which
# allows 'robust estimation' of the correlation

# Convex hull of a set of bivariate observations
# is 'vertices of smallest convex polyhedron in
# variable space within which on upon which all
# data points lie'

# Removing points lying on the convex hull can
# eliminate isolated outliers without disturbing
# the general shape of the bivariate distrib

# Calculate robust est of corr from remaining obs

# We find convex hull of our data

?chull

# gives us observations on the hull
(hull <- with(USairpollution, 
              chull(manu, popul)))

# this just plots all points:
with(USairpollution, 
     plot(manu, popul, pch = 1, 
          xlab = mlab, ylab = plab))

# plots the polygon
with(USairpollution, 
     # reference hull points with subscripts:
     polygon(manu[hull], popul[hull],
             # number and angle of lines:
             density = 15, angle = 30))

# Calculate correlation and removing points
# on convex hull
with(USairpollution,
     cor(manu[-hull],
         popul[-hull]))

# corr is 0.9225, higher than correlation 0.80
# removing outliers from bivariate boxplot

#### The Chi-Plot
# Augments scatterplot to highlight independence.
# Chi-plot transforms x, y values in scatterplot
# into values to show deviations from independence
# using chi-square tables

# scatterplot:
par(mfrow=c(1,2))
with(USairpollution, plot(manu, popul, 
                          xlab = mlab, 
                          ylab = plab, 
                          cex.lab = 0.9))

# chi-plot:
with(USairpollution, chiplot(manu, popul))

# Points that are NOT in the horizontal band
# indicate a departure from independence.

##### The Bubble and Other Glyph Plots
# To include more than 2 variables in scatterplot

# Bubble plot is the simplest: third variable
# represented by radii of circles proportioned
# to values

# Bubble plot of temp, wind, SO2:

ylim <- with(USairpollution, 
             # spreads out y axis:
             range(wind)) * c(0.95, 1)
#draws scatterplot:
plot(wind ~ temp, data = USairpollution, 
     xlab="Average annual temperature (Fahrenheit)",
     ylab="Average annual wind speed (m.p.h.)", 
     pch=10,
     ylim=ylim)
# adds bubbles:
with(USairpollution, 
     # size of circles tied to SO2:
     # centered on temp and wind:
     symbols(temp, wind, circles = SO2,
             # scales size of symbols and
             # adds symbols to existing plot
             inches = 0.5, add = TRUE))

?symbols

# Cities with moderate annual temps and wind
# speeds have greatest air pollution, but may
# be more variables involved

# Include all variables in basic temp and wind
# scatterplot by replacing circles with 5-sided
# stars with lengths of each side representing
# another variable

# spatial positions of cities in wind, temp
# scatterplot are combined with star repre of
# five other variables:
plot(wind ~ temp, data = USairpollution,
     xlab="Average annual temperature (Fahrenheit)",
     ylab="Average annual wind speed (m.p.h.)", 
     pch = 10, ylim=ylim)
with(USairpollution,
     # draws stars without temp or wind
    stars(USairpollution[,-c(2,5)], 
          locations = cbind(temp, wind),
          labels = NULL, add = TRUE, cex = 0.5))

# But stars plot fails to show anything conclusive

# Bubble and stars plots examples of symbol
# or 'glyph plots' in which data values control
# symbol parameters

# Could also simply represent all variables as
# side of 7-sided star and arrange resulting 
# stars in rectangular array
stars(USairpollution, cex = 0.55)

# N.O., Miami, Jax, Atl have similar shapes with
# distinctive higher average annual temps

##### The Scatterplot Matrix
# plots all pairs of numeric variables

# use enlarged dot symbols; can see outliers,
# non-linear precip with predays and SO2
pairs(USairpollution, pch = ".", cex = 3)

# strong linear manu and popul. Scatterplot
# matrix helpful for interpreting corr matrix:
# look at SO2 row with scatter matrix
round(cor(USairpollution), 4) 

# add regression lines for linear fits
pairs(USairpollution, 
      panel = function (x, y, ...) {
          points(x, y, ...)
          abline(lm(y ~ x), col = "grey")
      }, pch = ".", cex = 1.5)

##### Enhancing the Scatterplot with
##### Estimated Bivariate Densities

# Kernel Density Estimators

### SEE SLIDES FOR MORE INFORMATION

# Kernel function determines shape of "bumps"
# placed at observations; window width 'h' 
# determines their width

# For some grid x, we plot 3 kernel functions:

#1 rectangular:
rec <- function(x) (abs(x) < 1) * 0.5

#2 triangular
tri <- function(x) (abs(x) < 1) * (1 - abs(x))

# gaussian:
gauss <- function(x) 1/sqrt(2*pi) * exp(-(x^2)/2)

# set of x values:
x <- seq(from = -3, to = 3, by = 0.001)

# plot the rectangular
plot(x, rec(x), type = "l", ylim = c(0,1), 
     lty = 1, ylab = expression(K(x)),
     main = "THREE COMMONLY USED KERNEL FUNCTIONS")

# add lines for triangular
lines(x, tri(x), lty = 2)

# add lines for gaussian:
lines(x, gauss(x), lty = 3)

#add legend:
legend("topleft", legend = c("Rectangular", 
                             "Triangular", 
                             "Gaussian"), 
       lty = 1:3, 
       title = "kernel functions", 
       bty = "n")

# Create vector x
x <- c(0, 1, 1.1, 1.5, 1.9, 2.8, 2.9, 3.5);x

# how many elements?:
n <- length(x);n

# create larger vector from x for grid
xgrid <- seq(from = min(x) - 1, 
             to = max(x) + 1, by = 0.01) 
xgrid

# h determines width of bumps
h <- 0.4

# make bumps gaussian kernel function
bumps<-sapply(x,function(a)gauss((xgrid-a)/h)/(n*h))
bumps
nrow(bumps)
ncol(bumps)

# plot of individual bumps and their sum using
# kernel density function f. Bumps are centered
# at their observations

# this plots sums of rows of bumps
plot(xgrid, rowSums(bumps), 
     ylab = expression(hat(f)(x)),
     type = "l", xlab = "x", lwd = 2,
     main="GAUSSIAN BUMPS SUMMED, using KERNEL DENSITY f")
# rug shows where individual bumps originate
rug(x, lwd = 2)
# call function to draw bumps
out <- apply(bumps,2,function(b) lines(xgrid, b))
# reset original graphics state
par(op)

# Can be extended into two dimensions for bivariate
# cases. Here we use an Epanechnikov kernel for
# grid between (-1.1,-1.1) and (1.1,1.1).

### SEE SLIDES

# function for Epanechnikov kernel
epa <- function(x, y) 
  ((x^2 + y^2) < 1) * 2/pi * (1 - x^2 - y^2)
# sequence for grid
x <- seq(from = -1.1, to = 1.1, by = 0.05)
# apply the function for the kernel
epavals <- sapply(x, function(a) epa(a, x))
# persp() draws 3D wireframes and others
persp(x = x, y = x, z = epavals, 
      xlab = "x", ylab = "y", 
      zlab = expression(K(x, y)), 
      theta = -35, axes = TRUE, 
      box = TRUE)

#####  Three Dimensional Plots
# To extend bivariate

# Let's enhance scatterplot with estimated bivariate
# density using CYGOB1 data: Energy output and surface
# temperature of star cluster CYG OB1.

# Will be a scatterplot of density enhanced by
# the contours of estimated bivariate density using
# using function bkde2D()

library("KernSmooth")
CYGOB1
?CYGOB1

# scatterplot enhanced with bivariate density
CYGOB1d <- bkde2D(CYGOB1, 
                  bandwidth = sapply(CYGOB1, 
                                     dpik))
# CYGOB1d are bivariate densities
CYGOB1d
# plot original data
plot(CYGOB1, xlab = "log surface temperature",
             ylab = "log light intensity",
     main="LOGS of Light Intensities and Surface Temps")
# add contours for bivariate densities
contour(x = CYGOB1d$x1, y = CYGOB1d$x2, 
        z = CYGOB1d$fhat, add = TRUE)

# Is scatterplot of log light intensity and log
# of surface temp for stars in CYG OB1
# showing estimated bivariate density

# Use persp() to draw 3D wireframe
# Also known as a "Perspective Plot"
persp(x = CYGOB1d$x1, y = CYGOB1d$x2, 
      z = CYGOB1d$fhat,
      xlab = "log surface temperature",
      ylab = "log light intensity",
      zlab = "density",
      main="Perspective Plot of CYGOB1")

### TRELLIS GRAPHICS
# Approach to looking at high dimensional 
# structure in data using 1D, 2D, 3D plots

# 3D Plot of body measurements data, we recreate

##############################################
### Recreate measure Data
##############################################
measure <-
  structure(list(V1 = 1:20, 
                 V2 = c(34L, 37L, 38L, 36L, 38L, 
                        43L, 40L, 38L, 40L, 41L, 
                        36L, 36L, 34L, 33L, 36L, 
                        37L, 34L, 36L, 38L, 35L), 
                 V3 = c(30L, 32L, 30L, 33L, 29L, 
                        32L, 33L, 30L, 30L, 32L, 
                        24L, 25L, 24L, 22L, 26L, 
                        26L, 25L, 26L, 28L, 23L), 
                 V4 = c(32L, 37L, 36L, 39L, 33L, 
                        38L, 42L, 40L, 37L, 39L, 
                        35L, 37L, 37L, 34L, 38L, 
                        37L, 38L, 37L, 40L, 35L)), 
            .Names = c("V1", "V2", "V3", "V4"), 
            class = "data.frame", 
            row.names = c(NA, -20L))
measure <- measure[,-1]
names(measure) <- c("chest", "waist", "hips")
measure$gender <- gl(2, 10)
levels(measure$gender) <- c("male", "female")

# Need to load lattice if not done already
# library("lattice")

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, 
       breaks[-1], y, col="grey", ...)
}
pairs(measure[, c("chest", "waist","hips")],
      diag.panel = panel.hist,
      panel = function (x,y) {
        data <- data.frame(cbind(x,y))
        par(new = TRUE)
        den <- bkde2D(data, 
                      bandwidth=sapply(data,dpik))
        contour(x = den$x1, y = den$x2, 
                z = den$fhat, axes = FALSE)
      })

# 3D scatterplot of body measurements.
# Use points for males, triangles females
library("scatterplot3d")
with(measure, scatterplot3d(chest, waist, hips,
     pch =(1:2)[gender],type="h",angle = 55))

# 3D scatterplot of airpollution data
with(USairpollution, 
    scatterplot3d(temp, wind, SO2, 
                  type = "h",
                  angle = 55))

# panel plots for light wind and high wind
plot(xyplot(SO2 ~ temp| cut(wind, 2), 
            data = USairpollution))

# Shows that in cities with light winds,
# air pollution decreases with increasing
# temperature, but in cities with high winds
# are pollution does not appear to be
# strongly related with increasing temp

# Here have three dimensional plot of temp,
# wind, and precip for four levels of SO2.
# Are too few points in each panel
pollution <- with(USairpollution, 
                  equal.count(SO2,4))
plot(cloud(precip ~ temp * wind | pollution, 
           panel.aspect = 0.9,
           data = USairpollution))

# Earthquakes-latitude and longitude
# (location) by depth near Fiji
plot(xyplot(lat ~ long| cut(depth, 3), 
            data = quakes, 
            layout = c(3, 1), 
            xlab = "Longitude", 
            ylab = "Latitude"))

# Locations of earthquakes different at
# the various depths

### More Complex 3D plots from lattice

# Scatterplots of latitude and longitude
# conditioned on magnitude, with depth
# coding by shading
Magnitude <- with(quakes, equal.count(mag, 4))
depth.ord <- with(quakes, rev(order(depth)))
quakes.ordered <- quakes[depth.ord,]
depth.breaks <- with(quakes.ordered, 
                     do.breaks(range(depth),50))
quakes.ordered$color<-level.colors(quakes.ordered$depth,
                                   at=depth.breaks,
                                   col.regions=grey.colors)
plot(xyplot(lat ~ long | Magnitude, 
            data = quakes.ordered,
            aspect = "iso", 
            groups = color, 
            cex = 2, col = "black",
       panel = function(x, y, groups, ..., subscripts) {
           fill <- groups[subscripts]
           panel.grid(h = -1, v = -1)
           panel.xyplot(x, y, pch = 21, 
                        fill = fill, ...)
       },
       legend =
       list(right =
            list(fun = draw.colorkey,
                 args=list(key=list(col=gray.colors,
                                    at = depth.breaks),
                           draw = FALSE))),
            xlab = "Longitude", 
            ylab = "Latitude"))

# cloud plot of latitude and longitude 
# conditioned on magnitude
plot(cloud(depth ~ lat * long | Magnitude, 
           data = quakes,
           zlim = rev(range(quakes$depth)),
           screen = list(z = 105, x = -70), 
           panel.aspect = 0.9,
           xlab = "Longitude", 
           ylab = "Latitude", 
           zlab = "Depth"))
