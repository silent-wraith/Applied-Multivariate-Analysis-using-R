###############################################
#####   Water Voles MD Scaling Exercise   #####
###############################################

# These exercises relate to a study of water 
# voles (genus Arvicola) with the aim to compare 
# British populations of these animals with those 
# in Europe in order to investigate whether more 
# than one species might be present in Britain.
# The original data consisted of observations 
# of the presence or absence of 13 characteristics 
# (hint: k=13) in about 300 water vole skulls 
# arising from six British populations and eight 
# populations from the rest of Europe.

# we need the MVA package
library("MVA")

# Use the distance matrix data "watervoles" 
# provided in the HSAUR2 package. Find the 
# classical scaling solution and compute the 
# two criteria for assessing the required number 
# of dimensions as described above. Note that 
# the two criteria are not in agreement. Proceed 
# according to the second fit index and plot a 
# two-dimensional solution by extracting the 
# coordinates from the points elements of the 
# voles_mds object.

# This R code finds the classical scaling solution
# and computes the two criteria for assessing the
# required number of dimensions:

# Access data in HSAUR2 package:
data("watervoles", package = "HSAUR2")
voles_mds <- cmdscale(watervoles, k = 13, eig = TRUE)
voles_mds$eig

# Note that some of the eigenvalues are negative.
# We compute the criterion Pm1 by:
cumsum(abs(voles_mds$eig))/sum(abs(voles_mds$eig))

# and the criterion Pm2 by:
cumsum((voles_mds$eig)^2)/sum((voles_mds$eig)^2)

# The two criteria for judging the number of 
# dimensions are quite different. The second
# criterion suggests that two dimensions are
# appropriate, while the first criterion indicates
# that three of even four dimensions are needed.

# So we go with the second fit index and plot a 
# two-dimensional solution by extracting the 
# coordinates from the points elements of the 
# voles_mds object:
x <- voles_mds$points[,1]
y <- voles_mds$points[,2]
plot(x, y, xlab = "Coordinate 1", 
     ylab = "Coordinate 2",
     xlim = range(x)*1.2, type = "n")
text(x, y, labels = colnames(watervoles), 
     cex = 0.7)

# Interpret the plotted two-dimensional solution 
# from classical multidimensional scaling of the 
# distance matrix for water vole populations.

# It appears that the six British populations 
# are close to populations living in the Alps,
# Yugoslavia, Germany, Norway, and Pyrenees I
# of the species Arvicola terrestris) but rather 
# distant from the populations in Pyrenees II,
# North Spain and South Spain (species Arvicola 
# sapidus). This result implies that Arvicola
# terrestris might be present in Britain but is
# less likely that this is so for Arvicola sapidus. 

# But here, as the two-dimensional fit may not
# explain what is needed to represent the observed
# distances, we shall investigate the solution 
# in a little more detail using the "minimum
# spanning tree.

# The minimum spanning tree is defined as follows. 
# Suppose n points are given (possibly in many
# dimensions). Then a tree spanning these points 
# (that is, a spanning tree) is any set of straight 
# line segments joining pairs of points such that
# - 0 closed loops occur,
# ˆ- every point is visited at least one time, and
# - the tree is connected (has paths between each
# pair of points)

# We use function mst() from package ape to plot
# minimum spanning tree on the two-dimensional
# scaling solution:
install.packages("ape")
library("ape")
st <- mst(watervoles)
plot(x, y, xlab = "Coordinate 1", 
     ylab = "Coordinate 2",
     xlim = range(x)*1.2, type = "n")
for (i in 1:nrow(watervoles)) {
  w1 <- which(st[i, ] == 1)
  segments(x[i], y[i], x[w1], y[w1])
}
text(x, y, labels = colnames(watervoles), 
     cex = 0.7)

# The plot indicates, for example, that the 
# apparent closeness of the populations in Germany
# and Norway, suggested by the points representing 
# them in the MDS solution does not accurately
# reflect their calculated dissimilarity; the
# links of the minimum spanning tree show that 
# the Aberdeen and Elean Gamhna populations are 
# actually more similar to the German water voles 
# than to those from Norway. This suggests that
# the two-dimensional solution may not give an
# adequate representation of the whole distance 
# matrix.