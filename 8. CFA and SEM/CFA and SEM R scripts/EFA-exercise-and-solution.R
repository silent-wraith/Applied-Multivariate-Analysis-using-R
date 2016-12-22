##########################################
###     EXPLORATORY FACTOR ANALYSIS    ###  
###             EXERCISE               ###
##########################################

# The correlation matrix given below are from
# scores of 220 boys in six school subjects:
# (1) French; (2) English; (3) History;
# (4) Arithmetic; (5) Algebra and (6) Geometry

# Find the two-factor solution from an ML
# factor analysis. Interpret the factor loadings.
# Then plot these derived loadings and interpret.
# Finally, find an non-orthogonal rotation that
# allows easier interpretation of the results
# looking at the factor loadings directly, without
# the "visual utility" that is afforded by plotting
# the two-factor solution first.

# French     1.00
# English    0.44 1.00
# History    0.41 0.35 1.00
# Arithmetic 0.29 0.35 0.16 1.00
# Algebra    0.33 0.32 0.19 0.59 1.00
# Geometry   0.25 0.33 0.18 0.47 0.46 1.00


library("MVA")

# Create the grades correlation matrix:
gr <- c(0.44,
        0.41, 0.35,
        0.29, 0.35, 0.16,      
        0.33, 0.32, 0.19, 0.59,      
        0.25, 0.33, 0.18, 0.47, 0.46)      
grades <- diag(6) / 2
grades[upper.tri(grades)] <- gr
grades <- grades + t(grades)  
rownames(grades) <- colnames(grades) <- 
  c("French","English","History","Arithmetic","Algebra","Geometry")

# So we go with a two-factor solution
# which is as follows using varimax:
(s <- factanal(covmat = grades, 
               factors = 2, 
               method = "mle", 
               n.obs = 220))

# Trying to interpret the factor loadings, it looks
# like arithmetic and algebra dominate the first
# factors and French dominates the second. This
# interpretation does not necessarily make
# "intuitive sense".

# Look at structure of the
# outputted object
str(s)

# We want to plot these columns
s$loadings[,1:2]

# We can plot them like this:
plot(s$loadings[,1], s$loadings[,2], 
     type = "n", xlab = "Factor 1", 
     ylab = "Factor 2",
     main="VARIMAX (ORTHOGONAL) ROTATION")
text(s$loadings[,1], s$loadings[,2], 
     rownames(grades), cex = 1.3)

?factanal
# From the plot, it is pretty evident
# that the 'math skills'(Geometry, Algebra and
# Arithmetic) dominate the first factor,
# whereas 'non-math skills' (History, French 
# and English) dominate the second factor.

# The factanal() function only allows two
# functions for rotation, varimax, which is the
# default and is orthogonal, and promax, which is
# oblique. Also, I note on the Internet that
# varimax is not recommended (do not know why)

# We can try the oblique rotation with promax:
(s2 <- factanal(covmat = grades, 
                factors = 2, 
                rotation = "promax",
                method = "mle", 
                n.obs = 220))

# And plot them again:
plot(s2$loadings[,1], s2$loadings[,2], 
     type = "n", xlab = "Factor 1", 
     ylab = "Factor 2",
     main="PROMAX (OBLIQUE) ROTATION")
text(s2$loadings[,1], s2$loadings[,2], 
     rownames(grades), cex = 1.5)

# There is very little difference in the two plots
# but now looking at the factor loadings, it is
# much more obvious that the math skills dominate
# the first factor and the "non-math skills"
# dominate the second factor.