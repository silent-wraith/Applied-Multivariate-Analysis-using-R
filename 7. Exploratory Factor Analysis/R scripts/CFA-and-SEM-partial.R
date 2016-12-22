##########################################
###    CONFIRMATORY FACTOR ANALYSIS    ###  
###             AND SEM                ###
##########################################

# With CFA particular manifest variables are
# allowed to relate to particular factors while
# other manifest variables are constrained to
# have zero loadings on some of the factors.

library("MVA")

# Confirmatory factor analysis models are a 
# subset of a more general approach to modeling
# latent variables known as structural equation 
# modeling or covariance structure modeling. 

# Such models allow both response and explanatory
# latent variables linked by a series of linear 
# equations. Although more complex than confirma-
# tory factor analysis models, the aim of SEM is 
# essentially the same, namely to explain the 
# correlations or covariances of the observed 
# variables in terms of the relationships of 
# these variables to the assumed underlying 
# latent variables and the relationships 
# postulated between the latent variables 
# themselves.

# Create ability data for CFA
install.packages("sem")
library("sem")
ab <- c(0.73,
        0.70, 0.68,
        0.58, 0.61, 0.57,      
        0.46, 0.43, 0.40, 0.37,      
        0.56, 0.52, 0.48, 0.41, 0.72)      
ability <- diag(6) / 2
ability[upper.tri(ability)] <- ab
ability <- ability + t(ability)  
rownames(ability) <- colnames(ability) <- 
    c("SCA","PPE","PTE","PFE","EA","CP")

# Create Alienation data for SEM material
alienation <- matrix(c(11.839, 6.947, 6.819, 4.783, -3.834, -21.899,
                        6.947, 9.364, 5.091, 5.028, -3.889, -18.831,
                        6.819, 5.091,12.532, 7.495, -3.841, -21.748,
                        4.783, 5.028, 7.495, 9.986, -3.625, -18.775,
                       -3.834,-3.889,-3.841,-3.625,  9.60,   35.522,
                      -21.899,-18.831,-21.748,-18.775,35.522,450.283),
                      ncol = 6, byrow = TRUE)
rownames(alienation) <- colnames(alienation) <- c("Anomia67",
    "Powles67", "Anomia71", "Powles71", "Educ", "SEI")

# Will need druguse data again
d <-
c(0.447,       
  0.422, 0.619,       
  0.435, 0.604, 0.583,         
  0.114, 0.068, 0.053, 0.115,  
  0.203, 0.146, 0.139, 0.258, 0.349,          
  0.091, 0.103, 0.110, 0.122, 0.209, 0.221,
  0.082, 0.063, 0.066, 0.097, 0.321, 0.355, 0.201,
  0.513, 0.445, 0.365, 0.482, 0.186, 0.315, 0.150, 0.154,
  0.304, 0.318, 0.240, 0.368, 0.303, 0.377, 0.163, 0.219, 0.534,
  0.245, 0.203, 0.183, 0.255, 0.272, 0.323, 0.310, 0.288, 0.301, 0.302,
  0.101, 0.088, 0.074, 0.139, 0.279, 0.367, 0.232, 0.320, 0.204, 0.368, 0.340,
  0.245, 0.199, 0.184, 0.293, 0.278, 0.545, 0.232, 0.314, 0.394, 0.467, 0.392, 0.511)

druguse <- diag(13) / 2
druguse[upper.tri(druguse)] <- d
druguse <- druguse + t(druguse)

rownames(druguse) <- colnames(druguse) <- c("cigarettes", "beer", "wine", "liquor", "cocaine", 
        "tranquillizers", "drug store medication", "heroin", 
        "marijuana", "hashish", "inhalants", "hallucinogenics", "amphetamine")


### Confirmatory Factor Analysis Models

# In a confirmatory factor model the loadings 
# for some observed variables on some of the
# postulated common factors will be set a priori 
# to zero. Additionally, some correlations between
# factors might also be fixed at zero. Such a
# model is fitted to a set of data by estimating 
# its free parameters; i.e., those not fixed
# at zero by the investigator. Estimation is 
# usually by maximum likelihood.

# Ability and Aspiration

# Calsyn and Kenny (1977) recorded the values
# of the following six variables for 556
# white eighth-grade students:
# SCA: self-concept of ability;
# PPE: perceived parental evaluation;
# PTE: perceived teacher evaluation;
# PFE: perceived friend's evaluation;
# EA: educational aspiration;
# CP: college plans.

# Calsyn and Kenny (1977) postulated that two 
# underlying latent variables, ability and
# aspiration, generated the relationships 
# between the observed variables.

# The first four of the manifest variables were 
# assumed to be indicators of ability and the
# last two indicators of aspiration; the latent 
# variables, ability and aspiration are assumed
# to be correlated. The regression-like equations
# that specify the postulated model are
# SCA = lambda1f1 + 0f2 + u1;
# PPE = lambda2f1 + 0f2 + u2;
# PTE = lambda3f1 + 0f2 + u3;
# PFE = lambda4f1 + 0f2 + u4;
# AE = 0f1 + lambda5f2 + u5;
# CP = 0f1 + lambda6f2 + u6;

# where f1 represents the ability latent variable 
# and f2 represents the aspiration latent variable
# Note that, unlike in exploratory factor analysis, 
# a number of factor loadings are fixed at zero 
# and play no part in the estimation process.

# The model has a total of 13 parameters to 
# estimate, six factor loadings (lambda1 to 
# lambda6), six specific variances (var1 to var6), 
# and one correlation between ability and
# and aspiration (p).

# We plot observed correlation matrix of
# ability and aspiration data with six
# vaiances and 15 correlations, a total of 21
# terms in all. The values given are correlation
# coefficients x 100:
ord <- order.dendrogram(as.dendrogram(hclust(dist(ability))))
panel.corrgram <-    
    function(x, y, z, subscripts, at,  
             level = 0.9, label = FALSE, ...)
{
    require("ellipse", quietly = TRUE)
    x <- as.numeric(x)[subscripts]   
    y <- as.numeric(y)[subscripts]   
    z <- as.numeric(z)[subscripts]   
    zcol <- level.colors(z, at = at, 
                         col.regions = grey.colors, ...)
    for (i in seq(along = z)) {
        ell <- ellipse(z[i], level = level, 
                       npoints = 50,   
                       scale = c(.2, .2), 
                       centre = c(x[i], y[i]))
        panel.polygon(ell, col = zcol[i], 
                      border = zcol[i], ...)
    }
    if (label)  
        panel.text(x = x, y = y, 
                   lab = 100 * round(z, 2), 
                   cex = 0.8,
                   col = ifelse(z < 0, "white", "black"))
}

print(levelplot(ability[ord, ord], 
                at = do.breaks(c(-1.01, 1.01), 20),
          xlab = NULL, ylab = NULL, 
                colorkey = list(space = "top"),
          scales = list(x = list(rot = 90)),
          panel = panel.corrgram, label = TRUE))

# Create ability_model.txt sem model file and
# write it out to disk:
mod <- c("Ability     -> SCA, lambda1, NA",
         "Ability     -> PPE, lambda2, NA",
         "Ability     -> PTE, lambda3, NA",
         "Ability     -> PFE, lambda4, NA",
         "Aspiration  -> EA, lambda5, NA",
         "Aspiration  -> CP, lambda6, NA",
         "Ability    <-> Aspiration, rho, NA",
         "SCA        <-> SCA, theta1, NA",
         "PPE        <-> PPE, theta2, NA",
         "PTE        <-> PTE, theta3, NA",
         "PFE        <-> PFE, theta4, NA",
         "EA         <-> EA, theta5, NA",
         # Note that Ability and Aspiration
         # latent variables have their variances
         # fixed at one. It is the 'fixing' that
         # is important and not the value '1'.
         # These variances cannot be free parameters
         # to be estimated.
         "CP         <-> CP, theta6, NA",
         "Ability    <-> Ability, NA, 1",
         "Aspiration <-> Aspiration, NA, 1")
writeLines(mod, con = "ability_model.txt")

# Can print it off disk with this R script:
writeLines(readLines("ability_model.txt"))
# r <- file.remove("ability_model.txt")

# The model is specified via arrows in the 
# so-called reticular action model (RAM)
# notation. The text consists of three columns. 
# The first one corresponds to an arrow
# specification where single-headed or 
# directional arrows correspond to regression
# coefficients and double-headed or bidirectional
# arrows correspond to variance parameters. The 
# second column denotes parameter names, and the
# third one assigns values to fixed parameters. 
# Further details are available from the sem
# package documentation.

# The R code, contained in the package sem for
# fitting the model is
ability_model <- specifyModel(file = "ability_model.txt")
ability_sem <- sem(ability_model, ability, 556)

# Can print it off disk with this R script:
writeLines(readLines("ability_model.txt"))
# r <- file.remove("ability_model.txt")

# We produce results from fitting the ability
# and aspiration model to the observed corre-
# lations can be printed to the R console:
summary(ability_sem)

# The z values test whether parameters are
# significantly different from zero. All have 
# very small associated p-values.

# Of particular note amongst the parameter 
# estimates is the correlation between "true"
# ability and "true" aspiration; this is known 
# as a 'disattenuated' correlation and is 
# uncontaminated by measurement error in the
# observed indicators of the two latent variables. 

# In this case the estimate is rho = 0.666
# with a standard error of 0:031. An approximate 
# 95% confidence interval for the disattenuated
# correlation is [0.606; 0.727].

# For path diagram
su <- summary(ability_sem)

# A path diagram (see Everitt and Dunn 2001) for 
# the correlated, two-factor model is produced
# as a .pdf file.

# Note that the R function pathDiagram() "only"
# produces a textual representation of the graph, 
# here in file ability_sem.dot.

# The graphviz visualization software (Gansner
# and North 2000) needs to be installed to compile
# the corresponding PDF file.

# To create path diagram with graphviz installed:
pathDiagram(ability_sem, 
             file = "ability_sem", 
             ignore.double = FALSE, 
             edge.labels = "both")
