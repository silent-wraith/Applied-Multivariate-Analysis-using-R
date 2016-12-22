##########################################
###    CONFIRMATORY FACTOR ANALYSIS    ###  
###             AND SEM                ###
##########################################

library("MVA")

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
        "tranquilizers", "drug store medication", "heroin", 
        "marijuana", "hashish", "inhalants", "hallucinogenics", "amphetamine")


### Confirmatory Factor Analysis Models

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

# to remove the files:
file.remove("ability_sem.dot")
file.copy("ability_sem.pdf", "figs", 
          overwrite = TRUE)
file.remove("ability_sem.pdf")

### A CFA MODEL FOR DRUG USE
mod <- c("Alcohol   -> Cigs, lambda1, NA",
         "Alcohol   -> Beer, lambda3, NA",
         "Alcohol   -> Wine, lambda4, NA",
         "Alcohol   -> Liqr, lambda6, NA",
         "Cannabis  -> Cigs, lambda2, NA",
         "Cannabis  -> Wine, lambda5, NA",
         "Cannabis  -> Marj, lambda12, NA",
         "Cannabis  -> Hash, lambda13, NA",
         "Hard      -> Liqr, lambda7, NA",
         "Hard      -> Cocn, lambda8, NA",
         "Hard      -> Tran, lambda9, NA",
         "Hard      -> Drug, lambda10, NA",
         "Hard      -> Hern, lambda11, NA",
         "Hard      -> Hash, lambda14, NA",
         "Hard      -> Inhl, lambda15, NA",
         "Hard      -> Hall, lambda16, NA",
         "Hard      -> Amph, lambda17, NA",
         "Cigs     <-> Cigs, theta1, NA",
         "Beer     <-> Beer, theta2, NA",
         "Wine     <-> Wine, theta3, NA",
         "Liqr     <-> Liqr, theta4, NA",
         "Cocn     <-> Cocn, theta5, NA",
         "Tran     <-> Tran, theta6, NA",
         "Drug     <-> Drug, theta7, NA",
         "Hern     <-> Hern, theta8, NA",
         "Marj     <-> Marj, theta9, NA",
         "Hash     <-> Hash, theta10, NA",
         "Inhl     <-> Inhl, theta11, NA",
         "Hall     <-> Hall, theta12, NA",
         "Amph     <-> Amph, theta13, NA",
         "Alcohol  <-> Alcohol, NA, 1",
         "Cannabis <-> Cannabis, NA, 1",
         "Hard     <-> Hard, NA, 1",
         "Alcohol  <-> Cannabis, rho1, NA",
         "Alcohol  <-> Hard, rho2, NA",
         "Cannabis <-> Hard, rho3, NA")
writeLines(mod, con = "druguse_model.txt")

# Model allows for non-zero correlations
# between each pair of latents and so has
# 33 parameters to estimate: 17 loadings,
# the lambdas, 13 specific variances, and 
# three correlations between LVs. So the
# model has 91 - 33 = 58 degrees of freedom.

# We abbreviate the names of the variables:
rownames(druguse) <- colnames(druguse) <- c("Cigs", 
    "Beer", "Wine", "Liqr", "Cocn", "Tran", "Drug", 
    "Hern", "Marj", "Hash", "Inhl", "Hall", "Amph")

# Model is stored in text file
# druguse_model.txt:
writeLines(readLines("druguse_model.txt"))

# We use this code to fit the model:
druguse_model <- specifyModel(file = "druguse_model.txt")
druguse_sem <- sem(druguse_model, druguse, 1634)

# and to see results:
summary(druguse_sem)

# chi-square test for GoF is 324.09 with 58
# dof so p-value is very small so the model
# does NOT appear to fit the data very well.

# but before we reject it, we investigate
# fit in other ways: We examine the
# differences of the elements of the observed
# covariance matrix and the covariance
# matrix implied by the fitted model.
round(druguse_sem$S - druguse_sem$C, 3)
# Some of these "raw" residuals seem quite
# large in terms of a correlational scale;
# for example, drug store meds and inhalants
# is 0.115

# Finally, we can draw a path diagram and
# put on disk:
pathDiagram(druguse_sem, file = "druguse_sem", 
            ignore.double = FALSE, 
            edge.labels = "both") 

### ONE MORE: Stability over time of attitudes
# such as alienation and the relationship
# of such attitudes to background variables
# such as education and occupation.

# Data on attitude scales were collected from
# 932 people in two rural regions in Illinois
# at three time points, 1966, 1967, and 1971.

# We only consider the data from 1967 and 1971.
# Scores on the anomia scale and powerlessness
# scale were taken to be indicators of the
# assumed latent variable, alienation.

# Years of schooling (education) and Duncan's
# socioeconomic status were used as indicators
# of socioeconomic status.

# We convert covariance to correlation matrix
a <- cov2cor(alienation)
a

# And then make a fancy plot:
ord <- order.dendrogram(as.dendrogram(hclust(dist(a))))
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
                   col = ifelse(z < 0, 
                                "white", 
                                "black"))
}

# This call makes the plot appear:
print(levelplot(a[ord, ord], 
                at = do.breaks(c(-1.01, 1.01), 20),
                xlab = NULL, ylab = NULL, 
                colorkey = list(space = "top"),
                scales = list(x = list(rot = 90)),
                panel = panel.corrgram, 
                label = TRUE))

# Must fix scale of three latent variables SES,
# alienation 67, and alienation 71 to make the
# model identifiable. We set each to the scale
# of one of its indicator variables by fixing
# the corresponding regression coefficients to
# one.

# Model must estimate variances of eight error
# terms and the variance of the error term for
# latent variable SES.

# R code to fit the model:
mod <- c("SES      -> Educ, NA, 1",
    "SES           -> SEI, lambda1, NA",
    "Alienation67  -> Anomia67, NA, 1",
    "Alienation67  -> Powles67, lambda2, NA",
    "Alienation71  -> Anomia71, NA, 1",
    "Alienation71  -> Powles71, lambda3, NA",
    "SES           -> Alienation67, beta1, NA",
    "SES           -> Alienation71, beta2, NA",
    "Alienation67  -> Alienation71, beta3, NA",
    "Educ         <-> Educ, theta1, NA",
    "SEI          <-> SEI, theta2, NA",
    "SES          <-> SES, delta0, NA",
    "Anomia67     <-> Anomia67, theta3, NA",
    "Powles67     <-> Powles67, theta4, NA",
    "Anomia71     <-> Anomia71, theta5, NA",
    "Powles71     <-> Powles71, theta6, NA",
    "Alienation67 <-> Alienation67, delta1, NA",
    "Alienation71 <-> Alienation71, delta2, NA")
writeLines(mod, con = "alienation_model.txt")

# This is second model that we will use to
# try to improve the fit by allowing the 
# measurement errors for anomia in 1967 and
# and in 1971 to be correlated....all we need 
# do is add this line:
mod2 <- c(mod, "Anomia67 <-> Anomia71,psi,NA")
writeLines(mod2, con = "alienation_model2.txt")

# Can get rid of files if we want to
# file.remove("alienation_sem.dot")
# file.copy("alienation_sem.pdf", 
#           "figs", overwrite = TRUE)
# file.remove("alienation_sem.pdf")

# We run the model
alienation_model <- specifyModel(
  file = "alienation_model.txt")
alienation_sem <- sem(alienation_model, 
                      alienation, 932)

# here are parameter estimates:
summary(alienation_sem)

# Chi-square of 71.53 with 6 dof suggests model
# does not fit well.

# Here is the path diagram:
pathDiagram(alienation_sem, 
            file = "alienation_sem", 
            ignore.double = FALSE, 
            edge.labels = "both")

# We make the modifications to the model
# for the correlated measurement erros for
# the two measures of anomia:

alienation_model2 <- specifyModel(file = "alienation_model2.txt")
alienation_sem2 <- sem(alienation_model2, alienation, 932)
summary(alienation_sem2)

# Now chi-square is 6.35 with 5 dof....fit has 
# improved quite a bit.

