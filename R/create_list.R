library(vars)
library(matrixcalc)
library(lavaan)

##############
#  PART 0: PRIOR ANALYSIS 
##############
# In this part we conduct the data analysis. The results of  
# this analysis will be the INPUT to the functions of the 
# CausalSEM package
##############


# Data generation --------------------------------------------
# read in data and obtain estimates of bivariate vector autoregression
insulin <- c(17, 21, 25, 37, 43, 43, 43, 40, 57, 48, 45, 43, 28, 31, 29, 
             24, 23, 23, 25, 24, 24, 22, 22, 21, 22, 23, 24, 26, 31, 18)
glucose <- c(65, 75, 85, 105, 115, 125, 120, 123, 118, 110, 100, 85, 80,
             75, 70, 75, 75, 70, 70, 73, 75, 80, 78, 75, 70, 70, 68, 65,
             63, 60)
d <- data.frame(insulin = (insulin - mean(insulin)),
                glucose = (glucose - mean(glucose)))
VAR1 <- VAR(d, p = 1, type = "const")
B <- matrix(nrow = 2, c(0.05, 0.4, -0.6, 1.2), byrow = T)

# check if process satisfies regularity conditions
print(abs(eigen(B)$values)) 
theta_dgp <- c(0.05, 0.4, -0.6, 1.2, 131.76, 632.94, 254.12, 20, 40, 3,
               15, 2, 35, 10)

# specification of the data generating process (dgp)
model_dgp <- "

#error terms
ex1 =~ 1.0*x1
ex2 =~ 1.0*x2
ex3 =~ 1.0*x3
ey1 =~ 1.0*y1
ey2 =~ 1.0*y2
ey3 =~ 1.0*y3
ex1 ~~ 131.76*ex1
ey1 ~~ 632.94*ey1
ex2 ~~ 20*ex2
ey2 ~~ 40*ey2
ex3 ~~ 20*ex3
ey3 ~~ 40*ey3
ex2 ~~ 3*ey2
ex3 ~~ 3*ey3
ex1 ~~ 254.12*ey1
ex1 ~~ 15*ex2
ex2 ~~ 2*ex3
ey1 ~~ 35*ey2
ey2 ~~ 10*ey3

# regressions
x2~ 0.05*x1 + 0.4*y1
y2~ -0.6*x1 + 1.2*y1
x3~ 0.05*x2 + 0.4*y2
y3~ -0.6*x2 + 1.2*y2

# zero covariance restrictions
ex1~~0*ex3
ex1~~0*ey2
ex1~~0*ey3
ey1~~0*ex2
ey1~~0*ex3
ey1~~0*ey3
ex2~~0*ey3
ey2~~0*ex3
"
# specification of the model 
model_sem<-"

#error terms
ex1 =~ 1.0*x1
ex2 =~ 1.0*x2
ex3 =~ 1.0*x3
ey1 =~ 1.0*y1
ey2 =~ 1.0*y2
ey3 =~ 1.0*y3
ex1 ~~ psix1x1*ex1
ey1 ~~ psiy1y1*ey1
ex2 ~~ psix*ex2
ey2 ~~ psiy*ey2
ex3 ~~ psix*ex3
ey3 ~~ psiy*ey3
ex2 ~~ psixy*ey2
ex3 ~~ psixy*ey3
ex1 ~~ psix1y1*ey1
ex1 ~~ psix1x2*ex2
ex2 ~~ psixx*ex3
ey1 ~~ psiy1y2*ey2
ey2 ~~ psiyy*ey3

# regressions
x2 ~ cxx*x1 + cxy*y1
y2 ~ cyx*x1 + cyy*y1
x3 ~ cxx*x2 + cxy*y2
y3 ~ cyx*x2 + cyy*y2

# zero covariance restrictions
ex1 ~~ 0*ex3
ex1 ~~ 0*ey2
ex1 ~~ 0*ey3
ey1 ~~ 0*ex2
ey1 ~~ 0*ex3
ey1 ~~ 0*ey3
ex2 ~~ 0*ey3
ey2 ~~ 0*ex3
"
# sample N = 100 individuals from the population and obtain estimates
set.seed(34995)
d_100 <- simulateData(model_dgp, sample.nobs = 100, seed = 45903)
d_100 <- d_100[, c(1, 4, 2, 5, 3, 6)]
fit_100 <- lavaan(model_sem, data = d_100)


##############
#  PART 1: USE OF THE causalSEM PACKAGE
##############

# The intervention_effect()-function is called by the user. 
# The user must specify the following arguments (some optional):

model <- fit_100
intervention <- c("x2")
outcome <- c("y3")
intervention.levels <- c(11.54)
lower.bound <- c(-40)
upper.bound <- c(80)
causalOrder <- c("x1", "y1", "x2", "y2", "x3", "y3")

# creating a list

internal_list <- list(
  # Model information
  info_model = list(
    n_obs = nobs(fit_100), # number of observations
    n_var = ncol(lavInspect(fit_100, what = "data")), # number of manifest variables
    var_names = lavNames(fit_100, type = "ov"), # variable names of observed variables 
    n_par = length(coef(fit_100)), # total number of estimated parameters
    par_names = names(coef(fit_100)), # names of estimated parameters
    causal_order = causalOrder, # causal order of variables 
    model = parTable(fit_100)[,c("id", "lhs", "op", "rhs")] # model specification
  ),
  # Equality Constraints
  info_constraints = list(
    n_par_unique = length(unique(names(coef(fit_100)))), # number of distinct and functionally unrelated parameters 
    par_names_unique = unique(names(coef(fit_100))), # names of distinct and functionally unrelated parameters 
    eq_constr = attributes(lavTech(fit_100))$header # equality constraints
  ),
  # Interventions
  info_interventions = list(
    intervention_name = intervention,
    outcome_name = outcome,
    intervention_num = intervention.levels,
    lower_bound = lower.bound,
    upper_bound = upper.bound
  )
  #,
  # Raw Input
  #info_raw = list(
  #fitted_object = fit_100
  #)
)

