library(remstats)
library(remstimate)

covar <- data.frame(id = 1:10, time = 0, z = rnorm(n = 10))
colnames(covar)[colnames(covar) == "id"] <- "name"
# usethis::use_data(covar,overwrite = TRUE) 

## code to prepare `param` dataset 
# Normal variable (will be add)
param1 <- list(
  "baseline" = -4, 
  "inertia" = 0.5)

formula <- ~ 1+ inertia(scaling = "std")

constant <- list()


M<-1000

constant[[1]] <- generate_rem(formula, param1,covar,M)
