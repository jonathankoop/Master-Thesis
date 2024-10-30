stats <- ~ 1+ inertia(scaling = "std")
reh_tie <- remify::remify(edgelist = constant[[1]], model = "tie",actors = covar$name,directed = TRUE,origin = 0)

out <- remstats(reh = reh_tie, tie_effects = stats, attr_actors  = covar)
fit <- remstimate::remstimate(reh = reh_tie, stats = out,method = "MLE")

summary(fit)
