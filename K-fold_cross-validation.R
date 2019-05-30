#### K-fold cross-validation

# Create nfolds and result list
nfolds = 10 # in this example, 10 folds are created, which means the model will be run 10 times
df$k = kfold(df, nfolds) # nfolds is added to df
result = vector("list", length=nfolds)

# Run models with k-fold
for(i in 1:nfolds){
  df$ys = df$y
  df$ys[ df$k == i ] = NA
  model_formula = formula("y ~ 1 + x") # add model formula
  model_name = inla(model_formula, family="poisson", data=df,
                       control.family=list(variant=0),
                       control.predictor=list(compute=TRUE),
                       control.compute = list(config=TRUE, dic=TRUE, cpo=TRUE, waic=TRUE))
  # in this example, an basic INLA model was added
  result[[i]] = model_name
}

# Create out-of-sample variable to add in fitted values
df$oos = NA

# Add fitted values for model j in oos
for(j in 1:nfolds){
  fv = result[[j]]$summary.fitted.values[, 1]
  df$oos[ df$k == j ] = fv[ df$k == j ]
}

# Plot residuals
plot(df$id,df$ys-df$oos)

# Plot of outcome with out-of-sample fitted values
plot(df$ys,df$oos)

# Psuedo r-squared between outcome with out-of-sample fitted values
cor(df$ys,df$oos, use="complete.obs")