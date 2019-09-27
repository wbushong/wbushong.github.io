compute.indirect.effect <- function(data, predictor, mediator, dv, controls = c(), r = TRUE) {
  # Sample rows of data
  sample <- sample(nrow(data), replace = r)
  # Create relevant formulas (don't worry about this, this is just to make the function more general)
  a.formula <- formula(paste(mediator, "~", predictor))
  b.form <- paste(dv, "~", predictor, "+", mediator)
  for (i in controls) {
    b.form <- paste(b.form, "+", i)
  }
  b.formula <- formula(b.form)
  # Fit the 'a' and 'b' models
  a.model <- lm(a.formula, data[sample, ]) 
  b.model <- lm(b.formula, data[sample, ])
  # Extract the B's from these models
  predictor.strs <- grep(predictor, rownames(summary(a.model)$coefficients))
  a <- summary(a.model)$coefficients[predictor.strs, "Estimate"]
  b <- summary(b.model)$coefficients[mediator, "Estimate"]
  # Compute indirect effect term
  indirect.effect <- a * b
  return(indirect.effect)
}