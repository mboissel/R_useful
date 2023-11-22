install.packages("bestNormalize")
install.packages("data.table")
library(data.table)
library(bestNormalize) # to use ordered quantile normalization

str(datasets::women)
dt <- data.table::setDT(datasets::women)
str(dt)

# just test
plot(density(dt$height))
height_norm <- bestNormalize::orderNorm(dt$height)$x.t
plot(density(height_norm))

# or complete application to a dataset
run_norm <- function(cols, dt) {
  message("Apply 'Ordered Quantile Normalization' on continous variables.")
  setNames(
    lapply(cols, function(col_i) {
      res_norm <- bestNormalize::orderNorm(dt[[col_i]]) # keep model if want to reverse value later
      set(dt, j = col_i, value = res_norm$x.t)
      res_norm
    }), nm = cols)
}

cols_continuous <- names(datasets::women)
list_norm <- run_norm(cols_continuous, dt)

head(dt) # new dataset normalized (direct modification)
head(datasets::women) # old dataset original

## revers value
# ?bestNormalize
# predict(object, newdata = NULL, inverse = FALSE, ...)
# inverse	: if TRUE, performs reverse transformation

invisible(lapply(names(list_norm), function(col_i) {
  set(
    dt,
    j = col_i,
    value = predict(list_norm[[col_i]], dt[[col_i]], inverse = TRUE, warn = FALSE)
  ) # there may be outside observed range values to revert due to imputation
}))

head(dt) 
dt == datasets::women
