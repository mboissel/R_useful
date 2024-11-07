install.packages("bestNormalize")
install.packages("data.table")
library(data.table)
library(bestNormalize) # to use ordered quantile normalization

#### quick test ####

# Appliquer bestNormalize à votre variable Y
best_transform <- bestNormalize(Y)
# Afficher la transformation recommandée
print(best_transform)
# Transformer Y
Y_transformed <- predict(best_transform)

# Avantages de bestNormalize
# Comparaison automatisée de transformations : bestNormalize utilise une série de transformations et choisit la plus appropriée en fonction de critères de normalité.
# Retour facile à la valeur d’origine : une fois les analyses faites, il est possible d’inverser la transformation pour revenir aux valeurs d'origine, avec predict(best_transform, newdata = Y_transformed, inverse = TRUE).

# + Cas avec des données negatives et positives (pas de transformation log ou sqrt possible) : 
#   Utilisation de Yeo-Johnson : La transformation Yeo-Johnson, incluse dans bestNormalize, fonctionne bien pour des données ayant des valeurs négatives et positives.

#### Real application ####

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
