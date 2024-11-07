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



#### Somes alternatives transformation ####

# 1. sqrt 
Y_prim = sqrt(Y + c) # where Y+c > 0

# 2. norm data from reg obj
MASS::boxcox(reg) 
# Le paramètre λ est optimisé pour rendre les données aussi normales que possible.
# fonction MASS::boxcox() : calculer une valeur optimale de λ et suggérer une transformation possible pour Y.

# 3. Transformation de Tukey (ou Yeo-Johnson)
# La transformation de Tukey, également connue sous le nom de Yeo-Johnson transformation, est similaire à Box-Cox mais adaptée aux valeurs positives et négatives sans nécessiter l’ajout d’une constante.
library(car)
Y_prim <- powerTransform(Y)

# 4. Transformation sigmoïde
# Si Y est bornée (par exemple, comprise entre -40 et 40), une transformation sigmoïde comme la fonction tangente hyperbolique (tanh) ou sigmoïde logistique peut être utile pour limiter l'impact des valeurs extrêmes.
Y_prim <- tanh(Y)
# Ces transformations peuvent atténuer l'influence des valeurs extrêmes et rendre les résidus plus homogènes.

