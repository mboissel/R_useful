#### Snippet to treat missing values ####

#### load pkg ======================================================================================
## under R 4.1.2 ##
# renv::install("itertools")
# renv::install("missForest")
# renv::install("mice")
# renv::install("amices/ggmice") # install.packages("ggmice") from CRAN ? 
# renv::install("ggplot2", rebuild = TRUE)

library(missForest)
library(mice)
# library(ggmice)
# library(ggplot2)

set.seed(seed = 42)

#### Ressources & explanation ======================================================================

# ## https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
# 
# # MICE Package
# # MICE (Multivariate Imputation via Chained Equations) is one of the commonly used package by R users. 
# # Creating multiple imputations as compared to a single imputation (such as mean) takes care of uncertainty in missing values.
# # 
# # MICE assumes that the missing data are Missing at Random (MAR),
# # which means that the probability that a value is missing depends only on observed value and can be predicted using them. 
# # It imputes data on a variable by variable basis by specifying an imputation model per variable.
# # 
# # For example: Suppose we have X1, X2….Xk variables. If X1 has missing values, then it will be regressed on other variables X2 to Xk.
# # The missing values in X1 will be then replaced by predictive values obtained. Similarly,
# # if X2 has missing values, then X1, X3 to Xk variables will be used in prediction model as independent variables.
# # Later, missing values will be replaced with predicted values.
# # 
# # By default, 
# # linear regression is used to predict continuous missing values. 
# # Logistic regression is used for categorical missing values. 
# # Once this cycle is complete, multiple data sets are generated. 
# # These data sets differ only in imputed missing values. 
# # Generally, it’s considered to be a good practice to build models on these data sets separately and combining their results.
# # 
# # Precisely, the methods used by this package are:
# # 
# # PMM (Predictive Mean Matching)  – For numeric variables
# # logreg(Logistic Regression) – For Binary Variables( with 2 levels)
# # polyreg(Bayesian polytomous regression) – For Factor Variables (>= 2 levels)
# # Proportional odds model (ordered, >= 2 levels)

## Viz 
## https://amices.org/ggmice/

#### Test MICE pkg on numeric ======================================================================

# Generate 10% missing values at Random (thanks to missForest package)
iris.mis <- prodNA(iris, noNA = 0.1)
# Check missing values introduced in the data
summary(iris.mis)

# It returns a tabular form of missing value present in each variable in a data set.
md.pattern(iris.mis)

## test Let’s here focus on continuous values.
imputed_Data <- mice(subset(iris.mis, select = -c(Species)), m = 5, maxit = 50, method = "pmm", seed = 500)
# Here is an explanation of the parameters used:
# m  – Refers to 5 imputed data sets
# maxit – Refers to no. of iterations taken to impute missing values
# method – Refers to method used in imputation. we used predictive mean matching.
summary(imputed_Data)


# get complete data (2nd out of 5)
completeData <- complete(imputed_Data, 2)

# Also, if you wish to build models on all 5 datasets, you can do it in one go using with() command.
# You can also combine the result from these models and obtain a consolidated output using pool() command.
fit <- with(data = subset(iris.mis, select = -c(Species)), exp = lm(Sepal.Width ~ Sepal.Length + Petal.Width))  #build predictive model
combine <- mice::pool(object = fit, dfcom = nrow(iris.mis) - 2) #combine results of all 5 models 
## pool fail Error: No tidy method for objects of class qr + 'tidy.numeric' is deprecated. ...
summary(combine)


#### Test MICE pkg on numeric & categorial =========================================================

# Generate 10% missing values at Random (thanks to missForest package)
iris.mis <- prodNA(iris, noNA = 0.1)
# Check missing values introduced in the data
summary(iris.mis)

imputed_Data <- mice(iris.mis, m = 5, maxit = 50, seed = 500)
summary(imputed_Data) # Multiply imputed data set (mids)
imputed_Data

imputed_Data$imp$Sepal.Length
imputed_Data$imp$Species

completeData <- mice::complete(imputed_Data, 1) ## get the first imputation p.e.
completeData

# imputed_1_data <- mice(iris.mis, m = 1, maxit = 50, seed = 500) ## run only one imputation ? 
# imputed_1_data
# completed_1_data <- mice::complete(imputed_1_data, 1)


#### VIz ===========================================================================================
# visualize the incomplete data
# ggmice(iris.mis, ggplot2::aes(Sepal.Width, Sepal.Length)) + ggplot2::geom_point()

## error: Error: 'as_label' is not an exported object from 'namespace:ggplot2'

####################################################################################################
#### Dev function to build mean imputed dataset over all mids ======================================

## for numeric values = will use the mean 
## for categorial values = will use the major label predicted

get_imputation_consensus <- function(data_mis, obj_mice) {
  my_data <- as.data.frame(data_mis)
  stopifnot(class(obj_mice) == "mids")
  stopifnot(class(obj_mice$imp) == "list")
  
  my_data_imputed <- lapply(X = names(obj_mice$imp), FUN = function(ivar) {
    message(ivar)
    # ivar = "Sepal.Length"  # dev
    # ivar = "Species" # dev
    if (class(my_data[[ivar]]) %in% "numeric") {
      ## check binary var should stay binary 
      if (length(unique(my_data[[ivar]])) == 2) {
        message("numeric but binary")
        my_data[[ivar]][is.na(my_data[[ivar]])] <<- round(apply(X = imputed_Data$imp[[ivar]], MARGIN = 1, FUN = mean), digits = 0)
      } else {
        message("numeric")
        my_data[[ivar]][is.na(my_data[[ivar]])] <<- apply(X = imputed_Data$imp[[ivar]], MARGIN = 1, FUN = mean)
      }
    } else {
      message("categorial")
      my_data[[ivar]][is.na(my_data[[ivar]])] <<- apply(X = imputed_Data$imp[[ivar]], MARGIN = 1, FUN = function(x) {
        tab <- as.data.frame(table(unlist(x)))  
        tab$Var1[which.max(tab$Freq)]
      })
    }
    my_data[[ivar]]
  })
  # my_data_imputed # list of var imputed
  # my_data has been modified thanks to "<<-" operator
  return(my_data) 
}

res_final_imputed <- get_imputation_consensus(data_mis = iris.mis, obj_mice = imputed_Data)
## visual checks 
head(res_final_imputed, 12)
head(iris, 12)
