#### Detection outliers ####

#### libs ####
library(ggplot2)
library(patchwork)

#### params ####
outlier_time_IQR <- 3
ID_var <- "ID"

#### recherche variables with 2 outliers cibles ####

## 500 variables cliniques x 1000 individus
set.seed(123)
tab_full <- as.data.frame(matrix(rnorm(1000 * 500, mean = 10, sd = 2), ncol = 500))
colnames(tab_full) <- paste0("var", 1:500)
tab_full$ID <- paste0("ID_", 1:nrow(tab_full))
# # Ajout d'une colonne ID
ID_var <- "ID"
# target 2 outliers
target_ids <- c("ID_1", "ID_2")
tab_full[1, 1] <- 55
tab_full[2, 2] <- 75

# Fonction detect variables where are outliers
find_outlier_variables <- function(df, target_ids, outlier_time_IQR = 3) {
  # Extract
  target_data <- df[df[[ID_var]] %in% target_ids, ]
  
  # Init
  outlier_vars <- list()
  
  for (vari in setdiff(names(df), ID_var)) {  
    message(vari)
    if (is.numeric(df[[vari]])) {  # check numérique
      # Calcul  IQR
      iqr_value <- IQR(df[[vari]], na.rm = TRUE)
      q1 <- quantile(df[[vari]], 0.25, na.rm = TRUE)
      q3 <- quantile(df[[vari]], 0.75, na.rm = TRUE)
      lower_bound <- q1 - outlier_time_IQR * iqr_value
      upper_bound <- q3 + outlier_time_IQR * iqr_value
      
      # out ? 
      target_outliers <- target_data[[vari]] < lower_bound | target_data[[vari]] > upper_bound
      if (any(target_outliers, na.rm = TRUE)) {
        outlier_vars[[vari]] <- target_data[target_outliers, c(ID_var, vari), drop = FALSE]
      }
    }
  }
  
  return(outlier_vars)
}

outliers_found <- find_outlier_variables(df = tab_full, target_ids = target_ids, outlier_time_IQR = 3)

# show vars 
if (length(outliers_found) > 0) {
  for (var in names(outliers_found)) {
    print(paste("Variable:", var))
    print(outliers_found[[var]])
  }
} else {
  print("Aucune variable avec des outliers détectés.")
}

vars_wanted <- names(outliers_found)
vars_wanted

# interest variables
nums <- unlist(lapply(tab_full[, c(vars_wanted)], is.numeric), use.names = FALSE)  
vars_wanted <- vars_wanted[nums]

tab <- tab_full[,c(ID_var, vars_wanted)]

head(tab)
str(tab)

#### find outliers ####

# detect outliers
samples_outliers <- unique(unlist(lapply(vars_wanted, function(ivar) {
  df.IQR <- stats::IQR(tab[[ivar]], na.rm = TRUE)
  q1 <- quantile(tab[[ivar]], 0.25, na.rm = TRUE)
  q3 <- quantile(tab[[ivar]], 0.75, na.rm = TRUE)
  
  lower_bound <- q1 - outlier_time_IQR * df.IQR
  upper_bound <- q3 + outlier_time_IQR * df.IQR
  
  outliers <- tab[[ID_var]][
    !is.na(tab[[ivar]]) &
      (tab[[ivar]] < lower_bound | tab[[ivar]] > upper_bound)
  ]
  
  return(outliers)
})))
samples_outliers

# Fonction graph distri + outliers
plot_distribution <- function(df, variable, outliers, outlier_time_IQR) {
  message("[plot_distribution] ", variable)
  iqr_value <- IQR(df[[variable]], na.rm = TRUE)
  q1 <- quantile(df[[variable]], 0.25, na.rm = TRUE)
  q3 <- quantile(df[[variable]], 0.75, na.rm = TRUE)
  lower_bound <- q1 - outlier_time_IQR * iqr_value
  upper_bound <- q3 + outlier_time_IQR * iqr_value
  mean_value <- mean(df[[variable]], na.rm = TRUE)
  
  # extract outliers data 
  extreme_points <- df[df[[ID_var]] %in% outliers, ]
  
  ggplot(data = df, aes(x = get(variable))) +
    geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.6) +
    geom_vline(xintercept = mean_value, color = "red", linetype = "dashed", linewidth = 1.2) +
    geom_vline(xintercept = c(q1, q3), color = "orange", linetype = "dashed") +
    geom_vline(
      xintercept = c(lower_bound, upper_bound), color = "purple", linetype = "dotted", , linewidth = 1.1
    ) +
    geom_point(
      data = extreme_points, aes(x = get(variable), y = 0), color = "red", size = 3
    ) +
    geom_text(
      data = extreme_points, aes(x = get(variable), y = 0.5, label = get(ID_var)), 
      color = "red", vjust = -1, size = 3
    ) +
    labs(
      title = paste("Distribution de", variable, "avec points extrêmes"),
      subtitle = paste0("Bornes : ", outlier_time_IQR, " * IQR en dehors de Q1 et Q3"),
      x = "Valeurs",
      y = "Fréquence"
    ) +
    theme_minimal()
}

# graphs
plot1 <- plot_distribution(
  df = tab_full,
  variable = vars_wanted[1],
  outliers = samples_outliers,
  outlier_time_IQR = outlier_time_IQR
)
print(plot1)
plot2 <- plot_distribution(
  df = tab_full,
  variable = vars_wanted[2],
  outliers = samples_outliers,
  outlier_time_IQR = outlier_time_IQR
)
print(plot2)

# patchwork
plot1 + plot2

