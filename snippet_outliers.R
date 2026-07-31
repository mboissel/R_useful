#### Detection outliers ####
#' Detect extreme values in a numeric data.table
#'
#' @param dt A data.table with numeric columns only
#' @param vars Character vector of variable names to check.
#'   Default: all numeric columns.
#' @param method Method to detect outliers: "Tukey" or "Zscore"
#' @param threshold Multiplier for the bounds.
#'   Default: 1.5 for Tukey (classic boxplot), 3 for Zscore (3 SD).
#' @param summary Logical. If TRUE, returns a summary table (one row per
#'   variable). If FALSE, returns all outlier rows. Default: FALSE.
#'
#' @return A data.table with outlier details or summary, NULL if none found.
#'
#' @examples
#' dt <- data.table::data.table(
#'   PAM     = c(rnorm(43, 85, 10), 145, 150),
#'   LACTATE = c(rnorm(44, 1.8, 0.5), 8.2, NA),
#'   FC      = c(rnorm(44, 75, 10), 180, NA)
#' )
#'
#' # Tukey method (default threshold 1.5)
#' detect_outliers(dt, method = "Tukey")
#'
#' # Zscore method (default threshold 3)
#' detect_outliers(dt, method = "Zscore")
#'
#' # Summary only
#' detect_outliers(dt, method = "Tukey", summary = TRUE)
#'
#' @keywords internal
detect_outliers <- function(
    dt,
    vars      = names(dt)[sapply(dt, is.numeric)],
    method    = c("Tukey", "Zscore"),
    threshold = NULL,
    summary   = FALSE
) {
  
  # Validation
  method <- match.arg(method)
  
  # Threshold par défaut selon la méthode
  if (is.null(threshold)) {
    threshold <- switch(method,
                        "Tukey"  = 1.5,
                        "Zscore" = 3
    )
  }
  
  # Vérifier que les variables existent et sont numériques
  vars_invalid <- vars[!vars %in% names(dt)]
  if (length(vars_invalid) > 0) {
    stop(
      "[detect_outliers] Variables not found in dt: ",
      paste(vars_invalid, collapse = ", ")
    )
  }
  vars_non_numeric <- vars[!sapply(dt[, .SD, .SDcols = vars], is.numeric)]
  if (length(vars_non_numeric) > 0) {
    stop(
      "[detect_outliers] Non-numeric variables: ",
      paste(vars_non_numeric, collapse = ", ")
    )
  }
  
  # Fonction interne pour une variable
  detect_one <- function(var_name) {
    
    x       <- dt[[var_name]]
    x_clean <- x[!is.na(x)]
    n_total <- length(x_clean)
    
    if (n_total < 3) {
      warning(
        "[detect_outliers] Variable '", var_name, "' has less than 3 ",
        "non-missing values, skipping.",
        call. = FALSE
      )
      return(NULL)
    }
    
    # === Calcul des bornes selon la méthode ===
    if (method == "Tukey") {
      # Tukey : Q1 - threshold*IQR  /  Q3 + threshold*IQR
      q1    <- stats::quantile(x_clean, 0.25)
      q3    <- stats::quantile(x_clean, 0.75)
      iqr_x <- q3 - q1
      lower <- q1 - threshold * iqr_x
      upper <- q3 + threshold * iqr_x
      
      bound_info <- data.table::data.table(
        Q1          = round(q1, 3),
        Q3          = round(q3, 3),
        IQR         = round(iqr_x, 3),
        Lower_bound = round(lower, 3),
        Upper_bound = round(upper, 3)
      )
      
    } else {
      # Zscore : mean - threshold*SD  /  mean + threshold*SD
      mean_x <- mean(x_clean)
      sd_x   <- stats::sd(x_clean)
      lower  <- mean_x - threshold * sd_x
      upper  <- mean_x + threshold * sd_x
      
      bound_info <- data.table::data.table(
        Mean        = round(mean_x, 3),
        SD          = round(sd_x, 3),
        Lower_bound = round(lower, 3),
        Upper_bound = round(upper, 3)
      )
    }
    
    # Identifier les outliers
    is_outlier <- !is.na(x) & (x < lower | x > upper)
    n_outliers <- sum(is_outlier)
    
    if (summary) {
      # Mode résumé : 1 ligne par variable
      return(data.table::data.table(
        Variable   = var_name,
        Method     = method,
        Threshold  = threshold,
        N_total    = n_total,
        N_outliers = n_outliers,
        Pct        = round(100 * n_outliers / n_total, 1),
        bound_info
      ))
    } else {
      # Mode détail : 1 ligne par outlier
      if (n_outliers == 0) return(NULL)
      
      return(data.table::data.table(
        Variable   = var_name,
        Method     = method,
        Threshold  = threshold,
        Row_index  = which(is_outlier),
        Value      = x[is_outlier],
        bound_info
      ))
    }
  }
  
  # Appliquer sur toutes les variables
  result <- data.table::rbindlist(
    lapply(vars, detect_one),
    fill = TRUE
  )
  
  if (nrow(result) == 0) {
    message("[detect_outliers] No outliers detected with method '", method, "'.")
    return(NULL)
  }
  
  return(result)
}

## test ## 
# Données exemple
dt <- data.table::data.table(
  PAM     = c(rnorm(43, 85, 10), 145, 150),
  LACTATE = c(rnorm(44, 1.8, 0.5), 8.2, NA),
  FC      = c(rnorm(44, 75, 10), 180, NA)
)
dt
# Tukey - détail (voir Row_index)
detect_outliers(dt, method = "Tukey")

# Tukey - résumé
detect_outliers(dt, method = "Tukey", summary = TRUE)

# Zscore - détail
detect_outliers(dt, method = "Zscore")

# Zscore - résumé
detect_outliers(dt, method = "Zscore", summary = TRUE)

# Tukey avec threshold custom (boxplot étendu : 3 au lieu de 1.5)
detect_outliers(dt, method = "Tukey", threshold = 3)

# Sur des variables spécifiques seulement
detect_outliers(dt, vars = c("PAM", "FC"), method = "Zscore")



#### Other version  ####

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

