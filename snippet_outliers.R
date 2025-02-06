#### Detect outliers and show them #### 

#### libs ####
library(data.table)
library(ggplot2)
library(patchwork)

#### data ####
set.seed(123)
tab_full <- data.frame(
  ID = 1:100,
  value1 = c(rnorm(98, mean = 10, sd = 2), 30, 35),
  value2 = c(22, 23, rnorm(98, mean = 10, sd = 2))
)

#### params ####
# interest variables
out_var <- c("value1", "value2")
outlier_time_IQR <- 3

#### analyses ####

# detect outliers
samples_outliers <- unique(unlist(lapply(out_var, function(ivar) {
  df.IQR <- stats::IQR(tab_full[[ivar]], na.rm = TRUE)
  q1 <- quantile(tab_full[[ivar]], 0.25, na.rm = TRUE)
  q3 <- quantile(tab_full[[ivar]], 0.75, na.rm = TRUE)
  
  lower_bound <- q1 - outlier_time_IQR * df.IQR
  upper_bound <- q3 + outlier_time_IQR * df.IQR
  
  outliers <- tab_full$ID[
    !is.na(tab_full[[ivar]]) &
      (tab_full[[ivar]] < lower_bound | tab_full[[ivar]] > upper_bound)
  ]
  
  return(outliers)
})))
samples_outliers

# Fonction graph distri + outliers
plot_distribution <- function(dt, variable, outliers, outlier_time_IQR) {
  iqr_value <- IQR(dt[[variable]], na.rm = TRUE)
  q1 <- quantile(dt[[variable]], 0.25, na.rm = TRUE)
  q3 <- quantile(dt[[variable]], 0.75, na.rm = TRUE)
  lower_bound <- q1 - outlier_time_IQR * iqr_value
  upper_bound <- q3 + outlier_time_IQR * iqr_value
  mean_value <- mean(dt[[variable]], na.rm = TRUE)
  
  # extract outliers data 
  extreme_points <- dt[dt$ID %in% outliers, ]
  
  ggplot(data = dt, aes(x = get(variable))) +
    geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.6) +
    geom_vline(xintercept = mean_value, color = "red", linetype = "dashed", linewidth = 1.2) +
    geom_vline(xintercept = c(q1, q3), color = "orange", linetype = "dashed") +
    geom_vline(xintercept = c(lower_bound, upper_bound), color = "purple", linetype = "dotted", , linewidth = 1.1) +
    geom_point(data = extreme_points, aes(x = get(variable), y = 0), color = "red", size = 3) +
    geom_text(data = extreme_points, aes(x = get(variable), y = 0.5, label = ID), 
              color = "red", vjust = -1, size = 3) +
    labs(
      title = paste("Distribution de", variable, "avec points extrêmes"),
      subtitle = paste0("Bornes : ", outlier_time_IQR, " * IQR en dehors de Q1 et Q3"),
      x = "Valeurs",
      y = "Fréquence"
    ) +
    theme_minimal()
}

# graphiques
plot1 <- plot_distribution(
  dt = tab_full, variable = "value1",
  outliers = samples_outliers,
  outlier_time_IQR = outlier_time_IQR
)
print(plot1)
plot2 <- plot_distribution(tab_full, "value2", samples_outliers, outlier_time_IQR)
print(plot2)

# patchwork
plot1 + plot2
