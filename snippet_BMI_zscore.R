#' @title BMI Z-score Calculation for Children (WHO References)
#'
#' @description
#' Computes the BMI-for-age Z-score for children and adolescents based on
#' the World Health Organization (WHO) LMS reference tables.
#' The LMS method uses three parameters: L (skewness), M (median), and S
#' (coefficient of variation) to normalize BMI distributions by age and sex.
#'
#' @param bmi Numeric vector of Body Mass Index values (kg/m²)
#' @param age Numeric vector of ages in years (supported range: 0 to 87 years).
#'   The nearest available age in the WHO reference table is used.
#' @param gender Character or numeric vector indicating sex. Accepted values:
#'   \code{"Male"}, \code{"Female"}, \code{"1"} (Male), \code{"2"} (Female).
#'   Any other value returns \code{NA}.
#'
#' @return Numeric vector of BMI Z-scores, one per row of input.
#'   Returns \code{NA} if any of \code{bmi}, \code{age}, or \code{gender}
#'   is \code{NA} for a given individual, or if \code{gender} is not one
#'   of the accepted values.
#'
#' @details
#' The Z-score is computed using the LMS formula:
#'
#' \deqn{Z = \frac{(BMI / M)^L - 1}{L \times S}}
#'
#' where L, M and S are interpolated from the WHO reference table at the
#' nearest age value. Reference values are embedded directly in the function
#' and cover ages 0 to 87 years, separately for males and females.
#'
#' The function is vectorized via \code{apply()} and processes one individual
#' per row of the input matrix \code{cbind(bmi, age, gender)}.
#'
#' @references
#' WHO Multicentre Growth Reference Study Group (2006).
#' WHO Child Growth Standards: Length/height-for-age, weight-for-age,
#' weight-for-length, weight-for-height and body mass index-for-age.
#' Geneva: World Health Organization.
#' \url{https://www.who.int/tools/child-growth-standards}
#'
#' @examples
#' # Single child - Male, age 10, BMI 18
#' BMIZscore(bmi = 18, age = 10, gender = "Male")
#'
#' # Single child - Female, age 8, BMI 16
#' BMIZscore(bmi = 16, age = 8, gender = "Female")
#'
#' # Using numeric gender codes (1 = Male, 2 = Female)
#' BMIZscore(bmi = 18, age = 10, gender = "1")
#'
#' # Vectorized on a data.frame
#' children <- data.frame(
#'   bmi    = c(18, 16, 22, NA, 15),
#'   age    = c(10,  8, 12, 10,  5),
#'   gender = c("Male", "Female", "Male", "Male", "Female")
#' )
#' children$zscore <- BMIZscore(
#'   bmi    = children$bmi,
#'   age    = children$age,
#'   gender = children$gender
#' )
#' print(children)
#'
#' # Mixed NA handling
#' BMIZscore(
#'   bmi    = c(18, NA, 16),
#'   age    = c(10, 10,  8),
#'   gender = c("Male", "Male", "Female")
#' )
#' # Returns: numeric  NA  numeric
#'
#' @rdname BMIZscore
#' @export
BMIZscore <- function (bmi, age, gender) {
  
  #### who tables ####
  RefZscore <- structure(
    list(
      Age = c(0, 0.2, 0.3, 0.5, 0.7, 0.8, 1, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5, 11, 11.5, 12, 12.5, 13, 13.5, 14, 14.5, 15, 15.5, 16, 16.5, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87), 
      L.skewness.M = c(0.24, -0.29, -0.37, -0.48, -0.53, -0.54, -0.55, -0.47, -0.41, -0.35, -0.32, -0.29, -0.29, -0.3, -0.33, -0.37, -0.41, -0.47, -0.53, -0.59, -0.66, -0.72, -0.77, -0.82, -0.87, -0.9, -0.92, -0.93, -0.93, -0.93, -0.91, -0.9, -0.87, -0.85, -0.82, -0.8, -0.77, -0.74, -0.69, -0.65, -0.61, -0.57, -0.54, -0.51, -0.5, -0.49, -0.48, -0.48, -0.48, -0.48, -0.47, -0.47, -0.47, -0.47, -0.47, -0.47, -0.47, -0.46, -0.45, -0.45, -0.43, -0.42, -0.41, -0.39, -0.37, -0.36, -0.34, -0.32, -0.31, -0.29, -0.28, -0.26, -0.25, -0.23, -0.22, -0.21, -0.2, -0.19, -0.18, -0.17, -0.16, -0.16, -0.15, -0.14, -0.14, -0.13, -0.13, -0.12, -0.12, -0.12, -0.12, -0.12, -0.12, -0.12, -0.12, -0.12, -0.12, -0.12, -0.13, -0.13, -0.14, -0.14, -0.15, -0.15, -0.16, -0.17, -0.17, -0.18), 
      S.variation.M = c(0.0925, 0.0864, 0.0852, 0.0832, 0.0815, 0.0808, 0.0794, 0.0741, 0.0726, 0.0718, 0.0716, 0.072, 0.0729, 0.0742, 0.0759, 0.0779, 0.0802, 0.0826, 0.0851, 0.0877, 0.0902, 0.0928, 0.0952, 0.0975, 0.0997, 0.1017, 0.1035, 0.1052, 0.1065, 0.1077, 0.1086, 0.1093, 0.1099, 0.1102, 0.1105, 0.1106, 0.1106, 0.1106, 0.1106, 0.1106, 0.1107, 0.1111, 0.1116, 0.1124, 0.1132, 0.1141, 0.115, 0.116, 0.1168, 0.1175, 0.1182, 0.1188, 0.1193, 0.1198, 0.1203, 0.1207, 0.121, 0.1214, 0.1218, 0.1221, 0.1226, 0.123, 0.1234, 0.1238, 0.1243, 0.1248, 0.1253, 0.1259, 0.1264, 0.1271, 0.1277, 0.1283, 0.129, 0.1296, 0.1303, 0.131, 0.1317, 0.1324, 0.1331, 0.1338, 0.1345, 0.1352, 0.1359, 0.1366, 0.1372, 0.1379, 0.1385, 0.1391, 0.1397, 0.1403, 0.1408, 0.1414, 0.1418, 0.1422, 0.1426, 0.143, 0.1434, 0.1437, 0.144, 0.1443, 0.1447, 0.1451, 0.1454, 0.1458, 0.1462, 0.1467, 0.1471, 0.1476), 
      M.median.M = c(13.21, 15.38, 16.01, 16.84, 17.27, 17.37, 17.42, 16.58, 16.23, 15.98, 15.81, 15.69, 15.58, 15.51, 15.46, 15.44, 15.47, 15.53, 15.62, 15.75, 15.89, 16.04, 16.2, 16.36, 16.53, 16.73, 16.94, 17.2, 17.48, 17.8, 18.14, 18.49, 18.85, 19.18, 19.51, 19.81, 20.09, 20.35, 20.8, 21.18, 21.52, 21.83, 22.1, 22.32, 22.52, 22.7, 22.85, 22.99, 23.11, 23.25, 23.37, 23.49, 23.62, 23.75, 23.88, 24.02, 24.15, 24.27, 24.39, 24.48, 24.58, 24.66, 24.75, 24.82, 24.9, 24.98, 25.05, 25.12, 25.19, 25.25, 25.3, 25.34, 25.37, 25.38, 25.37, 25.36, 25.34, 25.33, 25.32, 25.33, 25.35, 25.37, 25.4, 25.42, 25.45, 25.48, 25.49, 25.5, 25.49, 25.48, 25.44, 25.4, 25.35, 25.29, 25.21, 25.12, 25.02, 24.9, 24.75, 24.59, 24.43, 24.26, 24.08, 23.93, 23.78, 23.64, 23.51, 23.38), 
      L.skewness.F = c(0.27, 0.14, 0.12, 0.09, 0.06, 0.05, 0.03, -0.03, -0.06, -0.09, -0.13, -0.17, -0.22, -0.27, -0.31, -0.36, -0.41, -0.45, -0.5, -0.54, -0.57, -0.61, -0.64, -0.67, -0.69, -0.71, -0.73, -0.75, -0.77, -0.79, -0.81, -0.82, -0.84, -0.85, -0.87, -0.88, -0.9, -0.92, -0.95, -0.99, -1.03, -1.07, -1.12, -1.16, -1.19, -1.22, -1.25, -1.28, -1.29, -1.31, -1.32, -1.32, -1.32, -1.32, -1.31, -1.3, -1.28, -1.27, -1.25, -1.22, -1.2, -1.17, -1.14, -1.11, -1.08, -1.05, -1.01, -0.98, -0.94, -0.9, -0.86, -0.82, -0.78, -0.74, -0.7, -0.66, -0.62, -0.58, -0.54, -0.5, -0.47, -0.43, -0.39, -0.35, -0.31, -0.28, -0.24, -0.2, -0.17, -0.13, -0.1, -0.06, -0.03, 0.01, 0.04, 0.07, 0.1, 0.14, 0.17, 0.2, 0.23, 0.26, 0.29, 0.32, 0.35, 0.38, 0.41, 0.44), 
      S.variation.F = c(0.0842, 0.0797, 0.0794, 0.0795, 0.0797, 0.0797, 0.0798, 0.079, 0.0785, 0.0781, 0.078, 0.0781, 0.0785, 0.0792, 0.0803, 0.0817, 0.0834, 0.0855, 0.0879, 0.0907, 0.0937, 0.0968, 0.1001, 0.1033, 0.1064, 0.1094, 0.1121, 0.1145, 0.1164, 0.1181, 0.1193, 0.1202, 0.1207, 0.1209, 0.1208, 0.1206, 0.1202, 0.1198, 0.1189, 0.1185, 0.1187, 0.1197, 0.1213, 0.1231, 0.1253, 0.1277, 0.1301, 0.1325, 0.1347, 0.137, 0.139, 0.1411, 0.1429, 0.1447, 0.1465, 0.1482, 0.1498, 0.1514, 0.153, 0.1547, 0.1562, 0.1577, 0.1592, 0.1605, 0.1617, 0.1627, 0.1637, 0.1646, 0.1653, 0.166, 0.1665, 0.1671, 0.1676, 0.168, 0.1685, 0.169, 0.1695, 0.17, 0.1706, 0.1711, 0.1716, 0.1722, 0.1726, 0.173, 0.1733, 0.1736, 0.1739, 0.1743, 0.1746, 0.1752, 0.1757, 0.1763, 0.177, 0.1777, 0.1784, 0.1792, 0.18, 0.1808, 0.1816, 0.1825, 0.1834, 0.1844, 0.1855, 0.1866, 0.1878, 0.1891, 0.1904, 0.1917), 
      M.median.F = c(12.92, 15.04, 15.66, 16.54, 17.01, 17.12, 17.2, 16.44, 16.12, 15.86, 15.64, 15.45, 15.31, 15.2, 15.14, 15.13, 15.16, 15.22, 15.32, 15.44, 15.59, 15.76, 15.96, 16.18, 16.44, 16.73, 17.04, 17.38, 17.74, 18.12, 18.49, 18.85, 19.19, 19.48, 19.74, 19.96, 20.13, 20.26, 20.44, 20.54, 20.59, 20.61, 20.63, 20.65, 20.69, 20.74, 20.8, 20.88, 20.96, 21.05, 21.16, 21.27, 21.39, 21.53, 21.68, 21.84, 21.99, 22.14, 22.29, 22.43, 22.57, 22.71, 22.83, 22.96, 23.07, 23.18, 23.3, 23.4, 23.5, 23.59, 23.67, 23.75, 23.82, 23.87, 23.92, 23.96, 23.99, 24.03, 24.06, 24.1, 24.14, 24.18, 24.21, 24.23, 24.24, 24.25, 24.25, 24.25, 24.25, 24.24, 24.22, 24.2, 24.16, 24.1, 24.03, 23.95, 23.86, 23.75, 23.64, 23.54, 23.42, 23.29, 23.15, 23.02, 22.88, 22.75, 22.6, 22.46)
    ), 
    .Names = c("Age", "L.skewness.M", "S.variation.M", "M.median.M", "L.skewness.F", "S.variation.F", "M.median.F"), 
    class = "data.frame", 
    row.names = c(NA, -108L)
  )
  
  #### warning age range ####
  
  age_min <- min(RefZscore[, "Age"])  # 0
  age_max <- max(RefZscore[, "Age"])  # 87
  
  ages_out <- which(!is.na(age) & (age < age_min | age > age_max))
  
  if (length(ages_out) > 0) {
    warning(
      "[BMIZscore] ", length(ages_out), " individual(s) have age outside ",
      "the WHO reference range [", age_min, ", ", age_max, "] years. ",
      "Indices: ", paste(ages_out, collapse = ", "), ". ",
      "The nearest available reference age will be used, ",
      "but results may not be reliable.",
      call. = FALSE
    )
  }
  
  #### compute ####
  
  mapply(function(bmi_i, age_i, gender_i) {
    if (all(!is.na(c(bmi_i, age_i, gender_i)))) {
      iindex <- which.min(abs(age_i - RefZscore[, "Age"]))
      switch(EXPR = as.character(gender_i),
             "Male" = {
               (((bmi_i / RefZscore[iindex, "M.median.M"])^RefZscore[iindex, "L.skewness.M"]) -1) /
                 (RefZscore[iindex, "L.skewness.M"]*RefZscore[iindex, "S.variation.M"])
             },
             "Female" = {
               (((bmi_i / RefZscore[iindex, "M.median.F"])^RefZscore[iindex, "L.skewness.F"]) -1) /
                 (RefZscore[iindex, "L.skewness.F"]*RefZscore[iindex, "S.variation.F"])
             },
             "1" = {
               (((bmi_i / RefZscore[iindex, "M.median.M"])^RefZscore[iindex, "L.skewness.M"]) -1) /
                 (RefZscore[iindex, "L.skewness.M"]*RefZscore[iindex, "S.variation.M"])
             },
             "2" = {
               (((bmi_i / RefZscore[iindex, "M.median.F"])^RefZscore[iindex, "L.skewness.F"]) -1) /
                 (RefZscore[iindex, "L.skewness.F"]*RefZscore[iindex, "S.variation.F"])
             },
             NA
      )
    } else {
      NA
    }
  }, bmi, age, gender)
  
  
}


#### test #### 

#' @title BMIZscore_old
#' @description BMIZscore old version
BMIZscore_old <- function (bmi, age, gender) {
    RefZscore <- structure(
        list(
            Age = c(0, 0.2, 0.3, 0.5, 0.7, 0.8, 1, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5, 11, 11.5, 12, 12.5, 13, 13.5, 14, 14.5, 15, 15.5, 16, 16.5, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87), 
            L.skewness.M = c(0.24, -0.29, -0.37, -0.48, -0.53, -0.54, -0.55, -0.47, -0.41, -0.35, -0.32, -0.29, -0.29, -0.3, -0.33, -0.37, -0.41, -0.47, -0.53, -0.59, -0.66, -0.72, -0.77, -0.82, -0.87, -0.9, -0.92, -0.93, -0.93, -0.93, -0.91, -0.9, -0.87, -0.85, -0.82, -0.8, -0.77, -0.74, -0.69, -0.65, -0.61, -0.57, -0.54, -0.51, -0.5, -0.49, -0.48, -0.48, -0.48, -0.48, -0.47, -0.47, -0.47, -0.47, -0.47, -0.47, -0.47, -0.46, -0.45, -0.45, -0.43, -0.42, -0.41, -0.39, -0.37, -0.36, -0.34, -0.32, -0.31, -0.29, -0.28, -0.26, -0.25, -0.23, -0.22, -0.21, -0.2, -0.19, -0.18, -0.17, -0.16, -0.16, -0.15, -0.14, -0.14, -0.13, -0.13, -0.12, -0.12, -0.12, -0.12, -0.12, -0.12, -0.12, -0.12, -0.12, -0.12, -0.12, -0.13, -0.13, -0.14, -0.14, -0.15, -0.15, -0.16, -0.17, -0.17, -0.18), 
            S.variation.M = c(0.0925, 0.0864, 0.0852, 0.0832, 0.0815, 0.0808, 0.0794, 0.0741, 0.0726, 0.0718, 0.0716, 0.072, 0.0729, 0.0742, 0.0759, 0.0779, 0.0802, 0.0826, 0.0851, 0.0877, 0.0902, 0.0928, 0.0952, 0.0975, 0.0997, 0.1017, 0.1035, 0.1052, 0.1065, 0.1077, 0.1086, 0.1093, 0.1099, 0.1102, 0.1105, 0.1106, 0.1106, 0.1106, 0.1106, 0.1106, 0.1107, 0.1111, 0.1116, 0.1124, 0.1132, 0.1141, 0.115, 0.116, 0.1168, 0.1175, 0.1182, 0.1188, 0.1193, 0.1198, 0.1203, 0.1207, 0.121, 0.1214, 0.1218, 0.1221, 0.1226, 0.123, 0.1234, 0.1238, 0.1243, 0.1248, 0.1253, 0.1259, 0.1264, 0.1271, 0.1277, 0.1283, 0.129, 0.1296, 0.1303, 0.131, 0.1317, 0.1324, 0.1331, 0.1338, 0.1345, 0.1352, 0.1359, 0.1366, 0.1372, 0.1379, 0.1385, 0.1391, 0.1397, 0.1403, 0.1408, 0.1414, 0.1418, 0.1422, 0.1426, 0.143, 0.1434, 0.1437, 0.144, 0.1443, 0.1447, 0.1451, 0.1454, 0.1458, 0.1462, 0.1467, 0.1471, 0.1476), 
            M.median.M = c(13.21, 15.38, 16.01, 16.84, 17.27, 17.37, 17.42, 16.58, 16.23, 15.98, 15.81, 15.69, 15.58, 15.51, 15.46, 15.44, 15.47, 15.53, 15.62, 15.75, 15.89, 16.04, 16.2, 16.36, 16.53, 16.73, 16.94, 17.2, 17.48, 17.8, 18.14, 18.49, 18.85, 19.18, 19.51, 19.81, 20.09, 20.35, 20.8, 21.18, 21.52, 21.83, 22.1, 22.32, 22.52, 22.7, 22.85, 22.99, 23.11, 23.25, 23.37, 23.49, 23.62, 23.75, 23.88, 24.02, 24.15, 24.27, 24.39, 24.48, 24.58, 24.66, 24.75, 24.82, 24.9, 24.98, 25.05, 25.12, 25.19, 25.25, 25.3, 25.34, 25.37, 25.38, 25.37, 25.36, 25.34, 25.33, 25.32, 25.33, 25.35, 25.37, 25.4, 25.42, 25.45, 25.48, 25.49, 25.5, 25.49, 25.48, 25.44, 25.4, 25.35, 25.29, 25.21, 25.12, 25.02, 24.9, 24.75, 24.59, 24.43, 24.26, 24.08, 23.93, 23.78, 23.64, 23.51, 23.38), 
            L.skewness.F = c(0.27, 0.14, 0.12, 0.09, 0.06, 0.05, 0.03, -0.03, -0.06, -0.09, -0.13, -0.17, -0.22, -0.27, -0.31, -0.36, -0.41, -0.45, -0.5, -0.54, -0.57, -0.61, -0.64, -0.67, -0.69, -0.71, -0.73, -0.75, -0.77, -0.79, -0.81, -0.82, -0.84, -0.85, -0.87, -0.88, -0.9, -0.92, -0.95, -0.99, -1.03, -1.07, -1.12, -1.16, -1.19, -1.22, -1.25, -1.28, -1.29, -1.31, -1.32, -1.32, -1.32, -1.32, -1.31, -1.3, -1.28, -1.27, -1.25, -1.22, -1.2, -1.17, -1.14, -1.11, -1.08, -1.05, -1.01, -0.98, -0.94, -0.9, -0.86, -0.82, -0.78, -0.74, -0.7, -0.66, -0.62, -0.58, -0.54, -0.5, -0.47, -0.43, -0.39, -0.35, -0.31, -0.28, -0.24, -0.2, -0.17, -0.13, -0.1, -0.06, -0.03, 0.01, 0.04, 0.07, 0.1, 0.14, 0.17, 0.2, 0.23, 0.26, 0.29, 0.32, 0.35, 0.38, 0.41, 0.44), 
            S.variation.F = c(0.0842, 0.0797, 0.0794, 0.0795, 0.0797, 0.0797, 0.0798, 0.079, 0.0785, 0.0781, 0.078, 0.0781, 0.0785, 0.0792, 0.0803, 0.0817, 0.0834, 0.0855, 0.0879, 0.0907, 0.0937, 0.0968, 0.1001, 0.1033, 0.1064, 0.1094, 0.1121, 0.1145, 0.1164, 0.1181, 0.1193, 0.1202, 0.1207, 0.1209, 0.1208, 0.1206, 0.1202, 0.1198, 0.1189, 0.1185, 0.1187, 0.1197, 0.1213, 0.1231, 0.1253, 0.1277, 0.1301, 0.1325, 0.1347, 0.137, 0.139, 0.1411, 0.1429, 0.1447, 0.1465, 0.1482, 0.1498, 0.1514, 0.153, 0.1547, 0.1562, 0.1577, 0.1592, 0.1605, 0.1617, 0.1627, 0.1637, 0.1646, 0.1653, 0.166, 0.1665, 0.1671, 0.1676, 0.168, 0.1685, 0.169, 0.1695, 0.17, 0.1706, 0.1711, 0.1716, 0.1722, 0.1726, 0.173, 0.1733, 0.1736, 0.1739, 0.1743, 0.1746, 0.1752, 0.1757, 0.1763, 0.177, 0.1777, 0.1784, 0.1792, 0.18, 0.1808, 0.1816, 0.1825, 0.1834, 0.1844, 0.1855, 0.1866, 0.1878, 0.1891, 0.1904, 0.1917), 
            M.median.F = c(12.92, 15.04, 15.66, 16.54, 17.01, 17.12, 17.2, 16.44, 16.12, 15.86, 15.64, 15.45, 15.31, 15.2, 15.14, 15.13, 15.16, 15.22, 15.32, 15.44, 15.59, 15.76, 15.96, 16.18, 16.44, 16.73, 17.04, 17.38, 17.74, 18.12, 18.49, 18.85, 19.19, 19.48, 19.74, 19.96, 20.13, 20.26, 20.44, 20.54, 20.59, 20.61, 20.63, 20.65, 20.69, 20.74, 20.8, 20.88, 20.96, 21.05, 21.16, 21.27, 21.39, 21.53, 21.68, 21.84, 21.99, 22.14, 22.29, 22.43, 22.57, 22.71, 22.83, 22.96, 23.07, 23.18, 23.3, 23.4, 23.5, 23.59, 23.67, 23.75, 23.82, 23.87, 23.92, 23.96, 23.99, 24.03, 24.06, 24.1, 24.14, 24.18, 24.21, 24.23, 24.24, 24.25, 24.25, 24.25, 24.25, 24.24, 24.22, 24.2, 24.16, 24.1, 24.03, 23.95, 23.86, 23.75, 23.64, 23.54, 23.42, 23.29, 23.15, 23.02, 22.88, 22.75, 22.6, 22.46)
        ), 
        .Names = c("Age", "L.skewness.M", "S.variation.M", "M.median.M", "L.skewness.F", "S.variation.F", "M.median.F"), 
        class = "data.frame", 
        row.names = c(NA, -108L)
    )
    apply(cbind(bmi, age, gender), 1, function (irow) {
        if (all(!is.na(irow))) {
            iindex <- which.min(abs(as.numeric(irow[2])-RefZscore[, "Age"]))
            switch(EXPR = irow[3],
                "Male" = {
                    (((as.numeric(irow[1]) / RefZscore[iindex, "M.median.M"])^RefZscore[iindex, "L.skewness.M"]) -1) /
                    (RefZscore[iindex, "L.skewness.M"]*RefZscore[iindex, "S.variation.M"])
                },
                "Female" = {
                    (((as.numeric(irow[1]) / RefZscore[iindex, "M.median.F"])^RefZscore[iindex, "L.skewness.F"]) -1) /
                    (RefZscore[iindex, "L.skewness.F"]*RefZscore[iindex, "S.variation.F"])
                },
                "1" = {
                    (((as.numeric(irow[1]) / RefZscore[iindex, "M.median.M"])^RefZscore[iindex, "L.skewness.M"]) -1) /
                    (RefZscore[iindex, "L.skewness.M"]*RefZscore[iindex, "S.variation.M"])
                },
                "2" = {
                    (((as.numeric(irow[1]) / RefZscore[iindex, "M.median.F"])^RefZscore[iindex, "L.skewness.F"]) -1) /
                    (RefZscore[iindex, "L.skewness.F"]*RefZscore[iindex, "S.variation.F"])
                },
                NA
            )
        } else {
            NA
        }
    })
}


if (FALSE) {
  
  # === Test Bug 1 apply which.min cbind ===
  
  # Cas 1 : Valeurs normales - vérifier que les résultats sont identiques
  z1_old <- BMIZscore_old(bmi = 18, age = 10, gender = "Male")
  z1_new <- BMIZscore(bmi = 18, age = 10, gender = "Male")
  cat("Cas 1 - Identiques :", isTRUE(all.equal(z1_old, z1_new)), "\n")
  
  # Cas 2 : BMI très précis (décimales) - le vrai test du bug
  # Avec cbind(), 18.756 devient "18.756" puis as.numeric("18.756")
  # Avec mapply(), 18.756 reste directement 18.756
  z2_old <- BMIZscore_old(bmi = 18.756123456789, age = 10.3, gender = "Male")
  z2_new <- BMIZscore(bmi = 18.756123456789, age = 10.3, gender = "Male")
  cat("Cas 2 - Précision décimale identique :", isTRUE(all.equal(z2_old, z2_new)), "\n")
  
  # Cas 3 : Vecteur avec NA
  z3_old <- BMIZscore_old(
    bmi    = c(18, NA, 16, 20),
    age    = c(10, 10,  8, 12),
    gender = c("Male", "Male", "Female", "Male")
  )
  z3_new <- BMIZscore(
    bmi    = c(18, NA, 16, 20),
    age    = c(10, 10,  8, 12),
    gender = c("Male", "Male", "Female", "Male")
  )
  cat("Cas 3 - NA bien géré :", is.na(z3_new[2]), "\n")
  cat("Cas 3 - Résultats identiques :", isTRUE(all.equal(z3_old, z3_new)), "\n")
  
  # Cas 4 : Gender numérique "1" et "2"
  z4_old <- BMIZscore_old(bmi = c(18, 16), age = c(10, 8), gender = c("1", "2"))
  z4_new <- BMIZscore(bmi = c(18, 16), age = c(10, 8), gender = c("1", "2"))
  cat("Cas 4 - Gender numérique identique :", isTRUE(all.equal(z4_old, z4_new)), "\n")
  
  # Cas 5 : Gender invalide → doit retourner NA
  z5_new <- BMIZscore(bmi = 18, age = 10, gender = "Unknown")
  cat("Cas 5 - Gender invalide retourne NA :", is.na(z5_new), "\n")
  
  
  # === Test Bug 2 : age range ===
  
  # Cas 1 : Age dans la plage → pas de warning
  cat("Cas 1 - Pas de warning attendu :\n")
  z1 <- withCallingHandlers(
    BMIZscore(bmi = 18, age = 10, gender = "Male"),
    warning = function(w) cat("WARNING:", conditionMessage(w), "\n")
  )
  
  # Cas 2 : Age hors plage (trop grand)
  cat("\nCas 2 - Warning attendu (age = 150) :\n")
  z2 <- withCallingHandlers(
    BMIZscore(bmi = 18, age = 150, gender = "Male"),
    warning = function(w) cat("WARNING:", conditionMessage(w), "\n")
  )
  
  # Cas 3 : Vecteur mixte (certains dans la plage, certains hors)
  cat("\nCas 3 - Warning attendu pour indices 2 et 4 :\n")
  z3 <- withCallingHandlers(
    BMIZscore(
      bmi    = c(18, 20, 16, 22),
      age    = c(10, 150,  8, -1),   # indices 2 et 4 hors plage
      gender = c("Male", "Male", "Female", "Male")
    ),
    warning = function(w) cat("WARNING:", conditionMessage(w), "\n")
  )
  
  # Cas 4 : Age = NA → pas de warning (NA ignoré)
  cat("\nCas 4 - Pas de warning pour NA :\n")
  z4 <- withCallingHandlers(
    BMIZscore(bmi = 18, age = NA, gender = "Male"),
    warning = function(w) cat("WARNING:", conditionMessage(w), "\n")
  )
  cat("Résultat NA :", is.na(z4), "\n")
}


