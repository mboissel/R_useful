
library(MatchIt)

data(lalonde) ## load data (toydata from package MatchIt)

head(lalonde) ## glance
# treat age educ black hispan married nodegree re74 re75       re78
# NSW1     1  37   11     1      0       1        1    0    0  9930.0460
# NSW2     1  22    9     0      1       0        1    0    0  3595.8940
# NSW3     1  30   12     1      0       0        0    0    0 24909.4500
# NSW4     1  27   11     1      0       0        1    0    0  7506.1460
# NSW5     1  33    8     1      0       0        1    0    0   289.7899
# NSW6     1  22    9     1      0       0        1    0    0  4056.4940

summary(as.factor(lalonde$treat))
# 0   1
# 429 185

#### STEP 1 : get matching nearest ####
set.seed(42)
to_match <- matchit(treat ~ age + married, data = lalonde, method = "nearest")
head(to_match$match.matrix) ## glance at paired data
# 1
# NSW1 "PSID349"
# NSW2 "PSID333"
# NSW3 "PSID199"
# NSW4 "PSID150"
# NSW5 "PSID217"
# NSW6 "PSID388"
dim(to_match$match.matrix)
# [1] 185   1

## check some of them
lalonde[which(rownames(lalonde) == "NSW1" | rownames(lalonde) == "PSID349"), ]
# treat age educ black hispan married nodegree     re74     re75     re78
# NSW1        1  37   11     1      0       1        1   0.0000    0 9930.046
# PSID349     0  37    7     0      0       1        1 963.9593    0    0.000

lalonde[which(rownames(lalonde) == "NSW3" | rownames(lalonde) == "PSID333"), ]
# treat age educ black hispan married nodegree     re74     re75     re78
# NSW2        1  22    9     0      1       0        1    0.000    0  3595.894
# PSID333     0  19    6     1      0       0        1 1955.348    0 14998.920

lalonde[which(rownames(lalonde) == "NSW2" | rownames(lalonde) == "PSID199"), ]
# treat age educ black hispan married nodegree     re74     re75     re78
# NSW2        1  22    9     0      1       0        1    0.000    0.000 3595.894
# PSID199     0  29   10     0      1       0        1 3732.403 1323.048 6694.101

#### STEP 2 : only keep matched data  ####
all_data_matched <- match.data(to_match, group = "all")
group_treated <- match.data(to_match, group = "treat")
group_control <- match.data(to_match, group = "control")

dim(all_data_matched)
# [1] 370  12
dim(group_treated)
# [1] 185  12
dim(group_control)
# [1] 185  12

#### STEP 3 : question ? ####

## difference of income (re78) between 2 groups ctrl and treated ?

## statistiquement : t-test

t.test(all_data_matched$re78 ~ all_data_matched$treat)
# p-value = 0.2661

#### viz ####
# library(ggplot2)
# ggplot(all_data_matched, aes(x = re78, y = as.factor(treat))) + 
#   geom_boxplot()

#### Rolling windows for matching on age ####

library(MatchIt)

# to_match <- matchit(
#   treat ~ age + married + educ,
#   data = lalonde,
#   method = "nearest",
#   caliper = c(age = 5, educ = 2)
# )

to_match <- matchit(
  treat ~ age + married,
  data = lalonde,
  method = "nearest",
  caliper = c(age = 5), # apply tolerance +/- 5 years
  std.caliper = FALSE
)

# sum up
summary(to_match)

# matched data
matched_data <- match.data(to_match)
head(matched_data)
tail(matched_data)
plot(to_match)
