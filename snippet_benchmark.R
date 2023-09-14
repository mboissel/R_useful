################################
## snippet to run a benchmark ##
################################
## (i.e. compare chunk of code, optimise running time...)

renv::install("microbenchmark")
renv::install("bench")
renv::install("ggplot2")
renv::install("magrittr")

## load package
library(microbenchmark) # example 1
library(bench) ## example 2

library(ggplot2) ## for the vizualisations
`%>%` <- magrittr::`%>%`

####  example 1 : microbenchmark  ==================================================================

## want to test 3 functions with different method
func_add4 <- function(x) {x+x+x+x}
func_by4 <- function(x) {4*x}
func_verybad <- function(x) {(((((((x)+((x)))+(x))+(x)))))}

mybenchmark_1 <- microbenchmark::microbenchmark(
  func_add4(1),
  func_by4(1),
  func_verybad(1),
  times = 100
) 
mybenchmark_1

## vizualisation 
mybenchmark_1 %>%
  as.data.frame() %>%
  mutate(
    t = microbenchmark:::convert_to_unit(time, "s")
  ) %>%
  ggplot(aes(x = expr, y = t, fill = expr)) +
  geom_boxplot() +
  # geom_violin(colour = "white") +
  labs(y = "Time (s)", x = NULL) +
  scale_y_continuous(limits = c(0, 0.000002)) + ## zoom but /!\  mask some values
  coord_flip()

#### with mark function : Compare seq ==============================================================

i <- 1e3
mybenchmark_2 <- bench::mark(
  seq(i),
  1:i,
  seq_len(i)
)
mybenchmark_2

## vizualisation 
autoplot(mybenchmark_2, type = "violin")


#### Compare read RDS ==============================================================================
##  test the difference with writeRDS/readRDS and write_rds/read_rds

path <- "/disk/PROJECT/abc/Afile.RDS"

microbenchmark::microbenchmark(
  read_rds(path),
  readRDS(path)
) %>%
  as.data.frame() %>%
  mutate(
    t = microbenchmark:::convert_to_unit(time, "s")
  ) %>%
  ggplot(aes(x = expr, y = t, fill = expr)) +
  geom_violin(colour = "white") +
  scale_fill_viridis(discrete = TRUE, guide = FALSE) +
  labs(y = "Time (s)", x = NULL) +
  scale_y_continuous(limits = c(0, 0.02)) +
  coord_flip()


#### Optim read a vcf ==============================================================================
path_snps
micben <- microbenchmark::microbenchmark(
  readtab = {
    data_snps = read.table(file = path_snps)
  }, 
  readtab_with_sep = {
    data_snps = read.table(file = path_snps, sep = "\t", header = FALSE) 
  }, 
  fread = {
    data_snps = data.table::fread(
      file = path_snps, 
      sep = "\t", 
      header = FALSE, 
      skip = "##"
    )
  }, 
  readtsv = {
    data_snps = read_tsv(
      file = path_snps,
      col_names = TRUE, 
      comment = "##", 
      cols(
        .default = col_character()
      )
    )
  }, 
  vroom = {
    data_snps =vroom::vroom(
      file = path_snps,
      delim = "\t", 
      col_names = FALSE, 
      col_types = cols(
        .default = col_character()
      ), 
      comment = "##"
    ) 
  }, 
  times = 10
)
# Unit: milliseconds                                                                                                                                  
#              expr      min       lq     mean   median       uq       max neval
#           readtab 55.50217 57.39520 65.62417 61.19451 68.90146 239.41159   100
#  readtab_with_sep 62.76394 65.30214 71.59428 68.83043 76.09885  93.03115   100
#             fread 15.29426 18.74580 23.52727 23.69598 25.74983  47.78111   100
#           readtsv 30.95760 32.56723 37.36739 34.63921 42.38080  55.88072   100
#             vroom 10.43327 15.55742 17.34348 16.50241 19.38034  30.64008   100
## VROOM WINS ! 
autoplot(micben)


