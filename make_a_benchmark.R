###############################
#### Make a microbenchmark ####
###############################

#### pacakge microbenchmark ####
library(microbenchmark)

## ex Test the difference with writeRDS/readRDS and write_rds/read_rds
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

#### pacakge bench ####
library(bench)

i <- 1e3
bm <- mark(
  seq(i),
  1:i,
  seq_len(i)
)
autoplot(bm, type = "violin")