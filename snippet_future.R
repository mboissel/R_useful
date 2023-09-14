message(timestamp(quiet = TRUE))
### Project Setup ==================================================================================

### Load Packages ==================================================================================
renv::install("future")
renv::install("future.apply")
suppressPackageStartupMessages({
  library(future)
  library(future.apply) # https://github.com/HenrikBengtsson/future.apply
})

future::plan()
# sequential: Resolves futures sequentially in the current R process.
# transparent: Resolves futures sequentially in the current R process and assignments will be done to the calling environment. Early stopping is enabled by default.
# multisession: Resolves futures asynchronously (in parallel) in separate R sessions running in the background on the same machine.
# multicore: Resolves futures asynchronously (in parallel) in separate forked R processes running in the background on the same machine. Not supported on Windows.
future::plan(sequential) # interactive
future::plan(strategy = multisession(workers = 10))

### Analysis =======================================================================================
# y <- lapply(mtcars, FUN = mean, trim = 0.10)
y <- future_lapply(mtcars, FUN = mean, trim = 0.10)

autre <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
autre2 <- c(45, 45, 45, 54, 54, 54, 54, 5, 45, 4, 54, 54, 545, 4)
z <- future_lapply(
  X = mtcars,
  autre_sec = autre2,
  future.globals = FALSE,
  FUN = function(x, autre_sec) {
    mean(c(x, autre_sec), trim = 0.10)
  }
)

### Complete =======================================================================================
message("Success!", appendLF = TRUE)
message(timestamp(quiet = TRUE))


## Explicitly close multisession workers by switching plan
# plan(sequential)
