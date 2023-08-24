## inspect difference ##


#### test Waldo ####

# renv::install("waldo")
library("waldo")

# addition
compare(c("a", "b", "c"), c("a", "b"))
#> `old`: "a" "b" "c"
#> `new`: "a" "b"


# deletion
compare(c("a", "b"), c("a", "b", "c"))
#> `old`: "a" "b"    
#> `new`: "a" "b" "c"

# modification
compare(c("a", "b", "c"), c("a", "B", "c"))
#> `old`: "a" "b" "c"
#> `new`: "a" "B" "c"

## test on dta 
compare(iris, rev(iris))
compare(head(iris, 10), head(iris, 11))


#### Diffy ####

## to check difference in deps https://diffify.com/R/

#### list pkg dep ####

# write.csv(
#   x = sapply(
#     unique(renv::dependencies()[["Package"]]),
#     function(x) as.character(packageVersion(x))
#   ),
#   file = "logs/depR412.csv"
# )

sapply(
  unique(renv::dependencies()[["Package"]]),
  function(x) as.character(packageVersion(x))
)

