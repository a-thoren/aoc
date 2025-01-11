input <- readLines("inputs/day19.txt")

patterns <- input[1] |> 
  stringr::str_split(",", simplify = TRUE) |> 
  stringr::str_trim()
designs <- input[3:length(input)]

cache <- collections::dict()
count_designs_3 <- function(design) {
  if (cache$has(design)) return(cache$get(design))
  if (design == "") return(1)
  
  s <- 0
  for (pattern in patterns) {
    if (startsWith(design, pattern)) {
      s <- s + count_designs_3(gsub(paste0("^", pattern), "", design))
    }
  }
  cache$set(design, s)
  return(s)
}

tictoc::tic("dict cache, loop")
res <- sapply(designs, count_designs_3)
tictoc::toc()

sum(res > 0)
sum(res)