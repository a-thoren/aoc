library(tidyverse)

input <- readLines("inputs/day1.txt") |> 
  stringr::str_split(" +")

left <- vector("numeric", length(input))
right <- vector("numeric", length(input))
for (i in seq_along(input)) {
  t <- as.numeric(input[[i]])
  left[i] <- t[1]
  right[i] <- t[2]
}

sum(abs(sort(left) - sort(right)))

t <- table(right)
sum(left * t[match(left, names(t))], na.rm = TRUE)
