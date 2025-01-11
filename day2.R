library(tidyverse)

lines <- readLines("./inputs/day2.txt") |> 
  stringr::str_split(" ") |> 
  lapply(as.numeric)

safe <- 0

for (l in lines) {
  len <- length(l)
  diff <- l[2:len] - l[1:(len - 1)]
  safe <- safe + (all(sign(diff[1]) == sign(diff)) && all(between(abs(diff), 1, 3)))
}

safe

part_2 <- function(l, recursed = FALSE) {
  diff <- diff(l)
  sign_table <- table(sign(diff))
  common_sign <- names(sign_table)[which.max(sign_table)]
  s <- common_sign == sign(diff)
  d <- between(abs(diff), 1, 3)
  b <- s & d
  if (!all(b) && !recursed) {
    l <- l[-which(!b)[1]]
    return(part_2(l, TRUE))
  }
  return(all(b))
} 

f <- function(l) {
  diff <- diff(l)
  return(all(sign(diff[1]) == sign(diff) & between(abs(diff), 1,3)))
}

part_2_2 <- function(l) {
  for (i in seq_along(l)) {
    if (f(l[-i])) {
      return(TRUE)
    }
  }
  return(FALSE)
}

part_2(c(7,6,4,2,1))
part_2(c(1,2,7,8,9))
part_2(c(9,7,6,2,1))
part_2(c(8,6,4,4,1))
part_2(c(1,3,6,7,9))
part_2(c(93,92,91,90,88))
part_2(c(3,1,2,3,4,5))

res <- sapply(lines, part_2_2)
sum(res)

res_2 <- sapply(lines, part_2)

sum(res != res_2)

test <- lines[which(res != res_2)]

test[[1]]
part_2_2(test[[1]])
part_2(test[[1]])
