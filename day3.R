library(tidyverse)

raw_lines <- readLines("inputs/day3.txt")

lines <- stringr::str_extract_all(raw_lines, "mul\\(\\d+,\\d+\\)")
s <- 0

f <- function(line) {
  digits <- stringr::str_extract_all(line, "\\d+")
  sum(sapply(digits, \(x) prod(as.numeric(x))))
}

sapply(lines, f) |> sum()

lines <- stringr::str_extract_all(raw_lines, "(mul\\(\\d+,\\d+\\)|do(n't){0,1}\\(\\))") |> unlist()

part_2 <- function(line) {
  dont <- which(line == "don't()")
  do <- c(which(line == "do()"), length(line) + 1)
  idx <- c()
  for (i in 1:length(dont)) {
    if (dont[i] %in% idx)
      next
    idx <- c(idx, dont[i]:do[which(do > dont[i])[1]])
  }
  line <- line[-c(idx, do, dont)]
  line |> stringr::str_extract_all("\\d+")
  return(f(line))
}

part_2_2 <- function(line) {
  s <- 0
  prev <- "do()"
  for (x in line) {
    if (grepl("do", x)) {
      prev <- x
      next
    }
    if (prev == "don't()") {
      next
    }
    s <- s + stringr::str_extract_all(x, "\\d+", TRUE) |> as.numeric() |> prod()
  }
  return(s)
}

part_2(lines)
part_2_2(lines)
