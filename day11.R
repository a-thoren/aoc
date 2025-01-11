input <- readLines("inputs/day11.txt") |>
  stringr::str_extract_all("\\d+", TRUE) |> 
  as.numeric()

n_digits <- function(x) {
  return(floor(log10(x)) + 1)
}

split_number <- function(x) {
  n_dig <- n_digits(x)
  if (!n_dig %% 2 == 0) stop("x does not contain an even number of digits")
  number_1 <- floor(x / 10^(n_dig / 2))
  number_2 <- x - number_1 * 10^(n_dig / 2)
  return(c(number_1, number_2))
}

process_stone <- function(x) {
  if (x == 0) {
    return(1)
  } else if (n_digits(x) %% 2 == 0) {
    return(split_number(x))
  } else {
    return(x * 2024)
  }
}

blink <- function(input) {
  unlist(sapply(input, process_stone))
}

cache <- data.frame()

solve <- function(stone, blinks) {
  if (blinks == 0) return(1)
  c <- cache |> 
    filter(stone == .env$stone, blinks == .env$blinks)
  if (nrow(c) > 0) {
    return(pull(c, val))
  } else {
    val <- sum(unlist(sapply(blink(stone), solve, blinks = blinks - 1)))
    cache <<- bind_rows(
      cache,
      data.frame(stone = stone, blinks = blinks, val = val)
    )
    return(val)
  }
}

sapply(input, solve, blinks = 25) |> sum()
sapply(input, solve, blinks = 75) |> sum()
