lines <- readLines("inputs/day5.txt")

rules <- lines[1:(which(lines == "") - 1)] |> stringr::str_split("\\|")

updates <- lines[(which(lines == "") + 1):length(lines)] |> stringr::str_split(",")

which_okay <- c()

for (update in updates) {
  okay <- TRUE
  for (rule in rules) {
    if (all(rule %in% update)) {
      m <- match(rule, update)
      if (m[2] < m[1]) {
        okay <- FALSE
        break
      }
    }
  }
  which_okay <- c(which_okay, okay)
}

sapply(updates[which_okay], \(x) x[ceiling(length(x) / 2)]) |>
  as.numeric() |>
  sum()

is_okay <- function(update) {
  okay <- TRUE
  for (rule in rules) {
    if (all(rule %in% update)) {
      m <- match(rule, update)
      if (m[2] < m[1]) {
        okay <- FALSE
        break
      }
    }
  }
  return(okay)
}

rearrange <- function(update) {
  for (rule in rules) {
    if (all(rule %in% update)) {
      m <- match(rule, update)
      if (m[1] > m[2]) {
        update <- append(update[-m[1]], update[m[1]], m[2] - 1)
      }
    }
  }
  return(update)
}

fixed_updates <- list()
for (i in which(!which_okay)) {
  update <- updates[[i]]
  while(!is_okay(update)) {
    update <- rearrange(update)
  }
  fixed_updates <- c(fixed_updates, list(update))
}

sapply(fixed_updates, \(x) x[ceiling(length(x) / 2)]) |> as.numeric() |> sum()
