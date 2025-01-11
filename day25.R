input <- readLines("inputs/day25.txt") |> 
  strsplit("") |> 
  do.call(what = rbind)

locks <- list()
keys <- list()

for (i in seq(1, nrow(input), 7)) {
  tmp <- input[i:(i + 6), ]
  if (tmp[1, 1] == ".") {
    key <- TRUE
    tmp[tmp == "#"] <- 7 - which(tmp == "#", arr.ind = TRUE)[, 1]
  } else {
    key <- FALSE
    tmp[tmp == "#"] <- which(tmp == "#", arr.ind = TRUE)[, 1] - 1
  }
  tmp[tmp == "."] <- 0
  tmp <- apply(tmp, 2, as.numeric)
  v <- apply(tmp, 2, max)
  if (key) {
    keys <- c(keys, list(v))
  } else {
    locks <- c(locks, list(v))
  }
}

locks <- do.call(rbind, locks)
keys <- do.call(rbind, keys)
s <- 0
for (i in seq_len(nrow(keys))) {
  key <- keys[i, ]
  r <- apply(locks + rep(key, each = nrow(locks)), 1, \(x) all(x <= 5))
  s <- s + sum(r)
}
s