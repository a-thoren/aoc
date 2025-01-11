input <- as.numeric(readLines("inputs/day22.txt"))


prices <- matrix(0, nrow = 2001, ncol = length(input))
diff <- matrix(NA, nrow = 2001, ncol = length(input))
seq <- matrix(NA, nrow = 2001, ncol = length(input))
s <- input
prices[1, ] <- s %% 10
for (i in 2:2001) {
  s <- bitwXor((s * 64) %% 16777216, s)
  s <- bitwXor(s %/% 32, s) %% 16777216
  s <- bitwXor((s * 2048) %% 16777216, s)
  prices[i, ] <- s %% 10
  diff[i, ] <- prices[i, ] - prices[i - 1, ]
  if (i >= 5) seq[i, ] <- colSums((diff[(i - 3):i, ] + 9) * 100^(3:0))
}

sum(s)

b <- rep(0, 100^4)

for (i in seq_along(input)) {
  d <- !duplicated(seq[, i]) & !is.na(seq[, i]) # row idx of first occurence of each sequence
  s <- seq[d, i] # the sequences for input i
  b[s] <- b[s] + prices[d, i]
}

print(max(b))
