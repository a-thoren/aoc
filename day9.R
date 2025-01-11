library(tidyverse)

input <- readLines("inputs/day9.txt") |> 
  strsplit("") |> 
  unlist() |> 
  as.numeric()

blocks <-   blocks <- sapply(
  seq(input),
  \(i) rep(
    ifelse(
      i %% 2 == 1,
      floor(i / 2),
      "."
    ),
    input[i]
  )
) |> 
  unlist()

part_1 <- function(blocks) {
  idx <- blocks == "."
  blocks_rearranged <- blocks
  blocks_rearranged[which(idx)] <- rev(blocks_rearranged[which(!idx)])[1:sum(idx)]
  blocks_rearranged <- blocks_rearranged[1:sum(!idx)]
  
  return(sum(as.numeric(blocks_rearranged) * (seq(blocks_rearranged) - 1)))
}

part_2 <- function(input, blocks) {
  free <- input[seq(2, length(input), 2)]
  cumulative_idx <- cumsum(input)
  free <- data.frame(
    idx = cumulative_idx[seq(free) * 2 - 1] + 1,
    length = free
  )
  for (block in rev(unique(blocks[blocks != "."]))) {
    i <- which(blocks == block)
    tmp_free <- free |> 
      filter(idx < min(i), length >= length(i)) |> 
      arrange(idx) |> 
      slice(1)
    if (nrow(tmp_free) > 0) {
      blocks[tmp_free$idx:(tmp_free$idx + length(i) - 1)] <- block
      blocks[i] <- "."
      if (length(i) < tmp_free$length) {
        free <- bind_rows(
          free,
          tmp_free |> 
            mutate(
              idx = idx + length(i),
              length = length - length(i)
            )
        )
      }
      free <- anti_join(free, tmp_free, by = "idx")
    }
  }
  
  blocks <- gsub("\\.", "0", blocks)
  return(sum(as.numeric(blocks) * (seq(blocks) - 1)))
}

part_1(blocks)
part_2(input, blocks)