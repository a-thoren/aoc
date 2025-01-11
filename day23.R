input <- readLines("inputs/day23.txt") |> 
  strsplit("-")

nodes <- unique(unlist(input))

g <- igraph::make_undirected_graph(n = length(nodes), edges = NULL) |>
  igraph::set_vertex_attr("name", value = nodes)

for (i in seq_along(input)) {
  g <- igraph::add_edges(g, match(input[[i]], nodes))
}

t <- igraph::cliques(g, min = 3)
sum(sapply(t, \(x) any(startsWith(names(x), "t")) && length(x) == 3))

n <- names(t[[which.max(sapply(t, length))]])
paste0(sort(n), collapse = ",")