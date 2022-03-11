library(igraph)
library(GGally)

important_graphs = function(g){
  G = list()
  dg = decompose.graph(g)
  for(i in 1:length(dg)){
    if (gorder(dg[[i]])>3)
      G[[i]] = dg[i]
  }
  return(G)
}

pretty_plot = function(g){
  b = edge_attr(g)
  c = vertex_attr(g)
  # c
  lout = layout_nicely(g)
  plot(g, edge.arrow.size = .1,
       edge.label = NA,
       layout = layout_nicely,
       curved = T,
       vertex.color = c$Sit69,
       cex = .5,
       layout = lout,
       vertex.label = NA,
       vertex.size = degree(g, mode = 'out')
  )
}