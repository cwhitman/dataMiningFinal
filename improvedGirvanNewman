library(igraph)

#' GirvanNewman algorithm to find communities.
#' Based on https://arxiv.org/abs/cond-mat/0308217
#' @param g igraph graph
#' @return graph whose disconnected components represent each community
improvedGirvanNewman <- function(g) {
  
  # Find modularity
  communities <- components(g)
  prevCommunities <- communities
  prevg <- g
  mod <- modularity(g, communities$membership)
  prevMod <- mod
  
  # While the modularity increases,
  maxIterations <- 1000000
  iteration <- 1
  while(mod >= prevMod & iteration < maxIterations) {
    iteration <- iteration +1
    
    # Calcualte betweenness
    betweeness <- edge.betweenness(g)
    
    # Delete the edges with maximum betweenness
    toDelete <- E(g)[which(max(betweeness)==betweeness)]

    # Delete all edges with high edge betweenness score.
    prevg <- g
    g <- delete.edges(g,toDelete)
    
    # Find communities from connected components. 
    prevCommunites <- communities
    communities <- components(g)
    
    # Recalulate modularity
    prevMod <- mod
    mod <- modularity(g, communities$membership)
    
    # If the graph is esmpty, stop.
    if(length(E(g)) ==0) {
      break
    }
  }
  
  return(prevg)
}
