library(igraph)

# Read in file and create i-graph.
tvshows <- read.csv("tvshow_edges.csv")
tvshows <-tvshows+1
g <- graph_from_edgelist(as.matrix(tvshows),directed=FALSE)

# Make example graph.
# g <- make_full_graph(5) %du% make_full_graph(5) %du% make_full_graph(5)
# g <- add_edges(g, c(1,6, 1,11, 6, 11))

# wtc <- cluster_walktrap(g)

# GirvanNewman algorithm to find communities.
# param g: igraph graph
# return: graph whose disconnected components represent each community
girvanNewman <- function(g) {
  
  # Find modularity
  communities <- components(g)
  prevCommunities <- communities
  prevg <- g
  mod <- modularity(g, communities$membership)
  prevMod <- mod
  
  maxIterations <- 1000000
  iteration <- 1
  while(mod >= prevMod & iteration < maxIterations) {
    iteration <- iteration +1
    
    # Calcualte betweenness
    betweeness <- edge.betweenness(g)
    
    # Delete the edges with maximum betweenness
    toDelete <- E(g)[which(max(betweeness)==betweeness)]
    
    # Randomly choose 1 edge to delete.
    toDelete <- sample(toDelete,1)

    
    prevg <- g
    g <- delete.edges(g,toDelete)
    
    # Find communities from connected components. 
    prevCommunites <- communities
    communities <- components(g)
    
    # Recalulate modularity
    prevMod <- mod
    mod <- modularity(g, communities$membership)
    
    if(length(E(g)) ==0) {
      break
    }
  }
  
  return(prevg)
}
