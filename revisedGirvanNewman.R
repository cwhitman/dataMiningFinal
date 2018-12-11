source('groupEdgeBetweeness.R')

#' GirvanNewman revised Algorithm
#' @param g igraph graph to mazimize.
#' @return a list containing the resulting graph, modularity values per iterations, and number of edges with the highest edge betweenness score per iteration.
girvanNewmanRevised <- function(g) {
  require(igraph)
  # Find modularity
  communities <- components(g)
  prevCommunities <- communities
  prevg <- g
  mod <- modularity(g, communities$membership)
  prevMod <- mod
  
  maxIterations <- 10000
  iteration <- 1
  modularityList <- vector(length=maxIterations)
  multiEdge <- vector(length = maxIterations)
  while(mod >= prevMod & iteration < maxIterations) {
    iteration <- iteration +1
    prevg <- g
    
    print(iteration)
    modularityList[iteration] <- mod
    # Calcualte betweenness
    betweeness <- edge.betweenness(g)
    
    # Delete the edges
    toDelete <- E(g)[which(max(betweeness)==betweeness)]
    
    # If multiple edges have maximum edge betweeness, then use group betweeness algorithm.
    edgeSize <- length(toDelete)
    multiEdge[iteration] <- edgeSize
    l <- 1
    while(edgeSize>1){
      l<-l+1
      
      # All possible subsets of size .
      subsets <- combn(toDelete,l)
      
      groupBetweeness <- list()
      for(subsetI in 1:ncol(subsets))
      {
        groupBetweeness[[subsetI]] <- groupEdgeBetweeness(g,E(g)[subsets[,subsetI]])
      }
      groupEdges <- which(groupBetweeness==max(unlist(groupBetweeness)))
      
      edgeSize <- length(groupEdges)
      toDelete <- E(g)[unique(unlist(as.vector(subsets[,groupEdges])))]
    }

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
  
  return(list(graph=prevg,mod=modularityList,edge=multiEdge))
}
