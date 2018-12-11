library(igraph)

#' Computes the group edge betweeness scores.
#' @param M_i A set of igraph edges.
#' @param g the igraph.
#' @return The group edge betweeness scores. 
groupEdgeBetweeness <- function(g,M_i) {

num_nodes <- length(V(g))

# Loop over all pairs of nodes.
groupBetweeness <- 0
for(i in 1:(num_nodes-1)) {
  for(j in (i+1):num_nodes) {
    
    # Find all shortest paths from node i to node j.
    allShortestPath <-all_shortest_paths(g, from=i, to = j, mode = c("all"),weights = NULL)
    numShortestPaths<- length(allShortestPath$res)
    
    numShortestPathsWithEdges <- 0
    # Calculate the number of these shortest paths that contains any edge M_I
    for(shortestPath in allShortestPath$res) {
      
      edgeExists <- FALSE
      # Iterate through edges.
      for(edgeI in 1:length(M_i)) {
        
        # See if any shortest paths are within the graph.
        edgeHead <- head_of(g,M_i[edgeI])
        edgeTail <- tail_of(g,M_i[edgeI])
        
        # See if any of the verticies in shortestPath contains the edge head.
        whichEdgeHeads <- which(edgeHead==shortestPath)
        
        # See if any of the verticies on either the left or right of the edge head is the edge tail.
        edgeExists <- edgeExists | any(
          unlist(sapply(whichEdgeHeads,function(whichEdgeHead){
            shortestPath[max(whichEdgeHead-1,1)]==edgeTail | 
                 shortestPath[min(whichEdgeHead-1,length(shortestPath))]==edgeTail})))
        
      }
      numShortestPathsWithEdges<- numShortestPathsWithEdges+edgeExists
    }
    
    groupBetweeness <- groupBetweeness + (numShortestPathsWithEdges/numShortestPaths)
    
  }
  
  
}

return(groupBetweeness)
}
