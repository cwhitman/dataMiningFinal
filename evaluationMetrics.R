#############################################
# Script to evaluate our graphs.
#############################################
require(igraph)

##################################
# Load in graphs.
###################################

# TODO: Load in Amazon data.
tvshows <- read.csv("tvshow_edges.csv")
tvshows <-tvshows+1
g <- graph_from_edgelist(as.matrix(tvshows),directed=FALSE)

# TODO: Change to actual graphs.
communitiesRevised <- sample(1:100,length(V(g)),replace=TRUE)
communitiesOriginal <- sample(1:100,length(V(g)),replace=TRUE)
communitiesImproved <- sample(1:100,length(V(g)),replace=TRUE)
communities <- list('original'=communitiesOriginal,'revised'=communitiesRevised,'improved'=communitiesImproved) 


####################################
# Evaluate.
####################################

# Modularity
mod <- c(original=0,revised=0,improved=0)

mod['original'] <- modularity(g,communitiesOriginal)
mod['revised'] <- modularity(g,communitiesRevised)
mod['improved'] <- modularity(g,communitiesImproved)


print("----Modularity-----")
print(mod)

# NMI Score (Normalzied Mutual Information)


NMI <- matrix(0,nrow=length(communities),ncol=length(communities),dimnames=list(names(communities),
                                         names(communities)))

for(rowI in 1:length(communities)) {
  for(colI in 1:length(communities)) {
    NMI[rowI,colI] <- compare(communities[[rowI]],communities[[colI]],method="nmi")
  }
}


print("----NMI-----")
print(NMI)

