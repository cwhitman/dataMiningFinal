# dataMiningFinal

Code for datamining final project. 

Implements the Girvan-Newman Clustering Algorithm and two improved versions.

## Algorithms

 - originalGirvanNewman.R implements the original algorithm described [here](https://arxiv.org/abs/cond-mat/0308217).

 - improvedGirvanNewman.R implements an improved version of the algorithm described [here](Community structure in networks: Girvan-Newman algorithm improvement).

 - revisedGirvanNewman.R implments an improved version of the algorithm described [here](http://ceur-ws.org/Vol-1649/200.pdf). This algorithm uses groupEdgeBetweeness.R to calculate the group edge betweeness scores.

## Data

Two edgelists from the [Stanford Large Network Dataset Collection](https://snap.stanford.edu/data/index.html) are also included.

 - tv_shows.csv represents a set of TV show, Facebook pages that are connected if two pages like each other's pages.

 - CA-GrQc.txt represents a set of authors who are connected if the two authors collaborated on a paper together.
