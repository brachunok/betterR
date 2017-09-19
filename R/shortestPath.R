
# 'network' is the matrix representation of the network we want to solve.
#
# The matrix is square with i=j=# of nodes. A zero value in the i-jth location
# indicates there is no edge between i and j. A value of c in the i-jth location
# indicates there is a weight of c on the edge connecting vertex i to vertex j.
#

# network <- matrix(c(0,6,2,16,0,0,0,
#                     0,0,0,5,4,0,0,
#                     0,7,0,0,3,8,0,
#                     0,0,0,0,0,0,3,
#                     0,0,0,4,0,0,10,
#                     0,0,0,0,0,0,1,
#                     0,0,0,0,0,0,0), nrow=7,byrow=T)
#
# network_list <- list(seq(1:length(network[,1])))
# # the index of the node we are attempting to go to
# sink <- 7
#
# # the index of the node we are starting at
# source <- 1


#' Dijskras Shortest Path
#'
#' @param network Network is a square matrix with a positive value indicating a weighted, directed edge between vertex i and j.
#' @param source
#' @param sink
#'
#' @return
#' @export
#'
#' @examples
sp<- function(network,source,sink){
# initialized the distance matrix
distance <- matrix(Inf,nrow=1,ncol=length(network[,1]))

# set the source distance to 0

distance[source] <- 0


# visited is an list of the places we have been

visited <- vector(mode="logical",length(network[,1]))


while(!all(visited)){

  # find the minimum distance which is not in the 'visited' set

  unvisitedList <- which(visited!=TRUE)
  current <- unvisitedList[which(distance[unvisitedList]==
                                   min(distance[unvisitedList]))][1]

  # label current as 'visited'
  visited[current] <- TRUE

  # find the 'current's' neighbors
  neighbors <- which(network[current,]!=0)

  # set the distance to the neighbors as the minimum of the current
  # distance and the distance from current

  for(neighbor in neighbors){

    distance[neighbor] <- min(distance[neighbor],
                              network[current,neighbor]+distance[current])



  }


}


#Now we have the minimum distances. We just backtrack to get the minimum path.

# the idea: tracebackwards from the sink. If the distance between the
# current node and a neighbor's shortest distance is distance between them
# then those must be on the critical path.

pathLength <- 0
path <- c(sink)
current <- sink
while(pathLength<distance[sink]){

  neighbors <-  which(network[,current]!=0)

  for(neighbor in neighbors){

    if( network[neighbor,current] == distance[current]-distance[neighbor]){

      #this indicates the neighbor-current arc is on the path
      path <- c(neighbor,path)
      pathLength <- pathLength + network[neighbor,current]
      current <- neighbor
    }



  }
}

return(c( (path),(distance[sink])))
}
