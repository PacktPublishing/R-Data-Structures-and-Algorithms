#########################################
######  CHAPTER 8 GRAPHS     ############
#########################################

## Graph Abstract Data type
Graph_ADT <- setRefClass(Class = "adjacency_Matrix",
                                fields = list(n = "integer"),
                                methods = list(
                                  ## Initialise a graph of n vertices
                                  Initialize = function(n){},
                                  
                                  ## Return number of vertices and edges
                                  num_vert = function(){},
                                  num_edges = function(){},
                                  
                                  ## Return weight of an edge for a pair of connecting vertices v1 and v2
                                  weightEdge = function(v1,v2){},
                                  
                                  ## Assign weight(wt) of an edge for a pair of connecting vertices v1 and v2
                                  assignEdge = function(v1,v2,wt){},
                                  
                                  ## Delete weight of an edge for a pair of connecting vertices v1 and v2
                                  deleteEdge = function(v1,v2){},
                                  
                                  ## Return first connecting vertex for a given vertex v
                                  firstVertex = function(v){},
                                  
                                  ## Return next connecting vertex for a given v and its neighbour w
                                  nextVertex = function(v,w){},
                                  
                                  ## Check for presence of an edge connection for a pair of vertices v1 and v2
                                  isEdge = function(v1,v2){}
                                 ))


## Adjacency matrix
adjacencyMatrix <- 
  setRefClass( Class = "adjacencyMatrix",
               fields = list(n = "integer"),
               methods = list(
                 ## Initialise the graph of n vertices
                 Initialize <- function(n){
                   numVertices <<- as.integer(n)  ## with n vertices
                   numEdges <<- 0L     ## with no connected edges
                   mark <<- list()       ## initialise mark list
                   
                   ## initialse the mark of all vertices to 0 (unvisited)
                   for(i in 1:numVertices)
                     mark[[i]] <<- 0L
                   
                   ## generate a new nxn matrix with initial weights as 0
                   mat <- matrix()
                   for(i in 1:numVertices)
                     for(j in 1:numVertices)
                       mat[i,j] <<- 0L
                 },
                 
                 ## get number of vertices
                 num_vert <- function() return(numVertices),
                 
                 ## get number of edges
                 num_edges <- function() return(numEdges),
                 
                 ## return the first adjacent neighbout of vertex index v
                 firstVertex <- function(v){
                   for(i in 1:numVertices)
                     if(mat[v,i] != 0)
                       return(i)
                   return(numVertices+1)
                 },
                 
                 ## return next adjacent vertices of index v after 
                 ## getting index w using firstVertex
                 nextVertex <- function(v,w){
                   for(i in (w+1):numVertices)
                     if(mat[v,i] != 0) 
                       return(i)
                   return(numVertices+1)
                 },
                 
                 ## Assign weight (wt) to each connected edge of indices v1 and v2
                 assignEdge <- function(v1,v2,wt){
                   if(wt<0) stop("Weight should be positive")
                   ## increase the count of edges as the weights are assigned
                   if(mat[v1,v2] == 0) numEdges <<- numEdges + 1L
                   ## replace 0 with the wt
                   mat[v1,v2] <<- wt
                 },
                 
                 ## Delete a connected edge between indices v1 and v2
                 deleteEdge <- function(v1,v2){
                   if(mat[v1,v2] != 0) numEdges <<- numEdges - 1L
                   mat[v1,v2] <<- 0
                 },
                 
                 ## Check whether an edge exists between indices v1 and v2
                 isEdge <- function(v1,v2){
                   return(mat[v1,v2] != 0)
                 },
                 
                 ## Get weight of the connected edge between indices v1 and v2
                 weightEdge <- function(v1,v2){
                   return(mat[v1,v2])
                 },
                 
                 ## Get the mark of a vertex of index v1
                 getMark <- function(v1){
                   return(mark[[v1]])
                 },
                 
                 ## initialise the mark of a vertex of index v1 with 1
                 initMark <- function(v1,val){
                   mark[[v]] <<- val
                 }
               ))

## Adjacency list
adjacencyList <- 
  setRefClass( Class = "adjacencyList",
               fields = list(n = "integer"),
               methods = list(
                 ## Initialise the graph of n vertices
                 Initialize <- function(n){
                   ## with n vertices
                   numVertices <<- n
                   ## with no connected edges
                   numEdges <<- 0L  
                   ## initialise mark list
                   mark <<- list()       
                   
                   ## initialse the mark of all vertices to 0 (unvisited)
                   for(i in 1:numVertices)
                     mark[[i]] <<- 0L
                   
                   ## generate a linked list of edges each for 
                   ## each vertex in the list
                   vertex <- list()
                   for(i in 1:numVertices)
                     vertex[[i]] <<- llistofEdges()
                 },
                 
                 ## get number of vertices
                 num_vert <- function() return(numVertices),
                 
                 ## get number of edges
                 num_edges <- function() return(numEdges),
                 
                 ## return the first adjacent neighbout of vertex index v
                 firstVertex <- function(v){
                   if(length(vertex[[v]]) == 0)
                     ## indicates no adjacent neighbour
                     return(numVertices+1)  
                   ## Move to the first adjacent vertex 
                   adjVert <<- firstAdjVert(vertex[[v]]) 
                   ## get the current position of AdjVert
                   pos <<- currentPos(vertex[[v]],adjVert)
                   ## get value of connecting edge
                   adjEdge <<- getValue(vertex[[v]],adjVert)
                   return(adjVert)
                 },
                 
                 ## return next adjacent vertices of index v after 
                 ## getting index w using firstVertex
                 nextVertex <- function(v,w){
                   if(isEdge(v,w)){
                     if(pos+1 > length(vertex[[v]])){
                       ## move the next adjcent vertex of w
                       adjVert <<- nextAdjVertex(vertex[[v]],w)
                       ## get the current position of adjcent vertex
                       pos <<- currentPos(vertex[[v]],adjVert)
                       ## get value of connecting edge
                       adjEdge <<- getValue(vertex[[v]],adjVert)
                       return(adjVert)
                     }
                     ## no connecting edge
                   } else return(numVertices+1) 
                 },
                 
                 ## Assign weight (wt) to each connected edge of indices v1 and v2
                 assignEdge <- function(v1,v2,wt){
                   if(wt<0) stop("Weight should be positive")
                   ##check whether edge exists between v1 and v2
                   if(isEdge(v1,v2)){
                     ## insert vertex v2 along with edge weight wt
                     insertVertex(vertex[[v1]],v2,wt)
                   } 
                 },
                 
                 ## Delete a connected edge between indices v1 and v2
                 deleteEdge <- function(v1,v2){
                   if(isEdge(v1,v2)){ 
                     removeEdge(v1,v2)
                     numEdges <<- numEdges - 1L
                   }
                 },
                 
                 ## Check whether an edge exists between indices v1 and v2
                 isEdge <- function(v1,v2){
                   pos <- currentPos(vertex[[v1]], firstAdjVert(vertex[[v1]]))
                   while(pos < length(vertex[[v1]])){
                     adjVert <- nextAdjVertex(vertex[[v1]],vertex[[v1]][pos])
                     if(adjVert == v2){
                       return(TRUE)} else {pos = pos+1 }
                   }
                 },
                 
                 ## Get weight of the connected edge between indices v1 and v2
                 weightEdge <- function(v1,v2){
                   if(isEdge(v1,v2)){
                     adjEdge <- getValue(vertex[[v1]],v2)
                     return(adjEdge)
                   } else {return (0)}
                 },
                 
                 ## Get the mark of a vertex of index v1
                 getMark <- function(v1){
                   return(mark[[v1]])
                 },
                 
                 ## initialise the mark of a vertex of index v1 with 1
                 initMark <- function(v1,val){
                   mark[[v]] <<- val
                 }
               ))

## Graph Traversing
graph_Traverse <- function(Graph_ADT,n,vertices)
{
  ## Initialise marks to zero
  verticesMarks <- list()
  for( i in 1:n)
    verticesMarks[[i]] <- Graph_ADT$initMark(vertices[i],0) ## 0 means not visited
  
  ## Inititate traversing upon checking for unmarked nodes
  for(i in 1:n)
    if(Graph_ADT$getMark(vertices[i])==0)
      initTraverse(verticesMarks,vertices[i])
}

## Depth First Search (DFS)
DepthFirstSearch <- function(Graph_ADT, n, v)
{
  ## Ensure all nodes are visited and processed prior node v
  preVisit(v)
  
  ## mark the node v under consideration as 1 (i.e. visited)
  VerticesMarks <- list()
  VerticesMarks[[v]] <- Graph_ADT$initMark(v,1)
  
  ## Recursively visit all connected nodes of v till all are marked as 1
  ## get the first vertex
  node <- Graph_ADT$firstVertex(v) 
  ## check node belongs to neighbouring nodes using conVert function
  while(node %in% conVert(v)){    
    ## check if the node is unvisited
    if(Graph_ADT$getMark(VerticesMarks[[node]] == 0))  
      ## recursively run DFS
      DepthFirstSearch(Graph_ADT,n, node)  
    ## assign next neighbouring vertex
    node <- Graph_ADT$nextVertex(v,node)     
  }
  
  ## Run post processing remaining un-visited nodes
  postVisit(v)
}
  

## Breadth First Search (BFS)
BreadthFirstSearch <- function(Graph_ADT,startVertex, queue, n)
{
  ## initialise an empty queue with a start vertex
  queue <- initQueue(startVertex)
  
  ## Initialise first vertex by marking it as 1 (visited)
  VerticesMarks <- list()
  VerticesMarks[[v]] <- Graph_ADT$initMark(v,1)
  
  ## Subsequently start processing in queues
  while(length(queue) != 0){
    ## extract first element in the queue
    v <- extQueue(queue)
    
    ## Pre-Process all directly connected nodes of v
    preVisit(v)
    
    ## Mark visited nodes with 1 and accordingly queue the nodes
    node <- firstVertex(v)
      while(node %in% conVert(v)){
        if(getMark(graph[node] == 0)){
          graph <- Graph_ADT$initMark(node,1)
          queue <- initQueue(node)
        }
        node <- Graph_ADT$nextVertex(startVertex,node)
      }
  }
}


## Topological Sort
## Main function to perform topological sort
Topological_DFS_sort <- function(Graph_ADT, n, vertices)
{
  ## initialise all nodes with 0 (unvisited)
  verticesMarks <- list()
  for(i in 1:n)
    verticesMarks[[i]] <<- Graph_ADT$initMark(vertices[i],0)
  
  ## Process all nodes by recursive traversing
  for(i in 1:n)
    if(Graph_ADT$getMark(vertices[i]) == 0)
      topological_secondary(Graph_ADT,i)
}

## recursive secondary function to help main function
topological_secondary <- function(Graph_ADT,i)
{
  ## Mark the node as 1 (visited)
  verticesMarks[[i]] <<- Graph_ADT$initMark(vertices[i], 1)
  
  ## Perform traversing across connected nodes
  v <- Graph_ADT$firstVertex(vertices[i])
  while(v %in% conVert(vertices[i])){
    if(Graph_ADT$getMark(vertices[i] == 0))
      topological_secondary(vertices,v)
    v <- Graph_ADT$nextVertex(vertices[i],v)
  }
  return(v)
}


## Topological sort using Breadth First Search (BFS)
Topological_BFS_sort <- function(Graph_ADT, queue, n, vertices)
{
  ## Initialise a list to track count of inwards edges for each node
  countEdge <- list()
  
  ## initialise count of each node to 0
  for (i in vertices)
    countEdge[[i]] <- 0
  
  ## Assign count (inward nodes) prerequisite to each node
  for(i in vertices){
    v <- Graph_ADT$firstVertex(vertices[i])
    while(v %in% conVert(vertices[i])){
      countEdge[[v]] <- countEdge[[v]] + 1
      v <- Graph_ADT$nextVertex(vertices[[i]],v)
    }
  }
  
  ## Initialize queue with nodes which have zero count of inward edges
  for(i in vertices)
    if(countEdge[[i]] == 0)
      queue <-  Graph_ADT$initQueue(i)
  
  ## Process the nodes which are in the queue
  while(length(queue) != 0){
    v <- extQueue(queue)
    print(v)
    w <- Graph_ADT$firstVertex(v)
    while(w %in% conVert(vertices[v])){
      ## Decrease the count prerequisite by 1
      countEdge[[w]] <- countEdge[[w]] - 1
      if(countEdge[[w]] == 0) ## no prerequisites
        queue <- initQueue(w)
      w <- Graph_ADT$nextVertex(vertices[v],w)
    }
  }
}


## Dijsktra's algorithm - Single-Source Shortest Path Algorithm
## Priority queue which supports push, entractMinVertex for Disjkstra's algorithm
PriorityQueueInit <- 
  setRefClass("PriorityQueueInit",
              fields = list(keys = "integer", values = "integer"),
              methods = list(
                push = function(key,value) {
                  keys <<- append(keys, key)
                  values <<- append(values, value)
                },
                extractMinVertex = function() {
                  minPos <- which(values==min(values))
                  key <- keys[[minPos]]
                  value <- values[[minPos]]
                  return(list(key=key,value=value))
                }
              ))
  

DijkstraShortestPath <- function(Graph_ADT, sourceVertex, vertices, n)
{
  library(hashmap)  ## To create new hashmap instances
  
  ## Initiate a new priority queue
  ## It can perform push, entractMinVertex
  ## push : add new vertices along with their distances from source node
  ## entractMinVertex : extract the vertex with minimum value
  ## key represents all the vertices of the graph
  ## value represents the vertex value or distance from the source vertex
  priorityQueue <- PriorityQueueInit$new()
  
  ## Initiate a hashmap to store shortest distance from source vertex to every vertex
  ## keys are all the vertices of the graph other than source vertex
  ## values are the corresponding distances from the source node
  ## Dimension of hashmap is n, where n is total number of vertices in graph G
  ## Initialize all vertices with distance as 0 which will later be updated 
  distanceMap <- hashmap(keys=vertices,
                         values = rep(0,n))
  
  ## Initiate another hashmap to store the parent vertex to keep a track of shortest 
  ## path from source vertex to every vertex
  ## key represents the child (to) vertex
  ## value represents the parent (from) vertex
  ## initialize key with source vertex and value with NULL
  parentMap <- hashmap(keys = sourceVertex,
                       values = "NULL")
  
  ## initialize priority queue with value of all vertices to infinity
  for( i in vertices)
    priorityQueue$push(vertices[i],Inf)
  
  ## Set the distance of sourceVertex as zero
  priorityQueue$values[which(priorityQueue$keys==sourceVertex)] <- 0
  
  ## Begin iteration till the all the vertices from priorityQueue becomes empty
  while(length(priorityQueue$keys) != 0){
    
    ## Extract vertex with minimum value from priority queue along with its value
    headVertex <- priorityQueue$extractMinVertex()
    
    ## Assign the key of the head vertex as current vertex
    currentVertex <- headVertex$key
    
    ## Append distancemap with current key and its value
    distanceMap[[currentVertex]] <- headVertex$value
    
    ## Check for all directly connected vertices for the current vertex
    for(conVert in getConVertex(graph,currentvertex)){
      ## get all the corresponding edge value
      edgeValue <- getEdgeValue(graph,currentvertex,conVert)
      
      ## Check priority queue contains the adjacent connected vertex (conVert) or not
      ## If yes, then proceed ahead 
      ## if no, conVert vertex already has shortest distance from source vertex
      if(!priorityQueue$keys %in% conVert){
        next
      }
      
      ## Now evaluate the distance of the adjacent vertex (conVert) with source vertex
      ## via current vertex. Add the distance of current vertex with edge value of 
      ## adjacent vertex (conVert)
      updDistance <- distanceMap[[currentVertex]] + edgeValue
      
      ## Check whether the value of the adjacent vertex in priorityQueue is greater than
      ## the updated distance or not. If yes, then decrease the value in the priorityQueue
      ## to the updated distance and also update the parent map of the adjacent vertex 
      ## current vertex
      if(priorityQueue$values[which(priorityQueue$keys==conVert)] > updDistance){
        priorityQueue$values[which(priorityQueue$keys==conVert)] <- updDistance
        parentmap[[conVert]]  <- currentVertex
      }
    }
  }
}


## Prim's algorithm
## Priority queue which supports push, entractMinVertex, containsVertex,
## getWeight for Prim's algorithm
PriorityQueueInit <- 
  setRefClass("PriorityQueueInit",
              fields = list(keys = "integer", values = "integer"),
              methods = list(
                push = function(key,value) {
                  keys <<- append(keys, key)
                  values <<- append(values, value)
                },
                extractMinVertex = function() {
                  minPos <- which(values==min(values))
                  key <- keys[[minPos]]
                  value <- values[[minPos]]
                  return(list(key=key,value=value))
                }
              ))

## Implement Prim's algorithm
primMST <- function(Graph_ADT,vertices,n)
{
  library(hashmap)  ## To create new hashmap instances
  
  ## Initiate a new priority queue
  ## It can perform push, entractMinVertex
  ## push : add new vertices along with their distances from source node
  ## entractMinVertex : extract the vertex with minimum value
  ## key represents all the vertices of the graph
  ## value represents the vertex value or distance from the source vertex
  priorityQueue <- PriorityQueueInit$new()
  
  ## Initiate a hashmap to store shortest distance from source vertex to every vertex
  ## keys are all the vertices of the graph other than source vertex
  ## values are the corresponding distances from the source node
  ## Dimension of hashmap is n, where n is total number of vertices in graph G
  ## Initialize all vertices with distance as 0 which will later be updated 
  distanceMap <- hashmap(keys=vertices,
                         values = rep(0,n))
  
  ## Initialise a list to store final MST result
  MSTResult <- list()
  
  ## initialize priority queue with value of all vertices to infinity
  for( i in vertices)
    priorityQueue$push(vertices[i],Inf)
  
  ## begin with a random vertex
  startVertex <<- vertices[sample(1:n, 1)]
  
  ## Set the distance of startVertex as zero
  priorityQueue$values[which(priorityQueue$keys==startVertex)] <- 0
  
  ## Begin iteration till the all the vertices from priorityQueue becomes empty
  while(length(priorityQueue$keys) != 0){
    
    ## Extract vertex with minimum value from priority queue along with its value
    headVertex <- priorityQueue$extractMinVertex()
    
    ## Assign the key of the head vertex as current vertex
    currentVertex <- headVertex$key
    
    ## Append distancemap with current key and its value
    distanceMap[[currentVertex]] <- headVertex$value
    
    ## Check for all directly connected vertices for the current vertex
    for(conVert in getConVertex(graph,currentvertex)){
      ## get all the corresponding edge value
      edgeValue <- getEdgeValue(graph,currentvertex,conVert)
      
      ## Check priority queue contains the adjacent connected vertex (conVert) or not
      ## If yes, then proceed ahead 
      ## if no, conVert vertex already has shortest distance from current vertex
      if(!priorityQueue$keys %in% conVert){
        next
      }
      
      ## Now evaluate the distance of the adjacent vertex (conVert) with current vertex. 
      ## Update the distance with the edge value
      updDistance <- edgeValue
      
      ## Check whether the value of the adjacent vertex in priorityQueue is greater than
      ## the updated distance or not. If yes, then decrease the value in the priorityQueue
      ## to the updated distance.
      if(priorityQueue$values[which(priorityQueue$keys==conVert)] > updDistance){
        priorityQueue$values[which(priorityQueue$keys==conVert)] <- updDistance
        MSTResult[[currentVertex]] <- conVert
      }
    }
  }
}

#######  Kruskal's Algorithm
## Define a reference class which can perform disjoint set operations
## The operations are UNION, DIFFER and FIND
## Union: to merge two sets together
## Differ: to check whether vertices are in different sets
## Find: to find whether a vertex is in a set or not
disjoinSetPointer <- setRefClass("disjoinSetPointer",
                                 fields = list(vertex = "vector",
                                               set1 = "vector",
                                               set2 = "vector",
                                               currentVertex = "integer"),
                                 methods = list(
                                   ## merge two sets
                                   union = function(set1,set2){
                                     return(c(set1,set2))
                                   },
                                   ## check whether set1 and set 2 are disjoint
                                   ## return TRUE if they are disjoint
                                   differ = function(set1,set2){
                                     if(sum(set1 %in% set1) ==0){
                                       return(TRUE)} else(return(FALSE))
                                   },
                                   ## Find whether a vertex is in a set or not
                                   ## returns root of the currentVertex
                                   ## function ROOT returns root of the vector
                                   find = function(currentVertex){
                                     return(ROOT(vertex[currentvertex]))
                                   }
                                 ))

## Define a reference class to store the edges along with from and to vertices on min-heap
kruskalArray <- setRefClass("kruskalArray",
                            fields = list(fromVertex = "numeric",
                                          toVertex = "numeric",
                                          weight = "numeric"),
                            methods = list(
                              ## insert new from and to vertices along with edge
                              push = function(f, t, w){
                                fromVertex <<- append(fromVertex,f)
                                toVertex <<- append(toVertex,t)
                                weight <<- append(weight,w)
                              },
                              ## extract from and to vertices having minimum edge value
                              ## alos remove from, to and edge value from the array
                              extractMinEdge = function() {
                                minPos <- which(weight==min(weight))
                                from <- fromVertex[[minPos]]
                                to <- toVertex[[minPos]]
                                fromVertex <<- fromVertex[[-minPos]]
                                toVertex <<- toVertex[[-minPos]]
                                weight <<- weight[[-minPos]]
                                return(list(from=from,to=to))
                              }
                            ))

## Implement Kruskals algorithm
kruskalMST <- function(Graph_ADT, n, e)
{
  ## initialse reference classes disjoinSetPointer and kruskalArray
  vertexArray <- disjoinSetPointer$new()
  edgeArray <- kruskalArray$new()
  
  ## Initialise a list to store final MST result
  MSTResult <- list()
  
  ##  Put all the edges in the edgeArray
  for(i in 1:n){
    j <- firstVertex(i)
    while(i <= n){
      edgeArray$push(i,j,Graph_ADT$weightEdge(i,j))
    }
  }
  
  ## Initialise n equivalent sets
  numMST <- n
  
  ## Iteratively combine equivalent sets based on edge weights
  ## edges are extracted based on their value. Smallest edges are extracted first
  for(i in 1:e){
    while(numMST >= 1){
      # get the from and to vertices having minimum edge value
      temp <- edgeArray$extractMinEdge()
      fromVertex <- temp$from
      toVertex <- temp$to
      
      ## Check whether two vertices are in different sets
      if(vertexArray$differ(fromvertex,toVertex)){
        ## if yes, then combine from and to vertices into one set
        vertexArray$union(fromvertex,toVertex)
        ## add this edge to MST
        MSTResult[[i]] <- c(fromVertex,toVertex)
        ## decrease the sets by 1
        numMST <- numMST - 1
      }
    }
  }
  return(MSTResult)  
}



