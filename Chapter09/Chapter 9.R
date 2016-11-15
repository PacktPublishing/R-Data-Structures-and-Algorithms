##################################################################################
######  CHAPTER 9 Programming and Randomized Algorithms     ############
##################################################################################

## Function to compute nth Fibonacci number using recursion
nfib<-function(n){
  assertthat::assert_that(n>0) & assertthat::assert_that(n<50)
  if(n==1 || n==2) return(1)
  val<- nfib(n-1) + nfib(n-2)
  return(val)
}

## Function to compute nth Fibonacci number using Dynamic Programming
nfib_DP<-function(n){
  assertthat::assert_that(n>0) & assertthat::assert_that(n<50)
  if(n<=2) return(1)
  lag2_val<-0
  lag1_val<-1
  nfibval<-1
  
  # Loop to compute Fibonacci value
  for(i in 3:n){
    lag2_val<-lag1_val
    lag1_val<-nfibval
    nfibval<-lag2_val+lag1_val
  }
  return(nfibval)
}


# Floyd-Warshall algorithm for All Pairs Shortest Paths computation
floydWarshall<-function(graph){
  nodes<-names(graph)
  dist<-graph  
  for (n in nodes){
    for(ni in nodes){
      for(nj in nodes){
        if((dist[[ni]][n]+dist[[n]][nj])<dist[[ni]][nj]){
          dist[[ni]][nj]<-dist[[ni]][n]+dist[[n]][nj]
        }
      }
    }
  }
  return(dist)
}


# Example script for Floyd-Warshall algorithm

# Defining graph structure
graph<-list()
graph[["A"]]=c("A"=0, "B"=8, "C"=Inf, "D"=Inf, "E"=1, "F"=Inf)
graph[["B"]]=c("A"=Inf, "B"=0, "C"=7, "D"=6, "E"=Inf, "F"=Inf)
graph[["C"]]=c("A"=Inf, "B"=Inf, "C"=0, "D"=6, "E"=Inf, "F"=Inf)
graph[["D"]]=c("A"=Inf, "B"=Inf, "C"=Inf, "D"=0, "E"=Inf, "F"=4)
graph[["E"]]=c("A"=Inf, "B"=3, "C"=Inf, "D"=Inf, "E"=0, "F"=9)
graph[["F"]]=c("A"=Inf, "B"=3, "C"=Inf, "D"=4, "E"=9, "F"=0)

# get shortest pair distance
APSP_Dist<-floydWarshall(graph) 



# Skip node data structure
skListNode<-function(val, height=1){
  
  # function to create empty environment
  create_emptyenv = function() {
    emptyenv()
  }
  
  # Create skiplist node
  skiplist <- new.env(parent=create_emptyenv())
  skiplist$element <- val
  skiplist$nextnode<-rep(list(NULL), height)
  class(skiplist) <- "skiplist"
  skiplist
}



# Function to find a value in skip list
findvalue<-function(skiplist, searchkey){
  for (i in level:1){
    # Assign head values
    skiplist<-skiplist$nextnode[[i]]
    while(!is.null(node) && node$element>searchkey){
      skiplist<-skiplist$nextnode[[i]]
    } 
    skiplist = skiplist$nextnode[0]
    if(!is.null(skiplist) && searchkey==skiplist$element){
      # Return element from Skip node
      return(skiplist$element) 
    } else
    {
      # Element not found return NULL
      return(NULL) 
    }
  }
}