####################         CHAPTER 5: SORTING ALGORITHMS       #############################

######################################################
##########   5.2.1 Insertion Sort     ################  
######################################################

Insertion_Sort <- function(V,n)
{
  if(n==0) stop("No elements to sort")
  for(i in 2:(length(V)))
  {
    val <- V[i]
    j <- i - 1
    while (j >= 1 && val <= V[j])
    {
      V[j+1] <- V[j]
      j <- j-1
    }
    V[j+1] <- val
  }
  return(V)
}

Insertion_Sort(V=c(20,12,65,8,10,16,43,35),n=8)

######################################################
##########   5.2.2 Bubble Sort        ################  
######################################################

Bubble_Sort <- function(V,n) {
  if(n==0) stop("No elements to sort")
  for(i in 1:length(V)) {
    flag <- 0
    for(j in 1:(length(V)-i)) {
      if ( V[j] > V[j+1] ) {
        val <- V[j]
        V[j] <- V[j+1]
        V[j+1] <- val
        flag <- 1
      } 
    }
    if(!flag) break
  }
  return(V)
}

Bubble_Sort(c(20,12,65,8,10,16,43,35),n=8)

######################################################
##########   5.2.3 Selection Sort     ################  
######################################################

Selection_Sort_loop <- function(V,n) {
  if(n==0) stop("No elements to sort")
  keys <- seq_along(V)
  for(i in keys) {
    small_pos <- (i - 1) + which.min(V[i:length(V)])
    temp <- V[i]
    V[i] <- V[small_pos]
    V[small_pos] <-temp
  }
  return(V)
}

Selection_Sort_loop(c(20,12,65,8,10,16,43,35),n=8)

######################################################
##########        5.3 Shellsort       ################  
######################################################

Shell_Sort <- function(V,n) {
  if(n==0) stop("No elements to sort")
  increment=round(n/2)  ## as.integer 
  while(increment>0) {
    for(i in (increment+1):n) {
      temp <- V[i]
      j=i
      while(j >= (increment+1)  && V[j-increment] > temp) {
        V[j] <- V[j-increment]
        j <- j-increment
      }
      V[j] <- temp
    }
    if(increment==2) {
      increment <- 1} else{
        increment <- round(increment/2.2)
      }
    }
  return(V)
}

Shell_Sort(c(20,12,65,8,10,16,43,35,23,88,2,56,41,27,67,56),n=16)

######################################################
##########        5.4 Mergesort       ################  
######################################################

Merge_Sort <- function(V) {
  if(length(V) == 0) stop("Not enough elements to sort")
  
  ## Merge function to sort two halves or sub-vectors
  merge_fn <- function(first_half, second_half) {
    result <- c()
    while(length(first_half) > 0 && length(second_half) > 0) {
      if(first_half[1] <= second_half[1]) {
        result <- c(result, first_half[1])
        first_half <- first_half[-1]
      } else {
        result <- c(result, second_half[1])
        second_half <- second_half[-1]
      }         
    }
    if(length(first_half) > 0) result <- c(result, first_half)
    if(length(second_half) > 0) result <- c(result, second_half)
    return(result)
  }
  
  ## Recursively split the parent vector into two halves (sub-vectors)
  if(length(V) <= 1) V else {
    middle <- length(V) / 2
    first_half <- V[1:floor(middle)]
    second_half <- V[floor(middle+1):length(V)]
    first_half <- Merge_Sort(first_half)
    second_half <- Merge_Sort(second_half)
    if(first_half[length(first_half)] <= second_half[1]) {
      c(first_half, second_half)
    } else {
      merge_fn(first_half, second_half)
    } 
  }
}

Merge_Sort(c(20,12,65,8,10,16,43,35,23,88,2,56,41,27,67,56))

######################################################
##########        5.5 Quicksort       ################  
######################################################

Quick_Sort <- function(V,n) {  
  if (n <= 1) return(V)
  left <- 0 ##start from left prior first element
  right <- n  ##start from rightmost element
  v <- V[n] ## initialize last element as pivot element
  
  ## Partition implementation
  repeat {
    while (left < n && V[left+1]  < v) left <- left+1
    while (right > 1 && V[right-1] >= v) right <- right-1
    if (left >= right-1) break
    ## Swap elements to put pivot in place
    temp <- V[left+1]
    V[left+1] <- V[right-1]
    V[right-1] <- temp
  }
  
  ## Recursive implementation of Quick sort
  if (left == 0) return(c(V[n], Quick_Sort(V[1:(n-1)],n=(n-1))))
  if (right == n) return(c(Quick_Sort(V[1:(n-1)],n=(n-1)), V[n]))
  return( c(Quick_Sort(V[1:left],n=left), V[n], Quick_Sort(V[(left+1):(n-1)],n=(n-left-1))))
}

Quick_Sort(V= c(20,12,65,8,10,16,43,35,23,88,2,56,41,27,67,55),n=16)


######################################################
##########        5.6 Heapsort        ################  
######################################################

V <- c(20,12,65,8,10,16,43,35,23,88,2,56,41,27,67,56)
Heap_Sort <- function(V)
{
  heapsize <- length(V)
  for (i in floor(length(V)/2):1)
    V <- max_heap(V, i,heapsize)
  for (i in length(V):2) {
    temp <- V[i]
    V[i] <- V[1]
    V[1] <- temp
    heapsize <- heapsize -1
    V <- max_heap(V, 1,heapsize)
  }
  return(V)
}

max_heap <- function(V, i,heapsize) {
  left <- 2*i
  right <- 2*i+1
  if (left<=heapsize && V[left]>V[i]){
    largest <- left}else{
    largest <- i
    }
  
  if (right<=heapsize && V[right]>V[largest])
    largest <- right
  
  if (largest != i) {
    temp2 <- V[largest]
    V[largest] <- V[i]
    V[i] <- temp2
    V <- max_heap(V, largest,heapsize)
  }
  return(V)
}

Heap_Sort(c(20,12,65,8,10,16,43,35,23,88,2))

######################################################
##########        5.7.1 Binsort       ################  
######################################################

# add item to bin
addItem=function(V,bin,maxValue,n){
  for(i in 1:n){
    val<-V[i]
    ix<-ceiling((val*n)/maxValue)
    if(is.na(bin[["binValues"]][[ix]][1])){
      bin[["binValues"]][[ix]][1]<-val
      bin[["nElement"]][ix]<-1
    } else
    {
      bin <- insertItem(val=val, ix=ix,bin=bin)
    }
  }
  return(bin)
}

# insert a item into a bin ensuring sorting
insertItem=function(val, ix,bin){
  nElement<-bin[["nElement"]][ix]
  pos<-NULL
  for(i in 1:nElement){
    if(val<bin[["binValues"]][[ix]][i]){
      pos<-i
    }
  }
  if(is.null(pos)){
    bin[["binValues"]][[ix]][nElement+1]<-val
  } else if(pos==1) {
    bin[["binValues"]][[ix]]<-c(val, bin[["binValues"]][[ix]][1])
  } else
  {
    bin[["binValues"]][[ix]]<-c(bin[["binValues"]][[ix]][1:(pos-1)], val, bin[["binValues"]][[ix]][pos:nElement])
  }
  bin[["nElement"]][ix]<-nElement+1
  return(bin)
}

# bind the list into a sorted vector
bindSorted_vec=function(bin,n){
  output <- c()
  currentIx<-1
  for(i in 1:n){
    if(!is.na(bin[["binValues"]][[i]][1])){
      nElement<-bin[["nElement"]][i]
      for(m in 1:nElement){
        output[currentIx]<-bin[["binValues"]][[i]][m]
        currentIx<-currentIx+1
      }
    }
  }
  return(output)
}

# binsort Algorithm
Bin_Sort=function(V,n,maxValue){
  bin <-list("binValues"=list(), "nElement"=NA)
  # create empty bins
  for(i in 1:n){
    bin[["binValues"]][[i]]<-NA
    bin[["nElement"]][i]<-0
  }
  ## Add elements into suitable bins
  bin <- addItem(V=V,bin=bin,maxValue=maxValue,n=n)
  ## Bind all bins inot a single sorted vector
  output <- bindSorted_vec(bin=bin,n=n)
  return(output)
}


## Example of bin sorting
V<-c(20,12,65,8,10,16,43,35,23,88,2,56,41,27,67,55)
n<-16
maxValue<-88
Bin_Sort(V=V,n=n,maxValue=maxValue)

######################################################
##########        5.7.2 Radixsort       ##############
######################################################

# add item to bin
addItem=function(V,bin,digLength,n){
  for(i in 1:n){
    val<-V[i]
    ## Extract the required digit from the number
    ix<-floor((val/digLength) %% 10)+1
    ## Assign element to each bin
    bin[["binValues"]][[ix]][bin[["nElement"]][ix]+1]<-val
    ## Track count of elements in each bin
    bin[["nElement"]][ix]<-bin[["nElement"]][ix] + 1
  }
  return(bin)
}

# bind the list into a sorted vector
bindSorted_vec=function(bin){
  output <- c()
  currentIx<-1
  for(i in 1:10){
    if(!is.na(bin[["binValues"]][[i]][1])){
      nElement<-bin[["nElement"]][i]
      for(m in 1:nElement){
        output[currentIx]<-bin[["binValues"]][[i]][m]
        currentIx<-currentIx+1
      }
    }
  }
  return(output)
}

# radixsort Algorithm
radix_Sort=function(V,n,maxValue,digLength){
  for(digLength in c(10^(0:digLength)))
  {
  bin <-list("binValues"=list(), "nElement"=NA)
  # create empty bins
  for(i in 1:10){
    bin[["binValues"]][[i]]<-NA
    bin[["nElement"]][i]<-0
  }
  bin <- addItem(V=V,bin=bin,digLength=digLength,n=n)
  V <- bindSorted_vec(bin=bin)
  }
  return(V)
}


## Example of radix sorting
V<-c(67,54,10,988,15,5,16,43,35,23,88,2,103,83)
n<-14
maxValue<-988
digLength <- 2
radix_Sort(V=V,n=n,maxValue=maxValue,digLength=digLength)

