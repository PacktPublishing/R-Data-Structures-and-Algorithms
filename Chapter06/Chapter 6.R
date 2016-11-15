####################         CHAPTER 6: Exploring search options       #############################

######################################################
##########   6.2.1 Sequential / Linear search     ####
######################################################


V <- c(12,15,8,4,56,32,0,3,20,28,32,25,36,18)
n=length(V)
S=11
Sequential_search <- function(V,S,n)
{
  i=1
  present = FALSE

  while(i <= n & present==FALSE)
  {
    if(V[i] == S)
      present=TRUE else i = i+1
  }
  
  return(list(present=present,key=i))
}
Sequential_search(V,S,n)

######################################################
##########   6.2.2 Ordered sequential search      ####
######################################################

V <- sort(V)

Seq_ord_search <- function(V,S,n)
{
  i=1
  present = FALSE
   
  while(i <= n & present==FALSE )
  {
    if(V[i] == S)
      present=TRUE else if(V[i] > S)
                            stop("element S not found") else i=i+1
  }
  
  return(present)
}
Seq_ord_search(V,S,n)

######################################################
##########   6.2.3 Jump Search        ################
######################################################

V <- sort(V)

Jump_search <- function(V,S,n)
{
  jump <- floor(sqrt(n))
  present = FALSE
  i=1
  
  while(jump < n & V[jump] < S)
  {
    i=jump
    jump = jump+floor(sqrt(n))
    
    if(jump>=n)
      stop("element S not found")
  }
  
  while(V[i] < S & i <= jump)
      i = i+1
  
  if(V[i]==S)
    present=TRUE
  
  return(present)
}

Jump_search(V,7,length(V))


######################################################
##########   6.2.4 Binary Search      ################
######################################################

## Recursive
V <- c(12,15,8,4,56,32,0,3,20,28,32,25,36,18)
S=32
l=1
h=length(V)

Bin_search_recursive <- function(V,S,l,h) {
  if ( h < l ) {
    stop("h should be more than l")
  } else {
    m <- floor((l + h) / 2)
    
    if ( V[m] > S )
      Bin_search_recursive(V, S,l,m-1)
    else if ( V[m] < S )
      Bin_search_recursive(V, S, m+1, h)
    else
      return(m)
  }
}

Bin_search_recursive(V,S,1,14)

## iterative

Bin_search_iterative <- function(V, S,n) {
  l=1
  h=n
  i = 0
  
  while ( l <= h ) {
    m <- floor((l + h)/2)
    if ( V[m] > S )
      h <- m - 1
    else if ( V[m] < S )
      l <- m + 1
    else if(V[m]==S)
      return(TRUE)
  }
  return(FALSE)
}

Bin_search_iterative(V,S,n)

######################################################
##########   6.2.5 Interpolation Search        #######
######################################################

Interpolation_search <- function(V,S,n)
{
  i=1; j=n; l=V[1]; h=V[j];
  
  if(S<l | S>h) return(FALSE)
  
  while(i < j)
  {
    k = floor(i+((j-i)*(S-l))/(h-l))
    print(k)
    split = V[k]
    if(S>split){
      i=k+1; l=split
    }else if(S < split){
      j=k-1; h=split
    }else if(V[k]==S){
      return(TRUE)}
  }
  return(FALSE) 
}

Interpolation_search(V,S,n)


######################################################
##########   6.3.1 Count based SOL             #######
######################################################

V <- c(1,2,3,4,5,6,7,8)
S <- c(6,4,6,7,5,7,6,1,4,6,7,5)
n_search = length(S)
n=length(V)

SOL_count <- function(V,S,n_search,n)
{
  if(is.null(V)) stop("NO elements in input vector")
  if(is.null(S)) stop("NO elemens to search")
  i=1
  count <- as.list(sapply(1:n,function(x) 0))
  names(count) <- V
  while(i<=n_search)
  {
    if(Sequential_search(V,S[i],n)$present){
      key <- Sequential_search(V,S[i],n)$key
      count[key][[1]] <- count[key][[1]] + 1
      count <- count[order(-unlist(count))]
      V <- as.numeric(names(count))
  }
  i=i+1
  }
  return(V)
}

SOL_count(V,S,n_search,n)



######################################################
#### 6.3.2  Move To Front Self Organizind List  ######
######################################################


SOL_move <- function(V,S,n_search,n)
{
  if(is.null(V)) stop("NO elements in input vector")
  if(is.null(S)) stop("NO elemens to search")
  i=1
  while(i<=n_search)
  {
    if(Sequential_search(V,S[i],n)$present){
      if(Sequential_search(V,S[i],n)$key !=1){
        key <- Sequential_search(V,S[i],n)$key
        temp <- V[key]
        V <- V[-key] 
        V <- c(temp,V)
      }
    }
    i <- i+1
  }
  return(V)
}

SOL_move(V,S,n_search,n)

######################################################
#### 6.3.3  Transpose Self Organizind List      ######
######################################################

SOL_transpose <- function(V,S,n_search,n)
{
  if(is.null(V)) stop("NO elements in input vector")
  if(is.null(S)) stop("NO elemens to search")
  i=1
  while(i<=n_search)
  {
    if(Sequential_search(V,S[i],n)$present){
      if(Sequential_search(V,S[i],n)$key !=1){
        key <- Sequential_search(V,S[i],n)$key
        temp <- V[key-1]
        V[key-1] <- V[key]
        V[key] <- temp
      }
    }
    i <- i+1
  }
  return(V)
}

SOL_transpose(V,S,n_search,n)

######################################################
##########   6.4  Hash functions    ##################
######################################################

hash_int <- function(K)
{
  return (K %% 18)
}

hash_string <- function(K,n)
{
  hashValue <- 0
  for(i in 1:n){
    hashValue <- hashValue+as.numeric(charToRaw(substr(K,i,i)))
  }
  return(hashValue)
}

