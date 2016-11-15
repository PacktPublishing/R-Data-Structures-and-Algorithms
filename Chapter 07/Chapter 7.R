#########################################
######  CHAPTER 7 INDEXING   ############
#########################################

#### Tree based Indexing : BST node structure   ####

bstnode <- function(key, value) {
  node <- new.env(hash = FALSE, parent = emptyenv())
  node$key <- key  	# Node key
  node$value <- value		# Node Value
  node$left <- NULL		# left children key
  node$right <- NULL		# Right children key
  class(node) <- "bstnode"
  return(node)
}

#### 2-3 Tree   ####
tttnode <- function(lkey=NULL, lvalue=NULL, rkey=NULL, rvalue=NULL) {
  node <- new.env(hash = FALSE, parent = emptyenv())
  node$lkey <- lkey  	# left Node key
  node$lvalue <- lvalue		# Node Value
  node$rkey <- rkey		# right Node key
  node$rvalue <- rvalue		# right Node Value
  node$left <- NULL		# left children key
  node$center <- NULL		# left children key
  node$right <- NULL		# Right children key
  class(node) <- "tttnode"
  return(node)
}

extttree <- tttnode(70, 70)

#### INSTERTION  ####
# Function to check if node is empty
check_empty<-function(node){
  ifelse((is.null(node$lkey) & is.null(node$rkey)), T, F)
}

# Function to insert if the node has empty space
leaf_insert<-function(node, key, val){
  if(check_empty(node)) return(tttnode(lkey=key, lvalue=val))
  if(is.null(node$rkey)){
    if(key>node$lkey){
      node$rkey<-key
      node$rvalue<-val
    } else
    {
      node$rkey<-node$lkey
      node$rvalue<-node$lvalue
      node$lkey<-key
      node$lvalue<-val
    }
  } else
  {
    node$left<-tttnode(key, val)
  }
  return(node)
} 



ttinsert<-function(node=NULL, key, val){
  if(check_empty(node)) return(tttnode(lkey=key, lvalue=val))
  if(is.null(node$left)) node<-leaf_insert(node, key, val)   
  ## Add element to internal nodes
  if(key<node$lkey){
    subtree = ttinsert(node$left, key, val)
    if (identical(subtree, node$left))
    {
      return(node);
    } else
    {
      assign("left", subtree, envir = node)
      return(node)
    }
  } else if(ifelse(is.null(node$rkey), T,  key<node$rkey)){
    subtree = ttinsert(node$center, key, val)
    if(identical(subtree, node$center)) 
    {
      return(node)
    } else
    {
      assign("center", subtree, envir = node)
      return(node)
    }
  } else
  {
    subtree = ttinsert(node$right, key, val)
    if(identical(subtree, node$right)) {
      return(node)
    } else
    {
      assign("right", subtree, envir = node)
      return(node)
    } 
  } 
} 


search_keys<-function(node, key){
  if (is.null(node)) return(NULL) # empty node
  if (node$lkey== key) return(node$lvalue)
  if(!is.null(node$rkey) & node$rkey==key) return(node$rvalue)
  if(key<node$lkey) {
    sort_keys(node$left, key)
  } else if(is.null(node$rkey)){
    sort_keys(node$center, key)
  } else if(key<node$rkey) {
    sort_keys(node$center, key)
  } else
  {
    sort_keys(node$right, key)
  }
}



#### B+ Trees  ####
bplusnode<-function(node=NULL, key, val){
  node <- new.env(hash = FALSE, parent = emptyenv())  
  node$keys<-keys
  node$child<-NULL
  node$isleaf<-NULL 
  node$d<-NULL
  class(node) <- "bplustree"
  return(node)
}

dlinkchildNode <- function(val, prevnode=NULL, node=NULL) {
  llist <- new.env(parent=create_emptyenv())
  llist$prevnode <- prevnode
  llist$element <- val
  llist$nextnode <- node
  class(llist) <- " dlinkchildNode"
  llist
}


#### Range Query ####
querry_search<-function(node, key1, Key2){
  ## Function to get values within leaf node using link list
  search_range<-function(child, key1, key2, val=NULL){
    if(child$element>key1 & child$key2){
      val<-c(val, child$element)
      search_range(child$nextnode, key1, key2, val)
    } else
    {
      return(val)
    }
  }
  
  if(key1>key2){
    temp<-Key2
    key2<-key1
    key1<-temp
  }
  child<-search_lower_key(node, key1)   # search lower leaf
  rangeVal<-search_range(child, key1, key2)   # Return Range 
  return(rangeVal)
}


