####################         CHAPTER 3: Link List            #############################

######################################################
#######   3.1.2 Element data type     ################  
######################################################

## Factor
fact1 <- factor(c("a","b","c","c","a","b"))
fact1
str(fact1)
fact2 <-factor(c("a","b","c","c","a","b"),labels=c(1,2,3),levels=c("c","a","b"))
fact2
str(fact2)

## Matrix
## Numeric Matrix
mat1 <- matrix(1:10,nrow=5)
mat1
mode(mat1)
## Categorical Matrix
mat2 <- matrix(c("ID","Total",1,10,2,45,3,26,4,8),ncol=2,byrow=T)
mat2
mode(mat2)

## Array
arr1 <- array(1:18,c(3,2,3)) 
arr1

##Data frames
Int <- c(1:5); Char <- letters[1:5]; 
Log <- c(T,F,F,T,F); Comp <- c(1i,1+2i,5,8i,4)
data.frame(Int,Char,Log,Comp)

## List
list1 <- list(age = c(1:5), #numeric vector
                          name = c("John","Neil","Lisa","Jane"), #character vector 
                          mat = matrix(1:9,nrow = 3), #numeric matrix
                          df = data.frame(name = c("John","Neil","Lisa","Jane"), gender = c("M","M","F","F")), #data frame
                          small_list = list(city = c("Texas","New Delhi","London"), country = c("USA","INDIA","UK"))) #list
list1

######################################################
#####################  3.3 Link List    ##############
######################################################
# Example list with array, data.frame, matrix, and character 
elist <- list(vec=1:4,df=data.frame(a=1:3, b=4:6),mat=matrix(1:4, nrow=2), name="pks")
elist[["vec"]]

## 3.3.1 Linear Link List
create_emptyenv <- function() {
  emptyenv()
}

isEmpty <- function(llist) {
  if(class(llist)!= "linkList") warning("Not linkList class")
  identical(llist, create_emptyenv())
}

linkListNode <- function(val, node=NULL) {
  llist <- new.env(parent=create_emptyenv())
  llist$element <- val
  llist$nextnode <- node
  class(llist) <- "linkList"
  llist
}

LList <-linkListNode(5,linkListNode(2,create_emptyenv()))

setNextNode<-function(llist){
  llist$nextnode
}
setNextElement<-function(llist){
  llist$element
}

sizeLinkList<-function(llist, size=0){
  if (isEmpty(llist)) 
  {
    return(size)
  } else
  {
    size<-size+1L
    sizeLinkList(llist$nextnode, size)
  }
}

addElement<-function(new, llist) 
{
  if (isEmpty(llist)) {
    llist<-linkedlist(new)
  } else
  {
    llist<-linkListNode(llist, new)
  }
  llist
}

delElement<-function(llist, pos=NULL){
  if(is.null(pos)) warning("Nothing to delete")
  listsize<-sizeLinkList(llist)
  if(pos>listsize) stop("Position greater than size of list")
  if (isEmpty(llist)) {
    warning("Empty List")
  } else if(pos==1){
    PreviousNode<-llist$nextnode
  } else 
  {
    PreviousNode<-linkListNode(llist$element)
    for(i in 1:(listsize-1)){
      if(pos==(i+1)){
        PreviousNode$nextnode<-setNextNode(llist$nextnode)
      } else
      {
        PreviousNode$nextnode<-llist$nextnode
        llist<-llist$nextnode
      }
    }
  }
  return(PreviousNode)
}

findItem<-function(llist, item, pos=0, itemFound=FALSE){
  if (itemFound==TRUE) 
  {
    return(itemFound)
  } else if(isEmpty(llist)){
    return(FALSE)
  } else
  {
    pos<-pos+1L
    if(llist$element==item) itemFound<-TRUE
    findItem(llist$nextnode, item, size, itemFound)
  }
}

## 3.3.2 Doubly Link List
dlinkListNode <- function(val, prevnode=NULL, node=NULL) {
  llist <- new.env(parent=create_emptyenv())
  llist$prevnode <- prevnode
  llist$element <- val
  llist$nextnode <- node
  class(llist) <- "dlinkList"
  llist
}

## 3.3.3 Circular Link List
cicularLinkList<-function(llist, val){
  if(isEmpty(llist)){
    llist<-linkListNode(val)
    head<-llist
  } else
  {
    llistNew<-linkListNode(val)
    llistNew$nextnode<-head
    llist<-linkListNode(llist, llistNew) 
  }
  llist
}

######################################################
##############  3.4 Array based List    ##############
######################################################
ALinkList<-setRefClass(Class = "ALinkList",
                       fields = list(
                         Alist="array",
                         listsize="integer",
                         arraySize="integer",
                         maxSize="integer"
                       ), 
                       methods = list(
                         initialize=function(...){
                           listsize<<-0L
                           arraySize<<-100L
                           Alist<<-array(dim = arraySize)
                           maxSize<<-arraySize
                         }
                       ))
listlen = function()
{
  return(listsize)
}

updateArrayList=function(){
  Alist<<-c(Alist, array(dim=arraySize))
  maxSize<<-maxSize+arraySize
}

addItem=function(item){
  if(maxSize<=listsize){
    updateArrayList()
  } 
  listsize<<-listsize+1L
  Alist[listsize]<-item
  return(listsize)
}

removeItem = function(i)
{
  Alist[i] <<- NULL
  listsize <<- listsize - 1L
}

searchItem = function(val){
  pointer<-1L
  while(pointer!=listsize){
    if(Alist[pointer]==val){
      break
    } 
    pointer<-pointer+1L
  }
  return(pointer)
}


