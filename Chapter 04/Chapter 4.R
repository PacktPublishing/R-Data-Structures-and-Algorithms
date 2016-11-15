####################         CHAPTER 3: Stacks and Queues           #############################

######################################################
#######   3.1.1 Array-based Stack     ################  
######################################################
Astack <- setRefClass(Class = "Astack",
                      fields = list(
                        Maxsize="integer",
                        topPos="integer",
                        ArrayStack="array"
                      ),
                      methods = list(
                        # Initialization function
                        initialize=function(defaultSize=100L,...) 
                        {
                          topPos<<-0L
                          Maxsize<<-defaultSize # 100L
                          ArrayStack<<-array(dim = Maxsize)
                        },
                        
                        # Check if stack is empty
                        isEmpty=function(){},
                        
                        # push value to stack
                        push=function(pushval){},
                        
                        # Pop value from stack
                        pop=function(){},
                        
                        # Function to get size of stack
                        stacksize=function(){},
                        
                        # Function to get top value of stack
                        top=function(){}
                      ))


isEmpty=function(){
  if(topPos==0) {
    cat("Empty Stack!")
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}


push=function(pushval){
  if((topPos+1L)>Maxsize) stop("Stack is OUT OF MEMORY!")
  topPos<<-topPos+1L
  ArrayStack[topPos]<<-pushval
}

pop=function(){
  isEmpty() # Check if stack is empty
  popval<-ArrayStack[topPos]
  ArrayStack[topPos]<<-NA
  topPos<<-topPos-1L
  return(popval)
}

stacksize=function(){
  stackIsEmpty<-isEmpty()
  ifelse(stackIsEmpty, return(0), return(topPos))
}

top=function(){
  stackIsEmpty<-isEmpty() 
  if(stackIsEmpty) {
    cat("Empty Stack") 
  } else 
  {
    return(ArrayStack[topPos])
  }
}

array_stack_ex<- Astack$new()
array_stack_ex$push(1)
array_stack_ex$push(2)
array_stack_ex$push(3)
array_stack_ex$pop()
array_stack_ex$push(5)
array_stack_ex$pop()
array_stack_ex$pop()
array_stack_ex$top()
array_stack_ex

######################################################
#######    3.1.2 Linked Stacks        ################  
######################################################

Linkstack <- setRefClass(Class = "Linkstack",
                         fields = list(
                           Lsize="integer",
                           Lstacktop="environment"),
                         methods = list(
                           # Initialization function
                           initialize=function(...) {
                             Lsize<<-0L
                           },
                           
                           # Check if stack is empty
                           isEmpty=function(){},
                           
                           # Function to create empty R environment
                           create_emptyenv=function(){},
                           
                           # Function to create node
                           Node = function(val, node=NULL) {},
                           
                           # push value to stack
                           push=function(pushval){},
                           
                           # Pop value from stack
                           pop=function(){},
                           
                           # Function to get top value of stack
                           top=function(){}
                         ))


isEmpty=function(){
  if(Lsize==0) {
    cat("Empty Stack!")
    return(TRUE)
  } else
  {
    return(FALSE)
  }
} 

create_emptyenv = function() {
  emptyenv()
},
Node = function(val, node=NULL) {
  llist <- new.env(parent=create_emptyenv())
  llist$element <- val
  llist$nextnode <- node
  llist
}

push=function(val){
  stackIsEmpty<-isEmpty()
  if(stackIsEmpty){
    Lstacktop<<-Node(val)
    Lsize<<-Lsize+1L
  } else
  {
    Lstacktop<<-Node(val, Lstacktop)
    Lsize<<-Lsize+1L
  }
}

pop=function(){
  stackIsEmpty<-isEmpty()
  if(stackIsEmpty){
    cat("Empty Stack")
  } else
  {
    Lstacktop<<-Lstacktop$nextnode
    Lsize<<-Lsize-1L
  }
}

topVal=function(){
  stackIsEmpty<-isEmpty()
  if(stackIsEmpty){
    cat("Empty Stack")
  } else
  {
    return(Lstacktop$element)
  }
}

link_stack_ex<-Linkstack$new()
link_stack_ex $push(1)
link_stack_ex $push(2)
link_stack_ex $push(3)
link_stack_ex $pop()
link_stack_ex $push(5)
link_stack_ex $pop()
link_stack_ex $pop()
a$topVal()
link_stack_ex


######################################################
#######  3.1.4 Implementing Recursion ################  
######################################################

recursive_fact<-function(n) {
  if(n<0) return(-1)
  if(n == 0) {
    return(1)
  } else { 
    return(n*recursive_fact(n-1))
  }
}

######################################################
#######  3.2.1 Array based Queues     ################  
######################################################
aqueue<-setRefClass(Class = "aqueue",
                    fields = list(
                      Alist="array",
                      queuesize="integer",
                      maxSize="integer",
                      rear = "integer",
                      top = "integer"
                    ), 
                    methods = list(
                      initialize=function(qSize, ...){
                        queuesize<<-0L
                        rear<<-1L
                        top<<-0L
                        maxSize<<-as.integer(qSize)
                        Alist<<-array(dim = maxSize)
                      }
                      # Queue is empty
                      isEmpty = function() {},
                      # Add element to the queue
                      enqueue = function(val){},
                      
                      # remove element from queue
                      dequeue = function() {},
                      
                      # size of queue
                      size = function() {}
                    ))

q<-aqueue$new()
q

isEmpty = function() {
  return(queuesize==0L)
}

enqueue = function(val){
  if(queuesize<maxSize){
    if(top==maxSize) top<<-0L
    top<<-top + 1L
    Alist[top]<<-val
    queuesize<<-queuesize+1L
  } else
  {
    cat("Queue Full!")
  }
},

dequeue = function() {
  if(queuesize>0L){
    Alist[rear]<<-NA
    ifelse(rear==maxSize, rear<<-1L, rear<<-rear+1L)
    queuesize<<-queuesize-1L
  } else
  {
    cat("Empty Queue!") 
  }
  

  ListQueue <- setRefClass(Class = "ListQueue",
                           fields = list(
                             Lsize="integer",
                             front="environment", 
                             rear = "environment",
                             Lqueue="environment"),
                           
                           methods = list(
                             initialize=function(...) {
                               Lsize<<-0L
                             },
                             
                             # Check if list is empty
                             isEmpty=function(){}, 
                             
                             # create empty environment
                             create_emptyenv = function() {},
                             
                             # Create node 
                             Node = function(val, node=NULL) {},
                             
                             # Function to add value to link list
                             enqueue=function(val){},
                             
                             # Function to remove node from link list
                             dequeue=function(){}
                             
                             # Function to get link list size
                             size=function(){} 
                           ))

  isEmpty=function(){
    if(Lsize==0) {
      cat("Empty Stack!")
      return(TRUE)
    } else
    {
      return(FALSE)
    }
  }

  create_emptyenv = function() {
    emptyenv()
  } 
  
  Node = function(val, node=NULL) {
    llist <-new.env(parent=create_emptyenv())
    llist$element <- val
    llist$nextnode <- node
    llist
  }
  
  enqueue=function(val){
    ListIsEmpty<-isEmpty()
    if(ListIsEmpty){
      Lqueue<<-Node(val)
      Lsize<<-Lsize+1L
      rear<<-Lqueue
    } else
    {
      newNode<-Node(val)
      assign("nextnode", newNode, envir = rear)
      rear<<-newNode
      Lsize<<-Lsize+1L
    }
  }
  
  dequeue=function(){
    stackIsEmpty<-isEmpty()
    if(stackIsEmpty){
      cat("Empty Queue")
    } else
    {
      Lqueue<<-Lqueue$nextnode
      Lsize<<-Lsize-1L
    }
  } 
  
#####################################################
######       3.3 Dictionaries        ################  
#####################################################  
Adict<-setRefClass(fields = list(
  Alist="list",
  listsize="integer",
  key="integer"
),
methods = list(
  # Re-initialize dictionary
  initialize=function(...){
    listsize<<-0L
    Alist<<-list()
  },
  
  # Check length of value
  size = function(){}, 
  
  # Add following key value pair in Array
  addElement = function(key, val){},
  
  # remove value with defined 
  removeElement = function(key){},
  
  # remove value with following
  findElement = function(key){},  
} 

size = function(){
  return(listsize) 
}

addElement = function(key, val){
  Alist[[key]]<<-val
  listsize<<-listsize+1L
}, 

removeElement = function(key){
  Alist[[key]]<<-NULL
  listsize<<-listsize-1L
}

findElement = function(key){
  return(key%in%names(Alist))
}


dictvar<-Adict$new()
dictvar$addElement("key1", 1)
dictvar$addElement("key2", 1)
dictvar
dictvar$Size()
dictvar$findElement("key1")
dictvar$removeElement("key1")


