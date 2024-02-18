# Simulate forward Galton-Watson branching process 
CreateBranchingTree <- function(depth, parameter) {
  while (1 == 1) {
    columns= c("Lable", "Generation", "Children")
    treedata = data.frame(matrix(nrow = 1, ncol = length(columns)))
    colnames(treedata) = columns
    
    Z_k=1
    L_k <- c("1")
    treedata[1,] <- c(paste(L_k, collapse = ' '), 0, 0)
    
    for(d in 0:depth){
      x <- c()
      L <- c()
      tail <- tail(treedata, Z_k)
      head <- head(treedata, nrow(treedata)-Z_k)
      for(i in 1:Z_k){
        tail$Generation[i] <- d
        if (d == depth) {
          tail$Children[i] <- 0
        }
        else {
          chnum <- rpois(1,parameter)
          x <- c(x, chnum)
          tail$Children[i] <- chnum
        }
      }
      treedata <- rbind(head, tail)
      Z <- sum(x)
      if (d == depth){
        break
      }
      if (length(L_k) != 0) {
        for (i in 1:length(L_k)) {
          if(x[i] != 0){
            name <- L_k[i]
            for (j in 1:x[i]){
              newname <- paste(name,".", j, sep = "")
              L <- c(L, newname)
            }
          }
        }
      }
      
      for (k in 1:length(L)) {
        if (length(L) == 0) {
          break
        }
        
        treedata[nrow(treedata) + 1,] = L[k]
      }
      
      Z_k <- Z
      L_k <- L
      if(Z_k == 0) {
        break
      }
    }
    if (max(treedata$Generation) == depth) {
      break
    }
  }
  
  colnames(treedata) = c("levelName", "generation_num", "child_num")
  treedata <- treedata %>%
    mutate( generation_num = as.integer(generation_num))
  treedata <- treedata %>%
    mutate( child_num = as.integer(child_num))
  sapply(treedata, class)
  exampleTree <- treedata
  return(exampleTree)
}
