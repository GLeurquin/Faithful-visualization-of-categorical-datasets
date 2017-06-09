cvr2ranks <- function(num.matrix) {

  rank.ds <- rank(num.matrix[1, ], ties.method = "min")
  
  for (i in 2:dim(num.matrix)[1]) {
    row <- rank(num.matrix[i, ], ties.method= "min")
    rank.ds <- rbind(rank.ds, row)
  }
  
  rownames(rank.ds) <- rownames(num.matrix)
  colnames(rank.ds) <- colnames(num.matrix)
  
  rank.ds
}

binarize <- function(rank.ds, theta) {
  nRows <- dim(rank.ds)[1]
  nCols <- dim(rank.ds)[2]
  
  iTheta <- round(theta*nCols)
  
  cat(paste("iTheta = ", iTheta, "\n"))
  
  bin.ds <- matrix(0, nrow = nRows, ncol = nCols)
  colnames(bin.ds) <- colnames(rank.ds)
  rownames(bin.ds) <- rownames(rank.ds)
  
  for (i in 1:nRows) {
    sel.cols <- which(rank.ds[i, ] > iTheta)
    bin.ds[i, sel.cols] <- 1		
  }
  
  bin.ds
  
}