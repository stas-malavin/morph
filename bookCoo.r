bookCoo <- function(Coo, p1, p2) {
  require(magrittr)
  C <- Coo$coo
  C <- lapply(C, function(x) coo_bookstein(x, p1, p2))
  C <- lapply(C, function(x) x[-c(p1, p2),])
  nrow <- length(C)
  ncol <- length(unlist(C[1]))
  M <- matrix(nrow = nrow, ncol = ncol)
  for (i in 1:length(C)) {
    M[i,] <- unlist(C[i])
  }
  return(M)
}