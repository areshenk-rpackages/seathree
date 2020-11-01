#' Unit quaternion mean
#'
#' Computes the mean of a matrix of unit quaternions.
#'
#' @param q An n x 4 matrix of unit quaternions.
#' @return A unit quaternion.

Qmean <- function(q) {

    M <- t(q) %*% q / nrow(q)
    e <- eigen(M, symmetric = T)$vectors[,1]
    return(e * sign(e[1]))
}
