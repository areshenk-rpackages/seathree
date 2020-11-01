#' Unit dual quaternion mean
#'
#' Computes the mean of a set of unit dual quaternions. The problem of defining
#' a mean on the space of unit DQs is not completely trivial, and optimal
#' methods are typically iterative and computationally intensive. This function
#' (for now) takes the naive approach of decomposing the DQ into vector and
#' quaternion components, computing the mean of each, and then reassembling
#' them into a DQ mean,
#'
#' @param d An n x 8 matrix of unit dual quaternions.
#' @return A unit dual quaternion.

DQmean <- function(d) {

    posrot <- apply(d, 1, dq2tr)
    v <- t(sapply(posrot, function(i) i$v))
    q <- t(sapply(posrot, function(i) i$q))

    v.mean <- colMeans(v)
    q.mean <- Qmean(q)
    d.mean <- tr2dq(v.mean, q.mean)
    return(d.mean)
}
