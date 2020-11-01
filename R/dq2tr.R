#' Convert a dual quaternion to a displacement
#'
#' Converts a (unit) dual quaternion to a translation vector in R3 and a unit
#' quaternion specifying a rotation.
#'
#' @param d A unit dual quaternion
#' @return A list containing a translation vector v and a unit quaternion q


dq2tr <- function(d) {
    q <- d[1:4]
    v <- 2*(d[5:8] %Q*% Qconj(q))
    ret <- list(v = v[-1], q = q)
    return(ret)

}
