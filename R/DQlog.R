#' Logarithm of a unit dual quaternion
#'
#' Computes the logarithm of a unit dual quaternion. Optionally, a unit dual
#' quaternion p can be provided, in which case the function returns
#' log( inv(p) * d) which can be interpreted as the projection of d onto
#' the tangent space at p.
#'
#' @param d A unit dual quaternion
#' @param p Point at which to take the logarithm
#' @return The logarithm of d

DQlog <- function(d, p = NULL) {

    if (is.null(p)) {
        p <- c(1, rep(0, 7))
    }
    if (isTRUE(all.equal(d-p, 0))) {
        return(rep(0, 8))
    }

    p.inv <- DQinverse(p)
    y <- DQnormalize(p.inv %DQ*% d)

    # Extract screw parameters
    tr <- dq2tr(y)
    v  <- tr$v
    q  <- tr$q

    # Logarithm parameters
    l <- sqrt(sum(v^2))
    s <- v / l
    phi <- 2 * acos(q[1])
    n <- q[2:4] / sin(phi/2)

    ld <- c(0, phi * n, 0, l * s)/2

    return(p %DQ*% ld)
}
