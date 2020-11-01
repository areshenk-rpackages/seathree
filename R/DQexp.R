#' Exponential of a unit dual quaternion
#'
#' Computes the exponential of a pure unit dual quaternion. Optionally, a unit dual
#' quaternion p can be provided, in which case the function returns can be
#' interpreted as the projection of the tangent vector d onto the space of unit
#' dual quaternions at p.
#'
#' @param d A unit dual quaternion
#' @param p Point at which to take the logarithm
#' @return The logarithm of d

DQexp <- function(d, p = NULL) {

    if (is.null(p)) {
        p <- c(1, rep(0, 7))
    }

    p.inv <- DQinverse(p)
    y <- (p.inv %DQ*% d) * 2

    # Recover DQ parameters
    phi <- sqrt(sum(y[2:4]^2))
    n   <- y[2:4]/phi
    l   <- sqrt(sum(y[6:8]^2))
    s   <- y[6:8]/l

    v <- s * l
    r <- c(cos(phi/2), n * sin(phi/2))

    return(p %DQ*% tr2dq(v, r))
}
