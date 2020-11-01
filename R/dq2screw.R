#' Convert a dual quaternion to a screw displacement
#'
#' Extract the parameters of a screw transformation from a dual quaternion
#'
#' @param d A dual quaternion
#' @return A list containing the dual angle theta, and the vector parts of the
#' pure dual quaternion s.


dq2screw <- function(d) {

    p <- d[1:4]
    q <- d[5:8]
    theta <- 2*acos(p[1])
    l <- p[2:4] / sin(theta/2)
    tvec <- as.numeric(2 * q %Q*% Qconj(p))[-1]
    d <- sum(tvec * l)
    m <- (cross(tvec, l) + (tvec - d * l) * cot(theta/2))/2

    ret <- list(theta = c(theta, d), s = c(l, m))
    return(ret)
}
