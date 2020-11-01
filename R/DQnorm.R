#' Dual quaternion norm
#'
#' Calculates the norm of a dual quaternion
#'
#' @param d A numeric vector of class DualQuaternion
#' @return The norm of d. A dual number given by a numeric 2-vector.

DQnorm <- function(d) {

    if (isTRUE(all.equal(d[1:4], 0))) {
        warning('Real part of d must be non-zero.')
        return(c(NA, NA))
    }

    qr <- d[1:4]
    qd <- d[5:8]
    qr.norm <- Qnorm(qr)
    d.norm  <- sum(qr * qd) / qr.norm
    return(c(qr.norm, d.norm))
}
