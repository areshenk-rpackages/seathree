#' Convert a displacement to a dual quaternion
#'
#' Converts a translation vector and unit quaternion to a dual quaternion specifying
#' the corresponding displacement.
#'
#' @param v A translation vector in R3
#' @param q A unit quaternion
#' @param warn Logical. Return a warning if q is not a unit quaternion?
#' @details Function generates a dual quaternion encoding a translation specified
#' by v and rotation specified by q. By default, if q is not a unit quaternion
#' (possibly due to numerical error), it will be quietly normalized. If warn = T,
#' the function will return a warning when this takes place.
#' @return A numeric vector of length 8


tr2dq <- function(v, q, warn = F) {

    # Check that v is admissible
    if (!is.numeric(v))  stop('v must be a numeric vector')
    if (!length(v) == 3) stop('v must be a vector of length 3')

    # Check that q is admissible
    if (!is.numeric(q))  stop('q must be a numeric vector')
    if (!length(q) == 4) stop('q must be a vector of length 4')

    # If q is not a unit quaternion, normalize. Output warning if requested.
    q.norm <- Qnorm(q)
    if (!isTRUE(all.equal(q.norm, 1))) {
        if (warn) {
            warning('q is not a unit quaternion. Normalizing.')
        }
        q <- as.numeric(Qnormalize(q))
    }

    # Write the dual quaternion corresponding to translation v and rotation q
    qt <- c(0, v)
    qr <- q
    qd <- as.numeric(qt %Q*% qr)/2

    dq <- unname(c(qr, qd))
    #class(dq) <- 'DualQuaternion'
    return(dq)

}
