#' Get the rotation encoded by a quaternion
#'
#' Decomposes a unit quaternion into an angle and axis of rotation.
#'
#' @param q A numeric 4-vector corresponding to a unit quaternion.
#' @param warn Logical. Return a warning if q is not a unit quaternion?
#' @details Function accepts a unit quaternion q and returns a named vector
#' containing the angle of rotation (theta) and a unit vector giving the axis of
#' rotation. By default, if q is not a unit quaternion (possibly due to numerical error),
#' it will be quietly normalized. If warn = T, the function will return a
#' warning when this takes place.
#' @return A named vector \code{(theta, r1, r2, r3)} specifying the angle and axis of rotation.

q2rot <- function(q, warn = F) {

    if (!is.numeric(q)) {
        stop('q must be a numeric vector')
    }
    if (length(q) != 4) {
        stop('q must be a quaternion vector of length 4')
    }

    n <- Qnorm(q)
    if (!isTRUE(all.equal(n, 1))) {
        if (warn) {
            warning('q is not a unit quaternion. Normalizing.')
        }
        q <- as.numeric(Qnormalize(q))
    }

    theta <- 2 * acos(q[1])
    r <- q[2:4] / sqrt(sum(q[2:4]^2))
    c('theta' = theta, 'r1' = r[1], 'r2' = r[2], 'r3' = r[3])

}
