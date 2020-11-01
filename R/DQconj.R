#' Dual quaternion conjugation
#'
#' Performs three kinds of dual quaternion conjugation
#'
#' @param d A numeric vector of class DualQuaternion
#' @param type Type of conjugation. See details.
#' @details Function implements three kinds of conjugation for a dual quaternion
#' d = x + ey.
#' \itemize{
#'  \item{type = 1: }{\code{d* = x - ey}}
#'  \item{type = 2: }{\code{d* = x* + ey*}}
#'  \item{type = 3: }{\code{d* = x* - ey*}}
#' }
#' @return The conjugate of d


DQconj <- function(d, type = 1) {

    if (!type %in% 1:3) {
        stop('Argument type must be an integer between 1 and 3')
    }

    # Type 1 conjugation: d* = x - ey
    if (type == 1) {
        ret <- d
        ret[5:8] <- ret[5:8] * -c(1,1,1,1)
    }

    # Type 2 conjugation: d* = x* + ey*
    if (type == 2) {
        ret <- c(Qconj(d[1:4]), Qconj(d[5:8]))
    }

    # Type 3 conjugation: d* = x* - ey*
    if (type == 3) {
        ret <- c(Qconj(d[1:4]), -Qconj(d[5:8]))
    }

    # class(ret) <- 'DualQuaternion'
    return(ret)
}
