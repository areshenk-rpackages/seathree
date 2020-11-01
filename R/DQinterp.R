#' Dual quaternion interpolation
#'
#' Interpolate between dual quaternions using dual linear blending (DLB)
#'
#' @param q,p Dual quaternions
#' @param t Interpolation parameter between 0 and 1
#' @details Function interpolates between p and q by computing and then normalizing
#' the dual quaternion q(1-t) + pt
#' @return A dual quaternion


DQinterp <- function(q, p, t) {

    # if (class(q) != 'DualQuaternion') {
    #     stop('q must be of class DualQuaternion')
    # }
    # if (class(p) != 'DualQuaternion') {
    #     stop('p must be of class DualQuaternion')
    # }

    if (!is.numeric(t)) {
        stop('t must be numeric')
        if (t < 0 | t > 1) {
            stop('t must be in the interval [0,1]')
        }
    }

    x <- q * (1-t) + p * t
    # class(x) <- 'DualQuaternion'
    return(DQnormalize(x))

}
