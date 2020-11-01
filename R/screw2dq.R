#' Convert a screw transformation to a dual quaternion
#'
#' Extract the parameters of a screw transformation from a dual quaternion
#'
#' @param theta Dual angle
#' @param s Vector parts of a pure dual quaternion
#' @return A unit dual quaternion


screw2dq <- function(theta, s) {

    p  <- c(cos(theta[1]/2), l[1:3] * sin(theta[1]/2))
    q  <- c(-(theta[2]/2) * sin(theta[1]/2), sin(theta[1]/2)*s[4:6] +
                (theta[2]/2) * cos(theta/2) * s[1:3])
    dq <- c(p,q)
    return(dq)
}
