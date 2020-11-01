#' Dual quaternion multiplication
#'
#' @param x,y Numeric vectors of class DualQuaternion
#' @return A numeric vector of class DualQuaternion

`%DQ*%` <- function(x, y) {

    p1 <- x[1:4]
    q1 <- x[5:8]
    p2 <- y[1:4]
    q2 <- y[5:8]

    ret <- c(p1 %Q*% p2, p1 %Q*% q2 + q1 %Q*% p2)
    return(ret)

}
