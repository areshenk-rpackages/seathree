#' Inverse dual quaternion
#'
#' @param d A numeric vector of class dual quaternion
#' @return The inverse of d


DQinverse <- function(d) {

    # if (class(d) != 'DualQuaternion') {
    #     stop('d must be of class DualQuaternion')
    # }

    if (isTRUE(all.equal(d[1:4], 0))) {
        stop('Scalar component is zero. d is not invertible.')
    }

    U <- d[1:4]
    V <- d[5:8]
    Uc <- Qconj(U)
    ret <- c(Uc, -Uc %Q*% V %Q*% Uc)
    # class(ret) <- 'DualQuaternion'
    return(ret)
}
