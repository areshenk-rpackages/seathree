#' Normalize a dual quaternion
#'
#' @param d A numeric 4-vector corresponding to a unit quaternion.
#' @details A dual quaternion d = r + .5e(rt)  is a unit dual quaternion when
#' when the rotation component r is a unit quaternion, and r and rt are orthogonal
#' as 4-vectors.
#' @return The normalized dual quaternion d


DQnormalize <- function(d) {

    if (isTRUE(all.equal(d[1:4], 0))) {
        warning('Real part of d must be non-zero.')
        return(rep(NA, 8))
    }

    qr <- d[1:4]
    qd <- d[5:8]
    qr.norm <- Qnorm(d[1:4])
    qrn <- qr / qr.norm
    qdn <- qd / qr.norm

    return(c(qrn, qdn))
}
