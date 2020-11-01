context("Basic operations")

# Positive definite matrixTesting data
data(kin)
x <- as.numeric(kin[1,])
y <- as.numeric(kin[60,])

test_that("Conversion to and from DQ", {

    # Conversion to DQ
    expect_error(tr2dq(x[1:3], y[4:7]), NA)
    q  <- tr2dq(x[1:3], x[4:7])

    expect_error(dq2tr(q), NA)
    tr <- dq2tr(q)

    expect_true(isTRUE(all.equal(tr[[1]], x[1:3])))
    expect_true(isTRUE(all.equal(tr[[2]], x[4:7])))

})

test_that("Multiplication", {

    q  <- tr2dq(y[1:3], y[4:7])
    p  <- tr2dq(y[1:3], y[4:7])
    expect_error(q %DQ*% p, NA)

})

test_that("Norm", {

    q  <- tr2dq(x[1:3], x[4:7])
    expect_true(isTRUE(all.equal(DQnorm(q), c(1,0))))

})

test_that("Conjugation", {

    q  <- tr2dq(x[1:3], x[4:7])
    expect_error(DQconj(q, 1), NA)
    expect_error(DQconj(q, 2), NA)
    expect_error(DQconj(q, 3), NA)

})

test_that("Inversion", {

    q  <- tr2dq(x[1:3], x[4:7])
    qinv <- DQinverse(q)
    expect_true(isTRUE(all.equal(q %DQ*% qinv, c(1,0,0,0,0,0,0,0))))

})
