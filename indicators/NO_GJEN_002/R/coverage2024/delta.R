# Delta-method SE on the response scale and bounded logit-Wald interval.
delta <- function(model, estimand) {
  p <- model$sdr$par.fixed
  V <- model$sdr$cov.fixed
  stopifnot(is.numeric(p), all(is.finite(p)), is.matrix(V),
            nrow(V) == length(p), ncol(V) == length(p), all(is.finite(V)))
  est <- estimand(p)
  stopifnot(is.finite(est), est > 0, est < 1)
  gradient <- function(scale) {
    vapply(seq_along(p), function(i) {
      h <- scale * max(1, abs(p[i]))
      lo <- hi <- p
      lo[i] <- p[i] - h
      hi[i] <- p[i] + h
      (estimand(hi) - estimand(lo)) / (2*h)
    }, numeric(1))
  }
  g <- gradient(1e-4)
  check <- gradient(5e-5)
  stopifnot(all(is.finite(g)), max(abs(g-check)) < 1e-5)
  variance <- drop(crossprod(check, V %*% check))
  stopifnot(is.finite(variance), variance >= -1e-12)
  se <- sqrt(max(0, variance))
  ci <- plogis(qlogis(est) + c(-1,1)*qnorm(.975)*se/(est*(1-est)))
  c(estimat = est, se = se, ki95_nedre = ci[1], ki95_ovre = ci[2])
}
