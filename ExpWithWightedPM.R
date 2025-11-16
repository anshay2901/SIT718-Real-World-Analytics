## Weighted Power Mean (general) 
weighted_power_mean <- function(x, w, p) {
  stopifnot(length(x) == length(w),
            all(x > 0),              # required for p<=0 cases
            all(w >= 0), sum(w) > 0)
  w <- w / sum(w)                    # ensure weights sum to 1
  
  if (isTRUE(all.equal(p, 0))) {
    # Geometric mean (special case)
    return(exp(sum(w * log(x))))
  } else {
    return( (sum(w * (x ^ p)))^(1/p) )
  }
}

## wrappers
harmonic_mean_w <- function(x, w) weighted_power_mean(x, w, p = -1)
quadratic_mean_w <- function(x, w) weighted_power_mean(x, w, p = 2)


bump <- function(x, i, delta) {
  x2 <- x
  x2[i] <- x2[i] + delta
  x2
}

run_experiment <- function(x, w, delta = 0.10) {
  low_i  <- which.min(x)
  high_i <- which.max(x)
  
  base_hm <- harmonic_mean_w(x, w)
  base_qm <- quadratic_mean_w(x, w)
  
  # Increase the lowest entry
  x_low  <- bump(x, low_i,  delta)
  hm_low <- harmonic_mean_w(x_low, w)
  qm_low <- quadratic_mean_w(x_low, w)
  
  # Increase the highest entry
  x_high  <- bump(x, high_i, delta)
  hm_high <- harmonic_mean_w(x_high, w)
  qm_high <- quadratic_mean_w(x_high, w)
  
  out <- data.frame(
    mean_type = c("Harmonic (p=-1)", "Harmonic (p=-1)", "Quadratic (p=2)", "Quadratic (p=2)"),
    which_bumped = c("lowest +delta", "highest +delta", "lowest +delta", "highest +delta"),
    baseline = c(base_hm, base_hm, base_qm, base_qm),
    new_value = c(hm_low, hm_high, qm_low, qm_high)
  )
  out$change <- out$new_value - out$baseline
  rownames(out) <- NULL
  return(out)
}

## sample inputs
w <- c(0.5, 0.2, 0.3)   # weights (will be normalized)
x <- c(0.7, 0.6, 0.3)   # feature values (positive)

## Single-shot check with delta = 0.10
res <- run_experiment(x, w, delta = 0.10)
print(res)

## - For Harmonic Mean (p < 1): bumping the LOW value increases the mean more
##   than bumping the HIGH value - “more affected by low inputs”.
## - For Quadratic Mean (p > 1): bumping the HIGH value increases the mean more
##   than bumping the LOW value - “more affected by high inputs”.

## sweep over multiple deltas and summarize
sweep_deltas <- function(x, w, deltas = seq(0.01, 0.30, by = 0.01)) {
  low_i  <- which.min(x)
  high_i <- which.max(x)
  base_hm <- harmonic_mean_w(x, w)
  base_qm <- quadratic_mean_w(x, w)
  
  collect <- lapply(deltas, function(d) {
    hm_low  <- harmonic_mean_w(bump(x, low_i,  d), w)
    hm_high <- harmonic_mean_w(bump(x, high_i, d), w)
    qm_low  <- quadratic_mean_w(bump(x, low_i,  d), w)
    qm_high <- quadratic_mean_w(bump(x, high_i, d), w)
    
    data.frame(
      delta = d,
      HM_change_low  = hm_low  - base_hm,
      HM_change_high = hm_high - base_hm,
      QM_change_low  = qm_low  - base_qm,
      QM_change_high = qm_high - base_qm
    )
  })
  do.call(rbind, collect)
}

sweep_res <- sweep_deltas(x, w)
print(head(sweep_res, 5))

## quick visual
matplot(
  sweep_res$delta,
  cbind(sweep_res$HM_change_low, sweep_res$HM_change_high),
  type = "l", lty = 1, xlab = "delta", ylab = "change in mean",
  main = "Harmonic Mean sensitivity"
)
legend("topleft", c("bump lowest", "bump highest"), lty = 1, col = 1:2, bty = "n")

matplot(
  sweep_res$delta,
  cbind(sweep_res$QM_change_low, sweep_res$QM_change_high),
  type = "l", lty = 1, xlab = "delta", ylab = "change in mean",
  main = "Quadratic Mean sensitivity"
)
legend("topleft", c("bump lowest", "bump highest"), lty = 1, col = 1:2, bty = "n")
