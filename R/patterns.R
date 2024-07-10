


#' @export
slope = function(x, slope) {
  stopifnot(exprs = {
    vek::is_num_vec(x)
    vek::is_num_vec_xyz1(slope)
  })

  i = 0:(length(x) - 1L)
  val = x + i * slope
  return(val)
}


#' @export
wave = function(x, wave_len, amplitude) {
  stopifnot(exprs = {
    vek::is_num_vec(x)
    vek::is_num_vec_xyz1(wave_len)
    vek::is_num_vec_xyz1(amplitude)
  })

  i = 0:(length(x) - 1L)
  val = x + sin(i * wave_len) * amplitude
  return(val)
}


#' @export
stairs = function(x, height, case_len) {
  stopifnot(exprs = {
    vek::is_num_vec(x)
    vek::is_num_vec_xyz1(height)
    vek::is_int_vec_x1(case_len)
  })

  len = length(x)

  num_unique = ceiling(len / case_len)
  y = seq(0L, height, length.out = num_unique)
  last_case_len = len %% case_len
  if (last_case_len == 0L)
    last_case_len = case_len

  vals = lapply(y[-length(y)], function(k) rep(k, case_len)) |>
    unlist(FALSE, FALSE)

  vals = c(vals, rep(y[length(y)], last_case_len))
  stopifnot(length(vals) == length(x))

  vals = x + vals
  return(vals)
}


#' @export
rnorm_runs = function(x, sigma, min_run, max_run, prob_run) {
  stopifnot(exprs = {
    vek::is_num_vec(x)
    vek::is_num_vec_xyz1(sigma)
    vek::is_int_vec_x1(min_run)
    vek::is_int_vec_x1(max_run)
    is_prop_xy1(prob_run)
    sigma >= 0L
    min_run > 0L
    max_run >= min_run
  })

  len = length(x)

  vals = numeric()
  while (length(vals) < len) {
    remaining_len = len - length(vals)
    run_len = rbinom(1L, max_run, prob_run)#|>
    run_len = max(run_len, min_run, na.rm = FALSE)
    run_len = min(run_len, remaining_len, na.rm = FALSE)

    val = rnorm(1L, 0L, sigma)
    vals = c(vals, rep(val, run_len))
  }

  stopifnot(length(vals) == len)
  vals = x + vals
  return(vals)
}
