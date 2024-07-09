
#' @export
clamp = function(x, lower, upper) {
  stopifnot(exprs = {
    vek::is_num_vec(x)
    vek::is_num_vec_xy(lower)
    vek::is_num_vec_xy(upper)
    all(lower <= upper, na.rm = FALSE)
  })

  x[x < lower] = lower
  x[x > upper] = upper
  return(x)
}


lapun = function(x, f) lapply(x, f) |> unlist(FALSE, FALSE)


is_prop_xy = function(x) {
  vek::is_num_vec_xyz(x) && all(x >= 0L & x <= 1L, na.rm = FALSE)
}


is_prop_xy1 = function(x) {
  vek::is_num_vec_xyz1(x) && x >= 0L && x <= 1L
}


is_list = function(x) {
  !is.object(x) &&
    is.list(x) &&
    length(class(x)) == 1L &&
    class(x) == "list"
}


throw_invalid_op_error = function(...) stop("Invalid operation")
