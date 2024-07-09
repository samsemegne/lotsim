#' @include helpers.R shared_behavior.R lot_series.R
NULL


#' @export
is.lot_object_wrapper = function(x) inherits(x, "lot_object_wrapper", FALSE)

#' @export
get_count.lot_object_wrapper = get_count.lot_series
#' @export
get_prop.lot_object_wrapper = get_prop.lot_series
#' @export
get_size.lot_object_wrapper = get_size.lot_series
#' @export
get_true_prop.lot_object_wrapper = get_true_prop.lot_series
#' @export
get_true_count.lot_object_wrapper = get_true_count.lot_series
#' @export
get_num_lot.lot_object_wrapper = function(x) {
  sum(lapun(x, get_num_lot), na.rm = FALSE)
}


#' @export
as.data.frame.lot_object_wrapper = as_df


#' @export
c.lot_object_wrapper = lot_add_many_op
#' @export
`+.lot_object_wrapper` = lot_add_op

#' @export
plot.lot_object_wrapper = function(x, ...) plot_lot_obj(x, ...)


is_wrapper_content = function(x) is.lot(x) || is.lot_series(x)


new_lot_object_wrapper = function(objects, ...) {
  stopifnot(exprs = {
    all(lapun(objects, is_wrapper_content), na.rm = FALSE)
  })

  structure(
    objects,
    class = "lot_object_wrapper",
    ...
  )
}


#' @export
lot_sample.lot_object_wrapper = function(x, size) {
  stopifnot(exprs = {
    vek::is_int_vec_x1(size)
    all(size > 0L, na.rm = FALSE)
    all(size <= get_size(x), na.rm = FALSE)
    all(lapun(x, is_wrapper_content), na.rm = TRUE)
  })

  f = Vectorize(lot_sample, SIMPLIFY = FALSE)
  sampled = f(x, size)

  new_lot_object_wrapper(
    sampled,
    method = "sampled"
  )
}










