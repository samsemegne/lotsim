#' @include helpers.R shared_behavior.R
NULL


#' @export
is.lot = function(x) inherits(x, "lot", FALSE)
#' @export
get_num_lot.lot = function(x) 1L
#' @export
get_true_prop.lot = function(x) attr(x, "true_prop", TRUE) %||% NA_real_
#' @export
get_true_count.lot = function(x) attr(x, "true_count", TRUE) %||% NA_integer_
#' @export
get_count.lot = function(x) attr(x, "count", TRUE) %||% stop("no count attr")
#' @export
get_prop.lot = function(x) attr(x, "prop", TRUE) %||% stop("no prop attr")
#' @export
get_size.lot = function(x) length(x)


#' @export
c.lot = lot_add_many_op
#' @export
`+.lot` = lot_add_op

#' @export
`-.lot` = throw_invalid_op_error
#' @export
`*.lot` = throw_invalid_op_error
#' @export
`^.lot` = throw_invalid_op_error
#' @export
`%%.lot` = throw_invalid_op_error


#' @export
as.data.frame.lot = as_df

#' @export
print.lot = function(x, ...) {
  cat(str(x, ...))
}


#' @export
new_lot = function(lot, ..., class = character()) {
  stopifnot(exprs = {
    vek::is_int_vec_x(lot)
    vek::is_chr_vec_xb(class)
    length(lot) > 0L
    all(lot %in% 0:1, na.rm = FALSE)
    !(class %in% c("lot", "lot_series", "lot_object_wrapper"))
  })

  count = sum(lot == 1L, na.rm = FALSE)

  structure(
    lot,
    class = c(class, "lot"),
    count = count,
    prop = count / length(lot),
    ...
  )
}


#' @export
lot_sample.lot = function(x, size) {
  stopifnot(exprs = {
    vek::is_int_vec_x1(size)
    size > 0L
  })

  sampled = sample(x, size, FALSE, NULL)

  new_lot(
    sampled,
    true_count = get_count(x),
    true_prop = get_prop(x),
    parent_size = get_size(x),
    method = "sampled"
  )
}

