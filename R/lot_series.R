#' @include helpers.R shared_behavior.R
NULL


#' @export
is.lot_series = function(x) inherits(x, "lot_series", FALSE)


#' @export
get_count.lot_series = function(x) lapun(x, get_count)
#' @export
get_prop.lot_series = function(x) lapun(x, get_prop)
#' @export
get_size.lot_series = function(x) lapun(x, get_size)
#' @export
get_true_prop.lot_series = function(x) lapun(x, get_true_prop)
#' @export
get_true_count.lot_series = function(x) lapun(x, get_true_count)
#' @export
get_num_lot.lot_series = function(x) length(x)


#' @export
plot.lot_series = function(x, ...) plot_lot_obj(x, ...)


#' @export
c.lot_series = lot_add_many_op
#' @export
`+.lot_series` = lot_add_op


#' @export
as.data.frame.lot_series = as_df

# TODO
#' @export
as.lot_series = function(x) {
  if (is.lot(x)) {
    return(new_lot_series(list(x)))
  }
  else if (is.lot_object_wrapper(x)) {
    lots = list()
    for (obj in x) {
      len = length(lots)
      n_lot = get_num_lot(obj)
      if (is.lot(obj))
        lots[len + 1L] = list(obj)
      else if (is.lot_series(obj))
        lots[len + 1:n_lot] = obj[1:n_lot]
      else
        stop("invalid state")
    }

    return(new_lot_series(lots))
  } else if (is.lot_series(x)) {
    warning("Argument 'x' is already a 'lot_series'")
    return(x)
  } else {
    stop("Can't coerce argument 'x' to 'lot_series'")
  }
}


new_lot_series = function(lots, ..., class = character()) {
  stopifnot(exprs = {
    is_list(lots)
    vek::is_chr_vec_xb(class)
    length(lots) > 0L
    all(lapun(lots, is.lot), na.rm = FALSE)
    !(class %in% c("lot", "lot_object_wrapper"))
  })

  structure(
    lots,
    class = c(class, "lot_series"),
    ...
  )
}


#' @export
new_lot_series__rbinom = function(size, prop) {
  stopifnot(exprs = {
    vek::is_int_vec_x(size)
    is_prop_xy(prop)
    all(size > 0L, na.rm = FALSE)
  })

  f = function(size, prop) {
    lot_data = stats::rbinom(size, 1L, prop)
    lot = new_lot(
      lot_data,
      true_prop = prop,
      method = "rbinom"
    )

    return(lot)
  }

  f = Vectorize(f, SIMPLIFY = FALSE)
  lots = f(size, prop)

  new_lot_series(lots)
}


#' @export
new_lot_series__shuffle = function(size, count) {
  stopifnot(exprs = {
    vek::is_int_vec_x(size)
    vek::is_int_vec_x(count)
    all(size > 0L, na.rm = FALSE)
    all(count >= 0L, na.rm = FALSE)
    all(count <= size, na.rm = FALSE)
  })

  f = function(size, count) {
    lot = rep.int(0L, size)
    lot[1:count] = 1L
    lot = sample(lot, length(lot), FALSE, NULL) # shuffle

    new_lot(
      lot,
      method = "shuffle"
    )
  }

  f = Vectorize(f, SIMPLIFY = FALSE)
  lots = f(size, count)

  new_lot_series(lots)
}


#' @export
lot_sample.lot_series = function(x, size) {
  stopifnot(exprs = {
    vek::is_int_vec_x(size)
    length(size) == 1L || length(size) == get_num_lot(x)
    all(size > 0L, na.rm = FALSE)
    all(size <= get_size(x), na.rm = FALSE)
    all(lapun(x, is.lot), na.rm = FALSE)
  })

  f = Vectorize(lot_sample, SIMPLIFY = FALSE)
  sampled = f(x, size)

  new_lot_series(
    sampled,
    method = "sampled"
  )
}

