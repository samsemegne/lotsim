
#' @export
get_true_prop = function(x)   UseMethod("get_true_prop")
#' @export
get_true_count = function(x)  UseMethod("get_true_count")
#' @export
get_prop = function(x)        UseMethod("get_prop")
#' @export
get_count = function(x)       UseMethod("get_count")
#' @export
get_size = function(x)        UseMethod("get_size")
#' @export
get_parent_size = function(x) UseMethod("get_parent_size")
#' @export
get_num_lot = function(x)     UseMethod("get_num_lot")
#' @export
lot_sample = function(x, ...) UseMethod("lot_sample")


as_df = function(x, ...) {
  data.frame(
    count = get_count(x),
    prop = get_prop(x),
    true_count = get_true_count(x),
    true_prop = get_true_prop(x),
    size = get_size(x),
    parent_size = get_parent_size(x),
    row.names = NULL,
    check.rows = FALSE,
    check.names = FALSE,
    fix.empty.names = FALSE,
    stringsAsFactors = FALSE
  )
}


lot_add_many_op = function(...) {
  dots = list(...)
  tmp = dots[[1L]]
  for (i in 2:(length(dots))) {
    tmp = lot_add_op(tmp, dots[[i]])
  }

  tmp
}


lot_add_op = function(a, b) {
  stopifnot(exprs = {
    is.lot(a) || is.lot_series(a) || is.lot_object_wrapper(a)
    is.lot(b) || is.lot_series(b) || is.lot_object_wrapper(b)
  })

  if (!is.lot_object_wrapper(a) && !is.lot_object_wrapper(b))
    return(new_lot_object_wrapper(list(a, b)))

  else if (is.lot_object_wrapper(a) && is.lot_object_wrapper(b)) {
    a[(length(a) + 1):(length(a) + length(b))] = b
    return(a)
  }

  if (is.lot_object_wrapper(a)) {
    a[[length(a) + 1L]] = b
    return(a)
  }
  else if (is.lot_object_wrapper(b)) {
    b_attr = attributes(b)
    b_attr$class = list(NULL) # unset class
    tmp = do.call(new_lot_object_wrapper, c(objects = a, b_attr))
    tmp[[length(tmp) + 1L]] = b
    return(b)
  }

  stop("invalid state")
}


plot_lot_obj = function(object, ...) {
  stopifnot(exprs = {
    is.lot_series(object) || is.lot_object_wrapper(object)
  })

  args = list(...)

  df = as.data.frame(object)

  flip = args$flip %||% TRUE
  stopifnot(vek::is_lgl_vec_x1(flip))

  add = args$add %||% FALSE
  stopifnot(vek::is_lgl_vec_x1(add))

  time = 1:nrow(df)
  x = if (flip) df$prop else time
  y = if (flip) time else df$prop
  true_x = if (flip) df$true_prop else time
  true_y = if (flip) time else df$true_prop

  what = args$what %||% "prop"
  stopifnot(exprs = {
    vek::is_chr_vec_xb1(what)
    what %in% c("prop", "true_prop")
  })

  if (!add) {
    xlab = if (flip) what else "t"
    ylab = if (flip) "t" else what
    xlab = args$xlab %||% xlab
    ylab = args$ylab %||% ylab

    xlim_upper = max(c(df$prop, df$true_prop), na.rm = TRUE) + .1
    xlim_upper = min(xlim_upper, 1L)
    xlim = c(0L, xlim_upper)
    temp = xlim
    xlim = if (flip) xlim else NULL
    ylim = if (flip) NULL else temp
    rm(temp)

    ylim = args$ylim %||% ylim
    if ("xlim" %in% names(args))
      xlim = args$xlim

    type = args$type %||% "l"
  }

  col = args$col %||% "black"
  lwd = args$lwd %||% 1L
  lty = args$lty %||% 1L

  if (!add) {
    if (what == "prop") {
      plot(x, y, type = type, xlab = xlab, ylab = ylab, xlim = xlim,
           ylim = ylim, col = col, lwd = lwd, lty = lty)
    }
    else if (what == "true_prop") {
      plot(true_x, true_y, type = type, xlab = xlab, ylab = ylab, xlim = xlim,
           ylim = ylim, col = col, lwd = lwd, lty = lty)
    }
    else {
      stop("invalid state")
    }
  } else {
    if (what == "prop")
      lines(x, y, col = col, lwd = lwd, lty = lty)
    else if (what == "true_prop")
      lines(true_x, true_y, col = col, lwd = lwd, lty = lty)
    else
      stop("invalid state")
  }
}
