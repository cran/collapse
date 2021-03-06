
# For principle innovations of this code see flag.R and flag.cpp

# Helper functions
checkld <- function(...) {
  if(any(names(list(...)) == "logdiff")) {
    warning("argument 'logdiff' was renamed to 'log'")
    TRUE
  } else FALSE
}
baselog <- base::log


fdiff <- function(x, n = 1, diff = 1, ...) UseMethod("fdiff") # , x

fdiff.default <- function(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, log = FALSE, rho = 1, stubs = TRUE, ...) {
  if(is.matrix(x) && !inherits(x, "matrix")) return(UseMethod("fdiff", unclass(x)))
  if(!missing(...)) if(checkld(...)) log <- list(...)[["logdiff"]] else unused_arg_action(match.call(), ...)
  if(log) x <- baselog(x)
  if(is.null(g)) return(.Call(Cpp_fdiffgrowth,x,n,diff,fill,0L,0L,NULL,G_t(t,0L),1L+log,rho,stubs,1))
  if(is.atomic(g)) {
    if(is.nmfactor(g)) nl <- fnlevels(g) else {
      g <- qG(g, na.exclude = FALSE)
      nl <- attr(g, "N.groups")
    }
    return(.Call(Cpp_fdiffgrowth,x,n,diff,fill,nl,g,NULL,G_t(t,2L),1L+log,rho,stubs,1))
  }
  if(!is_GRP(g)) g <- GRP.default(g, return.groups = FALSE, call = FALSE)
  .Call(Cpp_fdiffgrowth,x,n,diff,fill,g[[1L]],g[[2L]],g[[3L]],G_t(t,2L),1L+log,rho,stubs,1)
}

fdiff.pseries <- function(x, n = 1, diff = 1, fill = NA, log = FALSE, rho = 1, stubs = TRUE, ...) {
  if(!missing(...)) if(checkld(...)) log <- list(...)[["logdiff"]] else unused_arg_action(match.call(), ...)
  if(log) x <- baselog(x)
  index <- unclass(attr(x, "index"))
  if(length(index) > 2L) index <- list(finteraction(index[-length(index)]), index[[length(index)]])
  if(is.matrix(x))
    .Call(Cpp_fdiffgrowthm,x,n,diff,fill,fnlevels(index[[1L]]),index[[1L]],NULL,index[[2L]],1L+log,rho,stubs,1) else
      .Call(Cpp_fdiffgrowth,x,n,diff,fill,fnlevels(index[[1L]]),index[[1L]],NULL,index[[2L]],1L+log,rho,stubs,1)
}

fdiff.matrix <- function(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, log = FALSE, rho = 1, stubs = length(n) + length(diff) > 2L, ...) {
  if(!missing(...)) if(checkld(...)) log <- list(...)[["logdiff"]] else unused_arg_action(match.call(), ...)
  if(log) x <- baselog(x)
  if(is.null(g)) return(.Call(Cpp_fdiffgrowthm,x,n,diff,fill,0L,0L,NULL,G_t(t,0L),1L+log,rho,stubs,1))
  if(is.atomic(g)) {
    if(is.nmfactor(g)) nl <- fnlevels(g) else {
      g <- qG(g, na.exclude = FALSE)
      nl <- attr(g, "N.groups")
    }
    return(.Call(Cpp_fdiffgrowthm,x,n,diff,fill,nl,g,NULL,G_t(t,2L),1L+log,rho,stubs,1))
  }
  if(!is_GRP(g)) g <- GRP.default(g, return.groups = FALSE, call = FALSE)
  .Call(Cpp_fdiffgrowthm,x,n,diff,fill,g[[1L]],g[[2L]],g[[3L]],G_t(t,2L),1L+log,rho,stubs,1)
}

fdiff.grouped_df <- function(x, n = 1, diff = 1, t = NULL, fill = NA, log = FALSE, rho = 1, stubs = length(n) + length(diff) > 2L, keep.ids = TRUE, ...) {
  if(!missing(...)) if(checkld(...)) log <- list(...)[["logdiff"]] else unused_arg_action(match.call(), ...)
  g <- GRP.grouped_df(x, call = FALSE)
  tsym <- all.vars(substitute(t))
  nam <- attr(x, "names")
  gn <- which(nam %in% g[[5L]])
  if(length(tsym) && !anyNA(tn <- match(tsym, nam))) {
    if(length(tn) == 1L) {
      if(any(gn == tn)) stop("timevar coincides with grouping variables!")
      t <- .subset2(x, tn)
    } else {
      if(any(gn %in% tn)) stop("timevar coincides with grouping variables!")
      t <- .subset(x, tn)
    }
    gn <- c(gn, tn)
  }
  cld <- function(x) if(log) fdapply(x, baselog) else x
  if(length(gn)) {
    ax <- attributes(x)
    res <- .Call(Cpp_fdiffgrowthl,cld(.subset(x, -gn)),n,diff,fill,g[[1L]],g[[2L]],g[[3L]],G_t(t,2L),1L+log,rho,stubs,1)
    if(keep.ids) res <- c(.subset(x, gn), res)
    ax[["names"]] <- names(res)  # Works for multiple lags / differences !
    return(setAttributes(res, ax))
  }
  .Call(Cpp_fdiffgrowthl,cld(x),n,diff,fill,g[[1L]],g[[2L]],g[[3L]],G_t(t,2L),1L+log,rho,stubs,1)
}

fdiff.data.frame <- function(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, log = FALSE, rho = 1, stubs = length(n) + length(diff) > 2L, ...) {
  if(!missing(...)) if(checkld(...)) log <- list(...)[["logdiff"]] else unused_arg_action(match.call(), ...)
  if(log) x <- fdapply(x, baselog)
  if(is.null(g)) return(.Call(Cpp_fdiffgrowthl,x,n,diff,fill,0L,0L,NULL,G_t(t,0L),1L+log,rho,stubs,1))
  if(is.atomic(g)) {
    if(is.nmfactor(g)) nl <- fnlevels(g) else {
      g <- qG(g, na.exclude = FALSE)
      nl <- attr(g, "N.groups")
    }
    return(.Call(Cpp_fdiffgrowthl,x,n,diff,fill,nl,g,NULL,G_t(t,2L),1L+log,rho,stubs,1))
  }
  if(!is_GRP(g)) g <- GRP.default(g, return.groups = FALSE, call = FALSE)
  .Call(Cpp_fdiffgrowthl,x,n,diff,fill,g[[1L]],g[[2L]],g[[3L]],G_t(t,2L),1L+log,rho,stubs,1)
}

fdiff.list <- function(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, log = FALSE, rho = 1, stubs = length(n) + length(diff) > 2L, ...)
  fdiff.data.frame(x, n, diff, g, t, fill, log, rho, stubs, ...)

fdiff.pdata.frame <- function(x, n = 1, diff = 1, fill = NA, log = FALSE, rho = 1, stubs = length(n) + length(diff) > 2L, ...) {
  if(!missing(...)) if(checkld(...)) log <- list(...)[["logdiff"]] else unused_arg_action(match.call(), ...)
  if(log) x <- fdapply(x, baselog)
  index <- unclass(attr(x, "index"))
  if(length(index) > 2L) index <- list(finteraction(index[-length(index)]), index[[length(index)]])
  .Call(Cpp_fdiffgrowthl,x,n,diff,fill,fnlevels(index[[1L]]),index[[1L]],NULL,index[[2L]],1L+log,rho,stubs,1)
}




fgrowth <- function(x, n = 1, diff = 1, ...) UseMethod("fgrowth") # , x

fgrowth.default <- function(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, logdiff = FALSE, scale = 100, power = 1, stubs = TRUE, ...) {
  if(is.matrix(x) && !inherits(x, "matrix")) return(UseMethod("fgrowth", unclass(x)))
  if(!missing(...)) unused_arg_action(match.call(), ...)
  if(logdiff) x <- if(scale == 1) baselog(x) else scale * baselog(x)
  if(is.null(g)) return(.Call(Cpp_fdiffgrowth,x,n,diff,fill,0L,0L,NULL,G_t(t,0L),4L-logdiff,scale,stubs,power))
  if(is.atomic(g)) {
    if(is.nmfactor(g)) nl <- fnlevels(g) else {
      g <- qG(g, na.exclude = FALSE)
      nl <- attr(g, "N.groups")
    }
    return(.Call(Cpp_fdiffgrowth,x,n,diff,fill,nl,g,NULL,G_t(t,3L),4L-logdiff,scale,stubs,power))
  }
  if(!is_GRP(g)) g <- GRP.default(g, return.groups = FALSE, call = FALSE)
  .Call(Cpp_fdiffgrowth,x,n,diff,fill,g[[1L]],g[[2L]],g[[3L]],G_t(t,3L),4L-logdiff,scale,stubs,power)
}

fgrowth.pseries <- function(x, n = 1, diff = 1, fill = NA, logdiff = FALSE, scale = 100, power = 1, stubs = TRUE, ...) {
  if(!missing(...)) unused_arg_action(match.call(), ...)
  if(logdiff) x <- if(scale == 1) baselog(x) else scale * baselog(x)
  index <- unclass(attr(x, "index"))
  if(length(index) > 2L) index <- list(finteraction(index[-length(index)]), index[[length(index)]])
  if(is.matrix(x))
    .Call(Cpp_fdiffgrowthm,x,n,diff,fill,fnlevels(index[[1L]]),index[[1L]],NULL,index[[2L]],4L-logdiff,scale,stubs,power) else
      .Call(Cpp_fdiffgrowth,x,n,diff,fill,fnlevels(index[[1L]]),index[[1L]],NULL,index[[2L]],4L-logdiff,scale,stubs,power)
}

fgrowth.matrix <- function(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, logdiff = FALSE, scale = 100, power = 1, stubs = length(n) + length(diff) > 2L, ...) {
  if(!missing(...)) unused_arg_action(match.call(), ...)
  if(logdiff) x <- if(scale == 1) baselog(x) else scale * baselog(x)
  if(is.null(g)) return(.Call(Cpp_fdiffgrowthm,x,n,diff,fill,0L,0L,NULL,G_t(t,0L),4L-logdiff,scale,stubs,power))
  if(is.atomic(g)) {
    if(is.nmfactor(g)) nl <- fnlevels(g) else {
      g <- qG(g, na.exclude = FALSE)
      nl <- attr(g, "N.groups")
    }
    return(.Call(Cpp_fdiffgrowthm,x,n,diff,fill,nl,g,NULL,G_t(t,3L),4L-logdiff,scale,stubs,power))
  }
  if(!is_GRP(g)) g <- GRP.default(g, return.groups = FALSE, call = FALSE)
  .Call(Cpp_fdiffgrowthm,x,n,diff,fill,g[[1L]],g[[2L]],g[[3L]],G_t(t,3L),4L-logdiff,scale,stubs,power)
}

fgrowth.grouped_df <- function(x, n = 1, diff = 1, t = NULL, fill = NA, logdiff = FALSE, scale = 100, power = 1, stubs = length(n) + length(diff) > 2L, keep.ids = TRUE, ...) {
  if(!missing(...)) unused_arg_action(match.call(), ...)
  g <- GRP.grouped_df(x, call = FALSE)
  tsym <- all.vars(substitute(t))
  nam <- attr(x, "names")
  gn <- which(nam %in% g[[5L]])
  if(length(tsym) && !anyNA(tn <- match(tsym, nam))) {
    if(length(tn) == 1L) {
      if(any(gn == tn)) stop("timevar coincides with grouping variables!")
      t <- .subset2(x, tn)
    } else {
      if(any(gn %in% tn)) stop("timevar coincides with grouping variables!")
      t <- .subset(x, tn)
    }
    gn <- c(gn, tn)
  }
  cld <- function(x) if(!logdiff) x else if(scale != 1) fdapply(x, function(y) scale * baselog(y)) else fdapply(x, baselog)
  if(length(gn)) {
    ax <- attributes(x)
    res <- .Call(Cpp_fdiffgrowthl,cld(.subset(x, -gn)),n,diff,fill,g[[1L]],g[[2L]],g[[3L]],G_t(t,3L),4L-logdiff,scale,stubs,power)
    if(keep.ids) res <- c(.subset(x, gn), res)
    ax[["names"]] <- names(res)  # Works for multiple lags / differences !
    return(setAttributes(res, ax))
  }
  .Call(Cpp_fdiffgrowthl,cld(x),n,diff,fill,g[[1L]],g[[2L]],g[[3L]],G_t(t,3L),4L-logdiff,scale,stubs,power)
}

fgrowth.data.frame <- function(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, logdiff = FALSE, scale = 100, power = 1, stubs = length(n) + length(diff) > 2L, ...) {
  if(!missing(...)) unused_arg_action(match.call(), ...)
  if(logdiff) x <- if(scale == 1) fdapply(x, baselog) else fdapply(x, function(y) scale * baselog(y))
  if(is.null(g)) return(.Call(Cpp_fdiffgrowthl,x,n,diff,fill,0L,0L,NULL,G_t(t,0L),4L-logdiff,scale,stubs,power))
  if(is.atomic(g)) {
    if(is.nmfactor(g)) nl <- fnlevels(g) else {
      g <- qG(g, na.exclude = FALSE)
      nl <- attr(g, "N.groups")
    }
    return(.Call(Cpp_fdiffgrowthl,x,n,diff,fill,nl,g,NULL,G_t(t,3L),4L-logdiff,scale,stubs,power))
  }
  if(!is_GRP(g)) g <- GRP.default(g, return.groups = FALSE, call = FALSE)
  .Call(Cpp_fdiffgrowthl,x,n,diff,fill,g[[1L]],g[[2L]],g[[3L]],G_t(t,3L),4L-logdiff,scale,stubs,power)
}

fgrowth.list <- function(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, logdiff = FALSE, scale = 100, power = 1, stubs = length(n) + length(diff) > 2L, ...)
  fgrowth.data.frame(x, n, diff, g, t, fill, logdiff, scale, power, stubs, ...)

fgrowth.pdata.frame <- function(x, n = 1, diff = 1, fill = NA, logdiff = FALSE, scale = 100, power = 1, stubs = length(n) + length(diff) > 2L, ...) {
  if(!missing(...)) unused_arg_action(match.call(), ...)
  if(logdiff) x <- if(scale == 1) fdapply(x, baselog) else fdapply(x, function(y) scale * baselog(y))
  index <- unclass(attr(x, "index"))
  if(length(index) > 2L) index <- list(finteraction(index[-length(index)]), index[[length(index)]])
  .Call(Cpp_fdiffgrowthl,x,n,diff,fill,fnlevels(index[[1L]]),index[[1L]],NULL,index[[2L]],4L-logdiff,scale,stubs,power)
}

# Operator data frame methods templates

DG_data_frame_template <- function(x, n = 1, diff = 1, by = NULL, t = NULL, cols = is.numeric,
                         fill = NA, return = 1L, rho = 1, stubs = TRUE, keep.ids = TRUE, message = 2L, power = 1, ...) {

  if(!missing(...)) unused_arg_action(match.call(), ...)

  cld <- function(y) switch(return, y, fdapply(y, baselog), if(rho == 1) fdapply(y, baselog) else fdapply(y, function(k) rho * baselog(k)), y)

  if(is.call(by) || is.call(t)) {
    ax <- attributes(x)
    class(x) <- NULL
    nam <- names(x)

    if(is.call(by)) {
      if(length(by) == 3L) {
        cols <- ckmatch(all.vars(by[[2L]]), nam)
        gn <- ckmatch(all.vars(by[[3L]]), nam)
      } else {
        gn <- ckmatch(all.vars(by), nam)
        cols <- if(is.null(cols)) seq_along(x)[-gn] else cols2int(cols, x, nam)
      }
      by <- if(length(gn) == 1L) at2GRP(x[[gn]]) else GRP.default(x, gn, return.groups = FALSE, call = FALSE)
      if(!keep.ids) gn <- NULL
    } else {
      gn <- NULL
      if(length(cols)) cols <- cols2int(cols, x, nam)
      if(!is_GRP(by)) by <- if(is.null(by)) list(0L, 0L, NULL) else if(is.atomic(by)) # Necessary if by is passed externally !
        at2GRP(by) else GRP.default(by, return.groups = FALSE, call = FALSE)
    }

    if(is.call(t)) {
      tn <- ckmatch(all.vars(t), nam)
      t1 <- length(tn) == 1L
      t <- if(t1) x[[tn]] else GRP.default(x[tn], return.groups = FALSE, call = FALSE)[[2L]]
      cols <- if(is.null(cols)) seq_along(x)[-tn] else if(t1) cols[cols != tn] else fsetdiff(cols, tn)
      if(keep.ids) gn <- c(gn, tn)
    }

    res <- if(length(gn))
      c(x[gn], .Call(Cpp_fdiffgrowthl,cld(x[cols]),n,diff,fill,by[[1L]],by[[2L]],by[[3L]],G_t(t,message),return,rho,stubs,power)) else
        .Call(Cpp_fdiffgrowthl,cld(x[cols]),n,diff,fill,by[[1L]],by[[2L]],by[[3L]],G_t(t,message),return,rho,stubs,power)
    ax[["names"]] <- names(res)
    return(setAttributes(res, ax))
  } else if(length(cols)) { # Needs to be done like this, otherwise list-subsetting drops attributes !
    ax <- attributes(x)
    class(x) <- NULL
    x <- x[cols2int(cols, x, names(x), FALSE)]
    ax[["names"]] <- names(x)
    setattributes(x, ax)
  }

  if(is.null(by)) return(.Call(Cpp_fdiffgrowthl,cld(x),n,diff,fill,0L,0L,NULL,G_t(t,0L),return,rho,stubs,power))
  if(is.atomic(by)) {
    if(is.nmfactor(by)) nl <- fnlevels(by) else {
      by <- qG(by, na.exclude = FALSE)
      nl <- attr(by, "N.groups")
    }
    return(.Call(Cpp_fdiffgrowthl,cld(x),n,diff,fill,nl,by,NULL,G_t(t,message),return,rho,stubs,power))
  }
  if(!is_GRP(by)) by <- GRP.default(by, return.groups = FALSE, call = FALSE)
  .Call(Cpp_fdiffgrowthl,cld(x),n,diff,fill,by[[1L]],by[[2L]],by[[3L]],G_t(t,message),return,rho,stubs,power)
}

DG_pdata_frame_template <- function(x, n = 1, diff = 1, cols = is.numeric, fill = NA, return = 1L, rho = 1, stubs = TRUE,
                          keep.ids = TRUE, power = 1, ...) {

  if(!missing(...)) unused_arg_action(match.call(), ...)
  ax <- attributes(x)
  nam <- ax[["names"]]
  index <- unclass(ax[["index"]])

  if(keep.ids) {
    gn <- which(nam %in% names(index))
    if(length(gn) && is.null(cols)) cols <- seq_along(unclass(x))[-gn]
  } else gn <- NULL

  if(length(index) > 2L) index <- list(finteraction(index[-length(index)]), index[[length(index)]])

  cld <- function(y) switch(return, y, fdapply(y, baselog), if(rho == 1) fdapply(y, baselog) else fdapply(y, function(k) rho * baselog(k)), y)

  if(length(cols)) cols <- cols2int(cols, x, nam, FALSE)

  if(length(gn) && length(cols)) {
    class(x) <- NULL # Works for multiple lags !
    res <- c(x[gn], .Call(Cpp_fdiffgrowthl,cld(x[cols]),n,diff,fill,fnlevels(index[[1L]]),index[[1L]],NULL,index[[2L]],return,rho,stubs,power))
    ax[["names"]] <- names(res)
    return(setAttributes(res, ax))
  } else if(!length(gn)) # could speed up ?
    return(.Call(Cpp_fdiffgrowthl,cld(fcolsubset(x, cols)),n,diff,fill,fnlevels(index[[1L]]),index[[1L]],NULL,index[[2L]],return,rho,stubs,power))
  .Call(Cpp_fdiffgrowthl,cld(x),n,diff,fill,fnlevels(index[[1L]]),index[[1L]],NULL,index[[2L]],return,rho,stubs,power)
}

# Difference Operator (masks stats::D)  # use xt instead of by ?

# setGeneric("D")

D <- function(x, n = 1, diff = 1, ...) UseMethod("D") # , x

D.expression <- function(x, ...) if(missing(x)) stats::D(...) else stats::D(x, ...)
D.call <- function(x, ...) if(missing(x)) stats::D(...) else stats::D(x, ...)
D.name <- function(x, ...) if(missing(x)) stats::D(...) else stats::D(x, ...)

D.default <- function(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, rho = 1, stubs = TRUE, ...) {
  if(is.matrix(x) && !inherits(x, "matrix")) return(fdiff.matrix(x, n, diff, g, t, fill, FALSE, rho, stubs, ...))
  fdiff.default(x, n, diff, g, t, fill, FALSE, rho, stubs, ...)
}

D.pseries <- function(x, n = 1, diff = 1, fill = NA, rho = 1, stubs = TRUE, ...)
  fdiff.pseries(x, n, diff, fill, FALSE, rho, stubs, ...)

# setOldClass("pseries")
# setMethod("D", signature(expr = "pseries"), D.pseries)

D.matrix <- function(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, rho = 1, stubs = TRUE, ...)
  fdiff.matrix(x, n, diff, g, t, fill, FALSE, rho, stubs, ...)

# setMethod("D", "matrix")

D.grouped_df <- function(x, n = 1, diff = 1, t = NULL, fill = NA, rho = 1, stubs = TRUE, keep.ids = TRUE, ...) {
  x <- x # because of piped calls -> "." is not in global environment ...
  eval(substitute(fdiff.grouped_df(x, n, diff, t, fill, FALSE, rho, stubs, keep.ids, ...)))
}

D.data.frame <- function(x, n = 1, diff = 1, by = NULL, t = NULL, cols = is.numeric,
                         fill = NA, rho = 1, stubs = TRUE, keep.ids = TRUE, ...)
  DG_data_frame_template(x, n, diff, by, t, cols, fill, 1L, rho, stubs, keep.ids, 2L, ...)

D.list <- D.data.frame

D.pdata.frame <- function(x, n = 1, diff = 1, cols = is.numeric, fill = NA, rho = 1, stubs = TRUE,
                          keep.ids = TRUE, ...)
  DG_pdata_frame_template(x, n, diff, cols, fill, 1L, rho, stubs, keep.ids, ...)

# Log-Difference Operator

Dlog <- function(x, n = 1, diff = 1, ...) UseMethod("Dlog") # , x

Dlog.default <- function(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, rho = 1, stubs = TRUE, ...) {
  if(is.matrix(x) && !inherits(x, "matrix")) return(fdiff.matrix(x, n, diff, g, t, fill, TRUE, rho, stubs, ...))
  fdiff.default(x, n, diff, g, t, fill, TRUE, rho, stubs, ...)
}

Dlog.pseries <- function(x, n = 1, diff = 1, fill = NA, rho = 1, stubs = TRUE, ...)
  fdiff.pseries(x, n, diff, fill, TRUE, rho, stubs, ...)

Dlog.matrix <- function(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, rho = 1, stubs = TRUE, ...)
  fdiff.matrix(x, n, diff, g, t, fill, TRUE, rho, stubs, ...)

Dlog.grouped_df <- function(x, n = 1, diff = 1, t = NULL, fill = NA, rho = 1, stubs = TRUE, keep.ids = TRUE, ...) {
  x <- x
  eval(substitute(fdiff.grouped_df(x, n, diff, t, fill, TRUE, rho, stubs, keep.ids, ...)))
}

Dlog.data.frame <- function(x, n = 1, diff = 1, by = NULL, t = NULL, cols = is.numeric,
                         fill = NA, rho = 1, stubs = TRUE, keep.ids = TRUE, ...)
  DG_data_frame_template(x, n, diff, by, t, cols, fill, 2L, rho, stubs, keep.ids, 2L, ...)

Dlog.list <- Dlog.data.frame

Dlog.pdata.frame <- function(x, n = 1, diff = 1, cols = is.numeric, fill = NA, rho = 1, stubs = TRUE,
                          keep.ids = TRUE, ...)
  DG_pdata_frame_template(x, n, diff, cols, fill, 2L, rho, stubs, keep.ids, ...)


# Growth Operator

G <- function(x, n = 1, diff = 1, ...) UseMethod("G") # , x

G.default <- function(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, logdiff = FALSE, scale = 100, power = 1, stubs = TRUE, ...) {
  if(is.matrix(x) && !inherits(x, "matrix")) return(fgrowth.matrix(x, n, diff, g, t, fill, logdiff, scale, power, stubs, ...))
  fgrowth.default(x, n, diff, g, t, fill, logdiff, scale, power, stubs, ...)
}

G.pseries <- function(x, n = 1, diff = 1, fill = NA, logdiff = FALSE, scale = 100, power = 1, stubs = TRUE, ...)
  fgrowth.pseries(x, n, diff, fill, logdiff, scale, power, stubs, ...)

G.matrix <- function(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, logdiff = FALSE, scale = 100, power = 1, stubs = TRUE, ...)
  fgrowth.matrix(x, n, diff, g, t, fill, logdiff, scale, power, stubs, ...)

G.grouped_df <- function(x, n = 1, diff = 1, t = NULL, fill = NA, logdiff = FALSE, scale = 100, power = 1, stubs = TRUE, keep.ids = TRUE, ...) {
  x <- x
  eval(substitute(fgrowth.grouped_df(x, n, diff, t, fill, logdiff, scale, power, stubs, keep.ids, ...)))
}

G.data.frame <- function(x, n = 1, diff = 1, by = NULL, t = NULL, cols = is.numeric,
                         fill = NA, logdiff = FALSE, scale = 100, power = 1, stubs = TRUE, keep.ids = TRUE, ...)
  DG_data_frame_template(x, n, diff, by, t, cols, fill, 4L-logdiff, scale, stubs, keep.ids, 3L, power, ...)

G.list <- G.data.frame

G.pdata.frame <- function(x, n = 1, diff = 1, cols = is.numeric, fill = NA, logdiff = FALSE, scale = 100, power = 1, stubs = TRUE, keep.ids = TRUE, ...)
  DG_pdata_frame_template(x, n, diff, cols, fill, 4L-logdiff, scale, stubs, keep.ids, power, ...)
