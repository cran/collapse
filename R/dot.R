
. <- function(...) {
  l <- list(...)
  if(is.null(nam <- names(l))) names(l) <- .c(...)
  else if(anyv(nam, "")) {
    wn <- whichv(nam, "")
    nam[wn] <- .c(...)[wn]
    names(l) <- nam
  }
  l
}
