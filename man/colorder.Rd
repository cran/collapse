\name{colorder}
\alias{colorder}
\alias{colorderv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fast Reordering of Data Frame Columns
}
\description{
Efficiently reorder columns in a data frame (no copies). To do this by reference see also \code{data.table::setcolorder}.
}
\usage{
colorder(X, \dots, pos = c("front", "end", "exchange"))

colorderv(X, neworder = radixorder(names(X)), pos = c("front","end","exchange"),
          regex = FALSE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{a data frame or list.}
  \item{\dots}{for \code{colorder}: Column names of \code{X} in the new order (can also use sequences i.e. \code{col1:coln, colk, \dots}). For \code{colorderv}: Further arguments to \code{\link{grep}} if \code{regex = TRUE}.}
  \item{neworder}{a vector of column names, positive indices, a suitable logical vector, a function such as \code{is.numeric}, or a vector of regular expressions matching column names (if \code{regex = TRUE}). }
  \item{pos}{integer or character. Different options regarding column arrangement if \code{...length() < ncol(X)} (or \code{length(neworder) < ncol(X)}).
        \tabular{lllll}{\emph{ Int. }   \tab\tab \emph{ String }   \tab\tab \emph{ Description }  \cr
                 1 \tab\tab "front"   \tab\tab move specified columns to the front of \code{X} (the default). \cr
                 2 \tab\tab "end" \tab\tab move specified columns to the end of \code{X}. \cr
                 3 \tab\tab "exchange"   \tab\tab just exchange the positions of selected columns in \code{X}, other columns remain in the same position. \cr
  }
}
\item{regex}{logical. \code{TRUE} will do regular expression search on the column names of \code{X} using a (vector of) regular expression(s) passed to \code{neworder}. Matching is done using \code{\link{grep}}. \emph{Note} that multiple regular expressions will be matched in the order they are passed, and \code{\link{funique}} will be applied to the resulting set of indices. }
}
\value{
\code{X} with columns re-ordered (no deep copy).
}


%% ~Make other sections like Warning with \section{Warning }{\dots.} ~

\seealso{
\code{\link{roworder}}, \link[=A3-fast-data-manipulation]{Data Frame Manipulation}, \link[=collapse-documentation]{Collapse Overview}
}
\examples{
head(colorder(mtcars, vs, cyl:hp, am))
head(colorder(mtcars, vs, cyl:hp, am, pos = "end"))
head(colorder(mtcars, vs, cyl, pos = "exchange"))

## Same in standard evaluation
head(colorderv(mtcars, c(8, 2:4, 9)))
head(colorderv(mtcars, c(8, 2:4, 9), pos = "end"))
head(colorderv(mtcars, c(8, 2), pos = "exchange"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ manip }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
