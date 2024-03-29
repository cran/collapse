\name{frename}
\alias{rnm}
\alias{frename}
\alias{setrename}
\alias{relabel}
\alias{setrelabel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fast Renaming and Relabelling Objects
}
\description{
\code{frename} returns a renamed shallow-copy, \code{setrename} renames objects by reference. These functions also work with objects other than data frames that have a 'names' attribute. \code{relabel} and \code{setrelabel} do that same for labels attached to data frame columns.
}
\usage{
frename(.x, \dots, cols = NULL, .nse = TRUE)
rnm(.x, \dots, cols = NULL, .nse = TRUE)     # Shorthand for frename()

setrename(.x, \dots, cols = NULL, .nse = TRUE)

relabel(.x, \dots, cols = NULL, attrn = "label")

setrelabel(.x, \dots, cols = NULL, attrn = "label")

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{.x}{for \code{(f/set)rename}: an R object with a \code{"names"} attribute. For \code{(set)relabel}: a named list.
}
  \item{\dots}{either tagged vector expressions of the form \code{name = newname} / \code{name = newlabel} (\code{frename} also supports \code{newname = name}), a (named) vector of names/labels, or a single function (+ optional arguments to the function) applied to all names/labels (of columns/elements selected in \code{cols}).
}
  \item{cols}{If \code{\dots} is a function, select a subset of columns/elements to rename/relabel using names, indices, a logical vector or a function applied to the columns if \code{.x} is a list (e.g. \code{is.numeric}).}

  \item{.nse}{logical. \code{TRUE} allows non-standard evaluation of tagged vector expressions, allowing you to supply new names without quotes. Set to \code{FALSE} for programming or passing vectors of names.}

  \item{attrn}{character. Name of attribute to store labels or retrieve labels from.}
}
\value{
\code{.x} renamed / relabelled. \code{setrename} and \code{setrelabel} return \code{.x} invisibly.
}
\note{
Note that both \code{relabel} and \code{setrelabel} modify \code{.x} by reference. This is because labels are attached to columns themselves, making it impossible to avoid permanent modification by taking a shallow copy of the encompassing list / data.frame. On the other hand \code{frename} makes a shallow copy whereas \code{setrename} also modifies by reference.
}
\seealso{
\link[=fast-data-manipulation]{Data Frame Manipulation}, \link[=collapse-documentation]{Collapse Overview}
}
\examples{
## Using tagged expressions
head(frename(iris, Sepal.Length = SL, Sepal.Width = SW,
                   Petal.Length = PL, Petal.Width = PW))
head(frename(iris, Sepal.Length = "S L", Sepal.Width = "S W",
                   Petal.Length = "P L", Petal.Width = "P W"))

## Since v2.0.0 this is also supported
head(frename(iris, SL = Sepal.Length, SW = Sepal.Width,
                   PL = Petal.Length, PW = Petal.Width))

## Using a function
head(frename(iris, tolower))
head(frename(iris, tolower, cols = 1:2))
head(frename(iris, tolower, cols = is.numeric))
head(frename(iris, paste, "new", sep = "_", cols = 1:2))

## Using vectors of names and programming
newname = "sepal_length"
head(frename(iris, Sepal.Length = newname, .nse = FALSE))
newnames = c("sepal_length", "sepal_width")
head(frename(iris, newnames, cols = 1:2))
newnames = c(Sepal.Length = "sepal_length", Sepal.Width = "sepal_width")
head(frename(iris, newnames, .nse = FALSE))
# Since v2.0.0, this works as well
newnames = c(sepal_length = "Sepal.Length", sepal_width = "Sepal.Width")
head(frename(iris, newnames, .nse = FALSE))

## Renaming by reference
# setrename(iris, tolower)
# head(iris)
# rm(iris)
# etc...

## Relabelling (by reference)
# namlab(relabel(wlddev, PCGDP = "GDP per Capita", LIFEEX = "Life Expectancy"))
# namlab(relabel(wlddev, toupper))


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
