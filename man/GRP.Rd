\name{GRP, fgroup_by}
\alias{GRP}
\alias{GRP.GRP}
\alias{GRP.default}
\alias{GRP.factor}
\alias{GRP.qG}
\alias{GRP.pseries}
\alias{GRP.pdata.frame}
\alias{GRP.grouped_df}
\alias{fgroup_by}
\alias{gby}
\alias{fgroup_vars}
\alias{fungroup}
\alias{is_GRP}
\alias{print.GRP}
\alias{plot.GRP}
\alias{GRPnames}
\alias{as_factor_GRP}
\title{Fast Grouping / \emph{collapse} Grouping Objects}
\description{
  \code{GRP} performs fast, ordered and unordered, groupings of vectors and data frames (or lists of vectors) using \code{\link{radixorderv}}. The output is a list-like object of class 'GRP' which can be printed, plotted and used as an efficient input to all of \emph{collapse}'s fast statistical and transformation functions / operators, as well as to \code{\link{collap}}, \code{\link{BY}} and \code{\link{TRA}}.

\code{fgroup_by} is similar to \code{dplyr::group_by} but faster. It creates a grouped data frame with a 'GRP' object attached - for faster dplyr-like programming with \emph{collapse}'s fast functions.

There are also several conversion methods to convert to and from 'GRP' objects. Notable among these is \code{GRP.grouped_df}, which returns a 'GRP' object from a grouped data frame created with \code{dplyr::group_by} (or \code{fgroup_by}), and the duo \code{GRP.factor} and \code{as_factor_GRP}.
}
\usage{
GRP(X, \dots)

\method{GRP}{default}(X, by = NULL, sort = TRUE, decreasing = FALSE, na.last = TRUE,
    return.groups = TRUE, return.order = FALSE, call = TRUE, \dots)

\method{GRP}{factor}(X, \dots, group.sizes = TRUE, drop = FALSE, return.groups = TRUE,
    call = TRUE)

\method{GRP}{qG}(X, \dots, group.sizes = TRUE, return.groups = TRUE, call = TRUE)

\method{GRP}{pseries}(X, effect = 1L, \dots, group.sizes = TRUE, return.groups = TRUE,
    call = TRUE)

\method{GRP}{pdata.frame}(X, effect = 1L, \dots, group.sizes = TRUE, return.groups = TRUE,
    call = TRUE)

\method{GRP}{grouped_df}(X, \dots, return.groups = TRUE, call = TRUE)

# Identify, get group names, and convert GRP object to factor
is_GRP(x)
GRPnames(x, force.char = TRUE)
as_factor_GRP(x, ordered = FALSE)

# Fast, class-agnostic version of dplyr::group_by for use with fast functions, see details
fgroup_by(X, \dots, sort = TRUE, decreasing = FALSE, na.last = TRUE,
          return.order = FALSE)
      gby(X, \dots, sort = TRUE, decreasing = FALSE, na.last = TRUE,
          return.order = FALSE)

# Get grouping columns from a grouped data frame created with dplyr::group_by or fgroup_by
fgroup_vars(X, return = "data")

# Ungroup grouped data frame created with dplyr::group_by or fgroup_by
fungroup(X, \dots)

\method{print}{GRP}(x, n = 6, \dots)

\method{plot}{GRP}(x, breaks = "auto", type = "s", horizontal = FALSE, \dots)
}
\arguments{
  \item{X}{a vector, list of columns or data frame (default method), or a classed object (conversion / extractor methods).}

  \item{x}{a GRP object.}

  \item{by}{if \code{X} is a data frame or list, \code{by} can indicate columns to use for the grouping (by default all columns are used). Columns must be passed using a vector of column names, indices, or using a one-sided formula i.e. \code{~ col1 + col2}.}

  \item{sort}{logical. This argument only affects character vectors / columns passed. If \code{FALSE}, these are not ordered but simply grouped in the order of first appearance of unique elements. This provides a slight performance gain if only grouping but not alphabetic ordering is required (argument passed to \code{\link{radixorderv}}).}

  \item{ordered}{logical. \code{TRUE} adds a class 'ordered' i.e. generates an ordered factor.}

  \item{decreasing}{logical. Should the sort order be increasing or decreasing? Can be a vector of length equal to the number of arguments in \code{X} / \code{by} (argument passed to \code{\link{radixorderv}}).}

  \item{na.last}{logical. If missing values are encountered in grouping vector/columns, assign them to the last group (argument passed to \code{\link{radixorderv}}).}

  \item{return.groups}{logical. Include the unique groups in the created GRP object.}

  \item{return.order}{logical. Include the output from \code{\link{radixorderv}} in the created GRP object.}

  \item{group.sizes}{logical. \code{TRUE} tabulates factor levels using \code{\link{tabulate}} to create a vector of group sizes; \code{FALSE} leaves that slot empty when converting from factors.}

  \item{drop}{logical. \code{TRUE} efficiently drops unused factor levels beforehand using \code{\link{fdroplevels}}.}

  \item{call}{logical. \code{TRUE} calls \code{\link{match.call}} and saves it in the final slot of the GRP object.}

  \item{force.char}{logical. Always output group names as character vector, even if a single numeric vector was passed to \code{GRP.default}.}

 \item{effect}{\emph{plm} methods: Select which panel identifier should be used as grouping variable. 1L takes the first variable in the \code{plm::index}, 2L the second etc., identifiers can also be passed as a character string. More than one variable can be supplied. }

   \item{return}{an integer or string specifying what \code{fgroup_vars} should return. The options are:
      \tabular{lllll}{\emph{ Int. }   \tab\tab \emph{ String }   \tab\tab \emph{ Description }  \cr
                 1 \tab\tab "data"   \tab\tab full grouping columns (default) \cr
                 2 \tab\tab "unique"   \tab\tab unique rows of grouping columns \cr
                 3 \tab\tab "names" \tab\tab names of grouping columns \cr
                 4 \tab\tab "indices"   \tab\tab integer indices of grouping columns \cr
                 5 \tab\tab "named_indices"  \tab\tab named integer indices of grouping columns \cr
                 6 \tab\tab "logical"   \tab\tab logical selection vector of grouping columns \cr
                 7 \tab\tab "named_logical"     \tab\tab named logical selection vector of grouping columns \cr
      }
  }

 %  \item{untibble}{logical. \code{TRUE} also removes classes \code{c("tbl_df", "tbl")} from \code{X}. \code{fgroup_by} attaches an attribute \code{"was.tibble"} indicating if \code{X} was a tibble prior to grouping. The argument thus defaults to \code{TRUE} if this attribute is attached and equal to \code{FALSE}, so that \code{identical(X, X |> fgroup_by(...) |> fungroup())}, regardless of the nature of \code{X}. }

  \item{n}{integer. Number of groups to print out.}

  \item{breaks}{integer. Number of breaks in the histogram of group-sizes.}

  \item{type}{linetype for plot.}

  \item{horizontal}{logical. \code{TRUE} arranges plots next to each other, instead of above each other.}

  \item{\dots}{for \code{fgroup_by}: unquoted comma-separated column names of grouping columns. Otherwise: arguments to be passed to or from other methods.}
}
\details{
\code{GRP} is a central function in the \emph{collapse} package because it provides the key inputs to facilitate easy and efficient groupwise-programming at the \code{C/C++} level: Information about (1) the number of groups (2) an integer group-id indicating which values / rows belong to which group and (3) information about the size of each group. Provided with these informations, \emph{collapse}'s \link[=A1-fast-statistical-functions]{Fast Statistical Functions} pre-allocate intermediate and result vectors of the right sizes and (in most cases) perform grouped statistical computations in a single pass through the data.

The sorting and ordering functionality for \code{GRP} only affects (2), that is groups receive different integer-id's depending on whether the groups are sorted \code{sort = TRUE}, and in which order (argument \code{decreasing}). This in-turn changes the order of values/rows in the output of \emph{collapse} functions. \emph{Note} that \code{sort = FALSE} is only effective on character vectors, numeric grouping vectors will always produce ordered groupings.

%This in-turn changes the order of values/rows in the output of \emph{collapse} functions (the row/value corresponding to group 1 always comes out on top). The default setting with \code{sort = TRUE} and \code{decreasing = FALSE} results in groups being sorted in ascending order. This is equivalent to performing grouped operations in \emph{data.table} using \code{keyby}, whereas \code{sort = FALSE} is equivalent to \emph{data.table} grouping with \code{by}, however this only works if the \code{by} columns are character, numeric grouping columns are always ordered.

Next to \code{GRP}, there is the function \code{fgroup_by} as a significantly faster alternative to \code{dplyr::group_by}. It creates a grouped data frame by attaching a 'GRP' object to a data frame. \emph{collapse} functions with a grouped_df method applied to that data frame will yield grouped computations. Note that \code{fgroup_by} can only be used in combination with \emph{collapse} functions, not with \emph{dplyr} verbs such as \code{summarize} or \code{mutate}. The converse is not true, you can group data with \code{dplyr::group_by} and then apply \emph{collapse} functions. Note also the \code{fgroup_by} is class-agnostic, i.e. the classes of the data frame or list passed are preserved, and all standard methods (like subsetting with \code{`[`} or \code{print} methods) apply to the grouped object. Apart from the class 'grouped_df' which is added behind any classes the object might inherit (apart from 'data.frame'), a class 'GRP_df' is added in front. This class responds to \code{print} method and subset (\code{`[`}) methods. Both first call the corresponding method for the object and then print / attach the grouping information. \code{print.GRP_df} prints below the object print one line indicating the grouping variables, followed, in square brackets, by the following information: \code{[number of groups | average group size (standard-deviation of group sizes)]}.

\code{GRP} is an S3 generic function with one default method supporting vector and list input and several conversion methods:

The conversion of factors to 'GRP' objects by \code{GRP.factor} involves obtaining the number of groups calling \code{ng <- fnlevels(f)} and then computing the count of each level using \code{\link[=tabulate]{tabulate(f, ng)}}. The integer group-id (2) is already given by the factor itself after removing the levels and class attributes and replacing any missing values with \code{ng + 1L}. The levels are put in a list and moved to position (4) in the 'GRP' object, which is reserved for the unique groups. Going from factor to 'GRP' object thus only requires a tabulation of the levels, whereas creating a factor from a 'GRP' object using \code{as_factor_GRP} does not involve any computations, but may involve interactions if multiple grouping columns were used (which are then interacted to produce unique factor levels) or \code{\link{as.character}} conversions if the grouping column(s) were numeric (which are potentially expensive).

The method \code{GRP.grouped_df} takes the 'groups' attribute from a grouped data frame and converts it to a 'GRP' object. If the grouped data frame was generated using \code{fgroup_by}, all work is done already. If it was created using \code{dplyr::group_by}, a C routine is called to efficiently convert the grouping object.

\emph{Note}: For faster factor generation and a factor-light class 'qG' which avoids the coercion of factor levels to character also see \code{\link{qF}} and \code{\link{qG}}.
}
\value{
  A list-like object of class `GRP' containing information about the number of groups, the observations (rows) belonging to each group, the size of each group, the unique group names / definitions, whether the groups are ordered or not and (optionally) the ordering vector used to perform the ordering. The object is structured as follows:
  \tabular{lllllll}{\emph{ List-index }  \tab\tab \emph{ Element-name }   \tab\tab \emph{ Content type } \tab\tab \emph{ Content description} \cr

                 [[1]] \tab\tab N.groups   \tab\tab \code{integer(1)} \tab\tab Number of Groups \cr

                 [[2]] \tab\tab group.id \tab\tab \code{integer(NROW(X))} \tab\tab An integer group-identifier \cr

                 [[3]] \tab\tab group.sizes    \tab\tab \code{integer(N.groups)} \tab\tab Vector of group sizes \cr

                 [[4]] \tab\tab groups    \tab\tab \code{unique(X)} or \code{NULL} \tab\tab Unique groups (same format as input, sorted if \code{sort = TRUE}), or \code{NULL} if \code{return.groups = FALSE} \cr

                 [[5]] \tab\tab group.vars   \tab\tab \code{character} \tab\tab The names of the grouping variables \cr
                 [[6]] \tab\tab ordered   \tab\tab \code{logical(2)} \tab\tab \code{[1]- TRUE} if \code{sort = TRUE}, \code{[2]- TRUE} if \code{X} already sorted \cr

                 [[7]] \tab\tab order     \tab\tab \code{integer(NROW(X))} or \code{NULL} \tab\tab Ordering vector from \code{radixorderv} or \code{NULL} if \code{return.order = FALSE} (the default) \cr

                 [[8]] \tab\tab call \tab\tab \code{call()} or \code{NULL} \tab\tab The \code{GRP()} call, obtained from \code{match.call()}, or \code{NULL} if \code{call = FALSE}
                 }
}
\seealso{
\code{\link{radixorder}}, \code{\link{qF}}, \link[=A2-fast-grouping-ordering]{Fast Grouping and Ordering}, \link[=collapse-documentation]{Collapse Overview}
}
\examples{
## default method
GRP(mtcars$cyl)
GRP(mtcars, ~ cyl + vs + am)      # Or GRP(mtcars, c("cyl","vs","am")) or GRP(mtcars, c(2,8:9))
g <- GRP(mtcars, ~ cyl + vs + am) # Saving the object
print(g)                          # Printing it
plot(g)                           # Plotting it
GRPnames(g)                       # Retain group names
fsum(mtcars, g)                   # Compute the sum of mtcars, grouped by variables cyl, vs and am

## Convert factor to GRP object and vice-versa
GRP(iris$Species)
as_factor_GRP(g)
\donttest{ % No code relying on suggested package
## dplyr integration
library(dplyr)
mtcars |> group_by(cyl,vs,am) |> GRP()    # Get GRP object from a dplyr grouped tibble
mtcars |> group_by(cyl,vs,am) |> fmean()  # Grouped mean using dplyr grouping
mtcars |> fgroup_by(cyl,vs,am) |> fmean() # Faster alternative with collapse grouping

mtcars |> fgroup_by(cyl,vs,am)            # Print method for grouped data frame
}

}
\keyword{manip}
