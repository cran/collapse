## ---- echo = FALSE, message = FALSE, warning=FALSE------------------------------------------------
library(dplyr)
library(microbenchmark)
library(collapse)
knitr::opts_chunk$set(error = FALSE, message = FALSE, warning = FALSE, 
                      comment = "#", tidy = FALSE, cache = FALSE, collapse = TRUE,
                      fig.width = 8, fig.height = 5, 
                      out.width = '100%')

# knitr::opts_chunk$set(
#   comment = "#",
#     error = FALSE,
#      tidy = FALSE,
#     cache = FALSE,
#  collapse = TRUE,
#  fig.width = 8, 
#  fig.height= 5,
#  out.width='100%'
# )

NCRAN <- identical(Sys.getenv("NCRAN"), "TRUE")

oldopts <- options(width = 100L)
set.seed(101)

## ----eval=FALSE-----------------------------------------------------------------------------------
#  FUN.grouped_df(x, [w = NULL,] TRA = NULL, [na.rm = TRUE,]
#                 use.g.names = FALSE, keep.group_vars = TRUE, [keep.w = TRUE,] ...)

## -------------------------------------------------------------------------------------------------
library(collapse)
head(GGDC10S)

# Summarize the Data: 
# descr(GGDC10S, cols = is.categorical)
# aperm(qsu(GGDC10S, ~Variable, cols = is.numeric))

## -------------------------------------------------------------------------------------------------
library(dplyr)

GGDC10S %>% fNobs                       # Number of Observations
GGDC10S %>% fNdistinct                  # Number of distinct values
GGDC10S %>% select_at(6:16) %>% fmedian # Median
GGDC10S %>% select_at(6:16) %>% fmean   # Mean
GGDC10S %>% fmode                       # Mode
GGDC10S %>% fmode(drop = FALSE)         # Keep data structure intact

## -------------------------------------------------------------------------------------------------
GGDC10S %>% 
  group_by(Variable, Country) %>%
  select_at(6:16) %>% fmean


## -------------------------------------------------------------------------------------------------
GGDC10S %>% group_by(Variable, Country) %>% attr("groups")

## -------------------------------------------------------------------------------------------------
GGDC10S %>% group_by(Variable, Country) %>% GRP %>% str

## -------------------------------------------------------------------------------------------------
GGDC10S %>% fgroup_by(Variable, Country) %>% get_vars(6:16) %>% fmedian

microbenchmark(collapse = GGDC10S %>% fgroup_by(Variable, Country) %>% get_vars(6:16) %>% fmedian,
               hybrid = GGDC10S %>% group_by(Variable, Country) %>% select_at(6:16) %>% fmedian,
               dplyr = GGDC10S %>% group_by(Variable, Country) %>% select_at(6:16) %>% summarise_all(median, na.rm = TRUE))

## -------------------------------------------------------------------------------------------------
class(group_by(GGDC10S, Variable, Country))

class(fgroup_by(GGDC10S, Variable, Country))

## -------------------------------------------------------------------------------------------------
GGDC10S %>% group_by(Variable, Country) %>% select_at(6:16) %>% head(3)
GGDC10S %>% group_by(Variable, Country) %>% get_vars(6:16) %>% head(3)

## -------------------------------------------------------------------------------------------------
GGDC10S %>% group_by(Variable, Country) %>% select_at(6:16) %>% fmean %>% head(3)
GGDC10S %>% group_by(Variable, Country) %>% get_vars(6:16) %>% fmean %>% head(3)

## -------------------------------------------------------------------------------------------------
# fgroup_by fully supports grouped tibbles created with group_by or fgroup_by: 
GGDC10S %>% group_by(Variable, Country) %>% fgroup_vars %>% head(3)
GGDC10S %>% fgroup_by(Variable, Country) %>% fgroup_vars %>% head(3)

# The other possibilities:
GGDC10S %>% group_by(Variable, Country) %>% fgroup_vars("unique") %>% head(3)
GGDC10S %>% group_by(Variable, Country) %>% fgroup_vars("names")
GGDC10S %>% group_by(Variable, Country) %>% fgroup_vars("indices")
GGDC10S %>% group_by(Variable, Country) %>% fgroup_vars("named_indices")
GGDC10S %>% group_by(Variable, Country) %>% fgroup_vars("logical")
GGDC10S %>% group_by(Variable, Country) %>% fgroup_vars("named_logical")

## -------------------------------------------------------------------------------------------------
# Two equivalent calls, the first is substantially faster
GGDC10S %>% fsubset(Variable == "VA" & Year > 1990, Country, Year, AGR:GOV) %>% head(3)

GGDC10S %>% filter(Variable == "VA" & Year > 1990) %>% select(Country, Year, AGR:GOV) %>% head(3)

## -------------------------------------------------------------------------------------------------
GGDC10S %>%
  fgroup_by(Variable, Country) %>%
  get_vars(6:16) %>% {
    cbind(fmedian(.),
          add_stub(fmean(., keep.group_vars = FALSE), "mean_"))
    } %>% head(3)

## -------------------------------------------------------------------------------------------------
GGDC10S %>%
  fgroup_by(Variable, Country) %>% {
   add_vars(get_vars(., "Reg", regex = TRUE) %>% ffirst, # Regular expression matching column names
            num_vars(.) %>% fmean(keep.group_vars = FALSE) %>% add_stub("mean_"), # num_vars selects all numeric variables
            fselect(., PU:TRA) %>% fmedian(keep.group_vars = FALSE) %>% add_stub("median_"), 
            fselect(., PU:CON) %>% fmin(keep.group_vars = FALSE) %>% add_stub("min_"))      
  } %>% head(3)

## -------------------------------------------------------------------------------------------------
GGDC10S %>%
  fsubset(Variable == "VA", Country, AGR, SUM) %>% 
  fgroup_by(Country) %>% {
   add_vars(fgroup_vars(.,"unique"),
            fmean(., keep.group_vars = FALSE) %>% add_stub("mean_"),
            fsd(., keep.group_vars = FALSE) %>% add_stub("sd_"), 
            pos = c(2,4,3,5))
  } %>% head(3)

## -------------------------------------------------------------------------------------------------
# This aggregates numeric colums using the mean (fmean) and categorical columns with the mode (fmode)
GGDC10S %>% fgroup_by(Variable, Country) %>% collapg %>% head(3)

## -------------------------------------------------------------------------------------------------
# This aggregates numeric colums using the median and categorical columns using the first value
GGDC10S %>% fgroup_by(Variable, Country) %>% collapg(fmedian, flast) %>% head(3)

## -------------------------------------------------------------------------------------------------
GGDC10S %>% fgroup_by(Variable, Country) %>%
  collapg(list(fmean, fmedian), list(first, fmode, flast)) %>% head(3)

## -------------------------------------------------------------------------------------------------
GGDC10S %>% fgroup_by(Variable, Country) %>%
  collapg(list(fmean, fmedian), cols = is.numeric, return = "long") %>% head(3)

## -------------------------------------------------------------------------------------------------
GGDC10S %>% fgroup_by(Variable, Country) %>%
  collapg(custom = list(fmean = 6:8, fmedian = 10:12)) %>% head(3)

## -------------------------------------------------------------------------------------------------
# This computes a frequency-weighted grouped standard-deviation, taking the total EMP / VA as weight
GGDC10S %>%
  fgroup_by(Variable, Country) %>%
  fselect(AGR:SUM) %>% fsd(SUM) %>% head(3)

# This computes a weighted grouped mode, taking the total EMP / VA as weight
GGDC10S %>%
  fgroup_by(Variable, Country) %>%
  fselect(AGR:SUM) %>% fmode(SUM) %>% head(3)

## -------------------------------------------------------------------------------------------------
# This aggregates numeric colums using the weighted mean (the default) and categorical columns using the weighted mode (the default).
# Weights (column SUM) are aggregated using both the sum and the maximum. 
GGDC10S %>% group_by(Variable, Country) %>% 
  collapg(w = SUM, wFUN = list(fsum, fmax)) %>% head(3)

## -------------------------------------------------------------------------------------------------
GGDC10S %>% fsubset(Variable == "VA", Country, Year, AGR, SUM) %>%
  ftransform(AGR_perc = AGR / SUM * 100,  # Computing % of VA in Agriculture
             AGR_mean = fmean(AGR),       # Average Agricultural VA
             AGR = NULL, SUM = NULL) %>%  # Deleting columns AGR and SUM
             head

## -------------------------------------------------------------------------------------------------
# This replaces variables mpg, carb and wt by their log
mtcars %>% ftransform(fselect(., mpg, carb, wt) %>% lapply(log)) %>% head

# This adds the log of mpg, carb and wt
mtcars %>% ftransform(fselect(., mpg, carb, wt) %>% lapply(log) %>% add_stub("log.")) %>% head

## -------------------------------------------------------------------------------------------------
GGDC10S %>% fsubset(Variable == "VA", Country, Year, AGR, SUM) %>%
  fcompute(AGR_perc = AGR / SUM * 100,
           AGR_mean = fmean(AGR)) %>% head

## -------------------------------------------------------------------------------------------------
# This subtracts the median value from all data points i.e. centers on the median
GGDC10S %>% num_vars %>% fmedian(TRA = "-") %>% head

# This replaces all data points with the mode
GGDC10S %>% char_vars %>% fmode(TRA = "replace") %>% head

## -------------------------------------------------------------------------------------------------
# Replacing data with the 2nd quartile (25%)
GGDC10S %>%
  fselect(Variable, Country, AGR:SUM) %>% 
   fgroup_by(Variable, Country) %>% fnth(0.25, TRA = "replace_fill") %>% head(3)

# Scaling sectoral data by Variable and Country
GGDC10S %>%
  fselect(Variable, Country, AGR:SUM) %>% 
   fgroup_by(Variable, Country) %>% fsd(TRA = "/") %>% head



## -------------------------------------------------------------------------------------------------
# AGR_gmed = TRUE if AGR is greater than it's median value, grouped by Variable and Country
# Note: This calls fmedian.default
settransform(GGDC10S, AGR_gmed = AGR > fmedian(AGR, list(Variable, Country), TRA = "replace"))
tail(GGDC10S, 3)


## -------------------------------------------------------------------------------------------------
# This subtracts weighted group means from the data, using SUM column as weights.. 
GGDC10S %>%
  fselect(Variable, Country, AGR:SUM) %>% 
   fgroup_by(Variable, Country) %>% fmean(SUM, "-") %>% head


## -------------------------------------------------------------------------------------------------
# This scales and then subtracts the median
GGDC10S %>%
  fselect(Variable, Country, AGR:SUM) %>% 
   fgroup_by(Variable, Country) %>% fsd(TRA = "/") %>% fmedian(TRA = "-")

## -------------------------------------------------------------------------------------------------
# This adds a groupwise observation count next to each column
add_vars(GGDC10S, seq(7,27,2)) <- GGDC10S %>%
    fgroup_by(Variable, Country) %>% fselect(AGR:SUM) %>%
    fNobs("replace_fill") %>% add_stub("N_")

head(GGDC10S)
rm(GGDC10S)

## -------------------------------------------------------------------------------------------------
# This divides by the product
GGDC10S %>%
  fgroup_by(Variable, Country) %>%
    get_vars(6:16) %>% fprod(TRA = "/") %>% head

# Same thing
GGDC10S %>%
  fgroup_by(Variable, Country) %>%
    get_vars(6:16) %>% 
     TRA(fprod(., keep.group_vars = FALSE), "/") %>% head # [same as TRA(.,fprod(., keep.group_vars = FALSE),"/")]

## -------------------------------------------------------------------------------------------------
# This only demeans Agriculture (AGR) and Mining (MIN)
GGDC10S %>%
  fgroup_by(Variable, Country) %>%
    TRA(fselect(., AGR, MIN) %>% fmean(keep.group_vars = FALSE), "-") %>% head

## -------------------------------------------------------------------------------------------------
# Same as above, with one line of code using fmean.data.frame and ftransform...
GGDC10S %>% ftransform(fmean(list(AGR = AGR, MIN = MIN), list(Variable, Country), TRA = "-")) %>% head


## -------------------------------------------------------------------------------------------------
# Get grouped tibble
gGGDC <- GGDC10S %>% fgroup_by(Variable, Country)

# Get aggregated data
gsumGGDC <- gGGDC %>% fselect(AGR:SUM) %>% fsum
head(gsumGGDC)

# Get transformed (scaled) data
head(TRA(gGGDC, gsumGGDC, "/"))

## -------------------------------------------------------------------------------------------------
GGDC10S %>% # Same as ... %>% fmean(TRA = "replace")
  fgroup_by(Variable, Country) %>% get_vars(6:16) %>% fbetween %>% head(2)

GGDC10S %>% # Same as ... %>% fmean(TRA = "replace_fill")
  fgroup_by(Variable, Country) %>% get_vars(6:16) %>% fbetween(fill = TRUE) %>% head(2)

GGDC10S %>% # Same as ... %>% fmean(TRA = "-")
  fgroup_by(Variable, Country) %>% get_vars(6:16) %>% fwithin %>% head(2)

## -------------------------------------------------------------------------------------------------
GGDC10S %>% 
  fgroup_by(Variable, Country) %>% 
    fselect(Country, Variable, AGR:SUM) %>% fwithin(mean = "overall.mean") %>% head(3)

## -------------------------------------------------------------------------------------------------
GGDC10S %>% 
  fgroup_by(Variable, Country) %>% 
    fselect(Country, Variable, AGR:SUM) %>% fwithin(SUM, mean = "overall.mean") %>% head(3)

## -------------------------------------------------------------------------------------------------
# This efficiently scales and centers (i.e. standardizes) the data
GGDC10S %>%
  fgroup_by(Variable, Country) %>%
    fselect(Country, Variable, AGR:SUM) %>% fscale

## -------------------------------------------------------------------------------------------------
# Saving grouped tibble
gGGDC <- GGDC10S %>%
  fgroup_by(Variable, Country) %>%
    fselect(Country, Variable, AGR:SUM)

# Original means
head(fmean(gGGDC)) 

# Mean Preserving Scaling
head(fmean(fscale(gGGDC, mean = FALSE)))
head(fsd(fscale(gGGDC, mean = FALSE)))

## -------------------------------------------------------------------------------------------------
# Just using VA data for this example
gGGDC <- GGDC10S %>%
  fsubset(Variable == "VA", Country, AGR:SUM) %>% 
      fgroup_by(Country)

# This calculates the within- standard deviation for all columns
fsd(num_vars(ungroup(fwithin(gGGDC))))

# This scales all groups to take on the within- standard deviation while preserving group means 
fsd(fscale(gGGDC, mean = FALSE, sd = "within.sd"))


## -------------------------------------------------------------------------------------------------
GGDC10S %>%
  fselect(-Region, -Regioncode) %>% 
    fgroup_by(Variable, Country) %>% flag(-1:1, Year)

## -------------------------------------------------------------------------------------------------
GGDC10S %>%
  fselect(Variable, Country,AGR:SUM) %>% 
    fgroup_by(Variable, Country) %>% flag

## -------------------------------------------------------------------------------------------------
GGDC10S %>%
  fselect(-Region, -Regioncode) %>% 
    fgroup_by(Variable, Country) %>% fdiff(c(1, 10), 1:2, Year)

## -------------------------------------------------------------------------------------------------
GGDC10S %>%
  fselect(-Region, -Regioncode) %>% 
    fgroup_by(Variable, Country) %>% fdiff(c(1, 10), 1, Year, log = TRUE)

## -------------------------------------------------------------------------------------------------
GGDC10S %>%
  fselect(-Region, -Regioncode) %>% 
    fgroup_by(Variable, Country) %>% fdiff(t = Year, rho = 0.95)

## -------------------------------------------------------------------------------------------------
# Exact growth rates, computed as: (x/lag(x) - 1) * 100
GGDC10S %>%
  fselect(-Region, -Regioncode) %>% 
    fgroup_by(Variable, Country) %>% fgrowth(c(1, 10), 1, Year)

# Log-difference growth rates, computed as: log(x / lag(x)) * 100
GGDC10S %>%
  fselect(-Region, -Regioncode) %>% 
    fgroup_by(Variable, Country) %>% fgrowth(c(1, 10), 1, Year, logdiff = TRUE)

## -------------------------------------------------------------------------------------------------
# This computes the 1 and 10-year growth rates, for the current period and lagged by one period
GGDC10S %>%
  fselect(-Region, -Regioncode) %>% 
    fgroup_by(Variable, Country) %>% fgrowth(c(1, 10), 1, Year) %>% flag(0:1, Year)

## ---- eval=NCRAN----------------------------------------------------------------------------------
# This shows the groups in GGDC10S
GRP(GGDC10S, ~ Variable + Country)

# This replicates the data 200 times 
data <- replicate(200, GGDC10S, simplify = FALSE) 
# This function adds a number i to the country and variable columns of each dataset
uniquify <- function(x, i) ftransform(x, lapply(unclass(x)[c(1,4)], paste0, i))
# Making datasets unique and row-binding them
data <- unlist2d(Map(uniquify, data, as.list(1:200)), idcols = FALSE)
fdim(data)

# This shows the groups in the replicated data
GRP(data, ~ Variable + Country)

gc()

## ---- eval=NCRAN, warning=FALSE, message=FALSE----------------------------------------------------
## Selecting columns
# Small
microbenchmark(dplyr = select(GGDC10S, Country, Variable, AGR:SUM),
               collapse = fselect(GGDC10S, Country, Variable, AGR:SUM))

# Large
microbenchmark(dplyr = select(data, Country, Variable, AGR:SUM),
               collapse = fselect(data, Country, Variable, AGR:SUM))

## Subsetting columns 
# Small
microbenchmark(dplyr = filter(GGDC10S, Variable == "VA"),
               collapse = fsubset(GGDC10S, Variable == "VA"))

# Large
microbenchmark(dplyr = filter(data, Variable == "VA"),
               collapse = fsubset(data, Variable == "VA"))

## Ordering rows
# Small
microbenchmark(dplyr = arrange(GGDC10S, desc(Country), Variable, Year),
               collapse = roworder(GGDC10S, -Country, Variable, Year))

# Large
microbenchmark(dplyr = arrange(data, desc(Country), Variable, Year),
               collapse = roworder(data, -Country, Variable, Year), times = 2)


## Grouping 
# Small
microbenchmark(dplyr = group_by(GGDC10S, Country, Variable),
               collapse = fgroup_by(GGDC10S, Country, Variable))

# Large
microbenchmark(dplyr = group_by(data, Country, Variable),
               collapse = fgroup_by(data, Country, Variable), times = 10)

## Computing a new column 
# Small
microbenchmark(dplyr = mutate(GGDC10S, NEW = AGR+1),
               collapse = ftransform(GGDC10S, NEW = AGR+1))

# Large
microbenchmark(dplyr = mutate(data, NEW = AGR+1),
               collapse = ftransform(data, NEW = AGR+1))

## All combined with pipes 
# Small
microbenchmark(dplyr = filter(GGDC10S, Variable == "VA") %>% 
                       select(Country, Year, AGR:SUM) %>% 
                       arrange(desc(Country), Year) %>%
                       mutate(NEW = AGR+1) %>%
                       group_by(Country),
               collapse = fsubset(GGDC10S, Variable == "VA", Country, Year, AGR:SUM) %>% 
                       roworder(-Country, Year) %>%
                       ftransform(NEW = AGR+1) %>%
                       fgroup_by(Country))

# Large
microbenchmark(dplyr = filter(data, Variable == "VA") %>% 
                       select(Country, Year, AGR:SUM) %>% 
                       arrange(desc(Country), Year) %>%
                       mutate(NEW = AGR+1) %>%
                       group_by(Country),
               collapse = fsubset(data, Variable == "VA", Country, Year, AGR:SUM) %>% 
                       roworder(-Country, Year) %>%
                       ftransform(NEW = AGR+1) %>%
                       fgroup_by(Country), times = 10)

gc()

## ---- eval=NCRAN, warning=FALSE, message=FALSE----------------------------------------------------
## Grouping the data
cgGGDC10S <- fgroup_by(GGDC10S, Variable, Country) %>% fselect(-Region, -Regioncode)
gGGDC10S <- group_by(GGDC10S, Variable, Country) %>% fselect(-Region, -Regioncode)
cgdata <- fgroup_by(data, Variable, Country) %>% fselect(-Region, -Regioncode)
gdata <- group_by(data, Variable, Country) %>% fselect(-Region, -Regioncode)
rm(data, GGDC10S) 
gc()

## Conversion of Grouping object: This time would be required extra in all hybrid calls 
## i.e. when calling collapse functions on data grouped with dplyr::group_by
# Small
microbenchmark(GRP(gGGDC10S))

# Large
microbenchmark(GRP(gdata))


## Sum 
# Small
microbenchmark(dplyr = summarise_all(gGGDC10S, sum, na.rm = TRUE),
               collapse = fsum(cgGGDC10S))

# Large
microbenchmark(dplyr = summarise_all(gdata, sum, na.rm = TRUE),
               collapse = fsum(cgdata), times = 10)

## Mean
# Small
microbenchmark(dplyr = summarise_all(gGGDC10S, mean.default, na.rm = TRUE),
               collapse = fmean(cgGGDC10S))

# Large
microbenchmark(dplyr = summarise_all(gdata, mean.default, na.rm = TRUE),
               collapse = fmean(cgdata), times = 10)

## Median
# Small
microbenchmark(dplyr = summarise_all(gGGDC10S, median, na.rm = TRUE),
               collapse = fmedian(cgGGDC10S))

# Large
microbenchmark(dplyr = summarise_all(gdata, median, na.rm = TRUE),
               collapse = fmedian(cgdata), times = 2)

## Standard Deviation
# Small
microbenchmark(dplyr = summarise_all(gGGDC10S, sd, na.rm = TRUE),
               collapse = fsd(cgGGDC10S))

# Large
microbenchmark(dplyr = summarise_all(gdata, sd, na.rm = TRUE),
               collapse = fsd(cgdata), times = 2)

## Maximum
# Small
microbenchmark(dplyr = summarise_all(gGGDC10S, max, na.rm = TRUE),
               collapse = fmax(cgGGDC10S))

# Large
microbenchmark(dplyr = summarise_all(gdata, max, na.rm = TRUE),
               collapse = fmax(cgdata), times = 10)

## First Value
# Small
microbenchmark(dplyr = summarise_all(gGGDC10S, first),
               collapse = ffirst(cgGGDC10S, na.rm = FALSE))

# Large
microbenchmark(dplyr = summarise_all(gdata, first),
               collapse = ffirst(cgdata, na.rm = FALSE), times = 10)

## Number of Distinct Values
# Small
microbenchmark(dplyr = summarise_all(gGGDC10S, n_distinct, na.rm = TRUE),
               collapse = fNdistinct(cgGGDC10S))

# Large
microbenchmark(dplyr = summarise_all(gdata, n_distinct, na.rm = TRUE),
               collapse = fNdistinct(cgdata), times = 5)

gc()

## ---- eval=NCRAN, warning=FALSE, message=FALSE----------------------------------------------------
## Weighted Mean
# Small
microbenchmark(fmean(cgGGDC10S, SUM)) 

# Large 
microbenchmark(fmean(cgdata, SUM), times = 10) 

## Weighted Standard-Deviation
# Small
microbenchmark(fsd(cgGGDC10S, SUM)) 

# Large 
microbenchmark(fsd(cgdata, SUM), times = 10) 

## Statistical Mode
# Small
microbenchmark(fmode(cgGGDC10S)) 

# Large 
microbenchmark(fmode(cgdata), times = 10) 

## Weighted Statistical Mode
# Small
microbenchmark(fmode(cgGGDC10S, SUM)) 

# Large 
microbenchmark(fmode(cgdata, SUM), times = 10) 

gc()

## ---- eval=NCRAN, warning=FALSE, message=FALSE----------------------------------------------------

## Replacing with group sum
# Small
microbenchmark(dplyr = mutate_all(gGGDC10S, sum, na.rm = TRUE),
               collapse = fsum(cgGGDC10S, TRA = "replace_fill"))

# Large
microbenchmark(dplyr = mutate_all(gdata, sum, na.rm = TRUE),
               collapse = fsum(cgdata, TRA = "replace_fill"), times = 10)

## Dividing by group sum
# Small
microbenchmark(dplyr = mutate_all(gGGDC10S, function(x) x/sum(x, na.rm = TRUE)),
               collapse = fsum(cgGGDC10S, TRA = "/"))

# Large
microbenchmark(dplyr = mutate_all(gdata, function(x) x/sum(x, na.rm = TRUE)),
               collapse = fsum(cgdata, TRA = "/"), times = 10)

## Centering
# Small
microbenchmark(dplyr = mutate_all(gGGDC10S, function(x) x-mean.default(x, na.rm = TRUE)),
               collapse = fwithin(cgGGDC10S))

# Large
microbenchmark(dplyr = mutate_all(gdata, function(x) x-mean.default(x, na.rm = TRUE)),
               collapse = fwithin(cgdata), times = 10)

## Centering and Scaling (Standardizing)
# Small
microbenchmark(dplyr = mutate_all(gGGDC10S, function(x) (x-mean.default(x, na.rm = TRUE))/sd(x, na.rm = TRUE)),
               collapse = fscale(cgGGDC10S))

# Large
microbenchmark(dplyr = mutate_all(gdata, function(x) (x-mean.default(x, na.rm = TRUE))/sd(x, na.rm = TRUE)),
               collapse = fscale(cgdata), times = 2)

## Lag
# Small
microbenchmark(dplyr_unordered = mutate_all(gGGDC10S, dplyr::lag),
               collapse_unordered = flag(cgGGDC10S),
               dplyr_ordered = mutate_all(gGGDC10S, dplyr::lag, order_by = "Year"),
               collapse_ordered = flag(cgGGDC10S, t = Year))

# Large
microbenchmark(dplyr_unordered = mutate_all(gdata, dplyr::lag),
               collapse_unordered = flag(cgdata),
               dplyr_ordered = mutate_all(gdata, dplyr::lag, order_by = "Year"),
               collapse_ordered = flag(cgdata, t = Year), times = 2)

## First-Difference (unordered)
# Small
microbenchmark(dplyr_unordered = mutate_all(gGGDC10S, function(x) x - dplyr::lag(x)),
               collapse_unordered = fdiff(cgGGDC10S))

# Large
microbenchmark(dplyr_unordered = mutate_all(gdata, function(x) x - dplyr::lag(x)),
               collapse_unordered = fdiff(cgdata), times = 2)

gc()

## ---- eval=NCRAN, warning=FALSE, message=FALSE----------------------------------------------------
# Centering on overall mean
microbenchmark(fwithin(cgdata, mean = "overall.mean"), times = 10)

# Weighted Centering
microbenchmark(fwithin(cgdata, SUM), times = 10)
microbenchmark(fwithin(cgdata, SUM, mean = "overall.mean"), times = 10)

# Weighted Scaling and Standardizing
microbenchmark(fsd(cgdata, SUM, TRA = "/"), times = 10)
microbenchmark(fscale(cgdata, SUM), times = 10)

# Sequence of lags and leads
microbenchmark(flag(cgdata, -1:1), times = 10)

# Iterated difference
microbenchmark(fdiff(cgdata, 1, 2), times = 10)

# Growth Rate
microbenchmark(fgrowth(cgdata,1), times = 10)

## ---- echo=FALSE--------------------------------------------------------------
options(oldopts)

