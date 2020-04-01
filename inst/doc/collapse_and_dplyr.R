## ---- echo = FALSE, message = FALSE, warning=FALSE------------------------------------------------
library(dplyr)
library(microbenchmark)
library(collapse)
knitr::opts_chunk$set(error = FALSE, message = FALSE, warning = FALSE, 
                      comment = "#", tidy = FALSE, cache = TRUE, collapse = TRUE,
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
GGDC10S %>% fmode                       # Mode
GGDC10S %>% fmode(drop = FALSE)         # Keep data structure intact

## -------------------------------------------------------------------------------------------------
GGDC10S %>% 
  group_by(Variable,Country) %>%
  select_at(6:16) %>% fmean


## -------------------------------------------------------------------------------------------------
GGDC10S %>% 
  group_by(Variable,Country) %>%
  select_at(6:16) %>% fmedian

GGDC10S %>% 
  group_by(Variable,Country) %>%
  select_at(6:16) %>% fsd

## -------------------------------------------------------------------------------------------------
GGDC10S %>% group_by(Variable,Country) %>% attr("groups")

## -------------------------------------------------------------------------------------------------
GGDC10S %>% group_by(Variable,Country) %>% GRP %>% str

## -------------------------------------------------------------------------------------------------
GGDC10S %>%
  group_by(Variable,Country) %>%
  select_at(6:16) %>% {
    cbind(fmedian(.), 
          add_stub(fmean(., keep.group_vars = FALSE), "mean_"))
    } %>% head(3)

## -------------------------------------------------------------------------------------------------
GGDC10S %>% 
  group_by(Variable,Country) %>% {
   add_vars(group_keys(.), 
            ffirst(get_vars(., "Reg", regex = TRUE)),        # Regular expression matching column names
            add_stub(fmean(num_vars(.)), "mean_"),           # num_vars selects all numeric variables
            add_stub(fmedian(get_vars(., 9:12)), "median_"), # columns 9-12
            add_stub(fmin(get_vars(., 9:10)), "min_"))       # columns 9:10
  }

## -------------------------------------------------------------------------------------------------
GGDC10S %>% 
  group_by(Variable,Country) %>% {
   add_vars(group_keys(.), 
            add_stub(fmean(get_vars(., c("AGR","SUM"))), "mean_"), 
            add_stub(fsd(get_vars(., c("AGR","SUM"))), "sd_"), 
            pos = c(2,4,3,5))       
  }

## -------------------------------------------------------------------------------------------------
# This aggregates numeric colums using the mean (fmean) and categorical columns with the mode (fmode)
GGDC10S %>% group_by(Variable,Country) %>% collapg

## -------------------------------------------------------------------------------------------------
# This aggregates numeric colums using the median and categorical columns using the first value
GGDC10S %>% group_by(Variable,Country) %>% collapg(fmedian, flast)

## -------------------------------------------------------------------------------------------------
GGDC10S %>% group_by(Variable,Country) %>% 
  collapg(list(fmean, fmedian), list(first, fmode, flast))

## -------------------------------------------------------------------------------------------------
GGDC10S %>% group_by(Variable,Country) %>% 
  collapg(list(fmean, fmedian), cols = is.numeric, return = "long")

## -------------------------------------------------------------------------------------------------
GGDC10S %>% group_by(Variable,Country) %>% 
  collapg(custom = list(fmean = 6:8, fmedian = 10:12))

## -------------------------------------------------------------------------------------------------
# This compute a frequency-weighted grouped standard-deviation, taking the total EMP / VA as weight
GGDC10S %>% 
  group_by(Variable,Country) %>%
  select_at(6:16) %>% fsd(SUM)

# This compute a weighted grouped mode, taking the total EMP / VA as weight
GGDC10S %>% 
  group_by(Variable,Country) %>%
  select_at(6:16) %>% fmode(SUM)

## -------------------------------------------------------------------------------------------------
# This aggregates numeric colums using the weighted mean and categorical columns using the weighted mode
GGDC10S %>% group_by(Variable,Country) %>% collapg(w = .$SUM)

## -------------------------------------------------------------------------------------------------
GGDC10S %>% 
  group_by(Variable,Country) %>% {
    add_vars(fmean(select_at(., 6:16), SUM),      # Again select_at preserves grouping columns, 
             fmode(get_vars(., c(2:3,16)), SUM),  # get_vars does not! Both preserve attributes
             pos = c(5, 2:3))
  }

## ---- eval=NCRAN----------------------------------------------------------------------------------
# This replicates the data 200 times while keeping Country and Variable (columns 1 and 4) unique
data <- replicate(200, GGDC10S, simplify = FALSE) # gv and gv<- are shortcuts for get_vars and get_vars<-
uniquify <- function(x, i) `gv<-`(x, c(1,4), value = lapply(gv(x, c(1,4)), paste0, i))
data <- unlist2d(Map(uniquify, data, as.list(1:200)), idcols = FALSE)

dim(data)
GRP(data, c(1,4))$N.groups # This shows the number of groups. 

# Grouping: This is still a key bottleneck of dplyr compared to data.table and collapse
system.time(group_by(data,Variable,Country))
system.time(GRP(data, c(1,4)))               

library(microbenchmark)

# Selection 
microbenchmark(select_at(data, 6:16))
microbenchmark(get_vars(data, 6:16))

data <- data %>% group_by(Variable,Country) %>% select_at(6:16)

# Conversion of Grouping object: This time is also required in all computations below using collapse fast functions
microbenchmark(GRP(data)) 

# Sum 
system.time(fsum(data))
system.time(summarise_all(data, sum, na.rm = TRUE))

# Product
system.time(fprod(data))
system.time(summarise_all(data, prod, na.rm = TRUE))

# Mean
system.time(fmean(data))
system.time(summarise_all(data, mean, na.rm = TRUE))

# Weighted Mean
system.time(fmean(data, SUM)) # This cannot easily be performed in dplyr

# Median
system.time(fmedian(data))
system.time(summarise_all(data, median, na.rm = TRUE))

# Standard-Deviation
system.time(fsd(data))
system.time(summarise_all(data, sd, na.rm = TRUE))

# Weighted Standard-Deviation
system.time(fsd(data, SUM))

# Maximum
system.time(fmax(data))
system.time(summarise_all(data, max, na.rm = TRUE))

# First Value
system.time(ffirst(data, na.rm = FALSE))
system.time(summarise_all(data, first))

# Distinct Values
system.time(fNdistinct(data))
system.time(summarise_all(data, n_distinct, na.rm = TRUE))

# Mode
system.time(fmode(data))

# Weighted Mode
system.time(fmode(data, SUM))


## -------------------------------------------------------------------------------------------------
# This subtracts the median value from all data points i.e. centers on the median
GGDC10S %>% num_vars %>% fmedian(TRA = "-")

# This replaces all data points with the mode
GGDC10S %>% char_vars %>% fmode(TRA = "replace")

## -------------------------------------------------------------------------------------------------
# Demeaning sectoral data by Variable and Country (within transformation)
GGDC10S %>% 
  group_by(Variable,Country) %>%
  select_at(6:16) %>% fmean(TRA = "-")

# Scaling sectoral data by Variable and Country
GGDC10S %>% 
  group_by(Variable,Country) %>%
    select_at(6:16) %>% fsd(TRA = "/")

# Computing sercentages of sectoral data by Variable and Country
GGDC10S %>% 
  group_by(Variable,Country) %>%
    select_at(6:16) %>% fsum(TRA = "%")


## -------------------------------------------------------------------------------------------------
# Weighted demeaning (within transformation)
GGDC10S %>% 
  group_by(Variable,Country) %>%
  select_at(6:16) %>% fmean(SUM, "-")

# Weighted scaling
GGDC10S %>% 
  group_by(Variable,Country) %>%
  select_at(6:16) %>% fsd(SUM, "/")

## -------------------------------------------------------------------------------------------------
# This conducts a weighted between transformation (replacing with weighted mean)
GGDC10S %>% 
  group_by(Variable,Country) %>%
    select_at(6:16) %>% fmean(SUM, "replace")

# This also replaces missing values in each group
GGDC10S %>% 
  group_by(Variable,Country) %>%
    select_at(6:16) %>% fmean(SUM, "replace_fill")


## -------------------------------------------------------------------------------------------------
# This group-centers data on the overall mean of the data
GGDC10S %>% 
  group_by(Variable,Country) %>%
    select_at(6:16) %>% fmean(TRA = "-+") 

## -------------------------------------------------------------------------------------------------
# This scales and centers (i.e. standardizes) the data 
GGDC10S %>% 
  group_by(Variable,Country) %>%
    select_at(6:16) %>% fsd(TRA = "/") %>% fmean(TRA = "-")

## -------------------------------------------------------------------------------------------------
# This group-centers data on the group-medians and adds the new variables right next to the original ones
add_vars(GGDC10S, seq(7,27,2)) <- GGDC10S %>% 
    group_by(Variable,Country) %>% get_vars(6:16) %>% 
    fmedian(TRA = "-") %>% add_stub("demean_")

GGDC10S
rm(GGDC10S)

## -------------------------------------------------------------------------------------------------
# This divides by the product
GGDC10S %>% 
  group_by(Variable,Country) %>%
    select_at(6:16) %>% fprod(TRA = "/")

# Same thing 
GGDC10S %>% 
  group_by(Variable,Country) %>%
    select_at(6:16) %>% TRA(fprod(.),"/") # [same as TRA(.,fprod(.),"/")]

## -------------------------------------------------------------------------------------------------
# This only demeans Agriculture (AGR) and Mining (MIN)
GGDC10S %>% 
  group_by(Variable,Country) %>%
    select_at(6:16) %>% TRA(fmean(get_vars(.,c("AGR","MIN"))),"-") 

## -------------------------------------------------------------------------------------------------
# Get grouped tibble
gGGDC <- GGDC10S %>% group_by(Variable,Country)

# Get aggregated data
gsumGGDC <- gGGDC %>% select_at(6:16) %>% fsum
gsumGGDC

# Get transformed (scaled) data 
TRA(gGGDC, gsumGGDC, "/")

## -------------------------------------------------------------------------------------------------
GGDC10S %>% # Same as ... %>% fmean(TRA = "replace")
  group_by(Variable,Country) %>% select_at(6:16) %>% fbetween %>% head(2)

GGDC10S %>% # Same as ... %>% fmean(TRA = "replace_fill")
  group_by(Variable,Country) %>% select_at(6:16) %>% fbetween(fill = TRUE) %>% head(2)

GGDC10S %>% # Same as ... %>% fmean(TRA = "-")
  group_by(Variable,Country) %>% select_at(6:16) %>% fwithin %>% head(2)

GGDC10S %>% # Same as ... %>% fmean(TRA = "-+")
  group_by(Variable,Country) %>% select_at(6:16) %>% fwithin(mean = "overall.mean") %>% head(2)

## -------------------------------------------------------------------------------------------------
GGDC10S %>% # This does not center data on a properly computed weighted overall mean
  group_by(Variable,Country) %>% select_at(6:16) %>% fmean(SUM, TRA = "-+") 

GGDC10S %>% # This does a proper job by both subtracting weighted group-means and adding a weighted overall mean
  group_by(Variable,Country) %>% select_at(6:16) %>% fwithin(SUM, mean = "overall.mean") 

## -------------------------------------------------------------------------------------------------
# This efficiently scales and centers (i.e. standardizes) the data 
GGDC10S %>% 
  group_by(Variable,Country) %>%
    select_at(6:16) %>% fscale

## -------------------------------------------------------------------------------------------------
GGDC10S %>% 
  group_by(Variable,Country) %>%
     select_at(5:16) %>% flag(-1:1, Year)

## -------------------------------------------------------------------------------------------------
GGDC10S %>% 
  group_by(Variable,Country) %>%
     select_at(6:16) %>% flag

## -------------------------------------------------------------------------------------------------
GGDC10S %>% 
  group_by(Variable,Country) %>%
     select_at(5:16) %>% fdiff(c(1, 10), 1:2, Year)

## -------------------------------------------------------------------------------------------------
# Exact growth rates, computed as: (x - lag(x)) / lag(x) * 100
GGDC10S %>% 
  group_by(Variable,Country) %>%
     select_at(5:16) %>% fgrowth(c(1, 10), 1:2, Year)

# Log-difference growth rates, computed as: log(x / lag(x)) * 100
GGDC10S %>% 
  group_by(Variable,Country) %>%
     select_at(5:16) %>% fgrowth(c(1, 10), 1:2, Year, logdiff = TRUE)

## -------------------------------------------------------------------------------------------------
# This computes the 1 and 10-year growth rates, for the current period and lagged by one period
GGDC10S %>% 
  group_by(Variable,Country) %>%
     select_at(5:16) %>% fgrowth(c(1, 10), 1, Year) %>% flag(0:1, Year)

## ---- eval=NCRAN----------------------------------------------------------------------------------
dim(data)
GRP(data)

# Grouped Sum (mutate does not have an option to preserve missing values as given by "replace")
system.time(fsum(data, TRA = "replace_fill"))
system.time(mutate_all(data, sum, na.rm = TRUE))

# Dviding by grouped sum
system.time(fsum(data, TRA = "/"))
system.time(mutate_all(data, function(x) x/sum(x, na.rm = TRUE)))

# Mean (between transformation)
system.time(fmean(data, TRA = "replace_fill"))
system.time(fbetween(data, fill = TRUE))
system.time(mutate_all(data, mean, na.rm = TRUE))

# De-Mean (within transformation)
system.time(fmean(data, TRA = "-"))
system.time(fwithin(data))
system.time(mutate_all(data, function(x) x - mean(x, na.rm = TRUE)))

# Centering on overall mean
system.time(fwithin(data, mean = "overall.mean"))

# Weighted Demeaning
system.time(fwithin(data, SUM))
system.time(fwithin(data, SUM, mean = "overall.mean"))

# Scaling
system.time(fsd(data, TRA = "/"))
system.time(mutate_all(data, function(x) x/sd(x, na.rm = TRUE)))

# Standardizing
system.time(fscale(data))
# system.time(mutate_all(data, scale)) This takes 32 seconds to compute.. 

# Weighted Scaling and standardizing
system.time(fsd(data, SUM, TRA = "/"))
system.time(fscale(data, SUM))

# Lags and Leads
system.time(flag(data))
system.time(mutate_all(data, lag))
system.time(flag(data, -1))
system.time(mutate_all(data, lead))
system.time(flag(data, -1:1))

# Differences
system.time(fdiff(data))
system.time(fdiff(data,1,1:2))
system.time(fdiff(data, c(1,10)))
system.time(fdiff(data, c(1,10), 1:2))

# Growth Rates
system.time(fgrowth(data))
system.time(fgrowth(data,1,1:2))
system.time(fgrowth(data, c(1,10)))
system.time(fgrowth(data, c(1,10), 1:2))


## ---- echo=FALSE--------------------------------------------------------------
options(oldopts)

