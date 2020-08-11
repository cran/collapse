## ---- echo = FALSE, message = FALSE, warning=FALSE-------------------------------------------------
library(vars)
library(dplyr)  # Needed because otherwise dplyr is loaded in benchmark chunk not run on CRAN !!
library(magrittr)
library(microbenchmark) # Same thing
library(collapse)
library(data.table)
B <- collapse::B # making sure it masks vars::B by loading into GE
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

oldopts <- options(width = 101L)

X = mtcars[1:2]
by = mtcars$cyl

set.seed(101)

## --------------------------------------------------------------------------------------------------
library(collapse)
head(wlddev)

# The variables have "label" attributes. Use vlabels() to get and set labels
namlab(wlddev, class = TRUE)

## --------------------------------------------------------------------------------------------------
# A fast and detailed statistical description
descr(wlddev)

## --------------------------------------------------------------------------------------------------
head(as.data.frame(descr(wlddev)))

## --------------------------------------------------------------------------------------------------
varying(wlddev, wlddev$iso3c)

## --------------------------------------------------------------------------------------------------
head(varying(wlddev, wlddev$iso3c, any_group = FALSE))

## --------------------------------------------------------------------------------------------------
head(fNobs(wlddev, wlddev$iso3c))

head(fNdistinct(wlddev, wlddev$iso3c))

## --------------------------------------------------------------------------------------------------
qsu(wlddev, cols = 9:12, higher = TRUE) # higher adds skewness and kurtosis

## --------------------------------------------------------------------------------------------------
qsu(wlddev, by = ~region, cols = 9:12, vlabels = TRUE, higher = TRUE) 

## --------------------------------------------------------------------------------------------------
qsu(wlddev, pid = ~ iso3c, cols = c(1,4,9:12), vlabels = TRUE, higher = TRUE)

## --------------------------------------------------------------------------------------------------
qsu(wlddev, by = ~ region, pid = ~ iso3c, cols = 9:12, vlabels = TRUE, higher = TRUE)

## --------------------------------------------------------------------------------------------------
l <- qsu(wlddev, by = ~ region, pid = ~ iso3c, cols = 9:12, vlabels = TRUE, 
         higher = TRUE, array = FALSE)

str(l, give.attr = FALSE)

## --------------------------------------------------------------------------------------------------
head(unlist2d(l, idcols = c("Variable", "Trans"), row.names = "Region"))

## --------------------------------------------------------------------------------------------------
pwcor(wlddev[9:12], N = TRUE, P = TRUE)

## --------------------------------------------------------------------------------------------------
print(pwcor(fmean(wlddev[9:12], wlddev$iso3c), N = TRUE, P = TRUE), show = "lower.tri")

# N is same as overall N shown above...
print(pwcor(fwithin(wlddev[9:12], wlddev$iso3c), P = TRUE), show = "lower.tri")


## --------------------------------------------------------------------------------------------------
pwNobs(wlddev)

## --------------------------------------------------------------------------------------------------
head(GGDC10S)

namlab(GGDC10S, class = TRUE)

fNobs(GGDC10S)

fNdistinct(GGDC10S)

# The countries included:
cat(funique(GGDC10S$Country, ordered = TRUE))


## --------------------------------------------------------------------------------------------------
# Converting data to percentages of overall VA / EMP, dapply keeps the attributes, see section 6.1
pGGDC10S <- dapply(GGDC10S[6:15], `*`, 100 / GGDC10S$SUM) 
# Summarizing the sectoral data by variable, overall, between and within countries
su <- qsu(pGGDC10S, by = GGDC10S$Variable, pid = GGDC10S[c("Variable","Country")], higher = TRUE) 

# This gives a 4D array of summary statistics
str(su)

# Permuting this array to a more readible format
aperm(su, c(4,2,3,1))

## --------------------------------------------------------------------------------------------------
library(data.table)
library(ggplot2)

plotGGDC <- function(ctry) {
dat <- fsubset(GGDC10S, Country == ctry, Variable, Year, AGR:SUM)
fselect(dat, AGR:OTH) <- replace_outliers(dapply(fselect(dat, AGR:OTH), `*`, 1 / dat$SUM), 0, NA, "min")
dat$SUM <- NULL
dat$Variable <- recode_char(dat$Variable, VA = "Value Added Share", EMP = "Employment Share")
dat <- melt(qDT(dat), 1:2, variable.name = "Sector", na.rm = TRUE)

ggplot(aes(x = Year, y = value, fill = Sector), data = dat) +
  geom_area(position = "fill", alpha = 0.9) + labs(x = NULL, y = NULL) +
  theme_linedraw(base_size = 14) + facet_wrap( ~ Variable) +
  scale_fill_manual(values = sub("#00FF66", "#00CC66", rainbow(10))) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7), expand = c(0, 0)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0, 0),
                     labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 315, hjust = 0, margin = ggplot2::margin(t = 0)),
        strip.background = element_rect(colour = "grey20", fill = "grey20"),
        strip.text = element_text(face = "bold"))
}

# Plotting the structural transformation of Botswana
plotGGDC("BWA")


## --------------------------------------------------------------------------------------------------
library(magrittr) # Pipe operators
fselect(wlddev, country, year, PCGDP:ODA) %>% head(2)

fselect(wlddev, -country, -year, -(PCGDP:ODA)) %>% head(2)

library(microbenchmark)
microbenchmark(fselect = collapse::fselect(wlddev, country, year, PCGDP:ODA),
               select = dplyr::select(wlddev, country, year, PCGDP:ODA))

## --------------------------------------------------------------------------------------------------
# Computing the log of columns
fselect(wlddev, PCGDP:ODA) <- lapply(fselect(wlddev, PCGDP:ODA), log)
head(wlddev, 2)
# Efficient deleting
fselect(wlddev, country, year, PCGDP:ODA) <- NULL
head(wlddev, 2)
rm(wlddev)

## --------------------------------------------------------------------------------------------------
fselect(wlddev, PCGDP:ODA, return = "names")
fselect(wlddev, PCGDP:ODA, return = "indices")
fselect(wlddev, PCGDP:ODA, return = "named_indices")
fselect(wlddev, PCGDP:ODA, return = "logical")
fselect(wlddev, PCGDP:ODA, return = "named_logical")

## --------------------------------------------------------------------------------------------------
get_vars(wlddev, 9:12) %>% head(1)
get_vars(wlddev, c("PCGDP","LIFEEX","GINI","ODA")) %>% head(1)
get_vars(wlddev, "[[:upper:]]", regex = TRUE) %>% head(1)
get_vars(wlddev, "PC|LI|GI|OD", regex = TRUE) %>% head(1)
# Same as above, vectors of regular expressions are sequentially passed to grep
get_vars(wlddev, c("PC","LI","GI","OD"), regex = TRUE) %>% head(1)
get_vars(wlddev, is.numeric) %>% head(1)

# Returning other information
get_vars(wlddev, is.numeric, return = "names")
get_vars(wlddev, "[[:upper:]]", regex = TRUE, return = "named_indices")


## --------------------------------------------------------------------------------------------------
get_vars(wlddev, 9:12) <- lapply(get_vars(wlddev, 9:12), log)
get_vars(wlddev, 9:12) <- NULL
head(wlddev, 2)
rm(wlddev)

## --------------------------------------------------------------------------------------------------
head(num_vars(wlddev), 2)
head(cat_vars(wlddev), 2)
head(fact_vars(wlddev), 2)

# Replacing
fact_vars(wlddev) <- fact_vars(wlddev)

## --------------------------------------------------------------------------------------------------
# Returning only value-added data after 1990
fsubset(GGDC10S, Variable == "VA" & Year > 1990, Country, Year, AGR:GOV) %>% head(2)
# Same thing
fsubset(GGDC10S, Variable == "VA" & Year > 1990, -(Regioncode:Variable), -(OTH:SUM)) %>% head(2)

## --------------------------------------------------------------------------------------------------
ss(GGDC10S, 1:2, 6:16)  # or fsubset(GGDC10S, 1:2, 6:16)
ss(GGDC10S, -(1:2), c("AGR","MIN")) %>% head(2)

## --------------------------------------------------------------------------------------------------
microbenchmark(base = subset(GGDC10S, Variable == "VA" & Year > 1990, AGR:SUM), 
               collapse = fsubset(GGDC10S, Variable == "VA" & Year > 1990, AGR:SUM))

microbenchmark(GGDC10S[1:10, 1:10], ss(GGDC10S, 1:10, 1:10))

## --------------------------------------------------------------------------------------------------
roworder(GGDC10S, -Variable, Country) %>% ss(1:2, 1:8)

microbenchmark(collapse = collapse::roworder(GGDC10S, -Variable, Country), 
               dplyr = dplyr::arrange(GGDC10S, desc(Variable), Country))

## --------------------------------------------------------------------------------------------------
# Same as above
roworderv(GGDC10S, c("Variable", "Country"), decreasing = c(TRUE, FALSE)) %>% ss(1:2, 1:8)

## --------------------------------------------------------------------------------------------------
# If length(neworder) < fnrow(data), the default (pos = "front") brings rows to the front
roworderv(GGDC10S, neworder = which(GGDC10S$Country == "GHA")) %>% ss(1:2, 1:8)

# pos = "end" brings rows to the end
roworderv(GGDC10S, neworder = which(GGDC10S$Country == "BWA"), pos = "end") %>% ss(1:2, 1:8)

# pos = "exchange" arranges selected rows in the order they are passed, without affecting other rows
roworderv(GGDC10S, neworder = with(GGDC10S, c(which(Country == "GHA"), 
                                              which(Country == "BWA"))), pos = "exchange") %>% ss(1:2, 1:8)

## --------------------------------------------------------------------------------------------------
# The default is again pos = "front" which brings selected columns to the front / left
colorder(GGDC10S, Variable, Country, Year) %>% head(2)

## --------------------------------------------------------------------------------------------------
ftransform(GGDC10S, AGR_perc = AGR / SUM * 100, # Computing Agricultural percentage
                    Year = as.integer(Year),    # Coercing Year to integer
                    AGR = NULL) %>% tail(2)     # Deleting column AGR

# Computing scalar results replicates them
ftransform(GGDC10S, MIN_mean = fmean(MIN), Intercept = 1) %>% tail(2)

## --------------------------------------------------------------------------------------------------
GGDC10S %>% ftransform(num_vars(.) %>% lapply(log)) %>% tail(2)
GGDC10S %>% ftransform(num_vars(.) %>% lapply(sum, na.rm = TRUE)) %>% tail(2)
rm(GGDC10S)

## --------------------------------------------------------------------------------------------------
# Computing a new column and deleting some others by reference
settransform(GGDC10S, FIRE_MAN = FIRE / MAN,
                      Regioncode = NULL, Region = NULL)
tail(GGDC10S, 2)

# Bulk-processing the data into percentage terms
settransform(GGDC10S, fselect(GGDC10S, AGR:SUM) %>% lapply(`*`, 100/.$SUM))
tail(GGDC10S, 2)

# Same thing via replacement 
ftransform(GGDC10S) <- fselect(GGDC10S, AGR:SUM) %>% lapply(`*`, 100/.$SUM)
# Or using double pipes
GGDC10S %<>% ftransform(fselect(., AGR:SUM) %>% lapply(`*`, 100/.$SUM))
rm(GGDC10S)

## --------------------------------------------------------------------------------------------------
fcompute(GGDC10S, AGR_perc = AGR / SUM * 100, FIRE_MAN = FIRE / MAN) %>% tail(2)

## --------------------------------------------------------------------------------------------------
# Efficient adding logged versions of some variables
add_vars(wlddev) <- get_vars(wlddev, 9:12) %>% lapply(log10) %>% add_stub("log10.")
head(wlddev, 2)
rm(wlddev)

## --------------------------------------------------------------------------------------------------
add_vars(wlddev, "front") <- get_vars(wlddev, 9:12) %>% lapply(log10) %>% add_stub("log10.")
head(wlddev, 2)
rm(wlddev)

add_vars(wlddev, c(10,12,14,16)) <- get_vars(wlddev, 9:12) %>% lapply(log10) %>% add_stub("log10.")
head(wlddev, 2)
rm(wlddev)

## --------------------------------------------------------------------------------------------------
add_vars(wlddev, get_vars(wlddev, 9:12) %>% lapply(log) %>% add_stub("log."),
                 get_vars(wlddev, 9:12) %>% lapply(log10) %>% add_stub("log10.")) %>% head(2)

add_vars(wlddev,  get_vars(wlddev, 9:12) %>% lapply(log) %>% add_stub("log."), 
                  get_vars(wlddev, 9:12) %>% lapply(log10) %>% add_stub("log10."),
         pos = c(10,13,16,19,11,14,17,20)) %>% head(2)

identical(cbind(wlddev, wlddev), add_vars(wlddev, wlddev))
microbenchmark(cbind(wlddev, wlddev), add_vars(wlddev, wlddev))

## --------------------------------------------------------------------------------------------------
frename(GGDC10S, AGR = Agriculture, MIN = Mining) %>% head(2)
frename(GGDC10S, tolower) %>% head(2)
frename(GGDC10S, tolower, cols = .c(AGR, MIN)) %>% head(2)

## --------------------------------------------------------------------------------------------------
setrename(GGDC10S, AGR = Agriculture, MIN = Mining)
head(GGDC10S, 2)
rm(GGDC10S)

## --------------------------------------------------------------------------------------------------
microbenchmark(standard = tfm(gv(wlddev, 9:12), ODA_GDP = ODA/PCGDP),
               piped = get_vars(wlddev, 9:12) %>% ftransform(ODA_GDP = ODA/PCGDP))

## --------------------------------------------------------------------------------------------------
microbenchmark(na_omit(wlddev, na.attr = TRUE), na.omit(wlddev))

## --------------------------------------------------------------------------------------------------
na_omit(wlddev, cols = .c(PCGDP, LIFEEX)) %>% head(2)
# only removing missing data from numeric columns -> same and slightly faster than na_omit(wlddev) 
na_omit(wlddev, cols = is.numeric) %>% head(2)

## --------------------------------------------------------------------------------------------------
funique(GGDC10S$Variable)              # Unique values in order of appearance
funique(GGDC10S$Variable, sort = TRUE) # Sorted unique values

# If all values/rows are unique, the original data is returned (no copy)
identical(funique(GGDC10S), GGDC10S)

# Can remove duplicate rows by a subset of columns
funique(GGDC10S, cols = .c(Country, Variable)) %>% ss(1:2, 1:8)
funique(GGDC10S, cols = .c(Country, Variable), sort = TRUE) %>% ss(1:2, 1:8)

## --------------------------------------------------------------------------------------------------
# Efficient replacing missing values with NA
microbenchmark(replace_NA(GGDC10S, 0))

# Adding log-transformed sectoral data: Some NaN and Inf values generated
add_vars(GGDC10S, 6:16*2-5) <- fselect(GGDC10S, AGR:SUM) %>% lapply(log) %>% replace_Inf %>% add_stub("log.") 
head(GGDC10S, 2)
rm(GGDC10S)

## --------------------------------------------------------------------------------------------------
month.name
recode_char(month.name, ber = "C", "^J" = "A", default = "B", regex = TRUE)

## --------------------------------------------------------------------------------------------------
# replace all values below 2 and above 100 with NA
replace_outliers(mtcars, c(2, 100)) %>% head(3)        

# replace all value smaller than 2 with NA
replace_outliers(mtcars, 2, single.limit = "min") %>% head(3)

# replace all value larger than 100 with NA
replace_outliers(mtcars, 100, single.limit = "max") %>% head(3)

# replace all values above or below 3 column-standard-deviations from the column-mean with NA
replace_outliers(mtcars, 3) %>% tail(3)                        
                                                    

## --------------------------------------------------------------------------------------------------
str(EuStockMarkets)
# Efficient Conversion of data frames and matrices to data.table
microbenchmark(qDT(wlddev), qDT(EuStockMarkets), as.data.table(wlddev), as.data.frame(EuStockMarkets))

# Converting a time series to data.frame
head(qDF(AirPassengers))

## --------------------------------------------------------------------------------------------------
# This saves the row-names in a column named 'car'
head(qDT(mtcars, "car"))

N_distinct <- fNdistinct(GGDC10S)
N_distinct
# Converting a vector to data.frame, saving names
head(qDF(N_distinct, "variable"))

## --------------------------------------------------------------------------------------------------
# This converts the matrix to a list of 1860 row-vectors of length 4.
microbenchmark(mrtl(EuStockMarkets))

## --------------------------------------------------------------------------------------------------
microbenchmark(rowSums(qM(mtcars)), rowSums(mtcars))

## --------------------------------------------------------------------------------------------------
# Converting from character
str(wlddev$country)
fNdistinct(wlddev$country)
microbenchmark(qF(wlddev$country), as.factor(wlddev$country))

# Converting from numeric
str(wlddev$PCGDP)
fNdistinct(wlddev$PCGDP)
microbenchmark(qF(wlddev$PCGDP), as.factor(wlddev$PCGDP))


## ----eval=FALSE------------------------------------------------------------------------------------
#  FUN(x, g = NULL, [w = NULL,] TRA = NULL, [na.rm = TRUE,] use.g.names = TRUE, drop = TRUE)
#  

## --------------------------------------------------------------------------------------------------
fmean(mtcars$mpg) # Vector

fmean(mtcars)

fmean(mtcars, drop = FALSE)  # This returns a 1-row data-frame

m <- qM(mtcars) # Generate matrix
fmean(m)

fmean(m, drop = FALSE)  # This returns a 1-row matrix

## --------------------------------------------------------------------------------------------------
weights <- abs(rnorm(fnrow(mtcars))) # fnrow is a bit faster for data frames

fmean(mtcars, w = weights) # Weighted mean
fmedian(mtcars, w = weights) # Weighted median
fsd(mtcars, w = weights) # Frequency-weighted standard deviation
fmode(mtcars, w = weights) # Weighted statistical mode (i.e. the value with the largest sum of weights)

## --------------------------------------------------------------------------------------------------
fmean(mtcars, mtcars$cyl)

fmean(mtcars, fselect(mtcars, cyl, vs, am))

# Getting column indices 
ind <- fselect(mtcars, cyl, vs, am, return = "indices")
fmean(get_vars(mtcars, -ind), get_vars(mtcars, ind))  

## --------------------------------------------------------------------------------------------------
# This creates a factor, na.exclude = FALSE attaches a class 'na.included'
f <- qF(mtcars$cyl, na.exclude = FALSE)
# The 'na.included' attribute skips a missing value check on this factor
attributes(f)
# Saving data without grouping columns
dat <- get_vars(mtcars, -ind)
# Grouped standard-deviation
fsd(dat, f)

# Without option na.exclude = FALSE, anyNA needs to be called on the factor (noticeable on larger data).
f2 <- qF(mtcars$cyl)
microbenchmark(fsd(dat, f), fsd(dat, f2))

## --------------------------------------------------------------------------------------------------
# This creates a 'GRP' object. 
g <- GRP(mtcars, ~ cyl + vs + am) # Using the formula interface, could also use c("cyl","vs","am") or c(2,8:9)
str(g)

## --------------------------------------------------------------------------------------------------
print(g)
plot(g)

## --------------------------------------------------------------------------------------------------
fsd(dat, g)

# Grouped computation with and without prior grouping
microbenchmark(fsd(dat, g), fsd(dat, get_vars(mtcars, ind)))

## --------------------------------------------------------------------------------------------------
gmtcars <- fgroup_by(mtcars, cyl, vs, am)
fmedian(gmtcars)

head(fgroup_vars(gmtcars))

fmedian(gmtcars, keep.group_vars = FALSE)

## --------------------------------------------------------------------------------------------------
# Standard evaluation
dat <- get_vars(mtcars, c("mpg", "disp"))
add_vars(g[["groups"]],
         add_stub(fmean(dat, g, use.g.names = FALSE), "mean_"),
         add_stub(fsd(dat, g, use.g.names = FALSE), "sd_"),
         add_stub(fmin(dat, g, use.g.names = FALSE), "min_"),
         add_stub(fmax(dat, g, use.g.names = FALSE), "max_"))

# Non-Standard evaluation
fgroup_by(mtcars, cyl, vs, am) %>% fselect(mpg, disp) %>% {
  add_vars(fgroup_vars(., "unique"),
           fmean(., keep.group_vars = FALSE) %>% add_stub("mean_"),
           fsd(., keep.group_vars = FALSE) %>% add_stub("sd_"),
           fmin(., keep.group_vars = FALSE) %>% add_stub("min_"),
           fmax(., keep.group_vars = FALSE) %>% add_stub("max_"))
}

## --------------------------------------------------------------------------------------------------
# Grouped and weighted mean and sd and grouped min and max
add_vars(g[["groups"]],
         add_stub(fmean(dat, g, weights, use.g.names = FALSE), "w_mean_"),
         add_stub(fsd(dat, g, weights, use.g.names = FALSE), "w_sd_"),
         add_stub(fmin(dat, g, use.g.names = FALSE), "min_"),
         add_stub(fmax(dat, g, use.g.names = FALSE), "max_"))

# Binding and reordering columns in a single step: Add columns in specific positions
add_vars(g[["groups"]],
         add_stub(fmean(dat, g, weights, use.g.names = FALSE), "w_mean_"),
         add_stub(fsd(dat, g, weights, use.g.names = FALSE), "w_sd_"),
         add_stub(fmin(dat, g, use.g.names = FALSE), "min_"),
         add_stub(fmax(dat, g, use.g.names = FALSE), "max_"),
         pos = c(4,8,5,9,6,10,7,11))


## --------------------------------------------------------------------------------------------------
microbenchmark(call = add_vars(g[["groups"]],
         add_stub(fmean(dat, g, weights, use.g.names = FALSE), "w_mean_"),
         add_stub(fsd(dat, g, weights, use.g.names = FALSE), "w_sd_"),
         add_stub(fmin(dat, g, use.g.names = FALSE), "min_"),
         add_stub(fmax(dat, g, use.g.names = FALSE), "max_")))

## --------------------------------------------------------------------------------------------------
head(add_vars(get_vars(mtcars, ind),
              add_stub(fmean(dat, g, weights, "-"), "w_demean_"), # This calculates weighted group means and uses them to demean the data
              add_stub(fsd(dat, g, weights, "/"), "w_scale_"),    # This calculates weighted group sd's and uses them to scale the data
              add_stub(fmin(dat, g, "replace"), "min_"),          # This replaces all observations by their group-minimum
              add_stub(fmax(dat, g, "replace"), "max_")))         # This replaces all observations by their group-maximum

## --------------------------------------------------------------------------------------------------
# This defines the positions where we want to add these columns
pos <- c(2,8,3,9,4,10,5,11)

add_vars(mtcars, pos) <- c(add_stub(fmean(dat, g, weights, "-"), "w_demean_"),
                           add_stub(fsd(dat, g, weights, "/"), "w_scale_"),
                           add_stub(fmin(dat, g, "replace"), "min_"),
                           add_stub(fmax(dat, g, "replace"), "max_"))
head(mtcars)
rm(mtcars)

## --------------------------------------------------------------------------------------------------
# 2 different grouped and weighted computations (mutate operations) performed in one call
settransform(mtcars, carb_dwmed_cyl = fmedian(carb, cyl, weights, "-"),
                     carb_wsd_vs_am = fsd(carb, list(vs, am), weights, "replace"))

# Multivariate
settransform(mtcars, c(fmedian(list(carb_dwmed_cyl = carb, mpg_dwmed_cyl = mpg), cyl, weights, "-"),
                      fsd(list(carb_wsd_vs_am = carb, mpg_wsd_vs_am = mpg), list(vs, am), weights, "replace")))

# Nested (Computing the weighted 3rd quartile of mpg, grouped by cyl and carb being greater than it's weighted median, grouped by vs)
settransform(mtcars, 
 mpg_gwQ3_cyl = fnth(mpg, 0.75, list(cyl, carb > fmedian(carb, vs, weights, 1L)), weights, 1L))

head(mtcars)
rm(mtcars)

## --------------------------------------------------------------------------------------------------
collap(mtcars, mpg + disp ~ cyl + vs + am, list(fmean, fsd, fmin, fmax), w = weights, keep.col.order = FALSE)

## --------------------------------------------------------------------------------------------------
head(wlddev)

## --------------------------------------------------------------------------------------------------
head(collap(wlddev, ~ iso3c + decade))

## ----eval=FALSE------------------------------------------------------------------------------------
#  collap(X, by, FUN = fmean, catFUN = fmode, cols = NULL, w = NULL, wFUN = fsum,
#         custom = NULL, keep.by = TRUE, keep.w = TRUE, keep.col.order = TRUE,
#         sort.row = TRUE, parallel = FALSE, mc.cores = 1L,
#         return = c("wide","list","long","long_dupl"), give.names = "auto") # , ...

## --------------------------------------------------------------------------------------------------
# Same as collap(wlddev, ~ iso3c + decade, cols = 9:12)
head(collap(wlddev, PCGDP + LIFEEX + GINI + ODA ~ iso3c + decade))

## --------------------------------------------------------------------------------------------------
head(collap(wlddev, ~ iso3c + decade, list(fmean, fmedian, fsd), cols = 9:12))

## --------------------------------------------------------------------------------------------------
head(collap(wlddev, ~ iso3c + decade, list(fmean, fmedian, fsd), cols = 9:12, return = "long"))

## --------------------------------------------------------------------------------------------------
head(collap(wlddev, ~ iso3c + decade,
            custom = list(fmean = 9:12, fsd = 9:12,
                          ffirst = c("country","region","income"),
                          flast = c("year","date"),
                          fmode = "OECD")))

## --------------------------------------------------------------------------------------------------
dapply(mtcars, median)

dapply(mtcars, median, MARGIN = 1)

dapply(mtcars, quantile)

head(dapply(mtcars, quantile, MARGIN = 1))

head(dapply(mtcars, log)) # This is considerably more efficient than log(mtcars)

## --------------------------------------------------------------------------------------------------
is.data.frame(dapply(mtcars, log))
is.matrix(dapply(m, log))

## --------------------------------------------------------------------------------------------------
identical(log(m), dapply(mtcars, log, return = "matrix"))
identical(dapply(mtcars, log), dapply(m, log, return = "data.frame"))

## --------------------------------------------------------------------------------------------------
v <- iris$Sepal.Length   # A numeric vector
f <- iris$Species        # A factor

## default vector method
BY(v, f, sum)                          # Sum by species, about 2x faster than tapply(v, f, sum)

BY(v, f, quantile)                     # Species quantiles: by default stacked

BY(v, f, quantile, expand.wide = TRUE) # Wide format

## matrix method
miris <- qM(num_vars(iris))
BY(miris, f, sum)                          # Also returns as matrix

head(BY(miris, f, quantile))

BY(miris, f, quantile, expand.wide = TRUE)[,1:5]

BY(miris, f, quantile, expand.wide = TRUE, return = "list")[1:2] # list of matrices

## data.frame method
BY(num_vars(iris), f, sum)             # Also returns a data.frame etc...

## Conversions
identical(BY(num_vars(iris), f, sum), BY(miris, f, sum, return = "data.frame"))
identical(BY(miris, f, sum), BY(num_vars(iris), f, sum, return = "matrix"))

## --------------------------------------------------------------------------------------------------
# Note: All examples below generalize to vectors or data frames
stats <- fmean(miris)            # Saving stats
head(TRA(miris, stats, "-"), 3)  # Centering. Same as sweep(miris, 2, stats, "-")

## --------------------------------------------------------------------------------------------------
# 3 ways of centering data
all_identical(TRA(miris, fmean(miris), "-"),
              fmean(miris, TRA = "-"),   # better for any operation if the stats are not needed
              fwithin(miris))            # fastest, fwithin is discussed in section 6.5

# Simple replacing [same as fmean(miris, TRA = "replace") or fbetween(miris)]
head(TRA(miris, fmean(miris), "replace"), 3)

# Simple scaling [same as fsd(miris, TRA = "/")]
head(TRA(miris, fsd(miris), "/"), 3)

## --------------------------------------------------------------------------------------------------
# Grouped centering [same as fmean(miris, f, TRA = "-") or fwithin(m, f)]
head(TRA(miris, fmean(miris, f), "-", f), 3)

# Grouped replacing [same as fmean(m, f, TRA = "replace") or fbetween(m, f)]
head(TRA(miris, fmean(miris, f), "replace", f), 3)

# Groupwise percentages [same as fsum(m, f, TRA = "%")]
head(TRA(miris, fsum(miris, f), "%", f), 3)

## --------------------------------------------------------------------------------------------------
# Grouped centering on the overall mean [same as fmean(m, f, TRA = "-+") or fwithin(m, f, mean = "overall.mean")]
head(TRA(miris, fmean(miris, f), "-+", f), 3)
head(TRA(TRA(miris, fmean(miris, f), "-", f), fmean(miris), "+"), 3) # Same thing done manually!

# This group-centers data on the overall median!
head(fmedian(miris, f, TRA = "-+"), 3)

## --------------------------------------------------------------------------------------------------
# fscale doesn't rename columns
head(fscale(mtcars),2)

# By default adds a prefix
head(STD(mtcars),2)

# See that is works
qsu(STD(mtcars))

# We can also scale and center to a different mean and standard deviation:
t(qsu(fscale(mtcars, mean = 5, sd = 3))[,c("Mean","SD")])

# Or not center at all. In that case scaling is mean-preserving, in contrast to fsd(mtcars, TRA = "/")
t(qsu(fscale(mtcars, mean = FALSE, sd = 3))[,c("Mean","SD")])

## --------------------------------------------------------------------------------------------------
head(GGDC10S)

## --------------------------------------------------------------------------------------------------
# Standardizing Sectors by Variable and Country
STD_GGDC10S <- STD(GGDC10S, ~ Variable + Country, cols = 6:16)
head(STD_GGDC10S)

# Correlating Standardized Value-Added across countries
pwcor(fsubset(STD_GGDC10S, Variable == "VA", STD.AGR:STD.SUM))

## --------------------------------------------------------------------------------------------------
## Simple centering and averaging
head(fbetween(mtcars$mpg))

head(fwithin(mtcars$mpg))

all.equal(fbetween(mtcars) + fwithin(mtcars), mtcars)

## Groupwise centering and averaging
head(fbetween(mtcars$mpg, mtcars$cyl))

head(fwithin(mtcars$mpg, mtcars$cyl))

all.equal(fbetween(mtcars, mtcars$cyl) + fwithin(mtcars, mtcars$cyl), mtcars)

## --------------------------------------------------------------------------------------------------
head(W(wlddev, ~ iso3c, cols = 9:12))        # Center the 4 series in this dataset by country
head(add_vars(get_vars(wlddev, "iso3c"),     # Same thing done manually using fwithin...
     add_stub(fwithin(get_vars(wlddev, 9:12), wlddev$iso3c), "W.")))

## ---- fig.height=4---------------------------------------------------------------------------------
# This replaces missing values with the group-mean: Same as fmean(x, g, TRA = "replace_fill")
head(B(wlddev, ~ iso3c, cols = 9:12, fill = TRUE))

# This adds back the overall mean after subtracting out group means: Same as fmean(x, g, TRA = "-+")
head(W(wlddev, ~ iso3c, cols = 9:12, mean = "overall.mean"))
# Note: This is not just slightly faster than fmean(x, g, TRA = "-+"), but if weights are used, fmean(x, g, w, "-+")
# gives a wrong result: It subtracts weighted group means but then centers on the frequency-weighted average of those group means,
# whereas fwithin(x, g, w, mean = "overall.mean") will also center on the properly weighted overall mean.

# Visual demonstration of centering on the overall mean vs. simple centering
oldpar <- par(mfrow = c(1,3))
plot(iris[1:2], col = iris$Species, main = "Raw Data")                       # Raw data
plot(W(iris, ~ Species)[2:3], col = iris$Species, main = "Simple Centering") # Simple centering
plot(W(iris, ~ Species, mean = "overall.mean")[2:3], col = iris$Species,    # Centering on overall mean: Preserves level of data
     main = "Added Overall Mean")
par(oldpar)

## --------------------------------------------------------------------------------------------------
# When using operators in formulas, we need to remove missing values beforehand to obtain the same results as a Fixed-Effects package
data <- na_omit(fselect(wlddev, iso3c, year, PCGDP, LIFEEX))

# classical lm() -> iso3c is a factor, creates a matrix of 200+ country dummies.
coef(lm(PCGDP ~ LIFEEX + iso3c, data))[1:2]

# Centering each variable individually
coef(lm(W(PCGDP,iso3c) ~ W(LIFEEX,iso3c), data))

# Centering the data
coef(lm(W.PCGDP ~ W.LIFEEX, W(data, PCGDP + LIFEEX ~ iso3c)))

# Adding the overall mean back to the data only changes the intercept
coef(lm(W.PCGDP ~ W.LIFEEX, W(data, PCGDP + LIFEEX  ~ iso3c, mean = "overall.mean")))

# Procedure suggested by Mundlak (1978) - controlling for group averages instead of demeaning
coef(lm(PCGDP ~ LIFEEX + B(LIFEEX,iso3c), data))

## --------------------------------------------------------------------------------------------------
data$year <- qF(data$year, na.exclude = FALSE) # the country code (iso3c) is already a factor

# classical lm() -> creates a matrix of 196 country dummies and 56 year dummies
coef(lm(PCGDP ~ LIFEEX + iso3c + year, data))[1:2]

# Centering each variable individually
coef(lm(HDW(PCGDP, list(iso3c, year)) ~ HDW(LIFEEX, list(iso3c, year)), data))

# Centering the entire data
coef(lm(HDW.PCGDP ~ HDW.LIFEEX, HDW(data, PCGDP + LIFEEX ~ iso3c + year)))

# Procedure suggested by Mundlak (1978) - controlling for averages instead of demeaning
coef(lm(PCGDP ~ LIFEEX + HDB(LIFEEX, list(iso3c, year)), data))

## --------------------------------------------------------------------------------------------------
# The syntax is fFtest(y, exc, X, full.df = TRUE). 'exc' are exclusion restrictions.
# full.df = TRUE means count degrees of freedom in the same way as if dummies were created
data %$% fFtest(PCGDP, year, list(LIFEEX, iso3c))

## --------------------------------------------------------------------------------------------------
wlddev$year <- as.numeric(wlddev$year)

# classical lm() -> full country-year interaction, -> 200+ country dummies, 200+ trends, year and ODA
coef(lm(PCGDP ~ LIFEEX + iso3c*year + ODA, wlddev))[1:2]

# Same using HDW -> However lde::demeanlist is not nearly as fast on interactions..
coef(lm(HDW.PCGDP ~ HDW.LIFEEX, HDW(wlddev, PCGDP + LIFEEX ~ iso3c*year + ODA)))

# example of a simple continuous problem
head(HDW(iris[1:2], iris[3:4]))

# May include factors..
head(HDW(iris[1:2], iris[3:5]))

## --------------------------------------------------------------------------------------------------
mts <- psmat(wlddev, PCGDP ~ iso3c, ~ year)
str(mts)
plot(log10(mts), main = paste("Log10", vlabels(wlddev$PCGDP)), xlab = "Year")

## --------------------------------------------------------------------------------------------------
# Get panel series array
psar <- psmat(wlddev, ~ iso3c, ~ year, cols = 9:12)
str(psar)
plot(psar)

## ---- fig.height=7---------------------------------------------------------------------------------
# Plot array of Panel Series aggregated by region:
collap(wlddev, ~region + year, cols = 9:12) %>% psmat(~region, ~year) %>%
  plot(legend = TRUE, labs = vlabels(wlddev)[9:12])

## --------------------------------------------------------------------------------------------------
# This gives list of ps-matrices
psml <- psmat(wlddev, ~ iso3c, ~ year, 9:12, array = FALSE)
str(psml, give.attr = FALSE)

# Using unlist2d, can generate a data.frame
head(unlist2d(psml, idcols = "Variable", row.names = "Country"))[1:10]

## --------------------------------------------------------------------------------------------------
# Panel-ACF of GDP per Capita
psacf(wlddev, PCGDP ~ iso3c, ~year)
# Panel-Partial-ACF of GDP per Capia
pspacf(wlddev, PCGDP ~ iso3c, ~year)
# Panel- Cross-Correlation function of GDP per Capia and Life-Expectancy
psccf(wlddev$PCGDP, wlddev$LIFEEX, wlddev$iso3c, wlddev$year)
# Multivariate Panel-auto and cross-correlation function of 3 variables:
psacf(wlddev, PCGDP + LIFEEX + ODA ~ iso3c, ~year)

## --------------------------------------------------------------------------------------------------
# 1 lag
L(AirPassengers)

# 3 identical ways of computing 1 lag
all_identical(flag(AirPassengers), L(AirPassengers), F(AirPassengers,-1))

# 1 lead and 3 lags - output as matrix
head(L(AirPassengers, -1:3))

# ... this is still a time series object:
attributes(L(AirPassengers, -1:3))

## --------------------------------------------------------------------------------------------------
str(EuStockMarkets)

# Data is recorded on 260 days per year, 1991-1999
tsp(EuStockMarkets)
freq <- frequency(EuStockMarkets)

# There is some obvious seasonality
plot(stl(EuStockMarkets[,"DAX"], freq))

# 1 annual lead and 1 annual lag
head(L(EuStockMarkets, -1:1*freq))

# DAX regressed on it's own 2 annual lags and the lags of the other indicators
summary(lm(DAX ~., data = L(EuStockMarkets, 0:2*freq)))

## ---- message=TRUE---------------------------------------------------------------------------------
# This lags all 4 series
head(L(wlddev, 1, ~iso3c, ~year, cols = 9:12))

# Without t: Works here because data is ordered, but gives a message
head(L(wlddev, 1, ~iso3c, cols = 9:12))

# 1 lead and 2 lags of GDP per Capita & Life Expectancy
head(L(wlddev, -1:2, PCGDP + LIFEEX ~ iso3c, ~year))

## --------------------------------------------------------------------------------------------------
g <- c(1,1,1,2,2,2)
tryCatch(flag(1:6, 1, g, t = c(1,2,3,1,2,2)),
         error = function(e) e)
tryCatch(flag(1:6, 1, g, t = c(1,2,3,1,2,4)),
         error = function(e) e)

## --------------------------------------------------------------------------------------------------
# Different ways of regressing GDP on it's lags and life-Expectancy and it's lags

# 1 - Precomputing lags
summary(lm(PCGDP ~ ., L(wlddev, 0:2, PCGDP + LIFEEX ~ iso3c, ~ year, keep.ids = FALSE)))

# 2 - Ad-hoc computation in lm formula
summary(lm(PCGDP ~ L(PCGDP,1:2,iso3c,year) + L(LIFEEX,0:2,iso3c,year), wlddev))

# 3 - Precomputing panel-identifiers
g = qF(wlddev$iso3c, na.exclude = FALSE)
t = qF(wlddev$year, na.exclude = FALSE)
summary(lm(PCGDP ~ L(PCGDP,1:2,g,t) + L(LIFEEX,0:2,g,t), wlddev))

## --------------------------------------------------------------------------------------------------
plot(stl(AirPassengers, "periodic"))

## --------------------------------------------------------------------------------------------------
f <- qF(cycle(AirPassengers))
fFtest(fgrowth(AirPassengers), f)

## --------------------------------------------------------------------------------------------------
G(AirPassengers, c(0, 1, 12)) %>% cbind(W.G1 = W(G(AirPassengers), f)) %>% 
  plot(main = "Growth Rate of Airpassengers")

## --------------------------------------------------------------------------------------------------
plot(D(AirPassengers, c(1,12), 1:2))

## --------------------------------------------------------------------------------------------------
# sequence of leaded/lagged and iterated differences
y = 1:10
D(y, -2:2, 1:3)

## --------------------------------------------------------------------------------------------------
g = rep(1:2, each = 5)
t = rep(1:5, 2)

D(y, -2:2, 1:2, g, t)

## --------------------------------------------------------------------------------------------------
L(D(y, 0:2, 1:2, g, t), 0:1, g, t)

## --------------------------------------------------------------------------------------------------
head(G(GGDC10S, 1, 1, ~ Variable + Country, ~ Year, cols = 6:10))

## --------------------------------------------------------------------------------------------------
summary(lm(G(PCGDP,10,1,iso3c,year) ~
             L(PCGDP,10,iso3c,year) +
             G(LIFEEX,10,1,iso3c,year), data = wlddev))

## --------------------------------------------------------------------------------------------------
moddat <- HDW(L(G(wlddev, c(0, 10), 1, ~iso3c, ~year, 9:10), c(0, 10), ~iso3c, ~year), ~iso3c + qF(year))[-c(1,5)]
summary(lm(HDW.L10G1.PCGDP ~. , moddat))

## --------------------------------------------------------------------------------------------------
pwlddev <- plm::pdata.frame(wlddev, index = c("iso3c", "year"))
moddat <- HDW(L(G(pwlddev, c(0, 10), 1, 9:10), c(0, 10)))[-c(1,5)]
summary(lm(HDW.L10G1.PCGDP ~. , moddat))

## ---- warning=FALSE, message=FALSE-----------------------------------------------------------------
library(vars)
# The 6 most important non-government sectors (see section 1)
sec <- c("AGR","MAN","WRT","CON","TRA","FIRE")
# This creates a data.frame containing the value added of the 6 most important non-government sectors
data <- na_omit(fsubset(GGDC10S, Variable == "VA", c("Country","Year", sec)), cols = sec)
# Let's look at the log VA in agriculture across countries:
AGRmat <- log(psmat(data, AGR ~ Country, ~ Year, transpose = TRUE))   # Converting to panel series matrix
plot(AGRmat)

## --------------------------------------------------------------------------------------------------
# Subtracting a country specific cubic growth trend
AGRmat <- dapply(AGRmat, fHDwithin, poly(seq_row(AGRmat), 3), fill = TRUE)

plot(AGRmat)

## --------------------------------------------------------------------------------------------------
# Standardizing the cubic log-detrended data
AGRmat <- fscale(AGRmat)
plot(AGRmat)

## ---- fig.height=7---------------------------------------------------------------------------------
# Taking logs
get_vars(data, 3:8) <- dapply(get_vars(data, 3:8), log)
# Iteratively projecting out country FE and cubic trends from complete cases (still very slow)
get_vars(data, 3:8) <- HDW(data, ~ qF(Country)*poly(Year, 3), fill = TRUE, eps = 1e-05)
# Scaling
get_vars(data, 3:8) <- STD(data, ~ Country, cols = 3:8, keep.by = FALSE)

# Check the plot
plot(psmat(data, ~Country, ~Year))

## --------------------------------------------------------------------------------------------------
# This adds one lag of all series to the data
add_vars(data) <- L(data, 1, ~ Country, ~ Year, keep.ids = FALSE)
# This removes missing values from all but the first row and drops identifier columns (vars is made for time series without gaps)
data <- rbind(ss(data, 1, -(1:2)), na_omit(ss(data, -1, -(1:2))))
head(data)

## --------------------------------------------------------------------------------------------------
# saving the names of the 6 sectors
nam <- names(data)[1:6]

pVAR <- list(varresult = setNames(lapply(seq_len(6), function(i)    # list of 6 lm's each regressing
               lm(as.formula(paste0(nam[i], "~ -1 + . ")),          # the sector on all lags of
               get_vars(data, c(i, 7:fncol(data))))), nam),         # itself and other sectors, removing the missing first row
             datamat = ss(data, -1),                                # The full data containing levels and lags of the sectors, removing the missing first row
             y = do.call(cbind, get_vars(data, 1:6)),               # Only the levels data as matrix
             type = "none",                                         # No constant or tend term: We harmonized the data already
             p = 1,                                                 # The lag-order
             K = 6,                                                 # The number of variables
             obs = fnrow(data)-1,                                   # The number of non-missing obs
             totobs = fnrow(data),                                  # The total number of obs
             restrictions = NULL,
             call = quote(VAR(y = data)))

class(pVAR) <- "varest"

## --------------------------------------------------------------------------------------------------
serial.test(pVAR)

## --------------------------------------------------------------------------------------------------
# This computes the pairwise correlations between standardized sectoral growth rates across countries
corr <- fsubset(GGDC10S, Variable == "VA") %>%   # Subset rows: Only VA
           fgroup_by(Country) %>%                # Group by country
                get_vars(sec) %>%                # Select the 6 sectors
                   fgrowth %>%                   # Compute Sectoral growth rates (a time-variable can be passed, but not necessary here as the data is ordered)
                      fscale %>%                 # Scale and center (i.e. standardize)
                         pwcor                   # Compute Pairwise correlations

corr

# We need to impose K*(K-1)/2 = 15 (with K = 6 variables) restrictions for identification
corr[corr <= sort(corr)[15]] <- 0
corr

# The rest is unknown (i.e. will be estimated)
corr[corr > 0 & corr < 1] <- NA

# Using a diagonal shock vcov matrix (standard assumption for SVAR)
Bmat <- diag(6)
diag(Bmat) <- NA


# This estimates the Panel-SVAR using Maximum Likelihood:
pSVAR <- SVAR(pVAR, Amat = unclass(corr), Bmat = Bmat, estmethod = "direct")
pSVAR

## --------------------------------------------------------------------------------------------------
# psVAR$var$varresult is a list containing the 6 linear models fitted above, it is not displayed in full here.
str(pSVAR, give.attr = FALSE, max.level = 3)

## --------------------------------------------------------------------------------------------------
# The list-tree of this object has 5 levels of nesting
ldepth(pSVAR)

# This data has a depth of 1, thus this dataset does not contain list-columns
ldepth(data)

## --------------------------------------------------------------------------------------------------
# Is this object composed only of atomic elements e.g. can it be unlisted?
is.unlistable(pSVAR)

## --------------------------------------------------------------------------------------------------
# Does this object contain an element with "fitted" in its name?
has_elem(pSVAR, "fitted", regex = TRUE)

# Does this object contain an element with "residuals" in its name?
has_elem(pSVAR, "residuals", regex = TRUE)

## --------------------------------------------------------------------------------------------------
# Is there a matrix stored in this object?
has_elem(pSVAR, is.matrix)

## --------------------------------------------------------------------------------------------------
# This is the path to the residuals from a single equation
str(pSVAR$var$varresult$STD.HDW.AGR$residuals)

# get_elem gets the residuals from all 6 equations and puts them in a top-level list
resid <- get_elem(pSVAR, "residuals")
str(resid, give.attr = FALSE)

# Quick conversion to matrix and plotting
plot.ts(qM(resid), main = "Panel-VAR Residuals")

## --------------------------------------------------------------------------------------------------
# Regular expression search and retrieval of fitted values
plot.ts(qM(get_elem(pSVAR, "^fi", regex = TRUE)), main = "Panel-VAR Fitted Values")

## --------------------------------------------------------------------------------------------------
# This computes orthogonalized impulse response functions
pIRF <- irf(pSVAR)
# This computes the forecast error variance decompositions
pFEVD <- fevd(pSVAR)

## --------------------------------------------------------------------------------------------------
# See the structure of a vars IRF object:
str(pIRF, give.attr = FALSE)

## --------------------------------------------------------------------------------------------------
# Pool-out top-level atomic elements in the list
str(atomic_elem(pIRF))

## --------------------------------------------------------------------------------------------------
# Computing the cumulative impact after 10 periods
list_elem(pIRF) %>%                            # Pull out the sublist elements containing the IRF coefficients + CI's
  rapply2d(function(x) round(fsum(x), 2)) %>%  # Recursively apply the column-sums to coefficient matrices (could also use colSums)
  unlist2d(c("Type", "Impulse"))               # Recursively row-bind the result to a data.frame and add identifier columns

## --------------------------------------------------------------------------------------------------
# This binds the matrices after adding integer row-names to them to a data.table

data <- pIRF$irf %>%                      # Get only the coefficient matrices, discard the confidence bounds
           unlist2d(idcols = "Impulse",   # Recursive unlisting to data.table creating a factor id-column
                    row.names = "Time",   # and saving generated rownames in a variable called 'Time'
                    id.factor = TRUE,     # -> Create Id column ('Impulse') as factor
                    DT = TRUE)            # -> Output as data.table (default is data.frame)

head(data, 3)

data <- melt(data, 1:2)                   # Using data.table's melt
head(data, 3)

# Here comes the plot:
  ggplot(data, aes(x = Time, y = value, color = Impulse)) +
    geom_line(size = I(1)) + geom_hline(yintercept = 0) +
    labs(y = NULL, title = "Orthogonal Impulse Response Functions") +
    scale_color_manual(values = rainbow(6)) +
    facet_wrap(~ variable) +
    theme_light(base_size = 14) +
    scale_x_continuous(breaks = scales::pretty_breaks(n=7), expand = c(0, 0))+
    scale_y_continuous(breaks = scales::pretty_breaks(n=7), expand = c(0, 0))+
    theme(axis.text = element_text(colour = "black"),
      plot.title = element_text(hjust = 0.5),
      strip.background = element_rect(fill = "white", colour = NA),
      strip.text = element_text(face = "bold", colour = "grey30"),
      axis.ticks = element_line(colour = "black"),
      panel.border = element_rect(colour = "black"))


## --------------------------------------------------------------------------------------------------
data <- unlist2d(pFEVD, idcols = "variable", row.names = "Time", id.factor = TRUE, DT = TRUE) %>% 
            melt(c("variable", "Time"), variable.name = "Sector") 
head(data, 3)

# Here comes the plot:
  ggplot(data, aes(x = Time, y = value, fill = Sector)) +
    geom_area(position = "fill", alpha = 0.8) +
    labs(y = NULL, title = "Forecast Error Variance Decompositions") +
    scale_fill_manual(values = rainbow(6)) +
    facet_wrap(~ variable) +
    theme_linedraw(base_size = 14) +
    scale_x_continuous(breaks = scales::pretty_breaks(n=7), expand = c(0, 0))+
    scale_y_continuous(breaks = scales::pretty_breaks(n=7), expand = c(0, 0))+
    theme(plot.title = element_text(hjust = 0.5),
      strip.background = element_rect(fill = "white", colour = NA),
      strip.text = element_text(face = "bold", colour = "grey30"))


## ---- echo=FALSE--------------------------------------------------------------
options(oldopts)

