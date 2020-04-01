## ---- echo = FALSE, message = FALSE, warning=FALSE-------------------------------------------------
library(vars)
library(dplyr)  # Needed because otherwise dplyr is loaded in benchmark chunk not run on CRAN !!
library(microbenchmark) # Same thing
library(collapse)
B <- collapse::B # making sure it masks vars::B by loading into GE
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

oldopts <- options(width = 101L)

X = mtcars[1:2]
by = mtcars$cyl

set.seed(101)

## --------------------------------------------------------------------------------------------------
library(collapse)

head(wlddev)

# The variables have "label" attributes. Use vlabels() to get and set labels
namlab(wlddev, class = TRUE)

# This counts the number of non-missing values, more in section 2
fNobs(wlddev)

# This counts the number of distinct values, more in section 2
fNdistinct(wlddev)

# The countries included:
cat(levels(wlddev$iso3c))

# use descr(wlddev) for a more detailed description of each variable

## --------------------------------------------------------------------------------------------------
qsu(wlddev, pid = ~ iso3c, cols = c(1,4,9:12), vlabels = TRUE, higher = TRUE)

## --------------------------------------------------------------------------------------------------
head(GGDC10S)

namlab(GGDC10S, class = TRUE)

fNobs(GGDC10S)

fNdistinct(GGDC10S)

# The countries included:
cat(funique(GGDC10S$Country, ordered = TRUE))

# use descr(GGDC10S) for a more detailed description of each variable

## --------------------------------------------------------------------------------------------------
# Converting data to percentages of overall VA / EMP
pGGDC10S <- sweep(GGDC10S[6:15], 1, GGDC10S$SUM, "/") * 100
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
dat <- qDT(GGDC10S)[Country == ctry]
dat <- cbind(get_vars(dat, c("Variable","Year")), 
             replace_outliers(sweep(get_vars(dat, 6:15), 1, dat$SUM, "/"), 0, NA, "min"))
dat$Variable <- Recode(dat$Variable,"VA"="Value Added Share","EMP"="Employment Share")
dat <- melt(dat, 1:2, variable.name = "Sector")

ggplot(aes(x = Year, y = value, fill = Sector), data = dat) +
  geom_area(position = "fill", alpha = 0.9) + labs(x = NULL, y = NULL) +
  theme_linedraw(base_size = 14) + facet_wrap( ~ Variable) +
  scale_fill_manual(values = sub("#00FF66FF", "#00CC66", rainbow(10))) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7), expand = c(0, 0)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0, 0),
                     labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 315, hjust = 0, margin = ggplot2::margin(t = 0)),
        strip.background = element_rect(colour = "grey20", fill = "grey20"),
        strip.text = element_text(face = "bold"))
}
# Plotting the structural transformation of Tannzania
plotGGDC("TZA")


## ----eval=FALSE------------------------------------------------------------------------------------
#  FUN(x, g = NULL, [w = NULL,] TRA = NULL, [na.rm = TRUE,] use.g.names = TRUE, drop = TRUE)
#  

## --------------------------------------------------------------------------------------------------
fmean(mtcars)

fmean(mtcars, drop = FALSE)  # This returns a 1-row data-frame

m <- qM(mtcars) # This quickly converts objects to matrices
fmean(m)

fmean(mtcars, drop = FALSE)  # This returns a 1-row matrix


## --------------------------------------------------------------------------------------------------
fmean(mtcars, mtcars$cyl)

fmean(mtcars, mtcars[c("cyl","vs","am")])

## --------------------------------------------------------------------------------------------------
# Getting column indices [same as match(c("cyl","vs","am"), names(mtcars)) but gives error if non-matched]
ind <- get_vars(mtcars, c("cyl","vs","am"), return = "indices")

# Subsetting columns with get_vars is 2x faster than [.data.frame
fmean(get_vars(mtcars, -ind), get_vars(mtcars, ind))

## --------------------------------------------------------------------------------------------------
# This creates a (ordered) factor, about 10x faster than as.factor(mtcars$cyl)
f <- qF(mtcars$cyl, na.exclude = FALSE)
str(f)

# This creates a 'GRP' object. Grouping is done via radix ordering in C (using data.table's forder function)
g <- GRP(mtcars, ~ cyl + vs + am) # Using the formula interface, could also use c("cyl","vs","am") or c(2,8:9)
g
plot(g)

## --------------------------------------------------------------------------------------------------
dat <- get_vars(mtcars, -ind)

# Grouped mean
fmean(dat, f)

# Grouped standard-deviation
fsd(dat, f)

fsd(dat, g)

## --------------------------------------------------------------------------------------------------
dat <- get_vars(mtcars, c("mpg", "disp"))

# add_stub is a collapse predicate to add a prefix (default) or postfix to column names
cbind(add_stub(fmean(dat, g), "mean_"),
      add_stub(fsd(dat, g), "sd_"), 
      add_stub(fmin(dat, g), "min_"),
      add_stub(fmax(dat, g), "max_"))

## --------------------------------------------------------------------------------------------------
# This generates a random vector of weights
weights <- abs(rnorm(nrow(mtcars)))

# Grouped and weighted mean and sd and grouped min and max, combined using add_vars
add_vars(g[["groups"]],
         add_stub(fmean(dat, g, weights, use.g.names = FALSE), "w_mean_"),
         add_stub(fsd(dat, g, weights, use.g.names = FALSE), "w_sd_"), 
         add_stub(fmin(dat, g, use.g.names = FALSE), "min_"),
         add_stub(fmax(dat, g, use.g.names = FALSE), "max_"))

## --------------------------------------------------------------------------------------------------
# Binding and reordering columns in a single step: Add columns in specific positions 
add_vars(g[["groups"]],
         add_stub(fmean(dat, g, weights, use.g.names = FALSE), "w_mean_"),
         add_stub(fsd(dat, g, weights, use.g.names = FALSE), "w_sd_"), 
         add_stub(fmin(dat, g, use.g.names = FALSE), "min_"),
         add_stub(fmax(dat, g, use.g.names = FALSE), "max_"), 
         pos = c(4,8,5,9,6,10,7,11))


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
collap(mtcars, mpg + disp ~ cyl + vs + am, list(fmean, fsd, fmin, fmax), keep.col.order = FALSE)

## --------------------------------------------------------------------------------------------------
head(wlddev)

## --------------------------------------------------------------------------------------------------
head(collap(wlddev, ~ iso3c + decade))

## ----eval=FALSE------------------------------------------------------------------------------------
#  collap(X, by, FUN = fmean, catFUN = fmode, cols = NULL, custom = NULL,
#         keep.by = TRUE, keep.col.order = TRUE, sort.row = TRUE,
#         parallel = FALSE, mc.cores = 1L,
#         return = c("wide","list","long","long_dupl"), give.names = "auto") # , ...

## --------------------------------------------------------------------------------------------------
head(collap(wlddev, ~ iso3c + decade, cols = 9:12))

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

## ---- eval=NCRAN-----------------------------------------------------------------------------------
# Creating a data.table with 10 columns and 1 mio. obs, including missing values
testdat <- na_insert(qDT(replicate(10, rnorm(1e6), simplify = FALSE)), prop = 0.1) # 10% missing
testdat[["g1"]] <- sample.int(1000, 1e6, replace = TRUE) # 1000 groups
testdat[["g2"]] <- sample.int(100, 1e6, replace = TRUE) # 100 groups

# The average group size is 10, there are about 100000 groups
GRP(testdat, ~ g1 + g2) 

# dplyr vs. data.table vs. collap (calling Fast Functions):
library(dplyr)

# Sum
system.time(testdat %>% group_by(g1,g2) %>% summarize_all(sum, na.rm = TRUE))
system.time(testdat[, lapply(.SD, sum, na.rm = TRUE), keyby = c("g1","g2")])
system.time(collap(testdat, ~ g1 + g2, fsum))

# Product
system.time(testdat %>% group_by(g1,g2) %>% summarize_all(prod, na.rm = TRUE))
system.time(testdat[, lapply(.SD, prod, na.rm = TRUE), keyby = c("g1","g2")])
system.time(collap(testdat, ~ g1 + g2, fprod))

# Mean
system.time(testdat %>% group_by(g1,g2) %>% summarize_all(mean.default, na.rm = TRUE)) 
system.time(testdat[, lapply(.SD, mean, na.rm = TRUE), keyby = c("g1","g2")])
system.time(collap(testdat, ~ g1 + g2))

# Weighted Mean
w <- abs(100*rnorm(1e6)) + 1 
testdat[["w"]] <- w
# Seems not possible with dplyr ...
system.time(testdat[, lapply(.SD, weighted.mean, w = w, na.rm = TRUE), keyby = c("g1","g2")])
system.time(collap(testdat, ~ g1 + g2, w = w))

# Maximum
system.time(testdat %>% group_by(g1,g2) %>% summarize_all(max, na.rm = TRUE))
system.time(testdat[, lapply(.SD, max, na.rm = TRUE), keyby = c("g1","g2")])
system.time(collap(testdat, ~ g1 + g2, fmax))

# Median
system.time(testdat %>% group_by(g1,g2) %>% summarize_all(median.default, na.rm = TRUE)) 
system.time(testdat[, lapply(.SD, median, na.rm = TRUE), keyby = c("g1","g2")])
system.time(collap(testdat, ~ g1 + g2, fmedian))

# Variance
system.time(testdat %>% group_by(g1,g2) %>% summarize_all(var, na.rm = TRUE)) 
system.time(testdat[, lapply(.SD, var, na.rm = TRUE), keyby = c("g1","g2")])
system.time(collap(testdat, ~ g1 + g2, fvar)) 
# Note: fvar implements a numerically stable online variance using Welfords Algorithm.

# Weighted Variance
# Don't know how to do this fast in dplyr or data.table. 
system.time(collap(testdat, ~ g1 + g2, fvar, w = w))

# Last value
system.time(testdat %>% group_by(g1,g2) %>% summarize_all(last))
system.time(testdat[, lapply(.SD, last), keyby = c("g1","g2")])
system.time(collap(testdat, ~ g1 + g2, flast, na.rm = FALSE)) 
# Note: collapse functions ffirst and flast by default also remove missing values i.e. take the first and last non-missing data point

# Mode
# Defining a mode function in base R and applying it by groups is very slow, no matter whether you use dplyr or data.table. 
# There are solutions suggested on stackoverflow on using chained operations in data.table to compute the mode, 
# but those I find rather arcane and they are also not very fast. 
system.time(collap(testdat, ~ g1 + g2, fmode)) 
# Note: This mode function uses index hashing in C++, it's a blast !

# Weighted Mode
system.time(collap(testdat, ~ g1 + g2, fmode, w = w))

# Number of Distinct Values
# No straightforward data.table solution.. 
system.time(testdat %>% group_by(g1,g2) %>% summarize_all(n_distinct, na.rm = TRUE))
system.time(collap(testdat, ~ g1 + g2, fNdistinct)) 

## ---- eval=NCRAN-----------------------------------------------------------------------------------
# 12000 obs in 1500 groups: A more typical case
GRP(wlddev, ~ iso3c + decade)

library(microbenchmark)
dtwlddev <- qDT(wlddev)
microbenchmark(dplyr = dtwlddev %>% group_by(iso3c,decade) %>% select_at(9:12) %>% summarise_all(sum, na.rm = TRUE),
               data.table = dtwlddev[, lapply(.SD, sum, na.rm = TRUE), by = c("iso3c","decade"), .SDcols = 9:12],
               collap = collap(dtwlddev, ~ iso3c + decade, fsum, cols = 9:12),
               fast_fun = fsum(get_vars(dtwlddev, 9:12), GRP(dtwlddev, ~ iso3c + decade), use.g.names = FALSE)) # We can gain a bit coding it manually

# Now going really small:
dtmtcars <- qDT(mtcars)
microbenchmark(dplyr = dtmtcars %>% group_by(cyl,vs,am) %>% summarise_all(sum, na.rm = TRUE),      # Large R overhead
               data.table = dtmtcars[, lapply(.SD, sum, na.rm = TRUE), by = c("cyl","vs","am")],   # Large R overhead
               collap = collap(dtmtcars, ~ cyl + vs + am, fsum),                                   # Now this is still quite efficient
               fast_fun = fsum(dtmtcars, GRP(dtmtcars, ~ cyl + vs + am), use.g.names = FALSE))     # And this is nearly the speed of a full C++ implementation


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
BY(num_vars(iris), f, sum)             # Also returns a data.fram etc...

## Conversions
identical(BY(num_vars(iris), f, sum), BY(miris, f, sum, return = "data.frame"))
identical(BY(miris, f, sum), BY(num_vars(iris), f, sum, return = "matrix"))

## --------------------------------------------------------------------------------------------------
# Note: All examples below generalize to vectors or data.frames
stats <- fmean(miris)            # Savig stats
head(TRA(miris, stats, "-"), 3)  # Centering. Same as sweep(miris, 2, stats, "-")

## --------------------------------------------------------------------------------------------------
# 3 ways of centering data
all_identical(TRA(miris, fmean(miris), "-"),  
              fmean(miris, TRA = "-"),   # better for any operation if the stats are not needed
              fwithin(miris))            # fastest, fwithin is discussed in section 4.5 

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
head(fmedian(miris, f, "-+"), 3)

## --------------------------------------------------------------------------------------------------
# fsccale doesn't rename columns
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
pwcor(num_vars(filter(STD_GGDC10S, Variable == "VA")))

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
data <- na.omit(get_vars(wlddev, c("iso3c","year","PCGDP","LIFEEX")))

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
data$year <- qF(data$year) # the country code (iso3c) is already a factor

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
fFtest(data$PCGDP, data$year, get_vars(data, c("LIFEEX","iso3c")))

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

## ---- eval=NCRAN-----------------------------------------------------------------------------------
# The average group size is 10, there are about 100000 groups
GRP(testdat, ~ g1 + g2) 

# get indices of grouping columns 
ind <- get_vars(testdat, c("g1","g2"), "indices")

# Centering
system.time(testdat %>% group_by(g1,g2) %>% mutate_all(function(x) x - mean.default(x, na.rm = TRUE)))
system.time(testdat[, lapply(.SD, function(x) x - mean(x, na.rm = TRUE)), keyby = c("g1","g2")]) 
system.time(W(testdat, ~ g1 + g2))

# Weighted Centering
# Can't easily be done in dplyr.. 
system.time(testdat[, lapply(.SD, function(x) x - weighted.mean(x, w, na.rm = TRUE)), keyby = c("g1","g2")])
system.time(W(testdat, ~ g1 + g2, ~ w))

# Centering on the overall mean
# Can't easily be done in dplyr or data.table.
system.time(W(testdat, ~ g1 + g2, mean = "overall.mean"))      # Ordinary 
system.time(W(testdat, ~ g1 + g2, ~ w, mean = "overall.mean")) # Weighted

# Centering on both grouping variables simultaneously
# Can't be done in dplyr or data.table at all!
system.time(HDW(testdat, ~ qF(g1) + qF(g2), variable.wise = TRUE))        # Ordinary
system.time(HDW(testdat, ~ qF(g1) + qF(g2), w = w, variable.wise = TRUE)) # Weighted

# Proportions
system.time(testdat %>% group_by(g1,g2) %>% mutate_all(function(x) x/sum(x, na.rm = TRUE)))
system.time(testdat[, lapply(.SD, function(x) x/sum(x, na.rm = TRUE)), keyby = c("g1","g2")])
system.time(fsum(get_vars(testdat, -ind), get_vars(testdat, ind), TRA = "/"))

# Scaling
system.time(testdat %>% group_by(g1,g2) %>% mutate_all(function(x) x/sd(x, na.rm = TRUE)))
system.time(testdat[, lapply(.SD, function(x) x/sd(x, na.rm = TRUE)), keyby = c("g1","g2")])
system.time(fsd(get_vars(testdat, -ind), get_vars(testdat, ind), TRA = "/"))
system.time(fsd(get_vars(testdat, -ind), get_vars(testdat, ind), w, "/")) # Weighted Scaling. Need a weighted sd to do in dplyr or data.table

# Scaling and centering (i.e. standardizing)
system.time(testdat %>% group_by(g1,g2) %>% mutate_all(function(x) (x - mean.default(x, na.rm = TRUE))/sd(x, na.rm = TRUE)))
system.time(testdat[, lapply(.SD, function(x) (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)), keyby = c("g1","g2")])
system.time(STD(testdat, ~ g1 + g2))
system.time(STD(testdat, ~ g1 + g2, ~ w))  # Weighted standardizing: Also difficult to do in dplyr or data.table

# Replacing data with any ststistic, here the sum:
system.time(testdat %>% group_by(g1,g2) %>% mutate_all(sum, na.rm = TRUE))
system.time(testdat[, setdiff(names(testdat), c("g1","g2")) := lapply(.SD, sum, na.rm = TRUE), keyby = c("g1","g2")])
system.time(fsum(get_vars(testdat, -ind), get_vars(testdat, ind), TRA = "replace_fill")) # dplyr and data.table also fill missing values. 
system.time(fsum(get_vars(testdat, -ind), get_vars(testdat, ind), TRA = "replace")) # This preserves missing values, and is not easily implemented in dplyr or data.table

## --------------------------------------------------------------------------------------------------
mts <- psmat(wlddev, PCGDP ~ iso3c, ~ year)   
str(mts)
plot(mts, main = vlabels(wlddev)[9], xlab = "Year")     

## ---- fig.height=7---------------------------------------------------------------------------------
# Get panel-series array
psar <- psmat(wlddev, ~ iso3c, ~ year, cols = 9:12)                      
str(psar)
plot(psar, legend = TRUE)

# Plot array of Panel-Series aggregated by region:
plot(psmat(collap(wlddev, ~region+year, cols = 9:12),           
           ~region, ~year), legend = TRUE,
     labs = vlabels(wlddev)[9:12])

## --------------------------------------------------------------------------------------------------
# This gives list of ps-matrices
psml <- psmat(wlddev, ~ iso3c, ~ year, 9:12, array = FALSE)  
str(psml, give.attr = FALSE)

# Using unlist2d, can generate a data.frame
head(unlist2d(psml, idcols = "Variable", row.names = "Country"))[1:10]

## --------------------------------------------------------------------------------------------------
# Panel-ACF of GDP per Capia
psacf(wlddev, PCGDP ~ iso3c, ~year)
# Panel-Parial-ACF of GDP per Capia
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

# 3 identical ways of computing 1 lead
all_identical(flag(AirPassengers, -1), L(AirPassengers, -1), F(AirPassengers))

# 1 lead and 3 lags - output as matrix
head(L(AirPassengers, -1:3))     

# ... this is still a time-series object: 
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
# Different ways of regressing GDP on its's lags and life-Expectancy and it's lags

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
fFtest(AirPassengers, qF(cycle(AirPassengers)), poly(seq_along(AirPassengers), 3))

## --------------------------------------------------------------------------------------------------
plot(G(AirPassengers, c(0,1,12)))

## --------------------------------------------------------------------------------------------------
plot(D(AirPassengers, c(1,12), 1:2))

## --------------------------------------------------------------------------------------------------
# sequence of leaded/lagged and iterated differences
head(D(AirPassengers, -2:2, 1:3))

## --------------------------------------------------------------------------------------------------
y = 1:10
g = rep(1:2, each = 5)
t = rep(1:5, 2)

D(y, -2:2, 1:2, g, t)

## --------------------------------------------------------------------------------------------------
L(D(y, 0:2, 1:2, g, t), 0:1, g, t)

## --------------------------------------------------------------------------------------------------
tryCatch(D(y, 3, 2, g, t), error = function(e) e)

## --------------------------------------------------------------------------------------------------
head(G(wlddev, 0:1, 1, PCGDP + LIFEEX ~ iso3c, ~year))     

head(G(GGDC10S, 1, 1, ~ Variable + Country, ~ Year, cols = 6:10))     

## ---- warning=FALSE--------------------------------------------------------------------------------
head(qDT(wlddev)[, paste0("G.", names(wlddev)[9:12]) := fgrowth(.SD,1,1,iso3c,year), .SDcols = 9:12])


## --------------------------------------------------------------------------------------------------
summary(lm(G(PCGDP,10,1,iso3c,year) ~                    
             L(PCGDP,10,iso3c,year) +                    
             G(LIFEEX,10,1,iso3c,year), data = wlddev))

## --------------------------------------------------------------------------------------------------
moddat <- HDW(L(G(wlddev, c(0, 10), 1, ~iso3c, ~year, 9:10), c(0, 10), ~iso3c, ~year), ~iso3c + qF(year))[-c(1,5)]
summary(lm(HDW.L10G1.PCGDP ~. , moddat))

## --------------------------------------------------------------------------------------------------
microbenchmark(HDW(L(G(wlddev, c(0, 10), 1, ~iso3c, ~year, 9:10), c(0, 10), ~iso3c, ~year), ~iso3c + qF(year)))

## --------------------------------------------------------------------------------------------------
pwlddev <- plm::pdata.frame(wlddev, index = c("iso3c", "year"))
moddat <- HDW(L(G(pwlddev, c(0, 10), 1, 9:10), c(0, 10)))[-c(1,5)]
summary(lm(HDW.L10G1.PCGDP ~. , moddat))

## ---- eval=NCRAN-----------------------------------------------------------------------------------
# We have a balanced panel of 216 countries, each observed for 59 years
descr(wlddev, cols = c("iso3c", "year"))

# 1 Panel-Lag
suppressMessages(
microbenchmark(dplyr_not_ordered = wlddev %>% group_by(iso3c) %>% select_at(9:12) %>% mutate_all(lag),
               dplyr_ordered = wlddev %>% arrange(iso3c,year) %>% group_by(iso3c) %>% select_at(9:12) %>% mutate_all(lag),
               data.table_not_ordered = dtwlddev[, shift(.SD), keyby = iso3c, .SDcols = 9:12],
               data.table_ordered = dtwlddev[order(year), shift(.SD), keyby = iso3c, .SDcols = 9:12], 
               collapse_not_ordered = L(wlddev, 1, ~iso3c, cols = 9:12),
               collapse_ordered = L(wlddev, 1, ~iso3c, ~year, cols = 9:12),
               subtract_from_CNO = message("Panel-lag computed without timevar: Assuming ordered data")))

# Sequence of 1 lead and 3 lags: Not possible in dplyr
microbenchmark(data.table_not_ordered = dtwlddev[, shift(.SD, -1:3), keyby = iso3c, .SDcols = 9:12],
               data.table_ordered = dtwlddev[order(year), shift(.SD, -1:3), keyby = iso3c, .SDcols = 9:12], 
               collapse_ordered = L(wlddev, -1:3, ~iso3c, ~year, cols = 9:12))    

# 1 Panel-difference
microbenchmark(dplyr_not_ordered = wlddev %>% group_by(iso3c) %>% select_at(9:12) %>% mutate_all(function(x) x - lag(x)),
               dplyr_ordered = wlddev %>% arrange(iso3c,year) %>% group_by(iso3c) %>% select_at(9:12) %>% mutate_all(function(x) x - lag(x)), 
               data.table_not_ordered = dtwlddev[, lapply(.SD, function(x) x - shift(x)), keyby = iso3c, .SDcols = 9:12],
               data.table_ordered = dtwlddev[order(year), lapply(.SD, function(x) x - shift(x)), keyby = iso3c, .SDcols = 9:12], 
               collapse_ordered = D(wlddev, 1, 1, ~iso3c, ~year, cols = 9:12))                                                 

# Iterated Panel-Difference: Not straightforward in dplyr or data.table
microbenchmark(collapse_ordered = D(wlddev, 1, 2, ~iso3c, ~year, cols = 9:12))

# Sequence of Lagged/Leaded Differences: Not straightforward in dplyr or data.table
microbenchmark(collapse_ordered = D(wlddev, -1:3, 1, ~iso3c, ~year, cols = 9:12))

# Sequence of Lagged/Leaded and Iterated Differences: Not straightforward in dplyr or data.table
microbenchmark(collapse_ordered = D(wlddev, -1:3, 1:2, ~iso3c, ~year, cols = 9:12))

# The same applies to growth rates or log-differences. 
microbenchmark(collapse_ordered_growth = G(wlddev, 1, 1, ~iso3c, ~year, cols = 9:12),
               collapse_ordered_logdiff = G(wlddev, 1, 1, ~iso3c, ~year, cols = 9:12, logdiff = TRUE))

## ---- warning=FALSE, message=FALSE-----------------------------------------------------------------
library(vars)
# The 6 most important non-government sectors (see section 1)
sec <- c("AGR","MAN","WRT","CON","TRA","FIRE")
# This creates a data.table containing the value added of the 6 most important non-government sectors 
data <- qDT(GGDC10S)[Variable == "VA"] %>% get_vars(c("Country","Year", sec)) %>% na.omit
# Let's look at the log VA in agriculture across countries:
AGRmat <- log(psmat(data, AGR ~ Country, ~ Year, transpose = TRUE))   # Converting to panel-series matrix
plot(AGRmat)

## --------------------------------------------------------------------------------------------------
# Subtracting a country specific cubic growth trend
AGRmat <- dapply(AGRmat, fHDwithin, poly(seq_row(AGRmat), 3), fill = TRUE)

plot(AGRmat)

## --------------------------------------------------------------------------------------------------
# Standadizing the cubic log-detrended data
AGRmat <- fscale(AGRmat)
plot(AGRmat)

## ---- fig.height=7---------------------------------------------------------------------------------
# Taking logs
get_vars(data, 3:8) <- dapply(get_vars(data, 3:8), log)
# Iteratively projecting out country FE and cubic trends from complete cases (still very slow)
get_vars(data, 3:8) <- HDW(data, ~ qF(Country)*poly(Year, 3), fill = TRUE)
# Scaling 
get_vars(data, 3:8) <- STD(data, ~ Country, cols = 3:8, keep.by = FALSE)

# Check the plot
plot(psmat(data, ~Country, ~Year))

## --------------------------------------------------------------------------------------------------
# This adds one lag of all series to the data 
add_vars(data) <- L(data, 1, ~ Country, ~ Year, keep.ids = FALSE) 
# This removes missing values from all but the first row and drops identifier columns (vars is made for time-series without gaps)
data <- rbind(data[1, -(1:2)], na.omit(data[-1, -(1:2)])) 
head(data)

## --------------------------------------------------------------------------------------------------
# saving the names of the 6 sectors
nam <- names(data)[1:6]

pVAR <- list(varresult = setNames(lapply(seq_len(6), function(i)    # list of 6 lm's each regressing
               lm(as.formula(paste0(nam[i], "~ -1 + . ")),          # the sector on all lags of 
               get_vars(data, c(i, 7:length(data)))[-1])), nam),    # itself and other sectors, removing the missing first row
             datamat = data[-1],                                    # The full data containing levels and lags of the sectors, removing the missing first row
             y = do.call(cbind, get_vars(data, 1:6)),               # Only the levels data as matrix
             type = "none",                                         # No constant or tend term: We harmonized the data already
             p = 1,                                                 # The lag-order
             K = 6,                                                 # The number of variables
             obs = nrow(data)-1,                                    # The number of non-missing obs
             totobs = nrow(data),                                   # The total number of obs
             restrictions = NULL, 
             call = quote(VAR(y = data)))

class(pVAR) <- "varest"

## --------------------------------------------------------------------------------------------------
serial.test(pVAR)

## --------------------------------------------------------------------------------------------------
# This computes the pairwise correlations between standardized sectoral growth rates across countries
corr <- filter(GGDC10S, Variable == "VA") %>%   # Subset rows: Only VA
           group_by(Country) %>%                # Group by country
                get_vars(sec) %>%               # Select the 6 sectors
                   fgrowth %>%                  # Compute Sectoral growth rates (a time-variable can be passsed, but not necessary here as the data is ordered)
                      fscale %>%                # Scale and center (i.e. standardize)
                         pwcor                  # Compute Pairwise correlations

corr

# We need to impose K*(K-1)/2 = 15 (with K = 6 variables) restrictions for identification
corr[corr <= sort(corr)[15]] <- 0
corr

# The rest is unknown (i.e. will be estimated)
corr[corr > 0 & corr < 1] <- NA

# This estimates the Panel-SVAR using Maximum Likelihood:
pSVAR <- SVAR(pVAR, Amat = unclass(corr), estmethod = "direct")
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

# Qick conversion to matrix and plotting
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
# Plot the forecast-error variance decmpositions
plot(pFEVD)

## --------------------------------------------------------------------------------------------------
# Computing the cumulative impact after 10 periods
list_elem(pIRF) %>%                            # Pull out the sublist elements containing the IRF coefficients + CI's
  rapply2d(function(x) round(fsum(x), 2)) %>%  # Recursively apply the column-sums to coefficient matrices (could also use colSums)
  unlist2d(c("Type", "Impulse"))               # Recursively row-bind the result to a data.frame and add identifier columns
                             # Round result to 2 digits

## --------------------------------------------------------------------------------------------------
# This binds the matrices after adding integer row-names to them to a data.table

data <- pIRF$irf %>%                      # Get only the coefficient matrices, discard the confidence bounds
         lapply(setRownames) %>%          # Add integer rownames: setRownames(object, nm = seq_row(object))
           unlist2d(idcols = "Impulse",   # Recursive unlisting to data.table creating a factor id-column
                    row.names = "Time",   # and saving the generated rownames in a variable called 'Time'
                    id.factor = TRUE,     # -> Create Id column ('Impulse') as factor
                    DT = TRUE)            # -> Output as data.table (default is data.frame)

head(data)

# Coercing Time to numeric (from character)
data$Time <- as.numeric(data$Time)

# Using data.table's melt
data <- melt(data, 1:2)
head(data)

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
# Rewriting more compactly...
data <- unlist2d(lapply(pFEVD, setRownames), idcols = "variable", row.names = "Time",
                 id.factor = TRUE, DT = TRUE)
data$Time <- as.numeric(data$Time)
head(data)

data <- melt(data, 1:2, variable.name = "Sector")

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

