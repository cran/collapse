## ---- echo = FALSE, message = FALSE, warning=FALSE------------------------------------------------
library(data.table)    # Keep here becasue of not run options on CRAN
library(microbenchmark)
library(plm)
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

## -------------------------------------------------------------------------------------------------
library(collapse)

head(wlddev)

fNobs(wlddev)      # This column-wise counts the number of observations

fNdistinct(wlddev) # This counts the number of distinct values

## -------------------------------------------------------------------------------------------------
library(plm)

# This creates a panel-data frame
pwlddev <- pdata.frame(wlddev, index = c("iso3c", "year"))

str(pwlddev, give.attr = FALSE)

# A pdata.frame has an index attribute attached [retrieved using index(pwlddev) or attr(pwlddev, "index")]
str(index(pwlddev))

# This shows the individual and time dimensions
pdim(pwlddev)


## -------------------------------------------------------------------------------------------------
# Panel-Series of GDP per Capita and Life-Expectancy at Birth
PCGDP <- pwlddev$PCGDP
LIFEEX <- pwlddev$LIFEEX
str(LIFEEX)

## -------------------------------------------------------------------------------------------------
# Between-Transformations
head(fbetween(LIFEEX))                        # Between individual (default)

head(fbetween(LIFEEX, effect = "year"))       # Between time

# Within-Transformations
head(fwithin(LIFEEX))                         # Within individuals (default)

head(fwithin(LIFEEX, effect = "year"))        # Within time

## -------------------------------------------------------------------------------------------------
# This preserves missing values in the output
head(fbetween(PCGDP), 30)              

# This replaces all individuals with the group mean
head(fbetween(PCGDP, fill = TRUE), 30) 

## -------------------------------------------------------------------------------------------------
# This performed standard grouped centering
head(fwithin(LIFEEX))                          

# This adds the overall average Life-Expectancy (across countries) to the country-demeaned series
head(fwithin(LIFEEX, mean = "overall.mean"))  

## -------------------------------------------------------------------------------------------------
head(fbetween(num_vars(pwlddev)), 3)

head(fbetween(num_vars(pwlddev), fill = TRUE), 3)

head(fwithin(num_vars(pwlddev)), 3)

head(fwithin(num_vars(pwlddev), mean = "overall.mean"), 3)

## -------------------------------------------------------------------------------------------------
identical(fbetween(PCGDP), B(PCGDP))
identical(fbetween(PCGDP, fill = TRUE), B(PCGDP, fill = TRUE))
identical(fwithin(PCGDP), W(PCGDP))
identical(fwithin(PCGDP, mean = "overall.mean"), W(PCGDP, mean = "overall.mean"))

## -------------------------------------------------------------------------------------------------
head(B(pwlddev), 3)

head(W(pwlddev, cols = 9:12), 3) # Here using the cols argument

## -------------------------------------------------------------------------------------------------
# This replaces values by the ODA-weighted group mean and also preserves the weight variable (ODA, argument keep.w = TRUE)
head(B(pwlddev, w = ~ ODA), 3)

# This centers values on the ODA-weighted group mean
head(W(pwlddev, w = ~ ODA, cols = c("PCGDP","LIFEEX","GINI")), 3)

# This centers values on the ODA-weighted group mean and also adds the overall ODA-weighted mean of the data
head(W(pwlddev, w = ~ ODA, cols = c("PCGDP","LIFEEX","GINI"), mean = "overall.mean"), 3)

## -------------------------------------------------------------------------------------------------
# This simultaneously averages Life-Expectancy across countries and years 
head(HDB(LIFEEX)) # (same as running a regression on country and year dummies and taking the fitted values)

# This simultaneously centers Life-Expectenacy on countries and years 
head(HDW(LIFEEX)) # (same as running a regression on country and year dummies and taking the residuals)

## -------------------------------------------------------------------------------------------------
# Missing values are preserved in the output when fill = TRUE (the default)
head(HDB(PCGDP), 30)  

# When fill = FALSE, only the complete cases are returned
nofill <- HDB(PCGDP, fill = FALSE)
head(nofill, 30)

# This results in a shorter panel-vector 
length(nofill)   
length(PCGDP)

# The cases that were missing and removed from the output are available as an attribute
head(attr(nofill, "na.rm"), 30)

## -------------------------------------------------------------------------------------------------
# This column-wise centers the data on countries and years
tail(HDW(pwlddev), 10)

## -------------------------------------------------------------------------------------------------
# This centers the complete cases of the data data on countries and years and keeps missing cases
tail(HDW(pwlddev, variable.wise = FALSE), 10)

## -------------------------------------------------------------------------------------------------
# This centers the complete cases of the data data on countries and years, and removes missing cases
res <- HDW(pwlddev, fill = FALSE)
tail(res, 10)

tail(attr(res, "na.rm"))

## -------------------------------------------------------------------------------------------------
# This standardizes GDP per capita in each country
STD_PCGDP <- STD(PCGDP)

# Checks: 
head(fmean(STD_PCGDP, index(STD_PCGDP, 1)))
head(fsd(STD_PCGDP, index(STD_PCGDP, 1)))

# This standardizes GDP per capita in each year
STD_PCGDP_T <- STD(PCGDP, effect = "year")

# Checks: 
head(fmean(STD_PCGDP_T, index(STD_PCGDP_T, 2)))
head(fsd(STD_PCGDP_T, index(STD_PCGDP_T, 2)))

## -------------------------------------------------------------------------------------------------
head(STD(pwlddev, cols = 9:12))

head(STD(pwlddev, cols = 9:12, effect = "year"))

## -------------------------------------------------------------------------------------------------
# This will scale the data such that mean mean within each country is 5 and the standard deviation is 3
qsu(fscale(pwlddev$PCGDP, mean = 5, sd = 3))

## -------------------------------------------------------------------------------------------------
# Scaling without centering: Mean preserving with fscale / STD
qsu(fscale(pwlddev$PCGDP, mean = FALSE, sd = 3))

# Scaling without centering can also be done using fsd, but this does not preserve the mean
qsu(fsd(pwlddev$PCGDP, index(pwlddev, 1), TRA = "/"))


## -------------------------------------------------------------------------------------------------
fmean(pwlddev$PCGDP)  # Overall mean
fsd(W(pwlddev$PCGDP)) # Within sd

# Scaling and centerin such that the mean of each country is the overall mean, and the sd of each country is the within sd
qsu(fscale(pwlddev$PCGDP, mean = "overall.mean", sd = "within.sd"))

## -------------------------------------------------------------------------------------------------
# A panel-lag
head(flag(LIFEEX))      

# A panel-lead
head(flag(LIFEEX, -1))

# The lag and lead operators are even more parsimonious to employ:
all_identical(L(LIFEEX), flag(LIFEEX), plm::lag(LIFEEX))
all_identical(F(LIFEEX), flag(LIFEEX, -1), plm::lead(LIFEEX))

## -------------------------------------------------------------------------------------------------
# sequence of panel- lags and leads
head(flag(LIFEEX, -1:3))

all_identical(L(LIFEEX, -1:3), F(LIFEEX, 1:-3), flag(LIFEEX, -1:3))

# The native plm implementation also returns a matrix of lags but with different column names
head(plm::lag(LIFEEX, -1:3), 4)


## -------------------------------------------------------------------------------------------------
# This lags the entire data
head(flag(pwlddev))

# This lags only numeric columns and preserves panel-id's
head(L(pwlddev))

# This lags only columns 9 through 12 and preserves panel-id's
head(L(pwlddev, cols = 9:12))

## -------------------------------------------------------------------------------------------------
# This lags only columns 9 through 12 and preserves panel-id's
head(L(pwlddev, -1:3, cols = 9:12))

## -------------------------------------------------------------------------------------------------
# Panel-difference of Life Expectancy
head(fdiff(LIFEEX))

# Second panel-difference
head(fdiff(LIFEEX, diff = 2))

# Panel-growth rate of Life Expectancy
head(fgrowth(LIFEEX))

# Growth rate of growth rate of Life Expectancy
head(fgrowth(LIFEEX, diff = 2))

identical(D(LIFEEX), fdiff(LIFEEX))
identical(G(LIFEEX), fgrowth(LIFEEX))
identical(fdiff(LIFEEX), diff(LIFEEX)) # Same as plm::diff.pseries (which does not compute iterated panel-differences)

## -------------------------------------------------------------------------------------------------
# Panel log-difference of Life Expectancy
head(Dlog(LIFEEX))

# Panel log-difference growth rate (in percentage terms) of Life Expectancy
head(G(LIFEEX, logdiff = TRUE))

## -------------------------------------------------------------------------------------------------
# first and second forward-difference and first and second difference of lags 1-3 of Life-Expectancy
head(D(LIFEEX, -1:3, 1:2))

# Same with Log-differences 
head(Dlog(LIFEEX, -1:3, 1:2))

# Same with (exact) growth rates
head(G(LIFEEX, -1:3, 1:2))

## -------------------------------------------------------------------------------------------------
# Regression of GDP on Life Expectance with country and time FE
mod <- lm(PCGDP ~ LIFEEX, data = fHDwithin(fselect(pwlddev, PCGDP, LIFEEX), fill = FALSE))
mod

# Computing autocorrelation of residuals
r <- residuals(mod)
r <- pwcor(r, L(r, 1, substr(names(r), 1, 3)))  # Need this to compute a panel-lag
r

# Running the regression again quasi-differencing the transformed data
modCO <- lm(PCGDP ~ LIFEEX, data = fdiff(fHDwithin(fselect(pwlddev, PCGDP, LIFEEX), variable.wise = FALSE), rho = r, stubs = FALSE))
modCO

# In this case rho is almost 1, so we might as well just difference the untransformed data and go with that
# We also need to bootstrap this for proper standard errors. 

## -------------------------------------------------------------------------------------------------
# Sequence of differneces (same as above), adding one extra lag of the whole sequence
head(L(D(LIFEEX, -1:3, 1:2), 0:1))


## -------------------------------------------------------------------------------------------------
head(D(pwlddev, -1:3, 1:2, cols = 9:10), 3)

head(L(D(pwlddev, -1:3, 1:2, cols = 9:10), 0:1), 3)

## -------------------------------------------------------------------------------------------------
# Converting the panel-series to array, individual rows (default)
str(psmat(LIFEEX))

# Converting the panel-series to array, individual columns
str(psmat(LIFEEX, transpose = TRUE))

# Same as plm::as.matrix.pseries, apart from attributes
identical(unattrib(psmat(LIFEEX)),        
          unattrib(as.matrix(LIFEEX))) 
identical(unattrib(psmat(LIFEEX, transpose = TRUE)), 
          unattrib(as.matrix(LIFEEX, idbyrow = FALSE))) 

## -------------------------------------------------------------------------------------------------
psar <- psmat(pwlddev, cols = 9:12)
str(psar)

str(psmat(pwlddev, cols = 9:12, transpose = TRUE))

## -------------------------------------------------------------------------------------------------
# Looking at wealth, health and inequality in Brazil and Argentinia, 1990-1999
aperm(psar[c("BRA","ARG"), as.character(1990:1999), c("PCGDP", "LIFEEX", "GINI")])

## -------------------------------------------------------------------------------------------------
pslist <- psmat(pwlddev, cols = 9:12, array = FALSE)
str(pslist)

## -------------------------------------------------------------------------------------------------
head(unlist2d(pslist, idcols = "Variable", row.names = "Country Code"), 3)

## ---- eval=NCRAN----------------------------------------------------------------------------------
wlddevsmall <- get_vars(wlddev, c("iso3c","year","OECD","PCGDP","LIFEEX","GINI","ODA"))
wlddevsmall$iso3c <- as.character(wlddevsmall$iso3c)
data <- replicate(100, wlddevsmall, simplify = FALSE)
rm(wlddevsmall)
uniquify <- function(x, i) {
  x$iso3c <- paste0(x$iso3c, i)
  x
}
data <- unlist2d(Map(uniquify, data, as.list(1:100)), idcols = FALSE)
data <- pdata.frame(data, index = c("iso3c", "year"))
pdim(data)

## ---- eval=NCRAN----------------------------------------------------------------------------------
library(microbenchmark)
# Creating the extended panel-series for Life Expectancy (l for large)
LIFEEX_l <- data$LIFEEX
str(LIFEEX_l)

# Between Transformations
microbenchmark(Between(LIFEEX_l, na.rm = TRUE), times = 10)
microbenchmark(fbetween(LIFEEX_l), times = 10)

# Within Transformations
microbenchmark(Within(LIFEEX_l, na.rm = TRUE), times = 10)
microbenchmark(fwithin(LIFEEX_l), times = 10)

# Higher-Dimenional Between and Within Transformations
microbenchmark(fHDbetween(LIFEEX_l), times = 10)
microbenchmark(fHDwithin(LIFEEX_l), times = 10)

# Single Lag
microbenchmark(plm::lag(LIFEEX_l), times = 10)
microbenchmark(flag(LIFEEX_l), times = 10)

# Sequence of Lags / Leads
microbenchmark(plm::lag(LIFEEX_l, -1:3), times = 10)
microbenchmark(flag(LIFEEX_l, -1:3), times = 10)

# Single difference
microbenchmark(diff(LIFEEX_l), times = 10)
microbenchmark(fdiff(LIFEEX_l), times = 10)

# Iterated Difference
microbenchmark(fdiff(LIFEEX_l, diff = 2), times = 10)

# Sequence of Lagged / Leaded and iterated differences
microbenchmark(fdiff(LIFEEX_l, -1:3, 1:2), times = 10)

# Single Growth Rate
microbenchmark(fgrowth(LIFEEX_l), times = 10)

# Single Log-Difference
microbenchmark(fdiff(LIFEEX_l, logdiff = TRUE), times = 10)

# Panel-Series to Matrix Conversion
# system.time(as.matrix(LIFEEX_l))  This takes about 3 minutes to compute
microbenchmark(psmat(LIFEEX_l), times = 10)

## ---- eval=NCRAN----------------------------------------------------------------------------------
microbenchmark(L(data, cols = 3:6), times = 10)
library(data.table)
setDT(data)
# 'Improper' panel-lag
microbenchmark(data[, shift(.SD), by = iso3c, .SDcols = 3:6], times = 10)

# This does what L is actually doing (without sorting the data)
microbenchmark(data[order(year), shift(.SD), by = iso3c, .SDcols = 3:6], times = 10) 

## ---- eval=NCRAN----------------------------------------------------------------------------------
x <- rnorm(1e7)                                     # 10 million obs
g <- qF(rep(1:1e6, each = 10), na.exclude = FALSE)  # 1 million individuals
t <- qF(rep(1:10, 1e6), na.exclude = FALSE)         # 10 time-periods per individual

microbenchmark(fbetween(x, g), times = 10)
microbenchmark(fwithin(x, g), times = 10)
microbenchmark(flag(x, 1, g, t), times = 10)
microbenchmark(flag(x, -1:1, g, t), times = 10)
microbenchmark(fdiff(x, 1, 1, g, t), times = 10)
microbenchmark(fdiff(x, 1, 2, g, t), times = 10)
microbenchmark(fdiff(x, -1:1, 1:2, g, t), times = 10)

## -------------------------------------------------------------------------------------------------
# This checks for any variation within "iso3c", the first index variable: TRUE means data vary within country i.e. over time. 
varying(pwlddev)

## -------------------------------------------------------------------------------------------------
# This checks any variation within time variable, i.e. cross-sectional variation. 
varying(pwlddev, effect = "year")

## -------------------------------------------------------------------------------------------------
# This checks cross-sectional variation within each year for the 4 indicators. 
head(varying(pwlddev, effect = "year", cols = 9:12, any_group = FALSE))

## -------------------------------------------------------------------------------------------------
head(varying(pwlddev$GINI, any_group = FALSE), 20)

## -------------------------------------------------------------------------------------------------
head(fNdistinct(pwlddev$GINI, index(pwlddev, "iso3c")), 20)

head(round(fsd(pwlddev$GINI, index(pwlddev, "iso3c")), 2), 20)

## -------------------------------------------------------------------------------------------------
qsu(pwlddev, cols = 9:12, higher = TRUE)

## -------------------------------------------------------------------------------------------------
qsu(pwlddev, ~ income, cols = 9:12, higher = TRUE)

## ---- eval=NCRAN----------------------------------------------------------------------------------
qsu(LIFEEX_l)

microbenchmark(qsu(LIFEEX_l))

## -------------------------------------------------------------------------------------------------
# Overall pairwise correlations with pairwise observation count and significance testing (* = significant at 5% level)
pwcor(get_vars(pwlddev, 9:12), N = TRUE, P = TRUE)

# Between correlations
pwcor(fmean(get_vars(pwlddev, 9:12), pwlddev$iso3c), N = TRUE, P = TRUE)

# Within correlations
pwcor(W(pwlddev, cols = 9:12, keep.ids = FALSE), N = TRUE, P = TRUE)

## -------------------------------------------------------------------------------------------------
# Generating a (transposed) matrix of country GDPs per capita
tGDPmat <- psmat(PCGDP, transpose = TRUE)
tGDPmat[1:10, 1:10]

# plot the matrix (it will plot correctly no matter how the matrix is transposed)
plot(tGDPmat, main = "GDP per Capita")

# Taking series with more than 20 observation
suffsamp <- tGDPmat[, fNobs(tGDPmat) > 20]

# Minimum pairwise observations between any two series: 
min(pwNobs(suffsamp))

# We can use the pairwise-correlations of the annual growth rates to hierarchically cluster the economies:
plot(hclust(as.dist(1-pwcor(G(suffsamp)))))

# Finally we could do PCA on the growth rates:
eig <- eigen(pwcor(G(suffsamp)))
plot(seq_col(suffsamp), eig$values/sum(eig$values)*100, xlab = "Number of Principal Components", ylab = "% Variance Explained", main = "Screeplot")


## ---- fig.height=8--------------------------------------------------------------------------------
plot(psmat(pwlddev, cols = 9:12), legend = TRUE)

## -------------------------------------------------------------------------------------------------
psacf(pwlddev, cols = 9:12)

## -------------------------------------------------------------------------------------------------
pspacf(pwlddev, cols = 9:12)

## -------------------------------------------------------------------------------------------------
psccf(PCGDP, LIFEEX)

## -------------------------------------------------------------------------------------------------
# Testing GDP per Capita
fFtest(PCGDP, index(PCGDP))    # Testing individual and time-fixed effects
fFtest(PCGDP, index(PCGDP, 1)) # Testing individual effects
fFtest(PCGDP, index(PCGDP, 2)) # Testing time effects

# Same for Life-Expectancy
fFtest(LIFEEX, index(LIFEEX))    # Testing individual and time-fixed effects
fFtest(LIFEEX, index(LIFEEX, 1)) # Testing individual effects
fFtest(LIFEEX, index(LIFEEX, 2)) # Testing time effects


## -------------------------------------------------------------------------------------------------
cor.test(B(PCGDP), B(LIFEEX)) # Testing correlation of country means

cor.test(B(PCGDP, effect = 2), B(LIFEEX, effect = 2)) # Same for time-means

## -------------------------------------------------------------------------------------------------
fFtest(PCGDP, index(PCGDP), get_vars(pwlddev, c("LIFEEX","ODA")))    # Testing individual and time-fixed effects
fFtest(PCGDP, index(PCGDP, 2), get_vars(pwlddev, c("iso3c","LIFEEX","ODA")))    # Testing time-fixed effects

## -------------------------------------------------------------------------------------------------
phtest(PCGDP ~ LIFEEX, data = pwlddev)

## -------------------------------------------------------------------------------------------------
HT_est <- function(y, X1, Z2, X2 = NULL, Z1 = NULL, time.FE = FALSE) {
  
  # Create matrix of independent variables
  X <- cbind(Intercept = 1, do.call(cbind, c(X1, X2, Z1, Z2)))
  
  # Create instrument matrix: if time.FE, higher-order demean X1 and X2, else normal demeaning
  IVS <- cbind(Intercept = 1, do.call(cbind, 
               c(if(time.FE) fHDwithin(X1, na.rm = FALSE) else fwithin(X1, na.rm = FALSE), 
                 if(is.null(X2)) X2 else if(time.FE) fHDwithin(X2, na.rm = FALSE) else fwithin(X2, na.rm = FALSE),
                 Z1, fbetween(X1, na.rm = FALSE))))
  
  if(length(IVS) == length(X)) { # The IV estimator case
    return(drop(solve(crossprod(IVS, X), crossprod(IVS, y))))
  } else { # The 2SLS case
    Xhat <- qr.fitted(qr(IVS), X)  # First stage
    return(drop(qr.coef(qr(Xhat), y)))   # Second stage
  }
}

## ---- warning=FALSE-------------------------------------------------------------------------------
dat <- get_vars(wlddev, c("iso3c","year","OECD","PCGDP","LIFEEX","GINI","ODA"))
get_vars(dat, 4:7) <- lapply(get_vars(dat, 4:7), log) # Taking logs of the data
dat$OECD <- as.numeric(dat$OECD)                      # Creating OECD dummy
dat <- pdata.frame(droplevels(na_omit(dat)),          # Creating Panel-data.frame, after removing missing values
                   index = c("iso3c", "year"))        # and dropping unused factor levels
pdim(dat)
varying(dat)

## -------------------------------------------------------------------------------------------------
# This tests whether each of the covariates is correlated with alpha_i
phtest(LIFEEX ~ PCGDP, dat)  # Likely correlated 
phtest(LIFEEX ~ ODA, dat)    # Likely correlated 
phtest(LIFEEX ~ GINI, dat)   # Likely not correlated !
phtest(LIFEEX ~ PCGDP + ODA + GINI, dat)  # Fixed Effects is the appropriate model for this regression

## -------------------------------------------------------------------------------------------------
# Testing the correlation between OECD dummy and the Between-transformed Life-Expectancy (i.e. not accounting for other covariates)
cor.test(dat$OECD, B(dat$LIFEEX)) # -> Significant correlation of 0.21
 
# Getting the fixed-effects (estimates of alpha_i) from the model (i.e. accounting for the other covariates)
fe <- fixef(plm(LIFEEX ~ PCGDP + ODA + GINI, dat, model = "within"))
mODA <- fmean(dat$ODA, dat$iso3c)
# Again testing the correlation
cor.test(fe, mODA[match(names(fe), names(mODA))]) # -> Not Significant.. but probably due to small sample size, the correlation is still 0.13

## -------------------------------------------------------------------------------------------------
# This computes the regression of OECD on the GINI instrument: Weak IV problem !!
fFtest(dat$OECD, B(dat$GINI))


## -------------------------------------------------------------------------------------------------
HT_est(y = dat$LIFEEX, 
       X1 = get_vars(dat, "GINI"), 
       Z2 = get_vars(dat, "OECD"),
       X2 = get_vars(dat, c("PCGDP","ODA"))) 

## ---- eval=NCRAN----------------------------------------------------------------------------------
dat <- get_vars(data, c("iso3c","year","OECD","PCGDP","LIFEEX","GINI","ODA"))
get_vars(dat, 4:7) <- lapply(get_vars(dat, 4:7), log) # Taking logs of the data
dat$OECD <- as.numeric(dat$OECD)                      # Creating OECD dummy
dat <- pdata.frame(droplevels(na_omit(dat)),          # Creating Panel-data.frame, after removing missing values
                   index = c("iso3c", "year"))        # and dropping unused factor levels
pdim(dat)
varying(dat)

library(microbenchmark)
microbenchmark(HT_est = HT_est(y = dat$LIFEEX,     # The estimator as before
                      X1 = get_vars(dat, "GINI"),
                      Z2 = get_vars(dat, "OECD"),
                      X2 = get_vars(dat, c("PCGDP","ODA"))),
              HT_est_TFE =  HT_est(y = dat$LIFEEX, # Also Projecting out Time-FE
                      X1 = get_vars(dat, "GINI"),
                      Z2 = get_vars(dat, "OECD"),
                      X2 = get_vars(dat, c("PCGDP","ODA")),
                      time.FE = TRUE))

## ---- echo=FALSE--------------------------------------------------------------
options(oldopts)

