## ----echo=FALSE-----------------------------------------------------------------------------------
oldopts <- options(width = 100L)

## ----echo = FALSE, message = FALSE, warning=FALSE-------------------------------------------------
knitr::opts_chunk$set(error = FALSE, message = FALSE, warning = FALSE, 
                      comment = "#", tidy = FALSE, cache = TRUE, collapse = TRUE,
                      fig.width = 8, fig.height = 5, 
                      out.width = '100%')

## -------------------------------------------------------------------------------------------------
library(collapse)
set_collapse(mask = "manip") # version >= 2.0.0 

## -------------------------------------------------------------------------------------------------
mtcars |>
  subset(mpg > 11) |>
  group_by(cyl, vs, am) |>
  summarise(across(c(mpg, carb, hp), mean), 
            qsec_wt = weighted.mean(qsec, wt))

## -------------------------------------------------------------------------------------------------
fmean(mtcars$mpg)     # Vector
fmean(EuStockMarkets) # Matrix
fmean(mtcars)         # Data Frame

fmean(mtcars$mpg, w = mtcars$wt)  # Weighted mean
fmean(mtcars$mpg, g = mtcars$cyl) # Grouped mean
fmean(mtcars$mpg, g = mtcars$cyl, w = mtcars$wt)   # Weighted group mean
fmean(mtcars[5:10], g = mtcars$cyl, w = mtcars$wt) # Of data frame
fmean(mtcars$mpg, g = mtcars$cyl, w = mtcars$wt, TRA = "fill") # Replace data by weighted group mean
# etc...

## -------------------------------------------------------------------------------------------------
mtcars |>
  subset(mpg > 11) |>
  group_by(cyl, vs, am) |>
  summarise(across(c(mpg, carb, hp), fmean), 
            qsec_wt = fmean(qsec, wt))

## -------------------------------------------------------------------------------------------------
mtcars |>
  subset(mpg > 11) |>
  group_by(cyl, vs, am) |>
  select(mpg, carb, hp) |> 
  fmean()

## -------------------------------------------------------------------------------------------------
mtcars |> group_by(cyl) |> summarise(mpg = fmean(mpg) + min(qsec)) # Vectorized

## -------------------------------------------------------------------------------------------------
mtcars |> group_by(cyl) |> summarise(mpg = fmean(mpg) + fmin(qsec)) # Vectorized
mtcars |> group_by(cyl) |> summarise(mpg = mean(mpg) + min(qsec))   # Not vectorized

## -------------------------------------------------------------------------------------------------
mtcars |>
  subset(mpg > 11, cyl, vs, am, mpg, carb, hp, qsec, wt) |>
  group_by(cyl, vs, am) |>
  summarise(across(c(mpg, carb, hp), fmean), 
            qsec_wt = fmean(qsec, wt))

## -------------------------------------------------------------------------------------------------
mtcars |>
  subset(mpg > 11, cyl, vs, am, mpg, carb, hp) |>
  group_by(cyl, vs, am) |> 
  fmean()

## -------------------------------------------------------------------------------------------------
mtcars |>
  subset(mpg > 11, cyl, vs, am, mpg, carb, hp) |>
  group_by(cyl, vs, am, sort = FALSE) |> 
  fmean(nthreads = 3, na.rm = FALSE)

## -------------------------------------------------------------------------------------------------
mtcars |>
  mutate(mpg_median = fmedian(mpg, list(cyl, vs, am), TRA = "fill")) |> 
  head(3)

## -------------------------------------------------------------------------------------------------
mtcars |>
  mutate(across(c(mpg, disp, qsec), fmedian, list(cyl, vs, am), TRA = "fill")) |> 
  head(2)

# Or 
mtcars |>
  transformv(c(mpg, disp, qsec), fmedian, list(cyl, vs, am), TRA = "fill") |> 
  head(2)

## -------------------------------------------------------------------------------------------------
mtcars |>
  group_by(cyl, vs, am, return.groups = FALSE) |> 
  mutate(mpg_median = fmedian(mpg), 
         mpg_mean = fmean(mpg), # Or fbetween(mpg)
         mpg_demean = fwithin(mpg), # Or fmean(mpg, TRA = "-")
         mpg_scale = fscale(mpg), 
         .keep = "used") |>
  ungroup() |>
  head(3)

## ----include = FALSE------------------------------------------------------------------------------
set.seed(101)

## -------------------------------------------------------------------------------------------------
# c = country, s = sector, y = year, v = value
exports <- expand.grid(c = paste0("c", 1:8), s = paste0("s", 1:8), y = 1:15) |>
           mutate(v = round(abs(rnorm(length(c), mean = 5)), 2)) |>
           subset(-sample.int(length(v), 360)) # Making it unbalanced and irregular
head(exports)
nrow(exports)

## -------------------------------------------------------------------------------------------------
# Computing Balassa's (1965) RCA index: fast and memory efficient
# settfm() modifies exports and assigns it back to the global environment
settfm(exports, RCA = fsum(v, list(c, y), TRA = "/") %/=% fsum(fsum(v, y, TRA = "/"), list(s, y), TRA = "fill", set = TRUE))

## -------------------------------------------------------------------------------------------------
pivot(exports, ids = "c", values = "RCA", names = "s", 
      how = "wider", FUN = "mean", sort = TRUE)

## -------------------------------------------------------------------------------------------------
exports |> 
  mutate(RCA_growth = fgrowth(RCA, g = list(c, s), t = y)) |> 
  pivot(ids = "c", values = "RCA_growth", names = "s", 
        how = "wider", FUN = fmedian, sort = TRUE)

## -------------------------------------------------------------------------------------------------
# Taking the latest observation within the last 3 years
exports_latest <- subset(exports, y > 12 & y == fmax(y, list(c, s), "fill"), -y)
# How many sectors do we observe for each country in the last 3 years?
with(exports_latest, fndistinct(s, c))

## -------------------------------------------------------------------------------------------------
exports_latest |>
    mutate(RCA = fsum(v, c, TRA = "/") %/=% fsum(proportions(v), s, TRA = "fill")) |>
    pivot("c", "RCA", "s", how = "wider", sort = TRUE)

## ----echo=FALSE---------------------------------------------------------------
options(oldopts)

