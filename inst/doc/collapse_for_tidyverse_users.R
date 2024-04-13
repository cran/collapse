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

## ----echo=FALSE---------------------------------------------------------------
options(oldopts)

