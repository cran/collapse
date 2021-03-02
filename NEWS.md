# collapse 1.5.2

### Changes to Functionality

* The first argument of `ftransform` was renamed to `.data` from `X`. This was done to enable the user to transform columns named "X". For the same reason the first argument of `frename` was renamed to `.x` from `x` (not `.data` to make it explicit that `.x` can be any R object with a "names" attribute). It is not possible to depreciate `X` and `x` without at the same time undoing the benefits of the argument renaming, thus this change is immediate and code breaking in rare cases where the first argument is explicitly set. 

* The function `is.regular` to check whether an R object is atomic or list-like is depreciated and will be removed before the end of the year. This was done to avoid a namespace clash with the *zoo* package (#127).

### Bug Fixes

* For reasons of efficiency, most statistical and transformation functions used the C macro `SHALLOW_DUPLICATE_ATTRIB` to copy column attributes in a data frame. Since this macro does not copy S4 object bits, it caused some problems with S4 object columns such as POSIXct (e.g. computing lags/leads, first and last values on these columns). This is now fixed, all statistical functions (apart from `fvar` and `fsd`) now use `DUPLICATE_ATTRIB` and thus preserve S4 object columns (#91). 

<!-- Also `BY` now handles POSIXct properly. -->

* `unlist2d` produced a subsetting error if an empty list was present in the list-tree. This is now fixed, empty or `NULL` elements in the list-tree are simply ignored (#99).

### Additions

* A function `fsummarize` was added to facilitate translating *dplyr* / *data.table* code to *collapse*. Like `collap`, it is only very fast when used with the *Fast Statistical Functions*. 

* A function `t_list` is made available to efficiently transpose lists of lists. 

<!-- 
* A small set of row-wise statistical functions: `rowsums`, `rowmeans`, `rowNobs`, `rowmins` and `rowmaxs` is introduced for fast (grouped, weighted) row-wise computations on matrices and data frames. -->

### Improvements
<!-- 
* `ffirst` and `flast` were rewritten in C, with slightly better performance and reduced file size. -->

* C files are compiled -O3 on Windows, which gives a boost of around 20% for the grouping mechanism applied to character data.



# collapse 1.5.1
A small patch for 1.5.0 that:

* Fixes a numeric precision issue when grouping doubles (e.g. before `qF(wlddev$LIFEEX)` gave an error, now it works). 

* Fixes a minor issue with `fHDwithin` when applied to *pseries* and `fill = FALSE`.


# collapse 1.5.0
*collapse* 1.5.0, released early January 2021, presents important refinements and some additional functionality. 

### Back to CRAN

* I apologize for inconveniences caused by the temporal archival of *collapse* from December 19, 2020. This archival was caused by the archival of the important *lfe* package on the 4th of December. *collapse* depended on *lfe* for higher-dimensional centering, providing the `fHDbetween / fHDwithin` functions for generalized linear projecting / partialling out. To remedy the damage caused by the removal of *lfe*, I had to rewrite `fHDbetween / fHDwithin` to take advantage of the demeaning algorithm provided by *fixest*, which has some quite different mechanics. Beforehand, I made some significant changes to `fixest::demean` itself to make this integration happen. The CRAN deadline was the 18th of December, and I realized too late that I would not make this. A request to CRAN for extension was declined, so *collapse* got archived on the 19th. I have learned from this experience, and *collapse* is now sufficiently insulated that it will not be taken off CRAN even if all suggested packages were removed from CRAN. 

### Bug Fixes

* Segfaults in several *Fast Statistical Functions* when passed `numeric(0)` are fixed (thanks to @eshom and @acylam, [#101](https://github.com/SebKrantz/collapse/issues/101)). The default behavior is that all *collapse* functions return `numeric(0)` again, except for `fNobs`, `fNdistinct` which return `0L`, and `fvar`, `fsd` which return `NA_real_`.

### Changes to Functionality

* Functions `fHDwithin / HDW` and `fHDbetween / HDB` have been reworked, delivering higher performance and greater functionality: For higher-dimensional centering and heterogenous slopes, the `demean` function from the *fixest* package is imported (conditional on the availability of that package). The linear prediction  and partialling out functionality is now built around `flm` and also allows for weights and different fitting methods. 

* In `collap`, the default behavior of `give.names = "auto"` was altered when used together with the `custom` argument. Before the function name was always added to the column names. Now it is only added if a column is aggregated with two different functions. I apologize if this breaks any code dependent on the new names, but this behavior just better reflects most common use (applying only one function per column), as well as STATA's collapse. 

* For list processing functions like `get_elem`, `has_elem` etc. the default for the argument `DF.as.list` was changed from `TRUE` to `FALSE`. This means if a nested lists contains data frame's, these data frame's will not be searched for matching elements. This default also reflects the more common usage of these functions (extracting entire data frame's or computed quantities from nested lists rather than searching / subsetting lists of data frame's). The change also delivers a considerable performance gain. 


<!-- Missing value removal is still done using *data.table* source code, so these functions are now equipped for large and complex linear prediction and partialling-out problems. To fully utilize them users must install *fixest*. -->

* Vignettes were outsourced to the [website](<https://sebkrantz.github.io/collapse/articles/index.html>). This nearly halves the size of the source package, and should induce users to appreciate the built-in documentation. The website also makes for much more convenient reading and navigation of these book-style vignettes. 

<!-- , and also made available as PDF versions for download there -->

### Additions

* Added a set of 10 operators `%rr%`, `%r+%`, `%r-%`, `%r*%`, `%r/%`, `%cr%`, `%c+%`, `%c-%`, `%c*%`, `%c/%` to facilitate and speed up row- and column-wise arithmetic operations involving a vector and a matrix / data frame / list. For example `X %r*% v` efficiently multiplies every row of `X` with `v`. Note that more advanced functionality is already provided in `TRA()`, `dapply()` and the *Fast Statistical Functions*, but these operators are intuitive and very convenient to use in matrix or matrix-style code, or in piped expressions. 

* Added function `missing_cases` (opposite of `complete.cases` and faster for data frame's / lists).

* Added function `allNA` for atomic vectors. 

* New vignette about using *collapse* together with *data.table*, available [online](<https://sebkrantz.github.io/collapse/articles/index.html>).

### Improvements

* Time series functions and operators `flag / L / F`, `fdiff / D / Dlog` and `fgrowth / G` now natively support irregular time series and panels, and feature a 'complete approach' i.e. values are shifted around taking full account of the underlying time-dimension!

<!-- Thus *collapse* now provides a comprehensive, robust and extremely fast approach to working with time-dependent data in R. -->

* Functions `pwcor` and `pwcov` can now compute weighted correlations on the pairwise or complete observations, supported by C-code that is (conditionally) imported from the *weights* package. 

* `fFtest` now also supports weights.

* `collap` now provides an easy workaround to aggregate some columns using weights and others without. The user may simply append the names of *Fast Statistical Functions* with `_uw` to disable weights. Example: `collapse::collap(mtcars, ~ cyl, custom = list(fmean_uw = 3:4, fmean = 8:10), w = ~ wt)` aggregates columns 3 through 4 using a simple mean and columns 8 through 10 using the weighted mean. 

* The parallelism in `collap` using `parallel::mclapply` has been reworked to operate at the column-level, and not at the function level as before. It is still not available for Windows though. The default number of cores was set to `mc.cores = 2L`, which now gives an error on windows if `parallel = TRUE`.  

* function `recode_char` now has additional options `ignore.case` and `fixed` (passed to `grepl`), for enhanced recoding character data based on regular expressions. 

* `rapply2d` now has `classes` argument permitting more flexible use. 

* `na_rm` and some other internal functions were rewritten in C. `na_rm` is now 2x faster than `x[!is.na(x)]` with missing values and 10x faster without missing values. 


# collapse 1.4.2

* An improvement to the `[.GRP_df` method enabling the use of most *data.table* methods (such as `:=`) on a grouped *data.table* created with `fgroup_by`.

* Some documentation updates by Kevin Tappe.

# collapse 1.4.1
collapse 1.4.1 is a small patch for 1.4.0 that:

* fixes clang-UBSAN and rchk issues in 1.4.0 (minor bugs in compiled code resulting, in this case, from trying to coerce a `NaN` value to integer, and failing to protect a shallow copy of a variable).

* Adds a method `[.GRP_df` that allows robust subsetting of grouped objects created with `fgroup_by` (thanks to Patrice Kiener for flagging this).

# collapse 1.4.0
*collapse* 1.4.0, released early November 2020, presents some important refinements, particularly in the domain of attribute handling, as well as some additional functionality. The changes make *collapse* smarter, more broadly compatible and more secure, and should not break existing code.  <!-- , is a major update: -->

### Changes to Functionality

* *Deep Matrix Dispatch / Extended Time Series Support:* The default methods of all statistical and transformation functions dispatch to the matrix method if `is.matrix(x) && !inherits(x, "matrix")` evaluates to `TRUE`. This specification avoids invoking the default method on classed matrix-based objects (such as multivariate time series of the *xts* / *zoo* class) not inheriting a 'matrix' class, while still allowing the user to manually call the default method on matrices (objects with implicit or explicit 'matrix' class). The change implies that *collapse*'s generic statistical functions are now well suited to transform *xts* / *zoo* and many other time series and matrix-based classes. 

* *Fully Non-Destructive Piped Workflow:* `fgroup_by(x, ...)` now only adds a class *grouped_df*, not classes *table_df*, *tbl*, *grouped_df*, and preserves all classes of `x`. This implies that workflows such as `x %>% fgroup_by(...) %>% fmean` etc. yields an object `xAG` of the same class and attributes as `x`, not a tibble as before. *collapse* aims to be as broadly compatible, class-agnostic and attribute preserving as possible. 

<!-- Not a priority for now! Not really necessary at all, can always use base R converters! -->
* *Thorough and Controlled Object Conversions:* Quick conversion functions `qDF`, `qDT` and `qM` now have additional arguments `keep.attr` and `class` providing precise user control over object conversions in terms of classes and other attributes assigned / maintained. The default (`keep.attr = FALSE`) yields *hard* conversions removing all but essential attributes from the object. E.g. before `qM(EuStockMarkets)` would just have returned `EuStockMarkets` (because `is.matrix(EuStockMarkets)` is `TRUE`) whereas now the time series class and 'tsp' attribute are removed. `qM(EuStockMarkets, keep.attr = TRUE)` returns `EuStockMarkets` as before. 

<!--
In general `keep.attr = TRUE` gives a *soft* conversion were attributes necessary to establish the new data type ('dim', 'dimnames', 'names', 'row.names', 'class') are modified, but all other attributes are kept. 
This may be useful in some cases, for examples it is now possible to write something like `mtcars %>% fgroup_by(cyl, vs, am) %>% qM(TRUE) %>% fmean(attr(.,"groups"))`. 
 
 (ensuring that `qDF`, `qDT` and `qM` now truly behave like `as.data.frame`, `as.data.table` and `as.matrix`) -->

* *Smarter Attribute Handling:* Drawing on the guidance given in the R Internals manual, the following standards for optimal non-destructive attribute handling are formalized and communicated to the user: 

  + The default and matrix methods of the *Fast Statistical Functions* preserve attributes of the input in grouped aggregations ('names', 'dim' and 'dimnames' are suitably modified). If inputs are classed objects (e.g. factors, time series, checked by `is.object`), the class and other attributes are dropped. Simple (non-grouped) aggregations of vectors and matrices do not preserve attributes, unless `drop = FALSE` in the matrix method. An exemption is made in the default methods of functions `ffirst`, `flast` and `fmode`, which always preserve the attributes (as the input could well be a factor or date variable). 
  
  + The data frame methods are unaltered: All attributes of the data frame and columns in the data frame are preserved unless the computation result from each column is a scalar (not computing by groups) and `drop = TRUE` (the default). 
  
  + Transformations with functions like `flag`, `fwithin`, `fscale` etc. are also unaltered: All attributes of the input are preserved in the output (regardless of whether the input is a vector, matrix, data.frame or related classed object). The same holds for transformation options modifying the input ("-", "-+", "/", "+", "\*", "%%", "-%%") when using `TRA()` function or the `TRA = "..."` argument to the *Fast Statistical Functions*. 
  
  + For `TRA` 'replace' and 'replace_fill' options, the data type of the STATS is preserved, not of x. This provides better results particularly with functions like `fNobs` and `fNdistinct`. E.g. previously `fNobs(letters, TRA = "replace")` would have returned the observation counts coerced to character, because `letters` is character. Now the result is integer typed. For attribute handling this means that the attributes of x are preserved unless x is a classed object and the data types of x and STATS do not match. An exemption to this rule is made if x is a factor and an integer (non-factor) replacement is offered to STATS. In that case the attributes of x are copied exempting the 'class' and 'levels' attribute, e.g. so that `fNobs(iris$Species, TRA = "replace")` gives an integer vector, not a (malformed) factor. In the unlikely event that STATS is a classed object, the attributes of STATS are preserved and the attributes of x discarded. 
  
<!-- This simple but thorough (and now formalized) system of attribute handling should be optimal in more than 90% of common applications. -->

  
 <!--  
  + The default methods of statistical functions returning numeric values (`fmean`, `fmedian`, `fsum`, `fprod`, `fvar`, `fsd`, `fmin`, `fmax` and `fnth`, `fNobs` and `fNdistinct`) do not preserve the attributes of classed objects (e.g. univariate time series) in grouped aggregations. This is consistent with the matrix methods of these functions and avoids errors, particularly after computations on time series. -->
 
 <!--, random conversions to tibble are not part of its philosophy. -->

<!--
* All S3 generic functions with a `default` method for atomic vectors and a `matrix` method now have an additional internal dispatch from the `default` to the `matrix` method if a classed matrix object missing a 'matrix' class is passed to the generic. For example consider a matrix time series `x <- structure(matrix(1:9, ncol = 3), class = "ts", tsp = c(1, 3, 1))` inheriting only a 'ts' but not a 'matrix' class. In collapse 1.3.2 `fsum(x)` would invoke the default method and return a scalar value. Now `fsum(x)` returns the sum for each column in the matrix. The `matrix` method is only called from the `default` method if `is.matrix(x) && !inherits(x, "matrix")` evaluates to `TRUE`, thus it is still possible to manually invoke the default method on a matrix. As the example indicates, this change is warranted to improve the inherent compatibility of *collapse* with various time series and matrix based classes (such as *xts* / *zoo*). -->


* *Reduced Dependency Burden:* The dependency on the *lfe* package was made optional. Functions `fHDwithin` / `fHDbetween` can only perform higher-dimensional centering if *lfe* is available. Linear prediction and centering with a single factor (among a list of covariates) is still possible without installing *lfe*. This change means that *collapse* now only depends on base R and *Rcpp* and is supported down to R version 2.10. 

### Additions

* Added function `rsplit` for efficient (recursive) splitting of vectors and data frames. 

* Added function `fdroplevels` for very fast missing level removal + added argument `drop` to `qF` and `GRP.factor`, the default is `drop = FALSE`. The addition of `fdroplevels` also enhances the speed of the `fFtest` function.

* `fgrowth` supports annualizing / compounding growth rates through added `power` argument.

* A function `flm` was added for barebones (weighted) linear regression fitting using different efficient methods: 4 from base R (`.lm.fit`, `solve`, `qr`, `chol`), using `fastLm` from *RcppArmadillo* (if installed), or `fastLm` from *RcppEigen* (if installed). 

* Added function `qTBL` to quickly convert R objects to tibble.

* helpers `setAttrib`, `copyAttrib` and `copyMostAttrib` exported for fast attribute handling in R (similar to `attributes<-()`, these functions return a shallow copy of the first argument with the set of attributes replaced, but do not perform checks for attribute validity like `attributes<-()`. This can yield large performance gains with big objects).  

* helper `cinv` added wrapping the expression `chol2inv(chol(x))` (efficient inverse of a symmetric, positive definite matrix via Choleski factorization). 

* A shortcut `gby` is now available to abbreviate the frequently used `fgroup_by` function. 

* A print method for grouped data frames of any class was added.

### Improvements

* Faster internal methods for factors for `funique`, `fmode` and `fNdistinct`.

<!-- * `flag`, `fdiff`, `fgrowth` support *xts* / *zoo* via explicit methods for fast and secure computations on unordered data. --> 

* The *grouped_df* methods for `flag`, `fdiff`, `fgrowth` now also support multiple time variables to identify a panel e.g. `data %>% fgroup_by(region, person_id) %>% flag(1:2, list(month, day))`.

* More security features for `fsubset.data.frame` / `ss`, `ss` is now internal generic and also supports subsetting matrices. 

* In some functions (like `na_omit`), passing double values (e.g. `1` instead of integer `1L`) or negative indices to the `cols` argument produced an error or unexpected behavior. This is now fixed in all functions. 

* Fixed a bug in helper function `all_obj_equal` occurring if objects are not all equal. 

* Some performance improvements through increased use of pointers and C API functions.



# collapse 1.3.2
collapse 1.3.2, released mid September 2020: <!-- , is a minor update: -->

* Fixed a small bug in `fNdistinct` for grouped distinct value counts on logical vectors. 

* Additional security for `ftransform`, which now efficiently checks the names of the data and replacement arguments for uniqueness, and also allows computing and transforming list-columns.  

* Added function `ftransformv` to facilitate transforming selected columns with function - a very efficient replacement for `dplyr::mutate_if` and `dplyr::mutate_at`. 

* `frename` now allows additional arguments to be passed to a renaming function.  

# collapse 1.3.1
collapse 1.3.1, released end of August 2020, is a patch for v1.3.0 that takes care of some unit test failures on certain operating systems (mostly because of numeric precision issues). It provides no changes to the code or functionality.

# collapse 1.3.0
collapse 1.3.0, released mid August 2020: <!-- , is another major update: -->

### Changes to Functionality

* `dapply` and `BY` now drop all unnecessary attributes if `return = "matrix"` or `return = "data.frame"` are explicitly requested (the default `return = "same"` still seeks to preserve the input data structure).

* `unlist2d` now saves integer rownames if `row.names = TRUE` and a list of matrices without rownames is passed, and `id.factor = TRUE` generates a normal factor not an ordered factor. It is however possible to write `id.factor = "ordered"` to get an ordered factor id.  

* `fdiff` argument `logdiff` renamed to `log`, and taking logs is now done in R (reduces size of C++ code and does not generate as many NaN's). `logdiff` may still be used, but it may be deactivated in the future. Also in the matrix and data.frame methods for `flag`, `fdiff` and `fgrowth`, columns are only stub-renamed if more than one lag/difference/growth rate is computed. 

### Additions

* Added `fnth` for fast (grouped, weighted) n'th element/quantile computations.

* Added `roworder(v)` and `colorder(v)` for fast row and column reordering.  

* Added `frename` and `setrename` for fast and flexible renaming (by reference).  

* Added function `fungroup`, as replacement for `dplyr::ungroup`, intended for use with `fgroup_by`. 

* `fmedian` now supports weights, computing a decently fast (grouped) weighted median based on radix ordering. 

* `fmode` now has the option to compute min and max mode, the default is still simply the first mode. 

* `fwithin` now supports quasi-demeaning (added argument `theta`) and can thus be used to manually estimate random-effects models. 

* `funique` is now generic with a default vector and data.frame method, providing fast unique values and rows of data. The default was changed to `sort = FALSE`.   

* The shortcut `gvr` was created for `get_vars(..., regex = TRUE)`. 

* A helper `.c` was introduced for non-standard concatenation (i.e. `.c(a, b) == c("a", "b")`). 

### Improvements

* `fmode` and `fNdistinct` have become a bit faster.

* `fgroup_by` now preserves *data.table*'s.

* `ftransform` now also supports a data.frame as replacement argument, which automatically replaces matching columns and adds unmatched ones. Also `ftransform<-` was created as a more formal replacement method for this feature.

* `collap` columns selected through `cols` argument are returned in the order selected if `keep.col.order = FALSE`. Argument `sort.row` is depreciated, and replace by argument `sort`. In addition the `decreasing` and `na.last` arguments were added and handed down to `GRP.default`. 

* `radixorder` 'sorted' attribute is now always attached.

* `stats::D` which is masked when collapse is attached, is now preserved through methods `D.expression` and `D.call`. 

* `GRP` option `call = FALSE` to omit a call to `match.call` -> minor performance improvement.

* Several small performance improvements through rewriting some internal helper functions in C and reworking some R code. 

* Performance improvements for some helper functions, `setRownames` / `setColnames`, `na_insert` etc.  

* Increased scope of testing statistical functions. The functionality of the package is now secured by 7700 unit tests covering all central bits and pieces. 


# collapse 1.2.1
collapse 1.2.1, released end of May 2020: <!-- , is a patch for v1.2.0: -->

* Minor fixes for 1.2.0 issues that prevented correct installation on Mac OS X and a vignette rebuilding error on solaris.

* `fmode.grouped_df` with groups and weights now saves the sum of the weights instead of the max (this makes more sense as the max only applies if all elements are unique). 

# collapse 1.2.0
collapse 1.2.0, released mid May 2020: <!-- , is a major update of the package - changes and additions: -->

### Changes to Functionality
* *grouped_df* methods for fast statistical functions now always attach the grouping variables to the output in aggregations, unless argument `keep.group_vars = FALSE`. (formerly grouping variables were only attached if also present in the data. Code hinged on this feature should be adjusted)

* `qF` `ordered` argument default was changed to `ordered = FALSE`, and the `NA` level is only added if `na.exclude = FALSE`. Thus `qF` now behaves exactly like `as.factor`. 

* `Recode` is depreciated in favor of `recode_num` and `recode_char`, it will be removed soon. Similarly `replace_non_finite` was renamed to `replace_Inf`. 

* In `mrtl` and `mctl` the argument `ret` was renamed `return` and now takes descriptive character arguments (the previous version was a direct C++ export and unsafe, code written with these functions should be adjusted). 

* `GRP` argument `order` is depreciated in favor of argument `decreasing`. `order` can still be used but will be removed at some point. 

### Bug Fixes
* Fixed a bug in `flag` where unused factor levels caused a group size error. 

<!-- It is still recommended to remove unused factor levels when programming with factors, some functions check for them, others not. For example `fmean(data, f)` will simply generate a missing row for each unused factor level. If in doubt, use safer `GRP` objects for grouped programming. A general level check for all functions will not be implemented as this requires an additional pass in some cases. -->

### Additions

* Added a suite of functions for fast data manipulation: 
  + `fselect` selects variables from a data frame and is equivalent but much faster than `dplyr::select`.
  + `fsubset` is a much faster version of `base::subset` to subset vectors, matrices and data.frames. The function `ss` was also added as a faster alternative to `[.data.frame`. 
  + `ftransform` is a much faster update of `base::transform`, to transform data frames by adding, modifying or deleting columns. The function `settransform` does all of that by reference.
  + `fcompute` is equivalent to `ftransform` but returns a new data frame containing only the columns computed from an existing one. 
  + `na_omit` is a much faster and enhanced version of `base::na.omit`. 
  + `replace_NA` efficiently replaces missing values in multi-type data. 
  
  
* Added function `fgroup_by` as a much faster version of `dplyr::group_by` based on *collapse* grouping. It attaches a 'GRP' object to a data frame, but only works with *collapse*'s fast functions. This allows *dplyr* like manipulations that are fully *collapse* based and thus significantly faster, i.e. `data %>% fgroup_by(g1,g2) %>% fselect(cola,colb) %>% fmean`. Note that `data %>% dplyr::group_by(g1,g2) %>% dplyr::select(cola,colb) %>% fmean` still works, in which case the *dplyr* 'group' object is converted to 'GRP' as before. However `data %>% fgroup_by(g1,g2) %>% dplyr::summarize(...)` does not work.

* Added function `varying` to efficiently check the variation of multi-type data over a dimension or within groups.

* Added function `radixorder`, same as `base::order(..., method = "radix")` but more accessible and with built-in grouping features. 

* Added functions `seqid` and `groupid` for generalized run-length type id variable generation from grouping and time variables. `seqid` in particular strongly facilitates lagging / differencing irregularly spaced panels using `flag`, `fdiff` etc. 

* `fdiff` now supports quasi-differences i.e. $x_t - \rho x_{t-1}$ and quasi-log differences i.e. $log(x_t) - \rho log(x_{t-1})$. an arbitrary $\rho$ can be supplied.

* Added a `Dlog` operator for faster access to log-differences. 

### Improvements
* Faster grouping with `GRP` and faster factor generation with added radix method + automatic dispatch between hash and radix method. `qF` is now ~ 5x faster than `as.factor` on character and around 30x faster on numeric data. Also `qG` was enhanced. 

* Further slight speed tweaks here and there. 

* `collap` now provides more control for weighted aggregations with additional arguments `w`, `keep.w` and `wFUN` to aggregate the weights as well. The defaults are `keep.w = TRUE` and `wFUN = fsum`. A specialty of `collap` remains that `keep.by` and `keep.w` also work for external objects passed, so code of the form `collap(data, by, FUN, catFUN, w = data$weights)` will now have an aggregated `weights` vector in the first column. 

<!-- In such cases use `keep.w = FALSE` to omit the weights or `collap(data, by, FUN, catFUN, w = ~ weights)` to keep the column order. -->

* `qsu` now also allows weights to be passed in formula i.e. `qsu(data, by = ~ group, pid = ~ panelid, w = ~ weights)`. 

* `fgrowth` has a `scale` argument, the default is `scale = 100` which provides growth rates in percentage terms (as before), but this may now be changed. 

* All statistical and transformation functions now have a hidden list method, so they can be applied to unclassed list-objects as well. An error is however provided in grouped operations with unequal-length columns. 



# collapse 1.1.0
collapse 1.1.0 released early April 2020: <!--  - some small fixes and additions: -->

* Fixed remaining gcc10, LTO and valgrind issues in C/C++ code, and added some more tests (there are now ~ 5300 tests ensuring that *collapse* statistical functions perform as expected).

* Fixed the issue that supplying an unnamed list to `GRP()`, i.e. `GRP(list(v1, v2))` would give an error. Unnamed lists are now automatically named 'Group.1', 'Group.2', etc...

* Fixed an issue where aggregating by a single id in `collap()` (i.e. `collap(data, ~ id1)`), the id would be coded as factor in the aggregated data.frame. All variables including id's now retain their class and attributes in the aggregated data.

* Added weights (`w`) argument to `fsum` and `fprod`. 

* Added an argument `mean = 0` to `fwithin / W`. This allows simple and grouped centering on an arbitrary mean, `0` being the default. For grouped centering `mean = "overall.mean"` can be specified, which will center data on the overall mean of the data. The logical argument `add.global.mean = TRUE` used to toggle this in *collapse* 1.0.0 is therefore depreciated. 

* Added arguments `mean = 0` (the default) and `sd = 1` (the default) to `fscale / STD`. These arguments now allow to (group) scale and center data to an arbitrary mean and standard deviation. Setting `mean = FALSE` will just scale data while preserving the mean(s). Special options for grouped scaling are `mean = "overall.mean"` (same as `fwithin / W`), and `sd = "within.sd"`, which will scale the data such that the standard deviation of each group is equal to the within- standard deviation (= the standard deviation computed on the group-centered data). Thus group scaling a panel-dataset with `mean = "overall.mean"` and `sd = "within.sd"` harmonizes the data across all groups in terms of both mean and variance. The fast algorithm for variance calculation toggled with `stable.algo = FALSE` was removed from `fscale`. Welford's numerically stable algorithm used by default is fast enough for all practical purposes. The fast algorithm is still available for `fvar` and `fsd`. 

* Added the modulus (`%%`) and subtract modulus (`-%%`) operations to `TRA()`. 

* Added the function `finteraction`, for fast interactions, and `as.character_factor` to coerce a factor, or all factors in a list, to character (analogous to `as.numeric_factor`). Also exported the function `ckmatch`, for matching with error message showing non-matched elements.


# collapse 1.0.0 and earlier

* First version of the package featuring only the functions `collap` and `qsu` based on code shared by Sebastian Krantz on R-devel, February 2019.

* Major rework of the package using Rcpp and data.table internals, introduction of fast statistical functions and operators and expansion of the scope of the package to a broad set of data transformation and exploration tasks. Several iterations of enhancing speed of R code. Seamless integration of *collapse* with *dplyr*, *plm* and *data.table*. CRAN release of *collapse* 1.0.0 on 19th March 2020. 

