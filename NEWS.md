# FSAmisc 0.0.3 Jun22
* Fixed calls to functions in other packages by explicitly using `::`.
* Used `:::` (to "Rcmd Check's chagrin") to several internal functions in `FSA` so that those functions would work here (with the `FSA` dependency).
* `compIntercepts()`: Added. From `FSA`.
* `compSlopes()`: Added. From `FSA`.
* `diags()`: Added. From `FSA`.
* `dietOverlap()`: Modified. Corrected formula in Morisita's index (thanks to Colt Holley). 
* `filterD()`: Added. From `FSA`.
* `fitPlot()`: Added. From `FSA`.
* `hoCoefs()`: Added. From `FSA`.
* `residPlot()`: Added. From `FSA`.


# FSAmisc 0.0.2 Aug15
* `gReshape()`: Added.  From `FSA`.

# FSAmisc 0.0.1 Jul15
* Initial population.  Brought several functions and data sets from `FSA` v0.6.25.
