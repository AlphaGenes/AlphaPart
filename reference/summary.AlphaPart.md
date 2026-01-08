# A function to summarize AlphaPart object.

Breedng values of individuals are often summarized, either by year of
birth or some other classification. Function `summary.AlphaPart`
provides a way to ease the computation of such summaries on partitions
of breeding values.

## Usage

``` r
# S3 method for class 'AlphaPart'
summary(object, by, FUN, labelSum, subset,
  sums, cov,  ...)
```

## Arguments

- object:

  AlphaPart, output object from
  [`AlphaPart`](https://alphagenes.github.io/AlphaPart/reference/AlphaPart.md)
  function.

- by:

  Character, the name of a column by which summary function FUN should
  be applied; if `NULL` (default) summary is given for the whole table.

- FUN:

  Function, which function should be used in summary; function should
  return single value per each level of by.

- labelSum:

  Character, label used for the overall breeding value.

- subset:

  Logical, perform summary only on a subset of `object` subsetted by
  this argument.

- sums:

  Logical, link between
  [`AlphaPart`](https://alphagenes.github.io/AlphaPart/reference/AlphaPart.md)
  and `summary.AlphaPart()` (only for internal use!).

- cov:

  Logical, if FALSE returns `n` variances plus one additional column
  containing two times the sum of all covariances; otherwise returns `n`
  variance and `n(n-1)/2` covariances in the form of `2*Cov(., .)`,
  where `n` is the number of partitions. This argument only works when
  `FUN = var`. Defaut `cov = FALSE`.

- ...:

  Arguments passed to other functions (not used at the moment).

## Value

An object of class `summaryAlphaPart`, which is a list of data frames
with summary statistics on breeding value partitions. For each trait
there a dataframe holds summary for the "whole/original" breeding value
and its partitions. In addition another list is added (named `info`)
with the following components holdinfg meta info:

- `path` column name holding path information

- `nP` number of paths

- `lP` path labels

- `nT` number of traits

- `lT` trait labels

- `by` column name of variable by which summary was performed

- `warn` potential warning messages associated with this object

- `labelSum` column name of summary for "whole/original" breeding values

There is a handy plot method
([`plot.summaryAlphaPart`](https://alphagenes.github.io/AlphaPart/reference/plot.summaryAlphaPart.md))
for output.

## See also

[`AlphaPart`](https://alphagenes.github.io/AlphaPart/reference/AlphaPart.md)
for partitioning breeding values,
[`plot.summaryAlphaPart`](https://alphagenes.github.io/AlphaPart/reference/plot.summaryAlphaPart.md)
for plotting output of summary method

## Examples

``` r
## --- Partition additive genetic values by loc ---
res <- AlphaPart(x=AlphaPart.ped, colPath="country", colBV=c("bv1", "bv2"))
#> 
#> Size:
#>  - individuals: 8 
#>  - traits: 2 (bv1, bv2)
#>  - paths: 2 (domestic, import)
#>  - unknown (missing) values:
#> bv1 bv2 
#>   0   0 

## Summarize whole population
ret <- summary(res)

## Summarize population by generation (=trend)
ret <- summary(res, by="gen")

## Summarize population by generation (=trend) but only for domestic location
ret <- summary(res, by="gen", subset=res[[1]]$country == "domestic")

## --- Partition additive genetic values by loc and gender ---

AlphaPart.ped$country.gender <- with(AlphaPart.ped, paste(country, gender, sep="-"))
res <- AlphaPart(x=AlphaPart.ped, colPath="country.gender", colBV=c("bv1", "bv2"))
#> 
#> Size:
#>  - individuals: 8 
#>  - traits: 2 (bv1, bv2)
#>  - paths: 4 (domestic-F, domestic-M, import-F, import-M)
#>  - unknown (missing) values:
#> bv1 bv2 
#>   0   0 

## Summarize population by generation (=trend)
ret <- summary(res, by="gen")

## Summarize population by generation (=trend) but only for domestic location
ret <- summary(res, by="gen", subset=res[[1]]$country == "domestic")
```
