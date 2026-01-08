# Print method for objects of the class summaryAlphaPart.

Print method for objects of the class `summaryAlphaPart` (result of
`summary(AlphaPart(...))`).

## Usage

``` r
# S3 method for class 'summaryAlphaPart'
print(x, ...)
```

## Arguments

- x:

  summaryAlphaPart, output object from
  [`summary.AlphaPart`](https://alphagenes.github.io/AlphaPart/reference/summary.AlphaPart.md)
  function.

- ...:

  Arguments passed to other functions (not used at the moment).

## See also

[`summary.AlphaPart`](https://alphagenes.github.io/AlphaPart/reference/summary.AlphaPart.md)

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
