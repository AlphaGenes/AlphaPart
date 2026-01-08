# AlphaPartSubset.R

A function to choose the partition paths to keep.

## Usage

``` r
AlphaPartSubset(x, paths = NULL)
```

## Arguments

- x:

  AlphaPart or summaryAlphaPart, object from the `AlphaPart(...)` or
  `summary(AlphaPart(...), ...)` call.

- paths:

  Character, names of paths to be kept.

## Value

An object of class `AlphaPart` or `summaryAlphaPart` with only some
paths. Meta information in slot "info" is modified as well.

## Details

Displaying results of partitions for many paths is often confusing. This
function helps in selecting only paths of interest. Unspecified paths
are removed from the input object `x`. Meta information is modified
accordingly. Default setting does nothing.

## See also

[`AlphaPart`](https://alphagenes.github.io/AlphaPart/reference/AlphaPart.md)
for the main method,
[`summary.AlphaPart`](https://alphagenes.github.io/AlphaPart/reference/summary.AlphaPart.md)
for summary method that works on output of `AlphaPart`,
[`AlphaPartSum`](https://alphagenes.github.io/AlphaPart/reference/AlphaPartSum.md)
for sum method.

## Examples

``` r
## Small pedigree with additive genetic (=breeding) values
ped <- data.frame(  id=c(  1,   2,   3,   4,   5,   6),
                  fid=c(  0,   0,   2,   0,   4,   0),
                  mid=c(  0,   0,   1,   0,   3,   3),
                  loc=c("A", "B", "A", "B", "A", "A"),
                  gen=c(  1,   1,   2,   2,   3,   3),
                 trt1=c(100, 120, 115, 130, 125, 125),
                 trt2=c(100, 110, 105, 100,  85, 110))

## Partition additive genetic values
(tmp <- AlphaPart(x=ped, colBV=c("trt1", "trt2")))
#> 
#> Size:
#>  - individuals: 6 
#>  - traits: 2 (trt1, trt2)
#>  - paths: 2 (A, B)
#>  - unknown (missing) values:
#> trt1 trt2 
#>    0    0 
#> 
#> 
#>  Partitions of breeding values 
#>    - individuals: 6 
#>    - paths: 2 (A, B)
#>    - traits: 2 (trt1, trt2)
#> 
#>  Trait: trt1 
#> 
#>   id fid mid loc gen trt1  trt1_pa     trt1_w trt1_A trt1_B
#> 1  1   0   0   A   1  100 116.6667 -16.666667    100      0
#> 2  2   0   0   B   1  120 116.6667   3.333333      0    120
#> 3  3   2   1   A   2  115 110.0000   5.000000     55     60
#> 4  4   0   0   B   2  130 116.6667  13.333333      0    130
#> 5  5   4   3   A   3  125 122.5000   2.500000     30     95
#> 6  6   0   3   A   3  125  57.5000  67.500000     95     30
#> 
#>  Trait: trt2 
#> 
#>   id fid mid loc gen trt2  trt2_pa     trt2_w trt2_A trt2_B
#> 1  1   0   0   A   1  100 103.3333  -3.333333  100.0    0.0
#> 2  2   0   0   B   1  110 103.3333   6.666667    0.0  110.0
#> 3  3   2   1   A   2  105 105.0000   0.000000   50.0   55.0
#> 4  4   0   0   B   2  100 103.3333  -3.333333    0.0  100.0
#> 5  5   4   3   A   3   85 102.5000 -17.500000    7.5   77.5
#> 6  6   0   3   A   3  110  52.5000  57.500000   82.5   27.5
#> 

## Keep some partitions (working on object of class AlphaPart)
(tmp2 <- AlphaPartSubset(x=tmp, paths="A"))
#> 
#> 
#>  Partitions of breeding values 
#>    - individuals: 6 
#>    - paths: 1 (A)
#>    - traits: 2 (trt1, trt2)
#>    - warning: Consistency of the overall sum of partitions might not be correct due to the previous use of 'AlphaPartPathSubset'
#> 
#>  Trait: trt1 
#> 
#>   id fid mid loc gen trt1  trt1_pa     trt1_w trt1_A
#> 1  1   0   0   A   1  100 116.6667 -16.666667    100
#> 2  2   0   0   B   1  120 116.6667   3.333333      0
#> 3  3   2   1   A   2  115 110.0000   5.000000     55
#> 4  4   0   0   B   2  130 116.6667  13.333333      0
#> 5  5   4   3   A   3  125 122.5000   2.500000     30
#> 6  6   0   3   A   3  125  57.5000  67.500000     95
#> 
#>  Trait: trt2 
#> 
#>   id fid mid loc gen trt2  trt2_pa     trt2_w trt2_A
#> 1  1   0   0   A   1  100 103.3333  -3.333333  100.0
#> 2  2   0   0   B   1  110 103.3333   6.666667    0.0
#> 3  3   2   1   A   2  105 105.0000   0.000000   50.0
#> 4  4   0   0   B   2  100 103.3333  -3.333333    0.0
#> 5  5   4   3   A   3   85 102.5000 -17.500000    7.5
#> 6  6   0   3   A   3  110  52.5000  57.500000   82.5
#> 

## Summarize by generation
(tmpS <- summary(tmp, by="gen"))
#> 
#> 
#>  Summary of partitions of breeding values 
#>    - paths: 2 (A, B)
#>    - traits: 2 (trt1, trt2)
#> 
#>  Trait: trt1 
#> 
#>   gen N   Sum    A    B
#> 1   1 2 110.0 50.0 60.0
#> 2   2 2 122.5 27.5 95.0
#> 3   3 2 125.0 62.5 62.5
#> 
#>  Trait: trt2 
#> 
#>   gen N   Sum  A    B
#> 1   1 2 105.0 50 55.0
#> 2   2 2 102.5 25 77.5
#> 3   3 2  97.5 45 52.5
#> 

## Keep some partitions (working on object of class summaryAlphaPart)
(tmpS2 <- AlphaPartSubset(x=tmpS, paths="A"))
#> 
#> 
#>  Summary of partitions of breeding values 
#>    - paths: 1 (A)
#>    - traits: 2 (trt1, trt2)
#>    - warning: Consistency of the overall sum of partitions might not be correct due to the previous use of 'AlphaPartPathSubset'
#> 
#>  Trait: trt1 
#> 
#>   gen N   Sum    A
#> 1   1 2 110.0 50.0
#> 2   2 2 122.5 27.5
#> 3   3 2 125.0 62.5
#> 
#>  Trait: trt2 
#> 
#>   gen N   Sum  A
#> 1   1 2 105.0 50
#> 2   2 2 102.5 25
#> 3   3 2  97.5 45
#> 

## ... must be equal to
(tmpS3 <- summary(tmp2, by="gen"))
#> 
#> 
#>  Summary of partitions of breeding values 
#>    - paths: 1 (A)
#>    - traits: 2 (trt1, trt2)
#>    - warning: Consistency of the overall sum of partitions might not be correct due to the previous use of 'AlphaPartPathSubset'
#> 
#>  Trait: trt1 
#> 
#>   gen N   Sum    A
#> 1   1 2 110.0 50.0
#> 2   2 2 122.5 27.5
#> 3   3 2 125.0 62.5
#> 
#>  Trait: trt2 
#> 
#>   gen N   Sum  A
#> 1   1 2 105.0 50
#> 2   2 2 102.5 25
#> 3   3 2  97.5 45
#> 
```
