# AlphaPartSum.R

A function to sum partitions of several paths.

## Usage

``` r
AlphaPartSum(
  x,
  map = NULL,
  remove = TRUE,
  zeroPath = TRUE,
  call = "AlphaPartSum"
)
```

## Arguments

- x:

  summaryAlphaPart, object from the `AlphaPart(...)` or
  `summary(AlphaPart(...), ...)` call.

- map:

  List, a map of summing paths; see details and examples.

- remove:

  Logical, remove original paths or not.

- zeroPath:

  Logical, set called path to zero if it does not exist.

- call:

  character, for internal use with `AlphaPartSubset`).

## Value

An object of class `AlphaPart` or `summaryAlphaPart` with modified
partitions. Meta information in slot "info" is modified as well.

## Details

Sometimes partitions of particular paths are very small or we want to
sum paths that have some similarity. These actions are easy to achive
manually but this functions provides a way to do this consistently with
the given object `x`.

Arguments `map` must be a list of vectors of length at least two.
Vectors of length one are skipped. The idea is that the first element is
the new or existing path into which we add up all the remaining
specified paths, say `list(c("A", "B"), c("X", "X", "Y"), c("Z", "X"))`
would imply A = B, X = X + Y, and Z = X = X + Y. Note that once X is
changed its changed value is used in further calculations. Specify
different (new) names for new targets if you want to avoid this.

Be carefull with `remove=TRUE`, which is the default setting, as all
partitions defined after the first (target/new) partition in vector in
list will be removed, for example with
`list(c("A", "B"), c("X", "X", "Y"), c("Z", "X"))` partitions B and Y
will be removed, while X will not be removed as it is defined as a
target/new partition.

## See also

[`AlphaPart`](https://alphagenes.github.io/AlphaPart/reference/AlphaPart.md)
for the main method,
[`summary.AlphaPart`](https://alphagenes.github.io/AlphaPart/reference/summary.AlphaPart.md)
for summary method that works on output of `AlphaPart`,
[`AlphaPartSubset`](https://alphagenes.github.io/AlphaPart/reference/AlphaPartSubset.md)
for subset/keep method

## Examples

``` r
## Small pedigree with additive genetic (=breeding) values
ped <- data.frame(  id=c(  1,   2,   3,   4,   5,   6),
                  fid=c(  0,   0,   2,   0,   4,   0),
                  mid=c(  0,   0,   1,   0,   3,   3),
                  loc=c("A", "B", "A", "B", "A", "A"),
                  gen=c(  1,   1,   2,   2,   3,   3),
                 trt1=c(100, 120, 115, 130, 125, 125),
                 trt2=c(100, 110, 105,  140,  85, 110))

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
#> 1  1   0   0   A   1  100 116.6667 -16.666667  100.0    0.0
#> 2  2   0   0   B   1  110 116.6667  -6.666667    0.0  110.0
#> 3  3   2   1   A   2  105 105.0000   0.000000   50.0   55.0
#> 4  4   0   0   B   2  140 116.6667  23.333333    0.0  140.0
#> 5  5   4   3   A   3   85 122.5000 -37.500000  -12.5   97.5
#> 6  6   0   3   A   3  110  52.5000  57.500000   82.5   27.5
#> 

## Sum some partitions (working on object of class AlphaPart)
(tmp2 <- AlphaPartSum(x=tmp, map=list(c("X", "A", "B"), c("A", "B"))))
#> 
#> 
#>  Partitions of breeding values 
#>    - individuals: 6 
#>    - paths: 2 (A, X)
#>    - traits: 2 (trt1, trt2)
#>    - warning: Consistency of the overall sum of partitions might not be correct due to the previous use of 'AlphaPartSum'
#> 
#>  Trait: trt1 
#> 
#>   id fid mid loc gen trt1  trt1_pa     trt1_w trt1_A trt1_X
#> 1  1   0   0   A   1  100 116.6667 -16.666667      0    100
#> 2  2   0   0   B   1  120 116.6667   3.333333    120    120
#> 3  3   2   1   A   2  115 110.0000   5.000000     60    115
#> 4  4   0   0   B   2  130 116.6667  13.333333    130    130
#> 5  5   4   3   A   3  125 122.5000   2.500000     95    125
#> 6  6   0   3   A   3  125  57.5000  67.500000     30    125
#> 
#>  Trait: trt2 
#> 
#>   id fid mid loc gen trt2  trt2_pa     trt2_w trt2_A trt2_X
#> 1  1   0   0   A   1  100 116.6667 -16.666667    0.0    100
#> 2  2   0   0   B   1  110 116.6667  -6.666667  110.0    110
#> 3  3   2   1   A   2  105 105.0000   0.000000   55.0    105
#> 4  4   0   0   B   2  140 116.6667  23.333333  140.0    140
#> 5  5   4   3   A   3   85 122.5000 -37.500000   97.5     85
#> 6  6   0   3   A   3  110  52.5000  57.500000   27.5    110
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
#> 2   2 2 122.5 25 97.5
#> 3   3 2  97.5 35 62.5
#> 

## Sum some partitions (working on object of class summaryAlphaPart)
(tmpS2 <- AlphaPartSum(x=tmpS, map=list(c("X", "A", "B"), c("A", "B"))))
#> 
#> 
#>  Summary of partitions of breeding values 
#>    - paths: 2 (A, X)
#>    - traits: 2 (trt1, trt2)
#>    - warning: Consistency of the overall sum of partitions might not be correct due to the previous use of 'AlphaPartSum'
#> 
#>  Trait: trt1 
#> 
#>   gen N   Sum    A     X
#> 1   1 2 110.0 60.0 110.0
#> 2   2 2 122.5 95.0 122.5
#> 3   3 2 125.0 62.5 125.0
#> 
#>  Trait: trt2 
#> 
#>   gen N   Sum    A     X
#> 1   1 2 105.0 55.0 105.0
#> 2   2 2 122.5 97.5 122.5
#> 3   3 2  97.5 62.5  97.5
#> 

## ... must be equal to
(tmpS3 <- summary(tmp2, by="gen"))
#> 
#> 
#>  Summary of partitions of breeding values 
#>    - paths: 2 (A, X)
#>    - traits: 2 (trt1, trt2)
#>    - warning: Consistency of the overall sum of partitions might not be correct due to the previous use of 'AlphaPartSum'
#> 
#>  Trait: trt1 
#> 
#>   gen N   Sum    A     X
#> 1   1 2 110.0 60.0 110.0
#> 2   2 2 122.5 95.0 122.5
#> 3   3 2 125.0 62.5 125.0
#> 
#>  Trait: trt2 
#> 
#>   gen N   Sum    A     X
#> 1   1 2 105.0 55.0 105.0
#> 2   2 2 122.5 97.5 122.5
#> 3   3 2  97.5 62.5  97.5
#> 
```
