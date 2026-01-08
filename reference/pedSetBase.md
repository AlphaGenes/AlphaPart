# pedSetBase.R

A function to set the base population in the pedigree.

## Usage

``` r
pedSetBase(
  x,
  keep = NULL,
  unknown = NA,
  report = TRUE,
  colId = 1,
  colFid = 2,
  colMid = 3
)
```

## Arguments

- x:

  data.frame , with (at least) the following columns: individual,
  father, and mother identification, and year of birth; see arguments
  `colId`, `colFid`, `colMid`, and `colBY`

- keep:

  Logical, indicator that defines which individuals should stay in the
  the pedigree; see details.

- unknown:

  Value used to represent unknown/missing identification

- report:

  Logical, report success.

- colId:

  Numeric or character, position or name of a column holding individual
  identification.

- colFid:

  Numeric or character, position or name of a column holding father
  identification.

- colMid:

  Numeric or character, position or name of a column holding mother
  identification.

## Value

Object `x` with removed rows for some individuals and their presence as
parents. If `report=TRUE` progress is printed on the screen.

## Details

Base population in the pedigree is set by removing rows for some
individuals, while their presence as parents is also removed.

Arguments `down` and `na.rm` allow for repeated use of this function,
i.e., with `down=FALSE` and with `down=TRUE` (both in combination with
`na.rm=TRUE`) in order to propagate information over the pedigree until
"convergence".

This function can be very slow on large pedigrees with extensive
missingness of birth years.

## See also

[`orderPed`](https://rdrr.io/pkg/pedigree/man/orderPed.html) in pedigree
package

## Examples

``` r
## Example pedigree
ped <- data.frame(      id=1:10,
                       fid=c(0, 0, 0, 1, 1, 1, 3, 3, 3, 5),
                       mid=c(0, 0, 0, 2, 0, 2, 2, 2, 5, 0),
                  birth_dt=c(0, 0, 1, 2, 3, 3, 3, 4, 4, 5) + 2000)

## Set base population as those individuals that were born after year 2002
pedSetBase(x=ped, keep=ped$birth_dt > 2002, unknown=0)
#> All individuals: 10 
#> Removing: 4, 40 %
#> Kept: 6 
#>    id fid mid birth_dt
#> 5   5   0   0     2003
#> 6   6   0   0     2003
#> 7   7   0   0     2003
#> 8   8   0   0     2004
#> 9   9   0   5     2004
#> 10 10   5   0     2005
```
