# pedFixBirthYear.R

A function to fix (impute) missing birth years in pedigree.

## Usage

``` r
pedFixBirthYear(
  x,
  interval,
  down = FALSE,
  na.rm = TRUE,
  sort = TRUE,
  direct = TRUE,
  report = TRUE,
  colId = 1,
  colFid = 2,
  colMid = 3,
  colBY = 4
)
```

## Arguments

- x:

  data.frame , with (at least) the following columns: individual,
  father, and mother identification, and year of birth; see arguments
  `colId`, `colFid`, `colMid`, and `colBY`

- interval:

  Numeric, a value for generation interval in years.

- down:

  Logical, the default is to impute birth years based on the birth year
  of children starting from the youngest to the oldest individuals,
  while with `down=TRUE` birth year is imputed based on the birth year
  of parents in the opposite order.

- na.rm:

  Logical, remove `NA` values when searching for the minimal (maximal)
  year of birth in children (parents); setting this to `FALSE` can lead
  to decreased success of imputation

- sort:

  Logical, initially sort `x` using `orderPed()` so that children follow
  parents in order to make imputation as optimal as possible (imputation
  is performed within a loop from the first to the last unknown birth
  year); at the end original order is restored.

- direct:

  Logical, insert inferred birth years immediately so they can be used
  for successive individuals within the loop.

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

- colBY:

  Numeric or character, position or name of a column holding birth year.

## Value

Object `x` with imputed birth years based on the birth year of children
or parents. If `report=TRUE` success is printed on the screen as the
number of initially, fixed, and left unknown birth years is printed.

## Details

Warnings are issued when there is no information to use to impute birth
years or missing values (`NA`) are propagated.

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
## Example pedigree with missing (unknown) birth year for some individuals
ped0 <- data.frame(     id=c( 1, 2, 3,  4, 5, 6, 7,  8, 9, 10, 11, 12, 13, 14),
                        fid=c( 0, 0, 0,  1, 1, 1, 3,  3, 3,  5,  4,  0,  0, 12),
                        mid=c( 0, 0, 0,  2, 0, 2, 2,  2, 5,  0,  0,  0,  0, 13),
                        birth_dt=c(NA, 0, 1, NA, 3, 3, 3, 3, 4, 4, 5, NA, 6, 6) + 2000)

## First run - using information from children
ped1 <- pedFixBirthYear(x=ped0, interval=1)
#> Summary:
#>  - initially: 3 
#>  - fixed: 3 
#>  - left: 0 

## Second run - using information from parents
ped2 <- pedFixBirthYear(x=ped1, interval=1, down=TRUE)
#> Summary:
#>  - initially: 0 
#>  - fixed: 0 
#>  - left: 0 

## Third run - using information from children, but with no success
ped3 <- pedFixBirthYear(x=ped2, interval=1)
#> Summary:
#>  - initially: 0 
#>  - fixed: 0 
#>  - left: 0 
```
