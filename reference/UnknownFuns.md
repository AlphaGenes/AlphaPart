# Change given unknown value to NA and vice versa.

Unknown or missing values (`NA` in `R`) can be represented in various
ways (as 0, 999, etc.) in different programs. `isUnknown`,
`unknownToNA`, and `NAToUnknown` can help to change unknown values to
`NA` and vice versa.

## Usage

``` r
isUnknown(x, unknown=NA, ...)

unknownToNA(x, unknown, warning=FALSE, ...)

NAToUnknown(x, unknown, force=FALSE, call.=FALSE, ...)

# Default S3 method
isUnknown(x, unknown, ...)

# S3 method for class 'POSIXlt'
isUnknown(x, unknown, ...)

# S3 method for class 'list'
isUnknown(x, unknown, ...)

# S3 method for class 'data.frame'
isUnknown(x, unknown, ...)

# S3 method for class 'matrix'
isUnknown(x, unknown, ...)

unknownToNA(x, unknown, warning = FALSE, ...)

# Default S3 method
unknownToNA(x, unknown, warning, ...)

# S3 method for class 'factor'
unknownToNA(x, unknown, warning, ...)

# S3 method for class 'list'
unknownToNA(x, unknown, warning, ...)

# S3 method for class 'data.frame'
unknownToNA(x, unknown, warning, ...)

NAToUnknown(x, unknown, force = FALSE, call. = FALSE, ...)

# Default S3 method
NAToUnknown(x, unknown, force, call., ...)

# S3 method for class 'factor'
NAToUnknown(x, unknown, force, call., ...)

# S3 method for class 'list'
NAToUnknown(x, unknown, force, call., ...)

# S3 method for class 'data.frame'
NAToUnknown(x, unknown, force, call., ...)

.unknownList(x, unknown)
```

## Arguments

- x:

  generic, object with unknown value(s)

- unknown:

  generic, value used instead of `NA`

- ...:

  arguments pased to other methods (as.character for POSIXlt in case of
  isUnknown)

- warning:

  logical, issue warning if `x` already has `NA`

- force:

  logical, force to apply already existing value in `x`

- call.:

  logical, look in [`warning`](https://rdrr.io/r/base/warning.html)

## See also

[`AlphaPart`](https://alphagenes.github.io/AlphaPart/reference/AlphaPart.md)

## Author

Gregor Gorjanc
