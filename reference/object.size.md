# Report the Space Allocated for Objects

Provides an estimate of the memory that is being used to store `R`
objects.

## Usage

``` r
# S3 method for class 'object_sizes'
print(
  x,
  quote = FALSE,
  humanReadable = getOption("humanReadable"),
  standard = "IEC",
  units,
  digits = 1,
  width = NULL,
  sep = " ",
  justify = c("right", "left"),
  ...
)

# S3 method for class 'object_sizes'
format(
  x,
  humanReadable = getOption("humanReadable"),
  standard = "IEC",
  units,
  digits = 1,
  width = NULL,
  sep = " ",
  justify = c("right", "left"),
  ...
)

is.object_sizes(x)

as.object_sizes(x)

# S3 method for class 'object_sizes'
c(..., recursive = FALSE)
```

## See also

[`AlphaPart`](https://alphagenes.github.io/AlphaPart/reference/AlphaPart.md)
