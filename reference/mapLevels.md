# Mapping levels

`mapLevels` produces a map with information on levels and/or internal
integer codes. As such can be conveniently used to store level mapping
when one needs to work with internal codes of a factor and later
transfrorm back to factor or when working with several factors that
should have the same levels and therefore the same internal coding.

## Usage

``` r
mapLevels(x, codes=TRUE, sort=TRUE, drop=FALSE, combine=FALSE, ...)

mapLevels(x) <- value

# Default S3 method
mapLevels(x, codes, sort, drop, combine, ...)

# S3 method for class 'character'
mapLevels(x, codes, sort, drop, combine, ...)

# S3 method for class 'factor'
mapLevels(x, codes, sort, drop, combine, ...)

# S3 method for class 'list'
mapLevels(x, codes, sort, drop, combine, ...)

# S3 method for class 'data.frame'
mapLevels(x, codes, sort, drop, combine, ...)

.unlistLevelsMap(x, ind = FALSE)

# S3 method for class 'levelsMap'
print(x, ...)

# S3 method for class 'listLevelsMap'
print(x, ...)

# S3 method for class 'levelsMap'
x[i]

# S3 method for class 'listLevelsMap'
x[i]

is.levelsMap(x)

is.listLevelsMap(x)

.isCharacterMap(x)

as.levelsMap(x, check = TRUE, ...)

as.listLevelsMap(x, check = TRUE)

.checkLevelsMap(x, method)

.checkListLevelsMap(x, method)

# S3 method for class 'levelsMap'
c(..., sort = TRUE, recursive = FALSE)

# S3 method for class 'listLevelsMap'
c(..., sort = TRUE, recursive = FALSE)

# S3 method for class 'levelsMap'
sort(x, decreasing = FALSE, na.last = TRUE, ...)

# S3 method for class 'levelsMap'
unique(x, incomparables = FALSE, ...)

mapLevels(x) <- value

# Default S3 method
mapLevels(x) <- value

# S3 method for class 'list'
mapLevels(x) <- value

# S3 method for class 'data.frame'
mapLevels(x) <- value
```

## Arguments

- x:

  object whose levels will be mapped, look into details `codes` boolean,
  create integer levelsMap (with internal codes) or character levelsMap
  (with level names)

- codes:

  boolean, create integer levelsMap (with internal codes) or character
  levelsMap (with level names)

- sort:

  boolean, sort levels of character `x`, look into details

- drop:

  boolean, drop unused levels

- combine:

  boolean, combine levels, look into details

- ...:

  additional arguments for `sort`

- value:

  levelsMap or listLevelsMap, output of `mapLevels` methods or
  constructed by user, look into details

## See also

[`AlphaPart`](https://alphagenes.github.io/AlphaPart/reference/AlphaPart.md)

## Author

Gregor Gorjanc
