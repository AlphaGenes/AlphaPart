# write.csv.R

Save summaries of partitioned breeding values to CSV files on disk for
further analyses of processing with other software or just for saving
(backing up) results.

## Usage

``` r
write.csv(...)

# Default S3 method
write.csv(...)

# S3 method for class 'AlphaPart'
write.csv(x, file, traitsAsDir = FALSE, csv2 = TRUE, row.names = FALSE, ...)

# S3 method for class 'summaryAlphaPart'
write.csv(x, file, traitsAsDir = FALSE, csv2 = TRUE, row.names = FALSE, ...)
```

## Arguments

- ...:

  Other options passed to
  [`write.csv2`](https://rdrr.io/r/utils/write.table.html) or
  [`write.csv`](https://rdrr.io/r/utils/write.table.html).

- x:

  AlphaPart, object returned from
  [`AlphaPart`](https://alphagenes.github.io/AlphaPart/reference/AlphaPart.md)
  function or summaryAlphaPart, object returned from
  [`summary.AlphaPart`](https://alphagenes.github.io/AlphaPart/reference/summary.AlphaPart.md)
  function.

- file:

  Character, file name with or without .csv extension, e.g., both "file"
  and "file.csv" are valid.

- traitsAsDir:

  Logical, should results be saved within trait folders; the
  construction is `file.path(dirname(file), trait, basename(file))`;
  folders are created if they do not exist.

- csv2:

  Logical, export using
  [`write.csv2`](https://rdrr.io/r/utils/write.table.html) or
  [`write.csv`](https://rdrr.io/r/utils/write.table.html).

- row.names:

  Logical, export row names as well?

## Value

It contains:

- `write.csv` - see
  [`write.csv`](https://rdrr.io/r/utils/write.table.html) for details.

- `write.csv.AlphaPart` - for each trait (list component in `x`) a file
  is saved on disk with name "AlphaPart_trait.csv", where the file will
  hold original data and breeding value partitions. With
  `traitsAsDir=TRUE` files are saved as "trait/file_trait.csv". File
  names are printed on screen during the process of export and at the
  end invisibly returned.

- `write.csv.summaryAlphaPart` - for each trait (list component in `x`)
  a file partitions named "file_trait.csv" is saved on disk. With
  `traitsAsDir=TRUE` files are saved as "trait/file_trait\_\*.csv". File
  names are printed on screen during the process of export and at the
  end invisibly returned.

## Details

Function [`write.csv`](https://rdrr.io/r/utils/write.table.html) from
the utils package works when exported object is a
[`data.frame`](https://rdrr.io/r/base/data.frame.html) or a
[`matrix`](https://rdrr.io/r/base/matrix.html). This is an attempt to
make this function generic so that one can define `write.csv` methods
for other objects.

## Methods (by class)

- `default`: Default `write.csv` method.

- `AlphaPart`: Save partitioned breeding values to CSV files on disk on
  disk for further analyses or processing with other software or just
  for saving (backing up) results.

- `summaryAlphaPart`: Save summaries of partitioned breeding values to
  CSV files on disk for further analyses of processing with other
  software or just for saving (backing up) results.

## See also

[`write.csv`](https://rdrr.io/r/utils/write.table.html) help page on the
default `write.csv` and `write.csv2` methods in the utils package;
[`summary.AlphaPart`](https://alphagenes.github.io/AlphaPart/reference/summary.AlphaPart.md)
and
[`AlphaPart`](https://alphagenes.github.io/AlphaPart/reference/AlphaPart.md)
help pages on the objects of `summaryAlphaPart` and `AlphaPart` classes.

## Examples

``` r
## Partition additive genetic values
res <- AlphaPart(x=AlphaPart.ped, colPath="country", colBV=c("bv1", "bv2"))
#> 
#> Size:
#>  - individuals: 8 
#>  - traits: 2 (bv1, bv2)
#>  - paths: 2 (domestic, import)
#>  - unknown (missing) values:
#> bv1 bv2 
#>   0   0 

## Write summary on the disk and collect saved file names
fileName <- file.path(tempdir(), "AlphaPart")
ret <- write.csv(x=res, file=fileName)
#> /tmp/RtmpYPbNHI/AlphaPart_bv1.csv 
#> /tmp/RtmpYPbNHI/AlphaPart_bv2.csv 
print(ret)
#> [1] "/tmp/RtmpYPbNHI/AlphaPart_bv1.csv" "/tmp/RtmpYPbNHI/AlphaPart_bv2.csv"
file.show(ret[1])

## Clean up
files <- dir(path=tempdir(), pattern="AlphaPart*")
unlink(x=files)
```
