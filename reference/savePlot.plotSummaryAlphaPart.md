# Save plot objects on the disk for permanent storage. Function [`savePlot`](https://rdrr.io/r/grDevices/savePlot.html) from the grDevices package works for current page on graphical device. This is an attempt to make this function generic so that one can define `savePlot` methods for particular needs.

Save plot objects of class `plotSummaryAlphaPart` on the disk for
permanent storage.

## Usage

``` r
# S3 method for class 'plotSummaryAlphaPart'
savePlot(x, filename, type,
  device, pre.hook, traitsAsDir, ...)

# Default S3 method
savePlot(...)
```

## Arguments

- x:

  Object on which to chose savePLot method.

- filename:

  Character, filename to save to.

- type:

  Character, file/device type.

- device:

  Device, the device to save from.

- pre.hook:

  Function, call some code before calling print method for plots (see
  examples).

- traitsAsDir:

  Logical, should plots be saved within trait folders; the construction
  is `file.path(dirname(file), trait, basename(file))`. folders are
  created if they do not exist.

- ...:

  Arguments passed to `type` specific methods, say `width` and `height`
  for `type="pdf"` etc.

## Value

Beside the side effect of saving plots to disk, filenames are printed on
screen during the process and at the end invisibly returned.

## See also

[`savePlot`](https://rdrr.io/r/grDevices/savePlot.html) help page on the
default `savePlot` method in the grDevices package;
`savePlot.plotSummaryAlphaPart` help page on the method for the objects
of `plotSummaryAlphaPart` class; and
[`plot.summaryAlphaPart`](https://alphagenes.github.io/AlphaPart/reference/plot.summaryAlphaPart.md)
for ploting results of summaryAlphaPart object.

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

## Summarize population by generation (=trend)
ret <- summary(res, by="gen")

## Plot the partitions
p <- plot(ret, ylab=c("BV for trait 1", "BV for trait 2"), xlab="Generation")

## Save the plots
tmp <- savePlot(x = p, filename="test", type="png")
#> test_bv1.png 
#> $file
#> [1] "test_bv1.png"
#> 
#> test_bv2.png 
#> $file
#> [1] "test_bv2.png"
#> 

## Remove the files
unlink(tmp)
```
