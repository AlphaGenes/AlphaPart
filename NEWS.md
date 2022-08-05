
## Changes in version 0.9.6

FIXES
    * Downgrade for package methods from 4.1.3 to 3.6.2 to support old MAC

---

## Changes in version 0.9.5

CHANGES
    * Removed dependency: ```gdata```
    * Added dependency: ```methods```
    
BUG FIXES
    * tibble data is converted into data.frame to avoid crash R/RStudio

---

## Changes in version 0.9.4

FIXES
    * Internal function to transform tibble data into a data frame (stop crashing)
    * Covariance is only returned when the levels of ```colPath``` is higher than 1. 

---

## Changes in version 0.9.1

CHANGES
    * Improving performance - memory usage and execution time.
          * Execution time was improved in approximately 25%
          * Memory usage was improved in approximately 5%

---

## Changes in version 0.9.0

NEW FEATURES
    * AlphaPart now  has a method for partitioning trends in genetic mean and variance to understand breeding practices
    * New Arguments:
        * center: detect a shift in base population mean and attributes it as parent average effect rather than Mendelian sampling effect
        * scaleEBV: you can define whether is appropriate to center and/or scale the colBV columns in respect to the base population

CHANGES
    * New dependencies: dplyr, magrittr

---

## Changes in version 0.8.2

NEW FEATURES
    * The AlphaPart now has an associated paper at the Genetics Selection Evolution that you can use to cite the AlphaPart if you use it in a paper.
    * DOI: [https://doi.org/10.1186/s12711-021-00600-x](https://doi.org/10.1186/s12711-021-00600-x)
