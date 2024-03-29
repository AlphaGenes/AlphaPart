% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{print.AlphaPart}
\alias{print.AlphaPart}
\title{Print method for the output of AlphaPart function.}
\usage{
\method{print}{AlphaPart}(x, n, ...)
}
\arguments{
\item{x}{AlphaPart, output object from
\code{\link[AlphaPart]{AlphaPart}} function.}

\item{n}{Integer, number of the first and last rows in \code{x} to
print out using \code{\link{head}} and \code{\link{tail}}.}

\item{...}{Arguments passed to \code{print} function.}
}
\description{
Partitioning of breeding values if often performed on
quite large datasets, which quickly fills in the whole
screen. Print method therefore prints out paths, number of
individuals and the first and the last few lines for each trait to
quickly see what kind of data is in \code{x}.
}
\examples{
## Small pedigree with additive genetic (=breeding) values
ped <- data.frame(  id=c(  1,   2,   3,   4,   5,   6),
                   fid=c(  0,   0,   2,   0,   4,   0),
                   mid=c(  0,   0,   1,   0,   3,   3),
                   loc=c("A", "B", "A", "B", "A", "A"),
                   gen=c(  1,   1,   2,   2,   3,   3),
                  trt1=c(100, 120, 115, 130, 125, 125),
                  trt2=c(100, 110, 105, 100,  85, 110))

## Partition additive genetic values
tmp <- AlphaPart(x=ped, colBV=c("trt1", "trt2"))
print(tmp)

## Summarize by generation (genetic mean)
summary(tmp, by="gen")

## Summarize by generation (genetic variance)
summary(tmp, by="gen", FUN = var)


\donttest{
## There are also two demos
  demo(topic="AlphaPart_deterministic", package="AlphaPart",
       ask=interactive())
  demo(topic="AlphaPart_stochastic",     package="AlphaPart",
       ask=interactive())
}
}
\seealso{
\code{\link[AlphaPart]{AlphaPart}}, \code{\link{head}},
\code{\link{tail}}.
}
