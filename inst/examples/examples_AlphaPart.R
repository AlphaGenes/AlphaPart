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
