Association.test <-
function (x, hydro, strata.group, strata.name, species, nreps = 0, 
                               method = c("KS-Test", "CVM-Test")[1], Subset) 
{
  method.int <- charmatch(method, c("KS-Test", "CVM-Test"))
  if (is.na(method.int)) 
    stop("Invalid test method specified")
  if (method.int == 0) 
    stop("Ambiguous test method")
  attach(x)
  if (!missing(Subset)) {
    x <- x[Subset, ]
  }
  detach("x")
  obj.Strata <- x[[strata.name]]
  strata.use <- is.element(obj.Strata, strata.group$Strata)
  hydro <- (x[hydro])[strata.use, ]
  species <- (x[species])[strata.use, ]
  Strata <- obj.Strata[strata.use]
  if (is.null(species)) 
    species <- rep.int(1, length(hydro))
  nreps <- nreps - 1
  tempy <- cbind(hydro, species, Strata)
  if (any(is.na(hydro))) 
    tempy <- (na.omit(as.data.frame(tempy)))
  hydro <- as.vector(tempy[, 1])
  species <- as.vector(tempy[, 2])
  Strata <- as.vector(tempy[, 3])
  WH <- strata.group$NH
  na.strata <- match(strata.group$Strata, unique(Strata))
  WH[is.na(na.strata)] <- NA
  IWH <- cbind(strata.group$Strata, WH)
  IWH <- (na.omit(as.data.frame(IWH)))
  WH <- IWH$WH
  IWH <- IWH[[1]]
  WH <- (WH/sum(WH))
  yhi <- split(species, Strata)
  nh <- as.vector(sapply(yhi, length))
  yst <- sum(WH * as.vector(sapply(yhi, mean)))
  wh <- WH/nh
  Whi <- rep(0, length(species))
  for (i in seq(along = wh)) Whi[c(Strata == IWH[i])] <- wh[i]
  gt <- cumsum(sapply(split((Whi * (species - yst))/yst, hydro), 
                      sum))
  test.stat.rep <- vector(mode = "numeric", length = nreps)
  if (method.int == 1) {
    test.stat <- max(abs(gt))
    prefer.lab <- c("Kolmorogorov-Smirnov Type test")
    if (nreps != 0) {
      for (i in 1:nreps) {
        Ixh <- sample(hydro, replace = TRUE, prob = Whi)
        temp.stat <- cumsum(sapply(split((Whi * (species - 
                                                   yst))/yst, Ixh), sum))
        test.stat.rep[i] <- max(abs(temp.stat))
      }
    }
  }
  if (method.int == 2) {
    test.stat <- sum(gt^2)
    prefer.lab <- c("Cramer-Von Mises Type test")
    if (nreps != 0) {
      for (i in 1:nreps) {
        Ixh <- sample(hydro, replace = TRUE, prob = Whi)
        temp.stat <- cumsum(sapply(split((Whi * (species - 
                                                   yst))/yst, Ixh), sum))
        test.stat.rep[i] <- sum(temp.stat^2)
      }
    }
  }
  res <- list(test.stat = test.stat, test.stat.rep = c(test.stat, 
                                                       test.stat.rep), prefer.lab = prefer.lab, prefer2.lab = c("Randomization Test"), 
              prefer3.lab = c("P-level"), descrip = "Association Analysis")
  class(res) <- "assoc"
  res
}
