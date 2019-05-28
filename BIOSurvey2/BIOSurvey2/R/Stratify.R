Stratify <-
function (x, strata.group, strata.name, species, Subset) 
  {
    m <- match.call()
    if (!missing(Subset)) {
      x <- x[Subset, ]
    }
    obj.Strata <- x[[strata.name]]
    s.group <- is.element(strata.group$Strata, unique(obj.Strata))
    s.group.Strata <- strata.group$Strata[s.group]
    s.group.NH <- strata.group$NH[s.group]
    s.obj <- is.element(obj.Strata, s.group.Strata)
    x <- x[s.obj, ]
    species <- x[[species]]
    x <- x[s.obj, ]
    obj.Strata <- obj.Strata[s.obj]
    yhi <- split(species, obj.Strata)
    nh <- as.vector(sapply(yhi, length))
    yhi <- yhi[nh != 0]
    nh <- nh[nh != 0]
    res <- list(yhi = yhi, Strata = s.group.Strata, Nh = s.group.NH, 
                Wh = s.group.NH/sum(s.group.NH), nh = nh, call = m)
    class(res) <- "strata"
    return(res)
  }
