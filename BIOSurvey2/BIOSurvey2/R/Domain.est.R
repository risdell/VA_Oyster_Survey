Domain.est <-
  function(x, Strata, Domain, strata.obj, domain.obj, Species)
  {
    H <- length(strata.obj[,Strata])
    D <- length(domain.obj[,Domain])
    unique.domain <- unique(domain.obj[,Domain])
    STrata<-x[,Strata]
    DOmain<-x[,Domain]
    SPecies<-x[,Species]
    if(any(is.na(STrata))) {
      x <- x[!is.na(STrata)]
      DOmain <- x[,Domain][!is.na(STrata)]
      
    }
    if(any(is.na(DOmain))) {
      x <- x[!is.na(DOmain)]
      STrata <- STrata[!is.na(DOmain)]
      DOmain <- DOmain[!is.na(DOmain)]
    }
    nsdh <- matrix(0, H, D)
    
    ysdh <- matrix(0, H, D)
    ss.ysdh <- matrix(0, H, D)
    for(h in 1:H) {
      temp.strata<-STrata == strata.obj[,Strata][h]
      temp.domain<-DOmain[temp.strata]
      if(is.factor(temp.domain)) temp.domain<-temp.domain[,drop=TRUE]
      yk <- split(SPecies[temp.strata], temp.domain) 
      yk <- yk[is.element(names(yk), unique.domain)]
      names.domain <- is.element(unique.domain, names(yk))
      if(sum(names.domain) > 0) {
        ss.ysdh[h, match(names(yk), unique.domain)] <- sapply(yk, function(x)
        {
          sum((x - mean(x))^2)
        }
        )
        ysdh[h, match(names(yk), unique.domain)] <- sapply(yk, sum)
        nsdh[h, match(names(yk), unique.domain)] <- sapply(yk, length)
      }
    }
    
    nhI <- apply(nsdh, 1, sum)
    dimnames(ysdh) <- dimnames(nsdh) <-list(strata.obj[,Strata], unique.domain)
    
    ysdh <- ysdh[nhI != 0,  ]
    ss.ysdh <- ss.ysdh[nhI != 0,  ]
    nsdh <- nsdh[nhI != 0,  ]
    nsdhI <- 1 * (nsdh != 0)
    nh <- apply(nsdh, 1, sum) 
    ybsdh <- ysdh/(nsdh + (1 - nsdhI))
    Nh <- strata.obj$NH[nhI != 0] 
    Nd <- domain.obj$NH
    fh <- nh/Nh
    pdh <- sweep(nsdh, 1, nh, "/")
    
    Nhatd <- apply(sweep(pdh, 1, Nh, "*"), 2, sum)
    
    
    ybd <- apply(sweep(ysdh, 1, 1/fh, "*"), 2, sum)/Nhatd
    var.1 <- sweep(ss.ysdh, 1, (Nh^2 * (1 - fh))/(nh * (nh - 1)), "*")
    var.2 <- nsdhI * (nsdh * (1 - pdh) * (sweep(ybsdh, 2, ybd, "-"))^2)
    var.2 <- sweep(var.2, 1, (Nh^2 * (1 - fh))/(nh * (nh - 1)), "*")
    var.ybd <- (apply(var.1, 2, sum, na.rm = T) + (var.2 <- apply(var.2, 2, sum, na.rm = T)))/Nd^2
    names(ybd)<-names(var.2) <-names(var.ybd)<-unique.domain
    out <- list(Strata=Strata,Domain=Domain,ybd = ybd, var.ybd = var.ybd, var.diffdomain = var.2/Nhatd^2, se.ybd = sqrt(var.ybd),
                Nd = Nd, nsdh = nsdh, nh = nh, pdh = pdh, Nh = Nh, domain.obj = domain.obj)
    class(out) <- "domain.est"
    out
  }
