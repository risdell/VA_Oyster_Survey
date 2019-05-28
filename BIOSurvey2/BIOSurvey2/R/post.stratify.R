post.stratify <-
function(x, Strata.old, Strata.new, strata.new.obj,species)
{
  strata.old<-x[,Strata.old]
    strata.new<-x[,Strata.new]
    species<-x[,species]
    species <- species[!is.na(strata.new)]
    strata.old <- strata.old[!is.na(strata.new)]
    strata.new <- strata.new[!is.na(strata.new)]
  names.Strata.old<-unique(as.character(strata.old))
  no.Strata.old<-length(names.Strata.old)
  names.Strata.new<-unique(as.character(strata.new))
#  no.Strata.new<-length(names.Strata.new)
  res<-data.frame(Strata=names.Strata.old,Nh=rep(0,no.Strata.old),ybpos=rep(0,no.Strata.old),Vst.ybpos=rep(0,no.Strata.old),Random.n.penalty=rep(0,no.Strata.old))
  for(i in 1:no.Strata.old)
  {
    Nhg<-strata.new.obj$NH[strata.new.obj[,Strata.old]==names.Strata.old[i]]
    temp.names<-strata.new.obj[,Strata.new][strata.new.obj[,Strata.old]==names.Strata.old[i]]
    temp.species<-species[strata.old==names.Strata.old[i]]
    temp.strata<-strata.new[strata.old==names.Strata.old[i]]
    Nhg<-Nhg[is.element(temp.names,unique(temp.strata))]
    Whg<-Nhg/(N<-sum(Nhg))
    if(is.factor(temp.strata)) temp.strata<-temp.strata[,drop=TRUE]
    temp.data<-split(temp.species,temp.strata)
    ybpos<-sum(Whg*sapply(temp.data,mean))# Need to accomodate not all new strata present
    ng<-sapply(temp.data,length)
    Shg <- sapply(temp.data, var, na.rm = T)
    Random.n<-(1 - sum(ng)/N) *sum((1 - Whg) * Shg, na.rm = T)/(sum(ng)^2)
    Vst.ybpos <- (1 - sum(ng)/N) * sum(Whg * Shg, na.rm = T)/sum(ng) + Random.n
    res$ybpos[i]<-ybpos
    res$Nh[i]<-N
    res$Vst.ybpos[i]<-Vst.ybpos
    res$Random.n.penalty[i]<-Random.n
    class(res)<-c("post.stratify","data.frame")
  }
 
  return(res)
}
