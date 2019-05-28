summary.post.stratify <-
function(object,...)
{
  list(object,ystr.mean=sum(object$Nh*object$ybpos)/sum(object$Nh),V.mean=sum(object$Nh^2*object$Vst.ybpos)/sum(object$Nh)^2)
}
