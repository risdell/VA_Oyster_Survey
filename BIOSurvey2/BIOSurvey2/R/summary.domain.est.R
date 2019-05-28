summary.domain.est <-
function(object,...)
{
  out <- data.frame(Domain = as.character(object$domain.obj[,object$Domain]), Nd =
                      object$domain.obj$NH, nd = apply(object$nsdh, 2, sum), ybd = object$ybd, var.ybd = object$var.ybd, var.diffdomain = object$var.diffdomain, se.ybd = object$se.ybd)
  out1 <- list(sum.Nd = sum(out$Nd), yst = sum(out$Nd * out$ybd, na.rm = T)/sum(out$Nd, na.rm = T),
               var.yst = sum(out$Nd^2 * out$var.ybd, na.rm = T)/sum(out$Nd, na.rm = T)^2)
  list(out, out1)
}
