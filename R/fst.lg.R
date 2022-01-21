#' Calculate Fst per linkage group
#'
#' This function calculates Hudson's Fst
#' @param lg Vector of linkage groups
#' @return Value Fst values
#' @export
#' @examples
#' fst.stats.lgs<-lapply(lgs, fst.lg)
#'
fst.lg<-function(lg){
  fst<-fst.HVAxHVC[grep(paste("^",lg,"_",sep=""), fst.HVAxHVC$locus),]
  nloci<-length(na.exclude(fst$fst))
  fst.mean<-1-((sum(fst$numerator)/nloci)/(sum(fst$denominator)/nloci))
  fst.quantile<-quantile(fst$fst, prob=c(0.5,0.95,0.99,0.999,0.9999), names=F, na.rm=T)
  fst.max<-max(fst$fst, na.rm=T)
  return(c(nloci=nloci,mean=fst.mean,fst.quantile,fst.max))
}
