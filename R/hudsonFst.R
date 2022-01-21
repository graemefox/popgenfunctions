#' Calculates Hudson's Fst
#'
#' This function calculates Hudson's Fst
#' @param locus Locus for Fst Calculation
#' @param p1 Population 1 allele frequency vector
#' @param p2 Population 2 allele frequency vector
#' @return Value for Hudson's Fst
#' @export
#' @examples
#' fst.HVAxHVC<-hudsonFst(locus=pops.af$locus, p1=pops.af$afHVA,p2=pops.af$afHVC)
hudsonFst<-function(locus=NA, p1=NA, p2=NA){
  numerator<-p1 * (1 - p1) + p2 * (1 - p2)
  denominator<-p1 * (1 - p2) + p2 * (1 - p1)
  fst<-1 - numerator/denominator
  out<-data.frame(locus,numerator,denominator,fst)
  return(out)
}
