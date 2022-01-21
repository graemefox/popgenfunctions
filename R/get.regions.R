#' function to get size of regions
#' @param lb input
#' @param loci.ord input
#' @param loci.pos input
#' @param lgordscalen input
#' @return
#' @export
#' @examples
#'
get.regions<-function(lb=NA,ub=NA, loci.ord=NA, loci.pos=NA, lgordscalen=NA){
  nregions<-length(lb)
  bpsize<-numeric(nregions)
  for (i in 1:nregions){
    if (is.na(loci.ord[lb[i]]) || is.na(loci.ord[ub[i]])){
      cat ("I:", i, "\n")
    }
    if (loci.ord[lb[i]] == loci.ord[ub[i]]){ # same scaffold
      bpsize[i]<-loci.pos[ub[i]]-loci.pos[lb[i]]
    }
    else{
      bpsize[i]<-(lgordscalen[lgordscalen$ord==loci.ord[lb[i]],]$length
                  -loci.pos[lb[i]]
                  +sum(lgordscalen[(lgordscalen$ord>loci.ord[lb[i]] &
                                      lgordscalen$ord<loci.ord[ub[i]]),]$length)
                  +loci.pos[ub[i]])
    }
  }

  regions<-data.frame(lb.ord=loci.ord[lb], ub.ord=loci.ord[ub],
                      lb.sca=loci.sca[lb], ub.sca=loci.sca[ub],
                      lb.pos=loci.pos[lb], ub.pos=loci.pos[ub],
                      length=bpsize)
  return(regions)
}
