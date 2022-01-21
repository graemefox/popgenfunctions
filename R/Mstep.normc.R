#' Modified version of Mstep.norm for fitting HMM
#' #'
#' @param x input
#' @param cond input
#' @param pm input
#' @param pn input
#' @return
#' @export
#' @examples
#'
Mstep.normc <- function(x, cond, pm, pn){
  nms <- sort(names(pm))
  n <- length(x)
  m <- ncol(cond$u)
  if (all(nms==c("mean", "sd"))){
    mean <- as.numeric(matrix(x, nrow = 1) %*% cond$u)/apply(cond$u, MARGIN = 2, FUN = sum)
    sd <- pm$sd
    return(list(mean=mean, sd=sd))
  }
}
