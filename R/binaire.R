#' Title
#' @param data dd
#' @param x vf
#' @param y sv
#'
#' @return ff
#' @export
#' @examples
#' 1+1
#'
binaire<-function(data,x,y){
  i_x <- which(names(data) == x)
  i_y <- which(names(data) == y)
  plot(data[,i_x],data[,i_y],xlab =x,ylab=y)

}
