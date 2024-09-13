#' Title
#'
#' @param data fichie triee par grappe(data,k=)
#' @param element "CaO" par exemple
#'
#' @return
#' @export
#' @importFrom graphics boxplot
#' @importFrom grDevices windows
#'
#'
#' @examples
boxplot2<-function(data,element){
  i <- which(names(data) == element)
  windows()
  boxplot(data[,i]~data$Groupe,xlab="groupe",ylab=element)
  }
