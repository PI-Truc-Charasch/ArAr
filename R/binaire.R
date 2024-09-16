#' @title Diagramme binaire
#' @description En analyse de données, en archéométrie, il est souvent utile de tracer des diagrammes binaires entre deux éléments des variables d'analyse. Cette fonction permet de tracer un biplot pour un dataset du laboratoire.
#' @param data Un dataframe.
#' @param x Un string. Le nom de l'élément que l'on veut en abssice.
#' @param y Un string. Le nom de l'élément que l'on veut en ordonnée.
#'
#' @return Un biplot du diagramme binaire.
#' @importFrom utils data
#' @export
#' @examples
#' ## dataset de Waksman, 1999
#' data('montpellier')
#' binaire(montpellier,'CaO','Fe2O3')
#'
binaire<-function(data,x,y){
  i_x <- which(names(data) == x)
  i_y <- which(names(data) == y)
  plot(data[,i_x],data[,i_y],xlab =x,ylab=y)

}
