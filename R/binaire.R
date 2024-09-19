#' @title Diagramme binaire
#' @description En analyse de données, en archéométrie, il est souvent utile de tracer des diagrammes binaires entre deux éléments des variables d'analyse. Cette fonction permet de tracer un biplot pour un dataset du laboratoire.
#' @param data Un dataframe.
#' @param x Un string. Le nom de l'élément que l'on veut en abssice.
#' @param y Un string. Le nom de l'élément que l'on veut en ordonnée.
#' @param color Un booléen. Si TRUE, les points du biplot sont coloriés en fonction de la dernière colonne de `data`.
#' @return Un biplot du diagramme binaire.
#' @importFrom utils data
#' @importFrom khroma color
#' @importFrom graphics legend points par
#' @export
#' @examples
#' ## dataset de Waksman, 1999
#' data('montpellier')
#' binaire(montpellier,'CaO','Fe2O3')
#'
binaire<-function(data,x,y,color=FALSE){
  i_x<-which(names(data) == x)
  i_y<-which(names(data) == y)

  x_range<-range(data[, i_x]+1, na.rm = TRUE)
  y_range<-range(data[, i_y], na.rm = TRUE)
  par(mar = c(5, 4, 4, 8), xpd = TRUE) #-> on aggrandit la fenetre de plot pour pouvoir afficher la legende
  plot(data[, i_x], data[, i_y], xlab = x, ylab = y, xlim = x_range, ylim = y_range)

  #----mise en couleur des groupes----------------------------------------------------
  if (color){
    data_split=split(data[,1:ncol(data)-1],data[,ncol(data)])
    groupes <- unique(data[, ncol(data)])
    palette<-color("bright")(7)
    for (i in 1:length(data_split)){
      points(data_split[[i]][, i_x], data_split[[i]][, i_y], col = palette[i], pch = 16)
    }
    legend("bottomright", inset = c(-0.2, 0), legend = groupes, col = palette, pch = 16, title = "Groupes",box.lty=0)
  }

}
