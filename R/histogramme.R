#' @title Fonction histogramme
#' @description
#' Cette fonction cree un histogramme base sur les valeurs d'un element chimique ou d'une variable numerique fournie.
#' Elle permet de specifier des bornes inferieures et superieures ainsi qu'un pas pour controler la granularite de l'histogramme.
#' Elle est utile pour observer la distribution d'un element chimique dans un ensemble de donnees.
#' @param data dataset de base
#' @param element string du nom de l'element a tracer
#' @param borne_inferieure la borne inferieure de l'axe des x pour l'histogramme. Valeur prise au minimum des valeurs de l'element par defaut
#' @param borne_superieure la borne superieure de l'axe des x pour l'histogramme. Valeur prise au minimum des valeurs de l'element par defaut
#' @param pas le pas entre chaque barre de l'histogramme
#' @param echantillonage booleen. est ce qu'on affiche le nom des echantillons dans l'histogramme ? TRUE de base
#'
#' @return un graphique
#' @export
#'
#' @importFrom graphics hist
#' @importFrom graphics title
#' @importFrom graphics axis
#' @importFrom graphics box
#' @importFrom graphics text
#' @importFrom grDevices windows
#'
#' @examples
#' data('iris')
#' histogramme(iris,'Sepal.Length',1)
#'
#'
histogramme <- function(data,element, pas, borne_inferieure=NULL, borne_superieure=NULL, echantillonage=TRUE) {
  # Validation des parametres

  if (!element %in% names(data)) {
    stop("L'argument 'element' doit appartenir a la liste des elements de l'analyse XRF. Attention a respecter la casse")
  }
  # numero de l'element a analyser

  i <- which(names(data) == element)

  # valeur des bornes par defaut
  if (is.null(borne_inferieure)){
    borne_inferieure=round(min(data[,i])-1)
  }

  if (is.null(borne_superieure)){
    borne_superieure=round(max(data[,i])+1)
  }

  # Generer les intervalles de l'histogramme
  breaks<-seq(borne_inferieure, borne_superieure, by=pas)

  # Afficher l'histogramme
  windows()
  hist(data[,i], breaks = breaks, main = paste("Histogramme des valeurs de l'element chimique",element),
       xlab = "Valeurs de l'element", ylab = paste("Nombre d'element",element), col = "lightblue", border = "black")

  # Ajouter un cadre et ajuster les axes
  box()
  axis(1) # axe des x
  axis(2) # axe des y

  if (echantillonage) {
    x_min <- borne_inferieure
    indice <- order(data[,i])
    data2 <- data[indice,]

    x_max <- x_min + pas
    nombre <- nrow(data)
    name <- rownames(data2)
    liste <- data2[,i]
    a <- 1
    hauteur <- 0.5

    while (a != nombre + 1) {
      if (liste[a] > x_min & liste[a] <= x_max) {
        text(x_min + pas / 2, hauteur, name[a], cex = 0.7)
        hauteur <- hauteur + 1
      } else {
        hauteur <- 0.5
        x_min <- x_min + pas
        x_max <- x_max + pas
        a <- a - 1
      }
      a <- a + 1
    }
  }
}
