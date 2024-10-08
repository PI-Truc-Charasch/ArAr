#' @title Travail sur groupe
#' @description La fonction `travail_sur_groupe()` permet de tracer l'histogramme des distances de Mahalannobis au sein du groupe.
#' Elle reprend une fonctionnalité disponible sur le site de ceramologie. Pour plus d'information sur les distances de Mahalanobis, voir la documentation `ana_dis`.
#'
#'
#' @param data Un dataframe.
#' @param pas Un sclaire. Il correspond à la largeur des intervalles de histogramme.
#' @param normalize Un booleen. Si TRUE, les datas sont ramenées à 100 avant la classification, sinon elles sont gardées telles quelles.
#' @param echantillonage Un booleen. Si TRUE, le nom des échantillons apparaît sur l'histogramme.
#'
#' @return deux histogrammes
#' @export
#' @importFrom graphics boxplot plot
#' @importFrom stats cov
#' @importFrom stats mahalanobis
#' @importFrom stats pchisq
#' @importFrom stats sd
#' @importFrom utils data
#'
#' @examples
#' #Plus tard
#'
travail_sur_groupe<-function(data,pas,normalize=TRUE,echantillonage=TRUE){
  data<-selection_menu(data)
  nombre_element=length(data)
  nombre_ligne=nrow(data)
  #data[,8:14]=data[,8:14]*10e-5 #conversion ppm en % pour les éléments mineurs

  #calcul distance euclidienne
  distance_euclidean <- function(echantillon){
    mean_existing <- colMeans(data)
    distance <- sqrt(sum(((echantillon - mean_existing)/apply(data, 2, sd))^2))
    distance=distance/nombre_element #normalisation
    return(distance)
  }

  #calcul distance de Mahalannobis
  distance_mahalanobis <- function(echantillon){
    matrice=cov(data)
    #distance régularisée au cas où si le déterminant est nul
    svd_decomp <- svd(matrice)
    covariance_matrix_inv <- svd_decomp$u %*% diag(1 / svd_decomp$d) %*% t(svd_decomp$v)

    mean_existing<-colMeans(data)
    #diff <- echantillon - mean_existing
    #distance <- t(diff) %*% inv_matrice_reg %*% diff
    distance <- mahalanobis(echantillon,mean_existing,covariance_matrix_inv,inverted = TRUE)
    distance=sqrt(distance)/nombre_element
    return(distance)
  }


  #affichage histogramme
  afficher_hist <- function(distance){
    if (identical(distance,distance_euclidean)) {nom_distance='euclidiennes'}
    else {nom_distance='de Mahalanobis'}
    titre_graphe=paste("Histogramme des distances",nom_distance)

    liste_dist=apply(data,1,distance)
    #boxplot(liste_dist)

    borne_inferieure=floor(min(liste_dist)/pas)*pas #calculee en arrondissant la valeur minimale des donnees au pas inferieur le plus proche
    borne_superieure=ceiling(max(liste_dist)/pas)*pas #idem pas superieur le plus proche
    breaks<-seq(borne_inferieure, borne_superieure, by=pas)
    hist(liste_dist,freq=TRUE,breaks=breaks,xlab='distance',ylab="nombre d'echantillon",main=titre_graphe,col="lightblue")

    if (echantillonage) {
      x_min <- borne_inferieure
      liste_ordonne<-sort(liste_dist)
      x_max <- x_min + pas
      nombre <- length(liste_dist)
      name <- names(liste_ordonne)
      a <- 1
      hauteur <- 0.5
      liste=liste_ordonne

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



  afficher_hist(distance_euclidean)
  afficher_hist(distance_mahalanobis)


  #calcul de la densite de probabilite (Picon, 1972), estimation par la loi du chi2

  d=length(data)
  probabilities <- 1 - pchisq(distance_mahalanobis(data), df = d)
  print(probabilities)

}
