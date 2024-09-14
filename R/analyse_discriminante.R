#' @title Analyse discriminante
#' @description
#'
#' La méthode d'analyse de données utilisée ici s'appuie sur . On l'appelle analyse de données quadratique. distance de Mahalanobis permet d'identifier
#'
#' @param data dataset de base
#' @param pas pas
#' @param echantillon dataset a analyser
#' @param normalize permet de ramener à 100
#' @param echantillonage vrai ou faux
#'
#' @return deux histogrammes
#' @export
#' @importFrom graphics boxplot
#' @importFrom stats cov
#' @importFrom stats mahalanobis
#' @importFrom stats pchisq
#' @importFrom stats sd
#'
#' @examples
ana_dis<-function(data,pas,echantillon,normalize=TRUE,echantillonage=TRUE){

  data=selection_menu(data)

  data2=data
  data2[nrow(data)+1,]<-new

  nombre_element=length(data)
  nombre_ligne=nrow(data)
  #data[,8:14]=data[,8:14]*10e-5 #conversion ppm en % pour les elements mineurs
  #echantillon[,8:14]=echantillon[,8:14]*10e-5
  new=data[1,]
  new[,1:length(data)]=echantillon
  row.names(new)="ORS118"



  distance_euclidean <- function(echantillon){
    mean_existing <- colMeans(data)
    distance <- sqrt(sum(((echantillon - mean_existing)/apply(data, 2, sd))^2))
    distance=distance/nombre_element #normalisation
    return(distance)
  }

  distance_mahalanobis <- function(echantillon){
    matrice=cov(data2)

    svd_decomp <- svd(matrice)
    covariance_matrix_inv <- svd_decomp$u %*% diag(1 / svd_decomp$d) %*% t(svd_decomp$v)

    lambda=0.1
    matrice_reg<-matrice+lambda*diag(ncol(data))

    inv_matrice_reg<-solve(matrice_reg)

    mean_existing<-colMeans(data)
    #diff <- echantillon - mean_existing
    #distance <- t(diff) %*% inv_matrice_reg %*% diff
    distance <- mahalanobis(echantillon,mean_existing,covariance_matrix_inv,inverted = TRUE)
    distance=sqrt(distance)/nombre_element
    return(distance)
  }



  afficher_hist <- function(distance){
    if (identical(distance,distance_euclidean)) {nom_distance='euclidiennes'}
    else {nom_distance='de Mahalanobis'}
    titre_graphe=paste("Histogramme des distances",nom_distance)

    liste_dist=apply(data,1,distance)
    liste_dist_complete=c(liste_dist,distance(echantillon))
    #boxplot(liste_dist)

    borne_inferieure=floor(min(liste_dist_complete)/pas)*pas #calculee en arrondissant la valeur minimale des donnees au pas inferieur le plus proche
    borne_superieure=ceiling(max(liste_dist_complete)/pas)*pas #idem pas superieur le plus proche
    breaks<-seq(borne_inferieure, borne_superieure, by=pas)
    #windows()
    hist(liste_dist_complete,freq=TRUE,breaks=breaks,xlab='distance',ylab="nombre d'echantillon",main=titre_graphe,col="red")
    hist(liste_dist,freq=TRUE,breaks=breaks,xlab='distance',ylab="nombre d'echantillon",main=titre_graphe,col="lightblue",add=TRUE)



    if (echantillonage) {
      x_min <- borne_inferieure
      liste_ordonne<-sort(liste_dist_complete)
      x_max <- x_min + pas
      nombre <- length(liste_dist_complete)
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
  d=length(data)+1
  probabilities <- 1 - pchisq(distance_mahalanobis(new), df = d)
  print(probabilities)

}
