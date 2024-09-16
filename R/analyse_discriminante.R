#' @title Analyse discriminante quadratique
#' @description
#'
#' L'analyse discriminante quadratique permet d'attribuer une céramique donnée à un groupe de référence à l'aide d'un calcul de probabilité d'appartenance. On identifie au préalable dans la base donnée un certain nombre de groupes parmi lesquels on cherche à identifier celui auquel appartient la céramique étudiée.
#' Cette attribution s'exprime en termes de statistique par la distance de Mahalannobis et un histogramme est tracé.
#'
#' @param data Un dataframe. Les données sur lesquelles l'analyse sera effectuée.
#' @param pas Un sclaire. Il correspond à la largeur des intervalles de histogramme.
#' @param echantillon Un dataframe d'un ou plusieurs individus. Il s'agit des données pourlequelles il faut établir le groupe de référence.
#' @param normalize Un booleen. Si TRUE, les datas sont ramenées à 100 avant la classification, sinon elles sont gardées telles quelles.
#' @param echantillonage Un booleen. Si TRUE, le nom des échantillons apparaît sur l'histogramme.
#'
#' @details
#' \cr La probabilité d'appartenance d'une céramique x au groupe s'exprime par :
#' \deqn{f_{k}(x) = \frac{e^{-\frac{1}{2}(x - \mu_{k})' C_{k}^{-1} (x - \mu_{k})}}{(2\pi)^{\frac{n}{2}} \left| C_{k} \right|^{\frac{1}{2}}}}
#'
#' où :
#' \itemize{
#'   \item \eqn{\mu_k} est la moyenne des éléments du groupe \eqn{k},
#'   \item \eqn{C_k} est la matrice de covariance du groupe \eqn{k},
#'   \item \eqn{n} est le nombre de variables.
#' }
#'
#' Cette approche est néanmoins entièremment géométrique et ne prend pas en compte les probabilités _a priori_ des différents groupes.
#'
#
#' @return L'histogramme des distances euclidennes et de Mahalannobis.
#' @export
#' @importFrom graphics boxplot
#' @importFrom stats cov
#' @importFrom stats mahalanobis
#' @importFrom stats pchisq
#' @importFrom stats sd
#'
#' @references
#' Picon, M. (1984). *PACT 10*. Academic Press. \cr
#' M. J. Baxter(1994), *Exploratory multivariate analisys in archeology*, Editions Edinburgh University Press. \cr
#' Chenorkian, R. (1996), *Pratique archéologique statistique et graphique*, Editions Errance \cr
#'
#' @examples
#'
#' #data=as.dataframe(c(1,2,3))
#' #ana_dis(data)
#'
#'
#'
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
