#'@title Analyse de grappe
#'@description
#' L'analyse de grappe ou classification hiérarchique ascendante est une technique de classification automatique en analyse de données.
#' Contrairement à une analyse factorielle, elle prend en compte toutes les dimensions d'un problème, sans perte d'informations. Mais son principal avantage réside dans l'interprétation facile du résultat.
#'
#' @param data Un dataframe.
#' @param normalize Un booleen. Si TRUE, les datas sont ramenées à 100 avant la classification, sinon elles sont gardées telles quelles.
#' @param methode La methode de transformation utilisée au préalable de la classification. Peut être `"simple"` ou `"logarithme"`.
#' @param k,h Valeur scalaire. Coupe le dendogramme pour former `k` clusters ou à une hauteur `h`.
#'
#' @details
#' La fonction affiche un arbre hiérarchique. Une liste dataframe `data_triee` est automatiquement créée avec les échantillons rangées dans leur ordre d'apparition dans l'arbre.  Si le paramètre `k` ou `h` est renseigné, alors les groupes formés apparaissent sur le graphique et une colonne `Groupe` est ajoutée au fichier `data_triee`.
#' Un ensemble de tests statistiques sont appliqués aux valeurs groupées pour vérifier la pertinence des groupes. On obtient une liste des éléments caractéristiques pour chaque groupe, avec leur valeur-test. Un test d'identitié des moyennes est également appliqué pour l'ensemble des variables.\cr
#' \cr
#' Le calcul préalable de traitement des données par la méthode `"logarithme"` s'appuie la transformation centrée log-ratio introduite par Aitchison en 1986. Il permet d'éviter le phénomène de contamination de la matrice des covariances en supprimant une contrainte.
#'
#' @return Une liste de la classe hclust.
#' @export
#' @importFrom stats hclust rect.hclust cutree dist
#' @importFrom utils View
#' @importFrom grDevices windows dev.new
#' @importFrom nexus as_composition replace_zero transform_clr
#' @importFrom graphics boxplot
#' @importFrom FactoMineR catdes
#'
#' @examples
#'
#' ## Not run:
#' ## Données de Yokman 2010
#' data('rock')
#'
#' ## Analyse de grappe
#' #grappe(rock)
#'
#' # Les éléments 36, 38 et 42 sont marginaux, on les exclus
#' rock<-exclure(rock,'46')
#' rock<-exclure(rock,'38')
#' rock<-exclure(rock,'42')
#'
#' ## Deuxième analyse
#' #grappe(rock)
#'
#' ### Identification de quatre groupes
#' #grappe(rock,k=4)
#'
#' ## End(Not run)
#'
#' ## Exemple 2 Waksman, 1999
#'
#'
grappe <- function(data,normalize=TRUE,methode='simple',h=NULL,k=NULL){

  #==========================traitement des donnes===========================

  data_selectionne<-selection_menu(data)

  if (normalize){
    data<-data_selectionne #creation du dataframe a normaliser
    for (i in 1:nrow(data)){
      data[i,]=data_selectionne[i,]/sum(data_selectionne[i,])*100
    }
  }
  else {data<-data_selectionne}

  if (methode=='simple'){
    data_norm=scale(data)
  }
  else if (methode=='logarithme'){
    coda=as_composition(data)
    coda=replace_zero(coda,10e-7) #le log-ratio n'est pas défini en 0
    data_t=transform_clr(coda)
    data_norm=scale(data_t)
  }

  #=============================clustering===================================

  matrice_dist <- dist(data_norm) #matrice des distances euclidiennes
  hc <- hclust(matrice_dist,method="average")
  #NB : method="average" si classification en affinite moyenne non ponderee
  #     method="median" si classification en affinite moyenne ponderee
  if (methode=='simple'){sub='hclust(*,"average") - transformation simple'}
  else if (methode=='logarithme'){sub='hclust(*,"average") - transformation log-ratio'}
  dev.new(plot(hc, labels = data$Nom, main = "Dendrogramme",hang=-1,sub=sub, xlab = "Echantillons", ylab = "Distance"))



  if (!is.null(h)){
    rect.hclust(hc,h=h,border=2)
    groupes=cutree(hc,h=h)
    data$Groupe<-groupes

    description_groupes <- catdes(data, num.var = ncol(data)) #pour son fonctionnement mathématique voir paragraphe 3.7.2, Husson et al. 2010
    for (i in 1:length(description_groupes)){
      print(description_groupes$quanti[i])
      View(description_groupes$quanti[i])}
    assign("description_groupes", description_groupes, envir = .GlobalEnv)
    View(description_groupes[1])
    windows(title = 'principaux composants par groupe')
    plot(description_groupes)

    data2=split(data_triee[,1:ncol(data_triee)-1],data_triee[,ncol(data_triee)])
    assign("data_split", data2, envir = .GlobalEnv)

  }
  if (!is.null(k)){
    rect.hclust(hc,k=k,border=2)
    groupes=cutree(hc,k=k)
    data$Groupe<-as.factor(groupes)
    #windows()
    #boxplot(data_triee$CaO~data_triee$Groupe,xlab="groupe",ylab="CaO")
    description_groupes <- catdes(data, num.var = ncol(data)) #pour son fonctionnement mathématique voir paragraphe 3.7.2, Husson et al. 2010
    for (i in 1:k){
    print(description_groupes$quanti[i])
    View(description_groupes$quanti[i])}
    assign("description_groupes", description_groupes, envir = .GlobalEnv)
    View(description_groupes[1])
    windows(title = 'principaux composants par groupe')
    plot(description_groupes)

    data2=split(data_triee[,1:ncol(data_triee)-1],data_triee[,ncol(data_triee)])
    assign("data_split", data2, envir = .GlobalEnv)
  }

  data_triee <- data[hc$order, ]
  assign("data_triee", data_triee, envir = .GlobalEnv)
  print("le fichier data a ete modifie et enregistre sous la forme data_triee")
  return(hc)
}
