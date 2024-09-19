#'@title Analyse de grappe
#'@description
#' L'analyse de grappe ou classification hiérarchique ascendante est une technique de classification automatique en analyse de données.
#' Contrairement à une analyse factorielle, elle prend en compte toutes les dimensions d'un problème, sans perte d'informations. Mais son principal avantage réside dans l'interprétation facile du résultat.
#'
#' @param data Un dataframe.
#' @param normalize Un booleen. Si TRUE, les datas sont ramenées à 100 avant la classification, sinon elles sont gardées telles quelles.
#' @param methode La methode de transformation utilisée au préalable de la classification. Peut être `"simple"` ou `"logarithme"`.
#' @param k,h Valeur scalaire. Coupe le dendogramme pour former `k` clusters ou à une hauteur `h`.
#' @param color Un booleen. Si TRUE, le nom de chaque individu est colorié selon son groupe de référence. La dernière colonne de `data` doit alors être le numéro du groupe de référence.
#' @details
#' La fonction affiche un arbre hiérarchique.
#' Un ensemble de tests statistiques sont appliqués aux valeurs groupées pour vérifier la pertinence des groupes. On obtient une liste des éléments caractéristiques pour chaque groupe, avec leur valeur-test. Un test d'identitié des moyennes est également appliqué pour l'ensemble des variables.\cr
#' \cr
#' Le calcul préalable de traitement des données par la méthode `"logarithme"` s'appuie la transformation centrée log-ratio introduite par Aitchison en 1986. Il permet d'éviter le phénomène de contamination de la matrice des covariances en supprimant une contrainte.
#'
#' \cr
#' L'arbre est construit avec une matrice des distances euclidiennes et par affinité moyenne, conformément à l'usage en archéométrie.
#'
#' @return Retourne une liste contenant:
#' \item{data}{le fichier data triée dans l'ordre du dendogramme. Si `k` ou `h` est renseigné, une colonne `Groupe` est automatiquement ajoutée avec le numéro du groupe auquel appartient chaque échantillon}
#' \item{split}{une liste avec les éléments du datasets pour chaque groupe}
#' \item{hc}{l'arbre hiérarchique est retourner sous forme d'un objet de classe hclust. (Voir fonction R `hclust` pour plus de détails)}
#' \item{descriptif}{Si `k` ou `h` est renseigné. Retourne la liste contenant les tests statistiques réalisés sur les groupes.}
#' @export
#' @importFrom stats hclust rect.hclust cutree dist
#' @importFrom utils View head
#' @importFrom grDevices dev.new palette
#' @importFrom nexus as_composition replace_zero transform_clr
#' @importFrom graphics boxplot par
#' @importFrom FactoMineR catdes
#' @importFrom khroma color
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
grappe <- function(data,normalize=TRUE,methode='simple',h=NULL,k=NULL,color=FALSE){

  #==========================Traitement des donnés===========================
  #sauvegarde du dataset originel
  data0=data
  #selection des elements
  data_selectionne<-selection_menu(data) #on enlève des éléments à analyser
  if (is.factor(data_selectionne[[ncol(data_selectionne)]])){stop("La derniere colonne doit etre de type numerical. Cocher la case 'groupe' dans le menu de selection des elements.")}

  #normalisation
  if (normalize){
    data<-data_selectionne #creation du dataframe à normaliser
    for (i in 1:nrow(data)){
      data[i,]=data_selectionne[i,]/sum(data_selectionne[i,])*100
    }
  }
  else {data<-data_selectionne}

  #methodes
  if (methode=='simple'){
    data_norm=scale(data) #centree reduite
  }

  else if (methode=='logarithme'){
    coda=as_composition(data)
    coda=replace_zero(coda,10e-7) #le log-ratio n'est pas défini en 0
    data_t=transform_clr(coda) #j'applique le log-ratio
    data_norm=scale(data_t)
  }

  #=============================Clustering======================================

  matrice_dist <- dist(data_norm) #matrice des distances euclidiennes
  #calcul des clusters
  hc <- hclust(matrice_dist,method="average")
  #NB : method="average" si classification en affinite moyenne non ponderee
  #     method="median" si classification en affinite moyenne ponderee
  if (methode=='simple'){sub='hclust(*,"average") - transformation simple'}
  else if (methode=='logarithme'){sub='hclust(*,"average") - transformation log-ratio'}


 #========================Visualisation graphique===============================
  ## Plot initial---------------------------------------------------------------

  plot(hc, labels = data$Nom, main = "Dendrogramme",hang=-1,sub=sub, xlab = "Echantillons", ylab = "Distance")

  ## Mise en couleur des noms des individus en fonction de leur groupe de référence inital------
  if (color)
  { par(mar = c(5, 4, 4, 8), xpd = TRUE)
    plot(hc, labels = FALSE, main = "Dendrogramme",hang=-1,sub=sub, xlab = "Echantillons", ylab = "Distance")

  palette <- color("bright")(7)
  labels <- rownames(data)
  colors <- palette[data0[, ncol(data0)]]
  groupes <- unique(data0[, ncol(data0)])
  if (!(is.null(h)&is.null(k))){position_text=-0.8}
  else {position_text=-0.2} #permet d'avoir des résultats lisibles, tout en ne cachant pas les rectangles qui encadrent les clusters de la classification

  text(x = seq_along(labels),
       y = rep(position_text, length(labels)),
       labels = labels[hc$order],
       col = colors[hc$order],
       srt = 90, adj = 1, xpd = TRUE, cex = 0.8)
  legend("bottomright", inset = c(-0.2, 0), legend = groupes, col = palette[1:length(groupes)], pch = 15, title = "Groupes", box.lty = 0)
  }
  ## Détection des nouveaux groupes, tests statistiques sur ces groupes---------
    ### Détection par la hauteur
  if (!is.null(h)){
    rect.hclust(hc,h=h,border=2)
    #decoupage de l'arbre selon la hauteur
    groupes=cutree(hc,h=h)
    #ajout des groupes crees au fichier originel
    data$Groupe<-as.factor(groupes)
    #tests statistiques sur les groupes
    description_groupes <- catdes(data, num.var = ncol(data)) #pour son fonctionnement mathématique voir paragraphe 3.7.2, Husson et al. 2010
    for (i in 1:length(description_groupes)){
      print(head(description_groupes$quanti[i]))
     } # valeur-test et p-value par groupe
    print(head(description_groupes[1])) # test de Fisher et R²
    data_triee <- data[hc$order, ]
    View(data_triee)
    data2=split(data_triee[,1:ncol(data_triee)-1],data_triee[,ncol(data_triee)])
    res=list(data=data_triee,split=data2,hc=hc,descriptif=description_groupes)
    #   assign("data_split", data2, envir = .GlobalEnv)
  }
    ### Détection par le nombre de clusters
  else if (!is.null(k)){
    rect.hclust(hc,k=k,border=2)
    groupes=cutree(hc,k=k)
    data$Groupe<-as.factor(groupes)
    description_groupes <- catdes(data, num.var = ncol(data)) #calculs statistiques ; pour son fonctionnement mathématique voir paragraphe 3.7.2, Husson et al. 2010
    for (i in 1:k){
      print(head(description_groupes$quanti[i]))
      }
    # valeur-test et p-value par groupe
   print(head(description_groupes[1]))
    # test de Fisher et R²
    plot(description_groupes,cex=2)
    data_triee <- data[hc$order, ]
    View(data_triee)
    data2=split(data_triee[,1:ncol(data_triee)-1],data_triee[,ncol(data_triee)])
    res=list(data=data_triee,split=data2,hc=hc,descriptif=description_groupes)
  }
  else {
    #mettre les donnes dans l'ordre d'apparition dans la grappe
    data_triee <- data[hc$order, ]
    res=list(data=data_triee,hc=hc)
  }
  return(res)
}
