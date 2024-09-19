#' @title fonction d'exclusion
#' @description Renvoit un dataset sans un échantillon marginal du dataset ou une variable peu intéressante.
#' @param data Le dataset de base.
#' @param fichier Un string. Le nom de l'échantillon à enlever ou de l'élément chimique à ne pas prendre en compte dans l'analyse.
#'
#' @return Le dataframe sans l'élément marginal ou l'élément chimique.
#' @importFrom utils data
#' @export
#'
#' @examples
#' data('rock')
#' rock_sans_12<-exclure(rock,'13')
#'
#'
exclure<-function(data,fichier){

  if (fichier %in% rownames(data)) {
    data<-data[rownames(data) != fichier, ]
  }
  else if (fichier %in% colnames(data)){
    data<-data[,colnames(data) != fichier]
  }
  else {stop("l'element ou le nom de l'echantillon a ete mal ecrit")}

  print(paste(paste("attention, en excluant",fichier),"vous ne pourrez plus revenir en arriere"))
  print("...")
  print(paste(fichier,"a bien ete exclu du fichier"))
  return(data)
}
