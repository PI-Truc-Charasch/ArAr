#' Title dd
#'
#' @param data d
#' @param fichier  dd
#'
#' @return d
#' @export
#'
#' @examples
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
