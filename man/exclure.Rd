% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exclure.R
\name{exclure}
\alias{exclure}
\title{fonction d'exclusion}
\usage{
exclure(data, fichier)
}
\arguments{
\item{data}{Le dataset de base.}

\item{fichier}{Un string. Le nom de l'échantillon à enlever ou de l'élément chimique à ne pas prendre en compte dans l'analyse.}
}
\value{
Le dataframe sans l'élément marginal ou l'élément chimique.
}
\description{
Renvoit un dataset sans un échantillon marginal du dataset ou une variable peu intéressante.
}
\examples{
data('rock')
rock_sans_12<-exclure(rock,'13')


}
