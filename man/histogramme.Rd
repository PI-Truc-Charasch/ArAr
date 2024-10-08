% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/histogramme.R
\name{histogramme}
\alias{histogramme}
\title{Fonction histogramme}
\usage{
histogramme(
  data,
  element,
  pas,
  borne_inferieure = NULL,
  borne_superieure = NULL,
  echantillonage = TRUE
)
}
\arguments{
\item{data}{dataset de base}

\item{element}{string du nom de l'element a tracer}

\item{pas}{le pas entre chaque barre de l'histogramme}

\item{borne_inferieure}{la borne inferieure de l'axe des x pour l'histogramme. Valeur prise au minimum des valeurs de l'element par defaut}

\item{borne_superieure}{la borne superieure de l'axe des x pour l'histogramme. Valeur prise au minimum des valeurs de l'element par defaut}

\item{echantillonage}{booleen. est ce qu'on affiche le nom des echantillons dans l'histogramme ? TRUE de base}
}
\value{
un graphique
}
\description{
Cette fonction crée un histogramme base sur les valeurs d'un element chimique ou d'une variable numerique fournie.
Elle permet de specifier des bornes inferieures et superieures ainsi qu'un pas pour controler la granularite de l'histogramme.
Elle est utile pour observer la distribution d'un element chimique dans un ensemble de donnees.
}
\examples{
data('iris')
histogramme(iris,'Sepal.Length',1)


}
