---
title: "Documentation travail_sur_groupe"
output: rmarkdown::html_document
---

## Généralité

La fonction `travail_sur_groupe` permet d'obtenir un histogramme des distances euclidiennes de Mahalanobis de l'ensemble des éléments d'un groupe. Il affiche également les corrélations entre les variables éléments.  

* La distance euclidienne utilisée est "standardisée" par l'écart-type, afin ramener les éléments mineurs en ppm et les éléments majeurs en pourcent à une même échelle.  
La distance est ensuite normalisée par le nombre d'élément pour être comparable à d'autres distances au fur et à mesure du processus d'analyses de données.  

<center>
$d = \frac{1}{n} \sqrt{\sum_{i=1}^p \left( \frac{e_i - \mu_i}{\sigma_i} \right)^2}$
</center>  
  
  
* La distance de Mahalanobis est calculée avec l'inverse de la matrice des corrélations. Pour le calcul des probabilités voir (Picon, 1973) et (Lebart et al., 2010).

## Usage

```r
travail_sur_groupe(data,pas)
