% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/procv.R
\name{procv}
\alias{procv}
\title{procv}
\usage{
procv(val_procv, curvacv)
}
\arguments{
\item{val_procv}{valor de cota procurado na curva cota-volume}

\item{curvacv}{curva cota-volume (data frame contendo respectivamente as colunas cota e volume)}
}
\value{
a função retorna o volume correspondente à cota inserida pelo usuário no argumento val_procv.
}
\description{
A função retorna o volume correspondente a uma determinada cota em uma curva cota-volume
}
