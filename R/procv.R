
#' procv
#'
#' A função retorna o volume correspondente a uma determinada cota em uma curva cota-volume
#'
#' @param val_procv valor de cota procurado na curva cota-volume
#' @param curvacv curva cota-volume (data frame contendo respectivamente as colunas cota e volume)
#'
#' @return a função retorna o volume correspondente à cota inserida pelo usuário no argumento val_procv.
#' @export
#'
#'
#'
procv <- function(val_procv, curvacv) {
  if (length(ncol(curvacv)) == 0) {

    stop("O argumento \'curvacv\' deve ser um data frame de duas colunas")
  }
  else if (ncol(curvacv) != 2) {

    stop("O argumento \'curvacv\' deve ser um data frame de duas colunas")

    }
    else {

    num_linha <- which(curvacv[, 1] == val_procv)

    valor <- curvacv[num_linha, 2]

      if (length(valor) > 0) {

        return(valor)

      }
      else {

      warning("O valor procurado não se encontra na curva")

      }
    }
}


