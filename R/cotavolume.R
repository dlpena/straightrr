#' cotavolume
#'
#' Gera a curva cota-volume de um reservatório por meio de um polinômio do tipo
#' co + c1 x + c2 x^2 + c3 x^3 + c4  x^4
#'
#' @param ini valor da cota inicial da curva (obrigatório)
#'
#' @param fim valor da cota final da curva (obrigatório)
#'
#' @param passo passo de cota da curva (obrigatório)
#'
#' @param c0 valor da coeficiente do gator de expoente 0 da curva (opcional)
#'
#' @param c1 valor da coeficiente do gator de expoente 1 da curva (opcional)
#'
#' @param c2 valor da coeficiente do gator de expoente 2 da curva (opcional)
#'
#' @param c3 valor da coeficiente do gator de expoente 3 da curva (opcional)
#'
#' @param c4 valor da coeficiente do gator de expoente 4 da curva (opcional)
#'
#' @return a função retorna um data frame com as duas colunas, cota e volume,
#' calculcados com o passo e intervalo de cota definidos pelo usuário na função.
#'
#' @export


cotavolume <-  function(ini, fim, passo = 0.01, c0 = 0, c1 = 0, c2 = 0, c3 = 0, c4 = 0) {

  rngcota <- seq(ini, fim, by = passo)
  cota <-  NULL
  volume <- NULL

  for (i in rngcota) {

    cota <- c(cota, i)

    volume_i = c0 +
      (c1 * i) +
      (c2 * (i ** 2)) +
      (c3 * (i ** 3)) +
      (c4 * (i ** 4))

    volume <- c(volume, volume_i)

  }

 Cota_volume <- data.frame(cota, volume)

 Cota_volume

}
