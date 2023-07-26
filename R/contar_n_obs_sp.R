# Documentação da função contar_n_obs_sp() --------------------
#' Número total de observações para cada espécie
#'
#' @description
#' A função \code{contar_n_obs_sp()} gera uma tabela contendo o número total de observações para cada espécie.
#'
#' @usage contar_n_obs_sp(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pela função [filtrar_dados()]. Por configuração, carrega a base de dados burtos de aves e medios e grandes mamiferos do Pojeto Monitora Componente Florestal `monitora_aves_masto_florestal`.
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo três colunas, \code{nome_sp} contendo o nome da espécie, \code{nome_sp_abv} que contem o nome da espécie abreviado e \code{n} contendo o número de observações realizadas para cada espécie

#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # número de observações para cada espécie - dados brutos
#' contar_n_obs_sp()
#'
#' # número de observações para cada espécie - dados filtrados
#' dados_filtrados <- filtrar_dados(
#'   nome_ucs = "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_obs_sp(dados_filtrados)
contar_n_obs_sp <- function(
    dados = monitora_aves_masto_florestal
) {

  # gera o número de observações por espécie
  n_obs_sp <- dados |>
    dplyr::count(
      nome_sp,
      nome_sp_abv
    )

  return(n_obs_sp)

}

utils::globalVariables(
  c(
    "monitora_aves_masto_florestal",
    "n_obs_sp",
    "dados",
    "nome_sp",
    "nome_sp_abv"
  )
)
