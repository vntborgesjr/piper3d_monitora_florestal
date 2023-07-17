# Documentação da função contar_n_obs_validadas() --------------------
#' Número total de observações validadas por nível taxonômico
#'
#' @description
#' A função \code{contar_n_obs_validadas()} gera uma tabela ou um vetor contendo o número total de observações validadas para cada nível taxonômico.
#'
#' @usage contar_n_obs_validadas(
#'          dados = monitora_aves_masto_florestal,
#'          retorna_tabela = TRUE
#'        )
#'
#' @param dados recebe a \code{tibble} gerada pela função [filtrar_dados()]. Por configuração, carrega a base de dados burtos de aves e médios e grandes mamíferos do Pojeto Monitora Componente Florestal `monitora_aves_masto_florestal`.
#' @param retorna_tabela lógico, indica se o objeto retornado e uma uma \code{tibble} ou um \code{vetor}.
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo duas colunas, \code{validacao} contendo os níveis taxonômicos de validação e \code{n} contendo o número de observações validadas para cada nível taxonômico, ou um objeto do tipo \code{vector} nomeado contendo os niveis taxonomicos (nomes) e o número de observações para cada nível taxonômico.
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # observações para cada nível taxonômico - dados brutos
#' contar_n_obs_validadas()
#'
#' # observações para cada nível taxonômico - dados brutos
#' contar_n_obs_validadas(retorna_tabela = FALSE)
#'
#' # observações para cada nível taxonômico - dados filtrados
#' dados_filtrados <- filtrar_dados(
#'   nome_ucs = "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_obs_validadas(dados_filtrados)
contar_n_obs_validadas <- function(
    dados = monitora_aves_masto_florestal,
    retorna_tabela = TRUE
) {

  # gerar tabela com o número de observações validadas para cada nivel
  # taxonomico
  tabela_n_obs_validadas <- dados |>
    dplyr::count(validacao)

  # extrair os números de observações validadas
  n_obs_validadas <- tabela_n_obs_validadas |>
    dplyr::pull(var = n)

  # atribuir nome dos niveis taxonomicos aos valores
  names(n_obs_validadas) <- tabela_n_obs_validadas$validacao

  # retornar observações validadas...
  if (retorna_tabela == TRUE) {

    # em formato de tabela
    return(tabela_n_obs_validadas)

  } else {

    # retorna em formato de vetor
    return(n_obs_validadas)

  }

}

utils::globalVariables(
  c(
    "tabela_n_obs_validadas",
    "n_obs_validadas",
    "dados",
    "validacao",
    "n"
  )
)
