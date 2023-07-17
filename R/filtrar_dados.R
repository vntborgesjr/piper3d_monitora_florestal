# Documentacao funcao filtrar_dados() --------------------------
#' Filtra os dados a partir das Unidades de Conservação, das espécies e do nível taxonomico de validação
#'
#' @description
#' A função \code{filtrar_dados()} filtra a base de dados brutos a partir das colunas \code{nome_uc}, \code{nome_sp} e \code{validacao} que correspondem a(s) Unidade(s) de Conservação, a(s) espécie(s) e ao(s) nível(is) taxonômico(s) de validação desejadas, respectivamente.
#'
#' @usage filtrar_dados(
#'          dados = monitora_aves_masto_florestal,
#'          nome_ucs = NULL,
#'          nome_sps = NULL,
#'          validacao_obs = c("ordem", "familia", "genero", "especie", "na")
#'        )
#'
#' @param dados recebe uma \code{tibble} que contenha as colunas `nome_uc`, `nome_sp` e `validacao`. Por configuração, carrega a base de dados burto de aves e médios e grandes mamíferos do Pojeto Monitora Componente Florestal, `monitora_aves_mamiferos_florestal`.
#' @param nome_ucs recebe um vetor do tipo caracter contendo o nome de uma ou mais Unidades de Conservação.
#' @param nome_sps recebe um vetor do tipo caracter contendo o nome de uma ou mais espécies.
#' @param validacao_obs recebe um vetor do tipo caracter contendo o nome de um ou mais níveis taxonômicos de validação.
#'
#' @details
#' A filtragem da base de dados pode ser feita a partir do nome das Unidades de Conservação, a partir do nome das espécies, a partir do nível taxonômico de validação, ou a partir de suas combinações. Para maiores detalhes consulte os exemplos.
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo os dados do Projeto Monitora Componente Florestal, filtrados a partir da(s) Unidade(s) de Conservação, da(s) espécie(s), ou dos nível(is) taxonômico(s) de validação.
#'
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#' @export
#'
#' @examples
#' # carregar pacote
#' library(dplyr)
#'
#' # carregar dados filtrados por uma Unidade de Conservacão
#' dados_filtrados_uc1 <- filtrar_dados(
#' nome_ucs = "resex_tapajos_arapiuns"
#' )
#'
#' glimpse(dados_filtrados_uc1)
#'
#' # carregar dados filtrados por mais de uma Unidade de Conservação
#' dados_filtrados_uc2 <- filtrar_dados(
#' nome_ucs = c("resex_tapajos_arapiuns", "resex_barreiro_das_antas")
#' )
#'
#' glimpse(dados_filtrados_uc2)
#'
#' # carregar dados filtrados por mais de uma Unidade de Conservação e níveis taxonômicos de validação
#' dados_filtrados_uc_validacao1 <- filtrar_dados(
#' nome_ucs = "resex_tapajos_arapiuns",
#' validacao_obs = c("genero", "especie")
#' )
#'
#' glimpse(dados_filtrados_uc_validacao1)
#'
#' # carregar dados filtrados por uma espécie
#' dados_filtrados_sp1 <- filtrar_dados(
#' nome_sps = "dasyprocta_croconota"
#' )
#'
#' glimpse(dados_filtrados_sp1)
#'
#' # carregar dados filtrados por mais de uma especie
#' dados_filtrados_sp2 <- filtrar_dados(
#' nome_sps = c("dasyprocta_croconota", "dasyprocta_iacki")
#' )
#'
#' glimpse(dados_filtrados_sp2)
#'
#' # carregar dados filtrados por unidade de conservacao e especie
#' dados_filtrados_uc_sp <- filtrar_dados(
#' nome_ucs = "resex_tapajos_arapiuns",
#' nome_sps = c("dasyprocta_croconota")
#' )
#'
#' glimpse(dados_filtrados_uc_sp)
#'
#' # carregar dados filtrados por Unidades de Conservação, espécie e nível taxonômico de validação
#' dados_filtrados_uc_sp_validacao <- filtrar_dados(
#'   nome_ucs = c("resex_tapajos_arapiuns", "resex_barreiro_das_antas"),
#'   nome_sps = c("dasyprocta_croconota", "guerlinguetus_aestuans"),
#'   validacao_obs = "especie"
#' )
#'
#' glimpse(dados_filtrados_uc_sp_validacao)
filtrar_dados <- function(
    dados = monitora_aves_masto_florestal,
    nome_ucs = NULL,
    nome_sps = NULL,
    validacao_obs = c("ordem", "familia", "genero", "especie", "na")
) {

  # controlar as possibilidades de filtragem
  if (is.null(nome_ucs) & is.null(nome_sps)) {

    # gerar o tibble filtrado por UC e especie e nivel taxonomico de validacao
    dados_filtrados <- dados |>
      dplyr::filter(
        validacao %in% validacao_obs
      )

  } else if (!is.null(nome_ucs) & !is.null(nome_sps)) {

    # gerar o tibble filtrado por UC e especie e nivel taxonomico de validacao
    dados_filtrados <- dados |>
      dplyr::filter(
        nome_uc %in% nome_ucs,
        nome_sp %in% nome_sps,
        validacao %in% validacao_obs
      )

  } else  if (is.null(nome_ucs)) {

    # gerar o tibble filtrado por especie e nivel taxonomico de validacao
    dados_filtrados <- dados |>
      dplyr::filter(
        nome_sp %in% nome_sps,
        validacao %in% validacao_obs
      )

  } else if (is.null(nome_sps)) {

    # gerar o tibble filtrado por UC e nivel taxonomico de validacao
    dados_filtrados <- dados |>
      dplyr::filter(
        nome_uc %in% nome_ucs,
        validacao %in% validacao_obs
      )

  }

  # retornar o tibble com os dados filtrados e nivel taxonomico de validacao
  return(dados_filtrados)
}

utils::globalVariables(
  c(
    "dados_filtrados",
    "nome_uc",
    "nome_sp",
    "nome_ucs",
    "nome_sps",
    "validacao",
    "validacao_obs"
  )
)
