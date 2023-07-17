# Documentacao da funcao carregar_dados_brutos.xlsx -----------------------
#' Carrega os dados brutos originais em formato .xlsx
#'
#' @description
#' A funcao \code{carregar_dados_brutos_xlsx()} carrega o arquivo da planilha de dados originais, \code{"monitora_masto_aves_2023_04_04.xlsx"} e gera um novo arquivo \code{dados_brutos.rds}.
#'
#' @param dados recebe o caminho para carregar um arquivo .xlsx. Por configuracao, carrega os dados brutos dos Projeto Monitora Componente Florestal \code{"monitora_masto_aves_2023_04_04.xlsx"}. A versao dos dados brutos corresponde ao arquivo "Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP.xlsx"
# enviada por whatsapp no dia 08/03/2023
#'
#' @details
#' A funcao \code{carregar_dados_brutos_xlsx()} carrega e disponibiliza ao usario os dados brutos do Projeto Monitora Componente Florestal, contendo  as infromacoes sobre as amostragens por distancia (\emph{distance samplig}) de aves e pequenos e medios mamiferos realizadas desde 2014 em 40 Unidades de Conservacao do Brasil. O novo arquivo gerado fica disponivel para carregamento a partir do uso da funcao \code{carregar_dados_brutos_rds()}.
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo os dados brutos do Projeto Monitora Componente Florestal a partir do formato .xlsx.
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#' @export
#'
#' @examples \dontrun{carregar_dados_brutos_xlsx()}
carregar_dados_brutos_xlsx <- function(
    dados = "data-raw/monitora_masto_aves_2023_04_04.xlsx",
      sheet = "dados brutos"
) {
  
  # carregar dados brutos
  dados_brutos <- readxl::read_excel(
    path = here::here(dados),
    sheet = sheet
  )
  
  # grava uma versao dados_brutos.rds no diretorio inst/extdata
  readr::write_rds(
    dados_brutos,
    file = here::here("data-raw/dados_brutos.rds")
    )

  # retorna os dados butos
  return(dados_brutos)
}
