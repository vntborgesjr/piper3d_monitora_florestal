# Descrição
# gera tabela dinâmica dos dados brutos

gerar_tabdin_dados_brutos <- function(
    dados = readxl::read_excel(
      path = paste0(
        stringr::str_remove(
          getwd(), 
          "doc"
        ),
        "/data-raw/Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP.xlsx"
      ),
      sheet = "dados brutos"
    ),
    n_linhas = 1:1000
) {
  # gerar tabela dinâmica dos dados completos
  dados_brutos <- dados |> 
    slice(n_linhas) |> 
    DT::datatable(filter = list(position = "top"))
  
  # retornar a tabela dinamica dos dados brutos
  return(dados_brutos)
}