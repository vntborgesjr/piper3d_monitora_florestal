# Descrição
# gera tabela dinâmica dos dados filtrado

gerar_tabdin_dados_filtrados <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data/dados_completos.rds'
      )
    ),
    nome_uc = "Resex Tapajós-Arapiuns",
    nome_sp = "Dasyprocta croconota",
    n_linhas = 1:4500
) {
  # gerar tabela dinâmica dos dados filtrado
  dados_filtrado <- dados |>  
    dplyr::filter(
      uc_name == nome_uc,
      sp == nome_sp
    ) |> 
    datatable(
      filter = list(
        position = "top"
      )
    )
  
  # retornar a tabela dinamica dos dados filtrado
  return(dados_filtrado)
}