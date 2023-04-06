# Descrição
# gera tabela dinâmica dos dados selecionados

gerar_tabdin_dados_selecionados <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data/dados_selecionados.rds'
      )
    ),
    n_linhas = 1:4500
) {
  # gerar tabela dinâmica dos dados selecionados
  tabdin_dados_selecionados <- dados |> 
    slice(
      n_linhas
    ) |> 
    DT::datatable(
      filter = list(
        position = "top"
      )
    )
  
  # retornar a tabela dinamica dos dados selecionados
  return(tabdin_dados_selecionados)
}