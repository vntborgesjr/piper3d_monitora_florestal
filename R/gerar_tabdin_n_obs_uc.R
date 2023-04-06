# Descrição
# gera tabela dinâmica dos dados selecionados

gerar_tabdin_n_obs_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data/n_obs_uc.rds'
      )
    )
) {
  # gerar tabela dinâmica dos dados do número de observações por uc
  tabdin_n_ub_uc <- dados |> 
    DT::datatable(
      filter = list(
        position = "top"
      )
    )
  
  # retornar a tabela dinamica do número de observações por uc
  return(tabdin_n_ub_uc)
}