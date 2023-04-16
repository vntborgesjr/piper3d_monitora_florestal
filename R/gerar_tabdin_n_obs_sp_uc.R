# Descrição
# gera tabela dinâmica do número de observações por espécies por UC

gerar_tabdin_n_obs_sp_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data/n_obs_sp_uc.rds'
      )
    )
) {
  # gerar tabela dinâmica dos dados do número de observações por sp por UC
  tabdin_n_obs_sp_uc <- dados |> 
    DT::datatable(
      filter = list(
        position = "top"
      )
    )
  
  # retornar a tabela dinamica do número de observações por sp por UC
  return(tabdin_n_obs_sp_uc)
}