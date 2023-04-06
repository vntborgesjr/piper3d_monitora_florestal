# Descrição
# gera tabela dinâmica do número de observações por espécies

gerar_tabdin_n_obs_sp <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data/n_obs_sp.rds'
      )
    )
) {
  # gerar tabela dinâmica dos dados do número de observações por sp
  tabdin_n_obs_sp <- dados |> 
    DT::datatable(
      filter = list(
        position = "top"
      )
    )
  
  # retornar a tabela dinamica do número de observações por sp
  return(tabdin_n_obs_sp)
}