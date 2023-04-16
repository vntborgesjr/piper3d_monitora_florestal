# Descrição
# gera tabela dinâmica do número de observações por espécies

gerar_tabdin_n_obs_uc_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data/n_obs_uc_ano.rds'
      )
    )
) {
  # gerar tabela dinâmica dos dados do número de anos em que cada UC foi amostrada
  tabdin_n_obs_uc_ano <- dados |> 
    DT::datatable(
      filter = list(
        position = "top"
      )
    )
  
  # retornar a tabela dinamica do número de anos em que cada UC foi amostrada
  return(tabdin_n_obs_uc_ano)
}