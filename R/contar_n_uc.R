# fornece o número total de UCs na base de dados
contar_n_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_completos.rds"
      )
    )
) {
  #
  n_ucs <- dados |> 
    dplyr::distinct(uc_name) |> 
    nrow()
  
  return(n_ucs)
}

