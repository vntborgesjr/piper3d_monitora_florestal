# Descrição
# fornece o número total de espécies na base de dados
contar_n_sp <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_completos.rds"
      )
    )
) {
  #
  n_sp <- dados |> 
    dplyr::distinct(sp_name) |> 
    nrow()
  
  return(n_sp)
}

# Exemplo