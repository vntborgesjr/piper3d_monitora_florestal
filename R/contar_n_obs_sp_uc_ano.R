# Descrição
# fornece o número total de observações por espécie, por Uc e por ano.
contar_n_obs_sp_uc_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    )
) {
  # gera o número de observaçẽos por espécie, por Uc e por ano
  n_obs_sp_uc_ano <- dados |>  
    dplyr::filter(
      validation == "Espécie"
    ) |> 
    dplyr::count(
      uc_name, 
      uc_name_abv,
      sp_name, 
      sp_name_abv,
      year
    )
  
  # grava a tabela n_obs_sp_uc.rds no diretório data/
  readr::write_rds(
    n_obs_sp_uc_ano,
    file = paste0(
      here::here(),
      "/data/n_obs_sp_uc_ano.rds"
    )
  )
  
  # retorna o número de observaçẽos por espécie, por Uc e por ano
  return(n_obs_sp_uc_ano)
}

# Exemplo

#contar_n_obs_sp_uc_ano() 
