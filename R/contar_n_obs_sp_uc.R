# Descrição
# fornece o número total de observações por espécie por UC
contar_n_obs_sp_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    )
) {
  # gera o número de observaçẽos por espécie
  n_obs_sp_uc <- dados |>  
    dplyr::filter(
      validation == "Espécie"
    ) |> 
    dplyr::count(
      uc_name, 
      uc_name_abv,
      sp_name, 
      sp_name_abv
    )
  
  # grava a tabela n_obs_sp_uc.rds no diretório data/
  readr::write_rds(
    n_obs_sp_uc,
    file = paste0(
      here::here(),
      "/data/n_obs_sp_uc.rds"
    )
  )
  
  # retorna o número de observaçẽos por espécie
  return(n_obs_sp_uc)
}

# Exemplo

#contar_n_obs_sp_uc() 
