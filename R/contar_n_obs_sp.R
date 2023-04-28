# Descrição
# fornece o número total de observações por espécie.
contar_n_obs_sp <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_completos.rds"
      )
    )
) {
  #
  n_obs_sp <- dados |> 
    dplyr::count(
      sp_name,
      sp_name_abv
    )
  
  # grava a tabela n_uc_ano.rds no diretório data/
  readr::write_rds(
    n_obs_sp,
    file = paste0(
      here::here(),
      "/data/n_obs_sp.rds"
    )
  )
  
  return(n_obs_sp)
}

# Exemplo