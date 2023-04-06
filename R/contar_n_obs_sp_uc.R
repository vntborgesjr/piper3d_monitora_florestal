# Descrição
# fornece o número total de observações por espécie.

contar_n_obs_sp_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          "doc"
        ),
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
      sp, 
      #sp_abv
    )
  
  # grava a tabela n_obs_sp_uc.rds no diretório data/
  readr::write_rds(
    n_obs_sp_uc,
    file = paste0(
      stringr::str_remove(
        getwd(), 
        "doc"
      ),
      "/data/n_obs_sp_uc.rds"
    )
  )
  
  # retorna o número de observaçẽos por espécie
  return(n_obs_sp_uc)
}

# Exemplo

#contar_n_obs_sp_uc() 
