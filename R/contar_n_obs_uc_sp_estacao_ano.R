# Descrição
# fornece o número total de observações por espécie, por Uc, por estação e por ano.

contar_n_obs_uc_sp_estacao_ano <- function(
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
  # gera o número de observaçẽos por espécie, por Uc, por estação e por ano
  n_obs_uc_sp_estacao_ano <- dados |>  
    dplyr::filter(
      validation == "Espécie"
    ) |> 
    dplyr::count(
      uc_name, 
      uc_name_abv,
      sp,
      season,
      year
      #sp_abv
    )
  
  # grava a tabela n_obs_uc_sp_estacao_ano.rds no diretório data/
  readr::write_rds(
    n_obs_uc_sp_estacao_ano,
    file = paste0(
      stringr::str_remove(
        getwd(), 
        "doc"
      ),
      "/data/n_obs_uc_sp_estacao_ano.rds"
    )
  )
  
  # retorna o número de observaçẽos por espécie, por Uc, por estação e por ano
  return(n_obs_uc_sp_estacao_ano)
}

# Exemplo

#contar_n_obs_sp_uc() 
