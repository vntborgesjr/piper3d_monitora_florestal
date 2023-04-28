# Descrição
# gera uma tabela com o número de observações válidas para cada UC
contar_n_obs_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    )
) {
  # gerar a tabela com o número de observações por UC
  n_obs_uc <- dados  |>  
    dplyr::count(
      uc_name,
      uc_name_abv
    )
  
  # grava a tabela n_obs_uc.rds no diretório data/
  readr::write_rds(
    n_obs_uc,
    file = paste0(
      here::here(),
      "/data/n_obs_uc.rds"
    )
  )
  
  # retorna a tabela com o número de observações por UC
  return(n_obs_uc)
}

# Exemplo


