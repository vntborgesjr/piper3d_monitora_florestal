# Descrição
# gera uma tabela com o número de observações por UC em cada ano.

contar_n_obs_uc_ano <- function(
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
  # gerar a tabela com o número de observações por UC em cada ano
  n_obs_uc_ano <- dados  |>  
    dplyr::count(
      uc_name,
      uc_name_abv,
      year
    )
  
  # grava a tabela n_obs_uc.rds no diretório data/
  readr::write_rds(
    n_obs_uc_ano,
    file = paste0(
      stringr::str_remove(
        getwd(), 
        "doc"
      ),
      "/data/n_obs_uc_ano.rds"
    )
  )
  
  # retorna a tabela com o número de observações por UC
  return(n_obs_uc_ano)
}

# Exemplo


