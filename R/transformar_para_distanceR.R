# transforma os dados para o formato para análise no pacote Distance do R
transformar_para_distanceR <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        '/data/dados_completos.rds'
      )
    )
  ) {
  # transforma dados para formato distance R
  dados_transformados_dist_r <- dados_completos |>  
    dplyr::select(
      uc_code,
      uc_name,
      ea_number,
      Region.Label = `ea_name`,
      year,
      season,
      Sample.Label = sampling_day,
      Effort = day_effort,
      sp_name,
      sp_name_abv,
      distance,
      size = group_size,
      number_observers
    ) |> 
    dplyr::mutate(
      Area = 0
    ) |> 
    dplyr::relocate(
      Area,
      .before = Sample.Label
    )
  
  # grava uma versão dados_transformados_dist_r.rds no diretório data
  readr::write_rds(
    dados_transformados_dist_r,
    file = paste0(
      stringr::str_remove(
        getwd(), 
        "doc"
      ),
      "/data/dados_transformados_dist_r_completo.rds"
    )
  )
  
  # retorna o data.frame
  return(dados_transformados_dist_r)
}

