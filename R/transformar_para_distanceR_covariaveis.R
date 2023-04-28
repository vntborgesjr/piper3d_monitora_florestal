# transforma para o formato para análise no pacote distance do R
# adicionando duas covariáveis
transformar_para_distanceR_covariaveis <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        '/data/dados_completos.rds'
      )
    )
  ) {
  dados_transformados_dist_r_cov <- dados_completos |>  
    dplyr::select(
      Region.Label = `ea_name`,
      Sample.Label = `sampling_day`,
      Effort = day_effort,
      sp_name,
      sp_name_abv,
      distance,
      year,
      size = group_size,
      cense_time
    ) |> 
    dplyr::mutate(
      Area = 0,
      Sample.Label = lubridate::date(Sample.Label),
      object = 1:nrow(dados)
    ) |> 
    dplyr::relocate(
      Area,
      .before = Sample.Label
    )
  
  # garvar no diretório data/ arquivo dados_transformados_dist_r_cov.rds
  readr::write_rds(
    dados_transformados_dist_r_cov,
    file = paste0(
      stringr::str_remove(
        getwd(), 
        "doc"
      ),
      "/data/dados_transformados_dist_r_cov.rds"
    )
  )
  # retorna o data.frame
  return(dados_transformados_dist_r_cov)
}

