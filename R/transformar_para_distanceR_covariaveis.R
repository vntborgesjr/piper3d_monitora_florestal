# Descrição
# deve receber dados de uma espécie e UC('s) pré-selecionadas e 
# transforma para o formato para análise no pacote distance do R
# adicionando duas covariáveis
transformar_para_distanceR_covariaveis <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data/dados_completos.rds'
      )
    )
  ) {
  dados_transformados_dist_r_cov <- dados_completos |>  
    dplyr::select(
      Region.Label = `ea_name`,
      Sample.Label = `sampling_day`,
      Effort = day_effort,
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

# exemplo de uso da função
# lembre-se de especificar o diretório correto onde o arquivo se encontra
# na sua máquina
#source("R/carregar_dados_completos.R")
#source("R/carregar_dados_filtrados.R")

#dados1 <- carregar_dados_completos("/home/usuario/Documentos/Vitor/Piper3D/WWF/Monitora/Dados/data-raw/Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP.xlsx")
#dados2 <- carregar_dados_filtrados(dados = "/home/usuario/Documentos/Vitor/Piper3D/WWF/Monitora/Dados/data-raw/Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP.xlsx",
 #                                  nome_uc = "Resex Tapajós-Arapiuns",
  #                                 nome_sp = "Dasyprocta croconota")

#dados3 <- filtra_trasforma_para_distanceR(dados1)
#View(dados3)  

#dados4 <- transforma_para_distanceR2(dados2)
#View(dados4)
