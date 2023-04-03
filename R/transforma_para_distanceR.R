# Descrição
# recebe um conjunto de dados gerados a partir da função carregar_dados1() 
# e carregar_dados_completo() retorna um tibble no formato para análise no pacote 
# Distance do R
transforma_para_distanceR <- function(dados) {
  dados1 <- dados |>  
    dplyr::select(
      uc_code,
      uc_name,
      ea_number,
      Region.Label = `ea_name`,
      year,
      season,
      Sample.Label = sampling_day,
      Effort = day_effort,
      sp,
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
  
  # retorna o data.frame
  return(dados1)
}

# exemplo de uso da função
# lembre-se de especificar o diretório correto onde o arquivo se encontra
# na sua máquina
#source("R/carregar_dados_completo.R")
#source("R/carregar_dados_filtrados.R")

#dados1 <- carregar_dados1("/home/usuario/Documentos/Vitor/Piper3D/WWF/Monitora/Dados/data-raw/Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP.xlsx")
#dados2 <- carregar_dados2(dados = "/home/usuario/Documentos/Vitor/Piper3D/WWF/Monitora/Dados/data-raw/Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP.xlsx",
 #                                  nome_uc = "Resex Tapajós-Arapiuns",
  #                                 nome_sp = "Dasyprocta croconota")

#dados3 <- trasforma_para_distanceR_completo(dados1)
#View(dados3)  

#dados4 <- trasforma_para_distanceR_completo(dados2)
#View(dados4)
