# a função carregar_dados_filtrados carrega a planilha de dados selecioinando apenas
# a UC e espécie desejada no formato da Luciana Fusinatto
# deve carregar uma planilha que esteja no formato do arquivo:
# Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP.xlsx
# enviada pelo Gerson por whatsapp no dia 08/03/2023
carregar_dados_filtrados <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data/dados_completos.rds'
      )
    ),
    nome_uc = "Resex Tapajós-Arapiuns",
    nome_sp = "Dasyprocta croconota"
    
  ) {
  # gerar o data.frame desejado
  dados_filtrados <- dados |>  
    dplyr::filter(
      uc_name == nome_uc,
      sp == nome_sp
    )
  
  # grava uma versão dados_filtrados.rds no diretório data
  readr::write_rds(
    dados_filtrados,
    file = paste0(
      stringr::str_remove(
        getwd(), 
        "doc"
      ),
      "/data/dados_filtrados.rds"
    )
  )
  
  # retornar o data.frame
  return(dados_filtrados)
}

# exemplo de uso da função
# lembre-se de especificar o diretório correto onde o arquivo se encontra
# na sua máquina
#dados <- carregar_dados_filtrados(dados = /home/usuario/Documentos/Vitor/Piper3D/WWF/Monitora/Dados/data-raw/Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP.xlsx",
 #                        nome_uc = "Resex Tapajós-Arapiuns",
  #                       nome_sp = "Dasyprocta croconota")
#View(dados)
