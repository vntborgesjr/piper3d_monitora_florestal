# Descrição
# a função carregar_dados1() carrega toda a planilha de dados 
# selecionando automaticamente as colunas CDU, Local - Nome da Unidade de 
# Conservação
# deve carregar uma planilha que esteja no formato do arquivo:
# Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP
# enviada pelo Gerson por whatsapp no dia 08/03/2023

carregar_dados_selecionados <- function(
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
  
  # gerar o data.frame desejado
  dados_selecionados <- dados |> 
    filter(validation == "Espécie")
  
  # retornar o data.frame
  return(dados_selecionados)
}

# exemplo de uso da função
# lembre-se de especificar o diretório correto onde o arquivo se encontra
# na sua máquina
#dados <- carregar_dados_selecionados()
#View(dados)

