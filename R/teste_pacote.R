# remove.packages("distanceMonitoraflorestal")
# instalar o pacotes distanceMonitoraflorestal
# devtools::install_github(
#   "vntborgesjr/distanceMonitoraflorestal",
#   force = TRUE
# )

library(distanceMonitoraflorestal)

# testar funcoes de carregamento dos dados --------------------------------

dados_brutos <- carregar_dados_brutos(
  nomes_colunas_originais = FALSE
)

# testar funcoes de transformacao dos dados -------------------------------

dados_completos <- gerar_dados_completos(dados = dados_brutos)

dados_filtrados <- gerar_dados_filtrados(
  dados = dados_completos,
  nome_uc = "Resex Tapajos-Arapiuns",
  nome_sp = "Dasyprocta croconota"
)
dados_selecionados <- gerar_dados_selecionados(dados = dados_completos)

