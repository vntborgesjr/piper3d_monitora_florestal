source("R/carregar_dados_brutos_xlsx.R")
source("R/gerar_dados_completos.R")
source("R/filtrar_dados.R")
source("R/transformar_dados_formato_Distance.R")

# carregar dados brutos
dados <- carregar_dados_brutos_xlsx()

# gerar dados completos - demora...
dados_completos <- gerar_dados_completos(dados)

# dados filtrados com dias sem observações incluídos!
dados_filtrados <- filtrar_dados(
  dados = dados_completos,
  nome_uc %in% c("esec_da_terra_do_meio" ,"parna_da_serra_do_pardo"),
  nome_sp %in% c("sapajus_apella"),
  validacao_obs = "especie"
)

dados_filtrados |> 
  View()

# teste transformar formato distance sem repetição
# incluidas colunas repeated_visits e Effort_day
# funcionando!
dados_formato_distance_repeticao <- transformar_dados_formato_Distance(
  dados = dados_filtrados,
  Region.Label,
  Sample.Label,
  amostras_repetidas = TRUE
)

dados_formato_distance_repeticao |>
  View()

# teste transformar formato distance sem repetição
# precisa de ajuste
dados_formato_distance <- transformar_dados_formato_Distance(
  dados = dados_filtrados,
  Region.Label,
  Sample.Label,
  season,
  year,
  amostras_repetidas = FALSE
)

dados_formato_distance |>
  View()
