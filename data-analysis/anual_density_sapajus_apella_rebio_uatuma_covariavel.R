##%###################################################################%##
#                                                                       #
####       Estimativa de densidade por amostragem por distância      ####
####                  Reserva Biológica do Uatumã                    ####
####                 Sapajus apella com covariavel                   ####
####                    2 de dezembro de 2023                        ####
#                                                                       #
##%###################################################################%##

# carregar pacote ---------------------------------------------------------

library(distanceMonitoraflorestal)

# filtrar dados -----------------------------------------------------------
# filtrar o esforço do dia para conter apenas observações com o mesmo esforço total
sapajus_apella_rebio_uatuma <- filtrar_dados(
  dados = monitora_aves_masto_florestal,
  nome_uc == "rebio_do_uatuma",
  nome_sp == "sapajus_apella",
  validacao_obs = "especie"
) |> dplyr::filter(esforco_dia == 5000)

# transformar os dados para o formato Distance ----------------------------

sapajus_apella_rebio_uatuma_distance <- transformar_dados_formato_Distance(
  dados = sapajus_apella_rebio_uatuma, 
  year
)

# gerar uma lista com os dados separados por anos -------------------------

sapajus_apella_rebio_uatuma_distance_year <- split(
  x = sapajus_apella_rebio_uatuma_distance,
  f = sapajus_apella_rebio_uatuma_distance$year
)

# gerar lista com legenda dos gráficos ------------------------------------

legenda <- list(
  `2014` = c("a", "b", "c"),
  `2015` = c("d", "e", "f"),
  `2016` = c("g", "h", "i"),
  `2017` = c("j", "k", "l"),
  `2018` = c("m", "n", "o"),
  `2019` = c("p", "q", "r"),
  `2020` = c("s", "t", "u"),
  `2021` = c("v", "w", "x")
)

# histograma de distribuição de frequência de observações por dist --------

fig1 <- sapajus_apella_rebio_uatuma_distance_year |> 
  purrr::map(
    \(.x) tidyr::drop_na(
      .x, 
      distance
    )
  ) |> 
  purrr::map2(
    legenda,
    \(.x, .y) plotar_distribuicao_distancia_estatico(
      .x,
      largura_caixa = 1,
      legenda = .y
    )
  )

# salvar figura
# purrr::map(
#   1:8, 
#   \(.x) paste0("output/fig1", letters[.x], ".tiff")
# ) |> 
#   purrr::map2(
#     fig1,
#     \(.x, .y) ggplot2::ggsave(
#       filename = .x,
#       plot = .y,
#       width = 128,
#       height = 148,
#       units = "mm",
#       dpi = 300
#     )
#   )


# testar distancia de truncamento -----------------------------------------

# 2014, 2019 a 2021
sapajus_apella_rebio_uatuma_truncamento1 <- sapajus_apella_rebio_uatuma_distance_year[c(1, 6:8)] |> 
  purrr::map(
    \(.x) selecionar_distancia_truncamento(.x)
  )

# truncamento de 25% dados para os anos de 2014 e 2019 a 2021

# 2015 a 2018
sapajus_apella_rebio_uatuma_truncamento2 <- sapajus_apella_rebio_uatuma_distance_year[c(2:5)] |> 
  purrr::map(
    \(.x, .y) selecionar_distancia_truncamento(
      dados = .x,
      dist_truncamento = list(`20%` = "20%", 
                              `15%` = "15%", 
                              `10%` = "10%", 
                              `5%` = "5%")
    )
  )

# truncamento de 20% dados para os anos de 2015 a 2018

# o truncamento de 20% será utilizado


# histograma dos dados truncados ------------------------------------------

# sapajus_apella_rebio_uatuma_truncamento <- list(
#   `2014` = sapajus_apella_rebio_uatuma_truncamento1$`2014`,
#   `2015` = sapajus_apella_rebio_uatuma_truncamento2$`2015`,
#   `2016` = sapajus_apella_rebio_uatuma_truncamento2$`2016`,
#   `2017` = sapajus_apella_rebio_uatuma_truncamento2$`2017`,
#   `2018` = sapajus_apella_rebio_uatuma_truncamento2$`2018`,
#   `2019` = sapajus_apella_rebio_uatuma_truncamento1$`2019`,
#   `2020` = sapajus_apella_rebio_uatuma_truncamento1$`2020`,
#   `2021` = sapajus_apella_rebio_uatuma_truncamento1$`2021`
# )
# 
# sapajus_apella_rebio_uatuma_truncamento |> 
#   purrr::map(
#     \(.x) plotar_funcao_deteccao_selecao_distancia_truncamento(.x)
#   ) |> 
# 
# # salvar figura
# purrr::map(
#   1:8,
#   \(.x) paste0("output/fig2", letters[.x], ".tiff")
# ) |>
#   purrr::map2(
#     fig2,
#     \(.x, .y) ggplot2::ggsave(
#       filename = .x,
#       plot = .y,
#       width = 84,
#       height = 74,
#       units = "mm",
#       dpi = 300
#     )
#   )

# Ajustando diferentes modelos com covariáveis -------------------------
# ajustar o modelo global sem covariável para selecionar a função de detecção
# 

# Half-normal sem termos de ajuste e sem covariável (HN)  --------

sapajus_apella_distance_hn_year <- sapajus_apella_rebio_uatuma_distance |>
  dplyr::mutate(Region.Label = year) |> 
  ajustar_modelos_Distance(
    funcao_chave = "hn",
    truncamento = "20%"
  )

sapajus_apella_distance_hn_year <- sapajus_apella_distance_hn_year$`Sem termo`

# Half-normal sem termos de ajuste e com covariável (HN + Grupo)  --------

sapajus_apella_distance_hn_year_size <- sapajus_apella_rebio_uatuma_distance |>
  dplyr::mutate(Region.Label = year) |> 
  ajustar_modelos_Distance(
    funcao_chave = "hn",
    truncamento = "20%",
    formula = ~ size
  )

# Hazard-rate sem termos de ajuste e sem covariável (HR)  --------

sapajus_apella_distance_hr_year <- sapajus_apella_rebio_uatuma_distance |>
  dplyr::mutate(Region.Label = year) |> 
  ajustar_modelos_Distance(
    funcao_chave = "hr",
    truncamento = "20%"
  )

sapajus_apella_distance_hr_year <- sapajus_apella_distance_hr_year$`Sem termo`

# Hazard-rate sem termos de ajuste e com covariável (HR + Grupo)  --------

sapajus_apella_distance_hr_year_size <- sapajus_apella_rebio_uatuma_distance |>
  dplyr::mutate(Region.Label = year) |> 
  ajustar_modelos_Distance(
    funcao_chave = "hr",
    truncamento = "20%",
    formula = ~ size
  )

# Tabela com o resumo comparativo dos modelos -----------------------------

melhor_modelo_sapajus_apella_year_size <- selecionar_funcao_deteccao_termo_ajuste(
  sapajus_apella_distance_hn_year,
  sapajus_apella_distance_hn_year_size,
  sapajus_apella_distance_hr_year,
  sapajus_apella_distance_hr_year_size
)

melhor_modelo_sapajus_apella_year_size


# Gráficos de ajuste das funções de deteção às probabilidades de detecção --------

modelos_sapajus_apella_year_size <- gerar_lista_modelos_selecionados(
  sapajus_apella_distance_hn_year,
  sapajus_apella_distance_hn_year_size,
  sapajus_apella_distance_hr_year,
  sapajus_apella_distance_hr_year_size,
  nome_modelos_selecionados = melhor_modelo_sapajus_apella_year_size
)

plotar_funcao_deteccao_modelos_selecionados(modelos_sapajus_apella_year_size)

# Teste de bondade de ajuste dos modelos e Q-Q plots ----------------------

testar_bondade_ajuste(
  modelos_sapajus_apella_year_size,
  plot = TRUE,
  chisq = FALSE,
)

# Avaliando as estimativas de Abundância e Densidade ----------------------

# Área coberta pela Amostragem --------------------------------------------

resultado_area_coberta_amostragem <- gerar_resultados_Distance(
  dados = modelos_sapajus_apella_year_size,
  resultado_selecao_modelos = melhor_modelo_sapajus_apella_year_size,
  tipo_de_resultado = "area_estudo", 
  estratificacao = TRUE
)

resultado_area_coberta_amostragem

# Abundância --------------------------------------------------------------

resultado_abundancia <- 
  gerar_resultados_Distance(
    dados = modelos_sapajus_apella_year_size,
    resultado_selecao_modelos = melhor_modelo_sapajus_apella_year_size,
    tipo_de_resultado = "abundancia", 
    estratificacao = TRUE
  )

resultado_abundancia

resultado_abundancia |>
  dplyr::mutate(ano = as.integer(Regiao)) |> 
  dplyr::group_by(ano) |> 
  dplyr::summarise(n = sum(`Abundancia estimada`)) |> 
  ggplot2::ggplot() +
  ggplot2::aes(
    x = ano,
    y = n
  ) + 
  ggplot2::geom_line() +
  ggplot2::theme_minimal()

# Densidade ---------------------------------------------------------------

resultados_densidade <- 
  gerar_resultados_Distance(
    dados = modelos_sapajus_apella_year_size,
    resultado_selecao_modelos = melhor_modelo_sapajus_apella_year_size,
    tipo_de_resultado = "densidade", 
    estratificacao = TRUE
  )

resultados_densidade

resultados_densidade |>
  dplyr::filter(Rotulo != "Total") |> 
  dplyr::mutate(ano = as.integer(Rotulo)) |> 
  ggplot2::ggplot() +
  ggplot2::aes(
    x = ano,
    y = `Estimativa de densidade (ind/ha)`
  ) + 
  ggplot2::geom_line() +
  ggplot2::facet_wrap(facets = ggplot2::vars(Modelo)) +
  ggplot2::theme_minimal()

# Ajustando modelos para cada estrato temporal com covariável (ano) ----------------------

# Half-normal -------------------------------------------------------------
# dados globais usando a função de detecção e o termo de ajuste selecionados
sapajus_apella_distance_year_hn_global <- sapajus_apella_distance_hn_year_size

# ano a ano usando a função de detecção e o termo de ajuste selecionado --------
# 2014, 2016, 2017, 2019, 2021
sapajus_apella_distance_year_hn1 <- sapajus_apella_rebio_uatuma_distance_year[c(1, 3, 4, 6, 8)] |> 
  purrr::map(
    \(.x) ajustar_modelos_Distance(
      dados = .x,
      funcao_chave = "hn",
      truncamento = "20%",
      formula = ~ size
      
    )
  )

# 2015 e 2020
sapajus_apella_distance_year_hn2 <- sapajus_apella_rebio_uatuma_distance_year[c(2, 7)] |> 
  purrr::map(
    \(.x) ajustar_modelos_Distance(
      dados = .x,
      funcao_chave = "hn",
      truncamento = "10%",
      formula = ~ size
    )
  )

# 2018
sapajus_apella_distance_year_hn3 <- ajustar_modelos_Distance(
  dados = sapajus_apella_rebio_uatuma_distance_year$`2018`,
  funcao_chave = "hn",
  truncamento = "15%",
  formula = ~ size
)

# modelos ajustados para cada estrato temporal ----------------------------

sapajus_apella_2014_distance_hn <- sapajus_apella_distance_year_hn1$`2014`
sapajus_apella_2015_distance_hn <- sapajus_apella_distance_year_hn2$`2015`
sapajus_apella_2016_distance_hn <- sapajus_apella_distance_year_hn1$`2016`
sapajus_apella_2017_distance_hn <- sapajus_apella_distance_year_hn1$`2017`
sapajus_apella_2018_distance_hn <- sapajus_apella_distance_year_hn3
sapajus_apella_2019_distance_hn <- sapajus_apella_distance_year_hn1$`2019`
sapajus_apella_2020_distance_hn <- sapajus_apella_distance_year_hn2$`2020`
sapajus_apella_2021_distance_hn <- sapajus_apella_distance_year_hn1$`2021`

# Tabela com o resumo comparativo dos modelos -----------------------------

modelo_sapajus_apella_estrat_year <- comparar_aic_modelo_estratificado(
  sapajus_apella_distance_year_hn_global,
  sapajus_apella_2015_distance_hn,
  sapajus_apella_2014_distance_hn,
  sapajus_apella_2016_distance_hn,
  sapajus_apella_2017_distance_hn,
  sapajus_apella_2018_distance_hn, 
  sapajus_apella_2019_distance_hn, 
  sapajus_apella_2020_distance_hn, 
  sapajus_apella_2021_distance_hn, 
  nome_modelos = c(
    "Global",
    "2014",
    "2015",
    "2016",
    "2017",
    "2018",
    "2019",
    "2020",
    "2021"
  )
)

modelo_sapajus_apella_estrat_year

# Gráficos de ajuste das funções de deteção às probabilidades de detecção --------

modelos_sapajus_apella_estrat <- gerar_lista_modelos_selecionados(
  sapajus_apella_distance_year_hn_global,
  sapajus_apella_2015_distance_hn,
  sapajus_apella_2014_distance_hn,
  sapajus_apella_2016_distance_hn,
  sapajus_apella_2017_distance_hn,
  sapajus_apella_2018_distance_hn, 
  sapajus_apella_2019_distance_hn, 
  sapajus_apella_2020_distance_hn, 
  sapajus_apella_2021_distance_hn,
  nome_modelos_selecionados = modelo_sapajus_apella_estrat_year
)

plotar_funcao_deteccao_modelos_selecionados(modelos_sapajus_apella_estrat)

# rm(list = ls())