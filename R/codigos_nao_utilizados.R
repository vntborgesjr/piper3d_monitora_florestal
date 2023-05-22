
# codigos nao utilizados --------------------------------------------------

# rotina para rodar modelos distance com truncamento ----------------------
# define a largura das caixas, binagem
corte_cutias <- create_bins(
  cutia_tap_arap[!is.na(cutia_tap_arap$distance),], 
  c(0, 2, 4, 6, 8, 10, 15)
  )

# selecao de distancia de truncagem e a mesma usada nos dado com repeticao
# ajsute dos modelos
nome_do_ajuiste_unif <- corte_cutias |> 
  ajuste_modelos_distance_unif(truncamento = 15)

nome_do_ajuiste_hn <- corte_cutias |> 
  ajuste_modelos_distance_hn(truncamento = 15)

nome_do_ajuiste_hr <- corte_cutias |> 
  ajuste_modelos_distance_hr(truncamento = 15)

# plotar histograma das funcao da probabiolidade de deteccao
modelos_selecionados <- list(
  cutia_tap_arap_hr$`Sem termo`,
  cutia_tap_arap_unif$Cosseno,
  cutia_tap_arap_hn$Cosseno,
  cutia_tap_arap_unif$`Polinomial simples`,
  cutia_tap_arap_hn$`Sem termo`
) # allterar para os modelos selecionados

plotar_funcao_deteccao_modelos_selecionados(modelos_selecionados)

# funcoes nao utlizadas ---------------------------------------------------
# Documentacaoda funcao gerar_caracteristicas_area_estudo_taxa_encontro() --------
#' Title
#'
#' @param dados 
#'
#' @return
#' @export
#'
#' @examples
gerar_caracteristicas_area_estudo_taxa_encontro <- function(dados) {
  # área de estudo, tamanho da área de estudo, area coberta pelo esforço
  # amostral, esforço amostral em metros, número de detecções, número de
  # transectos (ea), taxa de encontro, coeficiente de variação da taxa
  # de encontro  
  caracteristicas_area_estudo_taxa_encontro <- dados$dht$individuals$summary[1:9]
  
  # retornar área de estudo, tamanho da área de estudo, area coberta pelo esforço
  # amostral, esforço amostral em metros, número de detecções, número de
  # transectos (ea), taxa de encontro, coeficiente de variação da taxa
  # de encontro  
  return(caracteristicas_area_estudo_taxa_encontro)
}

# Documentacao da funcao gerar_caracteristicas_densidade() -------------------------------------------------
#' Title
#'
#' @param dados 
#'
#' @return
#' @export
#'
#' @examples
gerar_caracteristicas_densidade <- function(dados) {
  # total, densidade estimada, erro padrão da densidade estimada, 
  # coeficiente de variação da densidade destimada, intervalo de
  # confiança inferior e superior do coeficiente de variação, 
  # gruas de liberdade
  caracteristicas_densidade <- dados$dht$individuals$D
  
  # área de estudo, tamanho da área de estudo, trilhas ou estações
  # amostrais, esforço total em cada trilha, abundância estimada em cada
  # estação amostral, número de detecções em cada estação amostral, 
  # área total amostrada
  return(caracteristicas_densidade)
}

# Documantacao da funcao gerar_caracteristicas_esforco_abundancia_deteccao()  -------------------------------------------------
#' Title
#'
#' @param dados 
#'
#' @return
#' @export
#'
#' @examples
gerar_caracteristicas_esforco_abundancia_deteccao <- function(dados) {
  # área de estudo, tamanho da área de estudo, trilhas ou estações
  # amostrais, esforço total em cada trilha, abundância estimada em cada
  # estação amostral, número de detecções em cada estação amostral, 
  # área total amostrada
  caracteristicas_esforco_abundancia_deteccao <- dados$dht$individuals$Nhat.by.sample[1:8]
  
  # área de estudo, tamanho da área de estudo, trilhas ou estações
  # amostrais, esforço total em cada trilha, abundância estimada em cada
  # estação amostral, número de detecções em cada estação amostral, 
  # área total amostrada
  return(caracteristicas_esforco_abundancia_deteccao)
}
