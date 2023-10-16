# Documentacao da funcao transformar_dados_formato_Distance() --------------------
#' Transforma os dados para o formato para análise no pacote `Distance`
#'
#' @description
#' A função `transformar_dados_formato_Distance()` transforma a base de dados para o formato de análise do pacote `Distance` podendo retornar a base de dados com ou sem amostras repetidas na estações amostrais.
#'
#' @param dados recebe a \code{tibble} gerada pela função [filtrar_dados()]. Por configuraçãoo, carrega a base de dados burtos de aves e médios e grandes mamíferos do Pojeto Monitora Componente Florestal `monitora_aves_masto_florestal`.
#' @param amostras_repetidas lógico, controla a saída dos dados transformados. Por configuração, retorna os dados com amostragens repetidas em cada estação amostral.
#'
#'  @details
#'  Quando o agrumento `amostras_repetidas = FALSE` será selecionado o dia em que foi registrado o maior número de observações em uma estação amostral.
#'
#' @return Retorna um objeto do tipo `tibble` contendo a base de dados configurada para ajustar os modelos do pacote `Distance` a partir da função [ajustar_modelo_distance().
#' @export
#'
#' @examples
#' # carregar pacote
#' library(dplyr)
#'
#' # gerar os dados filtrados
#' dados_filtrados <- filtrar_dados(
#'   nome_uc = "resex_tapajos_arapiuns",
#'   nome_sp = "dasyprocta_croconota",
#'   validacao_obs = "especie"
#' )
#'
#' # gerar os dados transformados com repeticao
#' dados_distance_com_repeticao <- transformar_dados_formato_Distance(dados = dados_filtrados)
#'
#' glimpse(dados_distance_com_repeticao)
#'
#' # gerar os dados transformados com repeticao
#' dados_distance_sem_repeticao <- transformar_dados_formato_Distance(
#'   dados = dados_filtrados,
#'   amostras_repetidas = FALSE
#' )
#'
#' glimpse(dados_distance_sem_repeticao)
transformar_dados_formato_Distance <- function(
    dados = monitora_aves_masto_florestal,
    ...,
    amostras_repetidas = TRUE
) {
  
  # tranformar dados mantendo as amostras repetidas
  dados_formato_distance <- dados |>
    dplyr::select(
      Region.Label = nome_uc,
      Sample.Label = nome_ea,
      Effort_day = esforco_dia,
      sampling_day = data_amostragem,
      distance = distancia,
      season = estacao,
      year = ano,
      size = tamanho_grupo,
      cense_time = tempo_censo,
      speed = velocidade_km_h
    ) |>
    dplyr::mutate(
      Area = 0,
      object = 1:nrow(dados)
    ) |>
    dplyr::relocate(
      Area,
      .before = Sample.Label
    )
  
  # calculo do esforco amostral total
  n_repeated_visits <- dados_formato_distance |> 
    # conta o numero de vezes que uma ea foi amostrada
    dplyr::count(
      ...,
      name = "repeated_visits"
    )
  
  dados_formato_distance <- dados_formato_distance |>
    dplyr::left_join(
      n_repeated_visits,
      dplyr::join_by(...),
      relationship = "many-to-many"
    ) |> 
    # gera a distancia total percorrida em cada ea
    dplyr::mutate(
      Effort = Effort_day * repeated_visits,
      .before = sampling_day
    ) |>
    # reposiciona as colunas
    dplyr::relocate(
      repeated_visits,
      .after = Sample.Label
    ) 

  # controlar a saída da amostragem
  if (amostras_repetidas == TRUE) {

    # retorna os dados mantendo as amostras repetidas
    return(dados_formato_distance)

  } else {

    # gerar filtro para eliminar amostras repetidas mantendo o dia com o maior n de obs
    # gerar o n de obs por data de amostragem numa 
    # mesma estação e ano
    n_obs_data <- dados_formato_distance |>
      dplyr::group_by(
        ...,
        sampling_day
      ) |>
      dplyr::count(sampling_day) |>
      dplyr::ungroup() 
    
    # gerar as datas com maior n de obs de acordo com
    # a estratificação especificada
    data_com_maior_n_obs <- n_obs_data |>
      dplyr::group_by(
        ...
      ) |>
      dplyr::filter(n == max(n)) |> 
      dplyr::ungroup() 
    
    # juntar as duas data.frames para obter as datas com maior n de obs
    # em cada ano e excluir datas de amostragem repitidas na mesma estacao
    # e ano
    dados_para_filtrar_por_data_sem_repeticao <- n_obs_data |>
      dplyr::semi_join(
        data_com_maior_n_obs,
        dplyr::join_by(
          ...,
          sampling_day,
          n),
      ) |>
      dplyr::distinct(
        ...,
        sampling_day
      ) 
    
    # gerar o filtro de datas repetindo dias com 
    # datas com mesmo número máximo de indivíduos
    # observados
    filtro_datas_sem_repeticao1 <- dados_para_filtrar_por_data_sem_repeticao$sampling_day
    
    # eliminar amostras repetidas a partir do dia com
    # maior distância perpendicular média
    dados_para_filtrar_por_data_sem_repeticao_media <- suppressWarnings(
      dados_formato_distance |>
        dplyr::filter(
          sampling_day %in% filtro_datas_sem_repeticao1
        ) |> 
        dplyr::group_by(
          ...,
          sampling_day
        ) |> 
        dplyr::summarise(
          distance_mean = mean(
            distance,
            na.rm = TRUE
          )
        ) |> 
        dplyr::filter(
          distance_mean == max(
            distance_mean,
            na.rm = TRUE
          )
        ) |> 
        dplyr::ungroup()
    )
    
    # gerar o filtro de datas não repetidas com data
    # com número máximo de indivíduos observados
    filtro_datas_sem_repeticao2 <- dados_para_filtrar_por_data_sem_repeticao_media$sampling_day
    
    # gerar coluna object
    dados_formato_distance <- dados_formato_distance |>
      dplyr::filter(
        sampling_day %in% filtro_datas_sem_repeticao2,
        !is.na(distance)
      ) |>
      dplyr::mutate(
        object = seq_along(sampling_day)
      ) 
    
    # retorna os dados mantendo as amostras repetidas
    return(dados_formato_distance)
    
  }
  
}

utils::globalVariables(
  c(
    "monitora_aves_masto_florestal",
    "dados",
    "amostras_repetidas",
    "dados_formato_distance_com_repeticao",
    "Region.Label",
    "Sample.Label",
    "nome_ea",
    "Effort",
    "esforco_total",
    "esforco_dia",
    "sampling_day",
    "data_amostragem",
    "distance",
    "distancia",
    "season",
    "year",
    "size",
    "tamanho_grupo",
    "cense_time",
    "tempo_censo",
    "Area",
    "object",
    "dados_formato_distance",
    "n_obs_data",
    "data_com_maior_n_obs",
    "dados_para_filtrar_por_data_sem_repeticao",
    "n",
    "n_max",
    "day",
    "month",
    "filtro_datas_sem_repeticao",
    "speed",
    "velocidade_km_h"
  )
)
