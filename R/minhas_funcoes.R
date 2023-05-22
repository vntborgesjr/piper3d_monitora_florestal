
# Carregar pacotes necessarios --------------------------------------------
library(Distance)
library(dplyr)
library(DT)
library(forcats)
library(ggpubr)
library(ggplot2)
library(here)
library(lubridate)
library(mrds)
library(plotly)
library(purrr)
library(readr)
library(readxl)
library(stringi)
library(stringr)
library(tidyr)
library(tidyselect)


# Documentacao da funcao ajuste_modelo_distace_hn() -----------------------
#' Title
#'
#' @param dados 
#' @param lista_termos_ajuste 
#' @param truncamento 
#'
#' @return
#' @export
#'
#' @examples
ajuste_modelos_distance_hn <- function(
    dados,
    lista_termos_ajuste = list(
      `Sem termo` = NULL, 
      Cosseno = "cos", 
      `Hermite polinomial` = "herm"
    ),
    truncamento = NULL,
    formula = ~ 1
) {
  # ajustando a função de detecção
  # Key function - Half-normal 
  modelos_ajustados <- purrr::map(
    lista_termos_ajuste, 
    \(.x) Distance::ds(
      data = dados,
      truncation = truncamento,
      formula = formula,
      key = "hn",
      adjustment = .x
    )
  )
  
  # retorna o output dos modelos
  return(modelos_ajustados)
  
}

# Documentacao da funcao ajuste_modelos_distance_hr() ----------------------
#' Title
#'
#' @param dados 
#' @param lista_termos_ajuste 
#' @param truncamento 
#'
#' @return
#' @export
#'
#' @examples
ajuste_modelos_distance_hr <- function(
    dados,
    lista_termos_ajuste = list(
      `Sem termo` = NULL, 
      Cosseno = "cos", 
      `Polinomial simples` = "poly" 
    ),
    truncamento = NULL,
    formula = ~ 1
) {
  # ajustando a função de detecção
  # Key function - Hazard-rate
  modelos_ajustados <- purrr::map(
    lista_termos_ajuste, 
    \(.x) Distance::ds(
      data = dados,
      truncation = truncamento,
      formula = formula,
      key = "hr",
      adjustment = .x
    )
  )
  
  # retorna o output dos modelos
  return(modelos_ajustados)
  
}

# Documentacao da funcao ajuste_modelos_distance_unif() -------------------
#' Title
#'
#' @param dados 
#' @param lista_termos_ajuste 
#' @param truncamento 
#'
#' @return
#' @export
#'
#' @examples
ajuste_modelos_distance_unif <- function(
    dados,
    lista_termos_ajuste = list(
      #`Sem termo` = NULL, 
      Cosseno = "cos", 
      `Polinomial simples` = "poly" 
    ),
    truncamento = NULL,
    formula = ~ 1
) {
  # ajustando a função de detecção
  # Key function - Uniforme
  modelos_ajustados <- purrr::map(
    lista_termos_ajuste, 
    \(.x) Distance::ds(
      data = dados,
      truncation = truncamento,
      formula = formula,
      key = "unif",
      adjustment = .x
    )
  )
  
  # retorna o output dos modelos
  return(modelos_ajustados)
  
}
# Documentacao da funcao carregar_dados_brutos.xlsx -----------------------
#' Carrega os dados brutos originais em formato .xlsx
#'
#' @description
#' A funcao \code{carregar_dados_brutos_xlsx()} carrega o arquivo da planilha de dados originais, \code{"monitora_masto_aves_2023_04_04.xlsx"} e gera um novo arquivo \code{dados_brutos.rds}.
#'
#' @param dados recebe o caminho para carregar um arquivo .xlsx. Por configuracao, carrega os dados brutos dos Projeto Monitora Componente Florestal \code{"monitora_masto_aves_2023_04_04.xlsx"}. A versao dos dados brutos corresponde ao arquivo "Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP.xlsx"
# enviada por whatsapp no dia 08/03/2023
#'
#' @details
#' A funcao \code{carregar_dados_brutos_xlsx()} carrega e disponibiliza ao usario os dados brutos do Projeto Monitora Componente Florestal, contendo  as infromacoes sobre as amostragens por distancia (\emph{distance samplig}) de aves e pequenos e medios mamiferos realizadas desde 2014 em 40 Unidades de Conservacao do Brasil. O novo arquivo gerado fica disponivel para carregamento a partir do uso da funcao \code{carregar_dados_brutos_rds()}.
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo os dados brutos do Projeto Monitora Componente Florestal a partir do formato .xlsx.
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#' @export
#'
#' @examples \dontrun{carregar_dados_brutos_xlsx()}
carregar_dados_brutos_xlsx <- function(
    dados = readxl::read_excel(
      path = paste0(
        here::here(),
        "/data-raw/monitora_masto_aves_2023_04_04.xlsx"
      ),
      sheet = "dados brutos"
    )
) {
  # grava uma versao dados_brutos.rds no diretorio inst/extdata
  # readr::write_rds(
  #   dados,
  #   file = paste0(
  #     here::here(),
  #     "/data-raw/dados_brutos.rds"
  #   )
  # )

  # retorna os dados butos
  return(dados)
}

# Documentacao da funcao carregar_dados_brutos_rds() ----------------------
#' Carrega os dados brutos originais em formato .rds
#'
#' @description
#' A funcao \code{carregar_dados_brutos_rds()} carrega os dados brutos em formato .rds.
#'
#'
#' @param dados recebe o caminho para carregar um arquivo .rds. Por configuracao, carrega os dados brutos dos Projeto Monitora Componente Florestal a partir do formato .rds
#' @details
#' A funcao \code{carregar_dados_brutos_rds()} carrega e disponibiliza ao usario os dados brutos do Projeto Monitora Componente Florestal, contendo  as infromacoes sobre as amostragens por distancia (\emph{distance samplig}) de aves e pequenos e medios mamiferos realizadas desde 2014 em 40 Unidades de Conservacao do Brasil.
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo os dados brutos do Projeto Monitora Componente Florestal.
#' @export
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#' @examples \dontrun{carregar_dados_brutos_rds()}
carregar_dados_brutos_rds <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data-raw/dados_brutos.rds"
      )
    )
) {
  # retorna os dados butos
  return(dados)
}

# Documentacao da funcao carregar_dados_completos() ----------------------
#' Carrega os dados brutos selecionando, trasformando e renomeando as colunas
#'
#' @description
#' A funcao \code{carregar_dados_completos()} carrega os dados brutos em formato .rds, selecionando, transformando e renomeando suas colunas.
#'
#'
#' @param dados recebe o caminho para carregar um arquivo .rds. Por configuracao, carrega os dados brutos dos Projeto Monitora Componente Florestal a partir do formato .rds
#' @details
#' A funcao \code{carregar_dados_completos()} disponibiliza ao usario os dados brutos do Projeto Monitora Componente Florestal, selecionando, transformando e renomeando suas colunas. (Fornecer mais detalhes sobre as transformacoes operadas).
#' Avisar que distancias de trilhas percorridas estao sendo imputadas quando os valores estao ausentes.
#' 
#' @return Retorna um objeto do tipo \code{tibble} contendo uma selecao de colunas transformadas e renomeadas a partir dos dados brutos do Projeto Monitora Componente Florestal.
#' @export
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#' @examples \dontrun{carregar_dados_completos()}
carregar_dados_completos <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        '/data-raw/dados_brutos.rds'
      )
    )
) {
  # padronizar separadores
  # gerar o data.frame desejado
  dados_completos <- dados |>
    dplyr::select(
      uc_code = CDUC,
      uc_name = `Local - Nome da Unidade de Conservacao`,
      ea_number = `Numero da Estacao Amostral`,
      ea_name = `Nome da EA`,
      season = `Estacao do ano`,
      year = Ano,
      sampling_day = `data da amostragem`,
      day_effort = `Esforco de amostragem tamanho da trilha (m)`,
      class = Classe,
      order = Ordem,
      family = Familia,
      genus = Genero,
      sp_name = `Especies validadas para analise do ICMBio`,
      validation = `Clasificacao taxonomica validada`,
      distance = `distancia (m)     do animal em relacao a trilha`,
      group_size = `n de animais`,
      observadores = `nome dos observadores`,
      cense_started_at = `horario de inicio  (h:mm)`,
      cense_stoped_at = `horario de termino (h:mm)`
      #cense_time = `Tempo de censo`
    ) |>
    dplyr::mutate(
      uc_category = stringi::stri_extract_first_words(
        uc_name
      ),
      # abrevia o nome das UCs
      uc_name_abv = forcats::lvls_revalue(
        uc_name,
        new_levels = c(
          "ETM", "EM", "EN", "ESGT", "FJ", "PCV", "PA", "PSBoc", "PSBod", "PSC",
          "PSM", "PSC", "PSD", "PSP", "PSO", "PPN", "PCO", "PI", "PJau", "PJur",
          "PMR", "PS", "PV", "PCA", "PMT", "RG", "RJ", "RTap", "RU", "RG",
          "RTrom", "RAT", "RBA", "RCI", "RCM", "RRC", "RROP", "RIA", "RRA", "RTA"
        )
      ),
      sp_name_abv = stringr::str_split(sp_name, " ") |>
        purrr::map(\(string) stringr::str_sub(string, 1, 4)) |>
        purrr::map(\(string) stringr::str_c(string, ".")) |>
        purrr::map(\(string) stringr::str_flatten(string, " ")) |>
        purrr::list_c(),
      validation = forcats::fct_recode(
        validation,
        "Especie" = "E",
        "Especie" = "e",
        "Familia" = "F",
        "Genero" = "G",
        "Genero" = "g",
        "Ordem" = "O"
      ),
      dplyr::across(
        tidyselect::where(is.character),
        as.factor
      ),
      cense_time = cense_stoped_at - cense_started_at,
      novo = stringr::str_replace_all(observadores,
                                      " e ",
                                      ", "),
      novo = stringr::str_replace_all(novo,
                                      " E ",
                                      ", "),
      novo = stringr::str_replace_all(novo,
                                      "/",
                                      ", "),
      novo = stringr::str_replace_all(novo,
                                      ";",
                                      ", "),
      novo = stringr::str_replace_all(novo,
                                      " a ",
                                      ", ")
    ) |>
    tidyr::separate_wider_delim(
      novo,
      ",",
      names = c("obs1", "obs2", "obs3", "obs4", "obs5", "obs6"),
      too_few = "align_start"
    ) |>
    dplyr::mutate(
      obs1 = ifelse(!is.na(obs1), 1, 0),
      obs2 = ifelse(!is.na(obs2), 1, 0),
      obs3 = ifelse(!is.na(obs3), 1, 0),
      obs4 = ifelse(!is.na(obs4), 1, 0),
      obs5 = ifelse(!is.na(obs5), 1, 0),
      obs6 = ifelse(!is.na(obs6), 1, 0),
      number_observers = obs1 + obs2 + obs3 + obs4 + obs5 + obs6
    ) |>
    dplyr::group_by(
      ea_name,
      sampling_day
    ) |>
    tidyr::nest() |>
    dplyr::mutate(
      day_effort2 = purrr::map(
        data,
        \(.x) rep(.x$day_effort[!is.na(.x$day_effort)][1])
      ),

      cense_time2 = purrr::map(
        data,
        \(.x) rep(.x$cense_time[!is.na(.x$cense_time)][1])
      ),
    ) |>
    tidyr::unnest(
      c(data, day_effort2, cense_time2)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      tidyselect::starts_with(
        c("uc", "ea")
      ),
      season,
      year,
      sampling_day,
      day_effort = day_effort2,
      sp_name:number_observers,
      cense_time = cense_time2,
      -cense_time,
      -day_effort,
      -tidyselect::starts_with("obs"),
      -tidyselect::ends_with("at")
    ) |>
    dplyr::relocate(
      uc_category,
      .before = uc_name
    ) |>
    dplyr::relocate(
      uc_name_abv,
      .after = uc_name
    ) |>
    dplyr::relocate(
      sp_name_abv,
      .after = sp_name
    ) |> 
    tidyr::fill(day_effort)
  
  # calculo do esforco
  dados_completos <- dados_completos |> 
    dplyr::distinct(
      ea_name,
      sampling_day
    ) |> 
    dplyr::count(
      ea_name, 
      name = "n_repeats"
    ) |> 
    dplyr::left_join(
      dados_completos,
      by = "ea_name"
    ) |> 
    dplyr::mutate(total_effort = day_effort*n_repeats) |> 
    dplyr::relocate(
      total_effort,
      .after = day_effort
    ) |> 
    group_by(
      uc_name,
      ea_name
    ) |> 
    filter(day_effort == max(day_effort))  |> 
    ungroup()
  
  # grava uma versao dados_completos.rds no diretorio inst/extdata
  # readr::write_rds(
  #   dados_completos,
  #   file = paste0(
  #     here::here(),
  #     "/data/dados_completos.rds"
  #   )
  # )
  
  # retornar o data.frame
  return(dados_completos)
}

# Documentacao funcao carregar_dados_filtrados() --------------------------

#' Carrega os dados filtrados a partir da UC's e da especie
#'
#' @description
#' A funcao \code{carregar_dados_filtrados()} carrega o arquivo de dados completos, \code{"dados_completos.rds"} e gera um novo arquivo \code{dados_filtrados.rds}.
#'
#' @param dados recebe o caminho para carregar um arquivo .rds. Por configuracao, carrega os dados filtrados pela Resex Tapajos-Arapiuns e especie \emph{Dsyprocta croconota} do Projeto Monitora Componente Florestal a partir doa arquivo \code{"dados_completos.rds"}.
#' @param nome_uc description
#' @param nome_sp descricao
#'
#' @details
#' A funcao \code{carregar_dados_filtrados()} carrega e disponibiliza ao usario os dados filtrados do Projeto Monitora Componente Florestal, contendo ...
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo os dados filtrados do Projeto Monitora Componente Florestal.
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#' @export
#'
#' @examples \dontrun{carregar_dados_filtrados()}
carregar_dados_filtrados <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_completos.rds"
      )
  ),
  nome_uc = "Resex Tapajos-Arapiuns",
  nome_sp = "Dasyprocta croconota"
) {
  # gerar o data.frame desejado
  dados_filtrados <- dados |>
    dplyr::filter(
      uc_name == nome_uc,
      sp_name == nome_sp
    )

  # grava uma versao dados_filtrados.rds no diretorio inst/extdata
  # readr::write_rds(
  #   dados_filtrados,
  #   file = paste0(
  #     here::here(),
  #     "/data/dados_filtrados.rds"
  #   )
  # )

  # retornar o data.frame
  return(dados_filtrados)
}

# Documentacao da funcao carregar_dados_selecionados() --------------------

#' Carrega os dados selecionados a partir das observacoes validadas ao nivel de especie
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{carregar_dados_selecionados()}
carregar_dados_selecionados <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_completos.rds"
      )
    )
) {
  # gerar o data.frame desejado
  dados_selecionados <- dados |>
    dplyr::filter(validation == "Especie")

  # grava uma versao dados_completos.rds no diretorio inst/extdata
  # readr::write_rds(
  #   dados_selecionados,
  #   file = paste0(
  #     here::here(),
  #     "/data/dados_selecionados.rds"
  #   )
  # )

  # retornar o data.frame
  return(dados_selecionados)
}

# Documentacao da funcao contar_n_ano_uc() --------------------

#' Gera uma tabela com o numero total de anos em que cada UC foi amostrada
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{contar_n_ano_uc()}
contar_n_ano_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    )
) {
  # gera o numero de anos em que cada UC foi amostrada
  n_ano_uc <- dados |>
    dplyr::count(
      year,
      uc_name,
      uc_name_abv
    ) |>
    dplyr::group_by(
      uc_name,
      uc_name_abv
    ) |>
    dplyr::count(
      year
    ) |>
    dplyr::summarise(
      n_anos = sum(n)
    )

  # grava a tabela n_ano_uc.rds no diretorio inst/extdata/
  # readr::write_rds(
  #   n_ano_uc,
  #   file = paste0(
  #     here::here(),
  #     "/data/n_ano_uc.rds"
  #   )
  # )

  # retorna o numero de UCs por ano
  return(n_ano_uc)
}

# Documentacao da funcao contar_n_obs_sp_ano() --------------------
#' Gera uma tabela com o numero total de observacoes por UC em cada ano
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{contar_n_obs_sp_ano()}
contar_n_obs_sp_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    )
) {
  # gerar a tabela com o numero de observacoes por UC em cada ano
  n_obs_sp_ano <- dados  |>
    dplyr::count(
      year,
      sp_name,
      sp_name_abv
    )

  # grava a tabela n_obs_uc.rds no diretorio inst/extdata/
  # readr::write_rds(
  #   n_obs_sp_ano,
  #   file = paste0(
  #     here::here(),
  #     "/data/n_obs_sp_ano.rds"
  #   )
  # )

  # retorna a tabela com o numero de observacoes por UC
  return(n_obs_sp_ano)
}

# Documentacao da funcao contar_n_obs_sp_uc_ano() --------------------
#' Gera uma tabela com o numero total de observacoes por especie e por UC em cada ano
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{contar_n_obs_sp_uc_ano()}
contar_n_obs_sp_uc_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    )
) {
  # gera o numero de observacoes por especie, por Uc e por ano
  n_obs_sp_uc_ano <- dados |>
    dplyr::count(
      uc_name,
      uc_name_abv,
      sp_name,
      sp_name_abv,
      year
    )

  # grava a tabela n_obs_sp_uc.rds no diretorio inst/extdata/
  # readr::write_rds(
  #   n_obs_sp_uc_ano,
  #   file = paste0(
  #     here::here(),
  #     "/data/n_obs_sp_uc_ano.rds"
  #   )
  # )

  # retorna o numero de observacoes por especie, por Uc e por ano
  return(n_obs_sp_uc_ano)
}

# Documentacao da funcao contar_n_obs_sp_uc_estacao_ano() --------------------
#' Gera uma tabela com o numero total de observacoes por especie, por UC e por estacao em cada ano
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{contar_n_obs_sp_uc_estacao_ano()}
contar_n_obs_sp_uc_estacao_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    )
) {
  # gera o numero de observacoes por especie, por Uc, por estacao e por ano
  n_obs_sp_uc_estacao_ano <- dados |>
    dplyr::count(
      uc_name,
      uc_name_abv,
      sp_name,
      sp_name_abv,
      season,
      year
    )

  # grava a tabela n_obs_uc_sp_estacao_ano.rds no diretorio inst/extdata/
  # readr::write_rds(
  #   n_obs_sp_uc_estacao_ano,
  #   file = paste0(
  #     here::here(),
  #     "/data/n_obs_sp_uc_estacao_ano.rds"
  #   )
  # )

  # retorna o numero de observacoes por especie, por Uc, por estacao e por ano
  return(n_obs_sp_uc_estacao_ano)
}

# Documentacao da funcao contar_n_obs_sp_uc() --------------------
#' Gera uma tabela com o numero total de observacoes por especie e por UC
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{contar_n_obs_sp_uc()}
contar_n_obs_sp_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    )
) {
  # gera o numero de observacoes por especie
  n_obs_sp_uc <- dados |>
    dplyr::count(
      uc_name,
      uc_name_abv,
      sp_name,
      sp_name_abv
    )

  # grava a tabela n_obs_sp_uc.rds no diretorio inst/extdata/
  # readr::write_rds(
  #   n_obs_sp_uc,
  #   file = paste0(
  #     here::here(),
  #     "/data/n_obs_sp_uc.rds"
  #   )
  # )

  # retorna o numero de observacoes por especie
  return(n_obs_sp_uc)
}

# Documentacao da funcao contar_n_obs_sp() --------------------
#' Gera uma tabela com o numero total de observacoes por especie
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{contar_n_obs_sp()}
contar_n_obs_sp <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_completos.rds"
      )
    )
) {
  #
  n_obs_sp <- dados |>
    dplyr::count(
      sp_name,
      sp_name_abv
    )

  # grava a tabela n_uc_ano.rds no diretorio inst/extdata/
  # readr::write_rds(
  #   n_obs_sp,
  #   file = paste0(
  #     here::here(),
  #     "/data/n_obs_sp.rds"
  #   )
  # )

  return(n_obs_sp)
}

# Documentacao da funcao contar_n_obs_uc_ano() --------------------
#' Gera uma tabela com o numero total de observacoes por UC em cada ano
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{contar_n_obs_uc_ano()}
contar_n_obs_uc_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    )
) {
  # gerar a tabela com o numero de observacoes por UC em cada ano
  n_obs_uc_ano <- dados  |>
    dplyr::count(
      uc_name,
      uc_name_abv,
      year
    )

  # grava a tabela n_obs_uc.rds no diretorio inst/extdata/
  # readr::write_rds(
  #   n_obs_uc_ano,
  #   file = paste0(
  #     here::here(),
  #     "/data/n_obs_uc_ano.rds"
  #   )
  # )

  # retorna a tabela com o numero de observacoes por UC
  return(n_obs_uc_ano)
}

# Documentacao da funcao contar_n_obs_uc() --------------------
#' Gera uma tabela com o numero total de observacoes por UC
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{contar_n_obs_uc()}
contar_n_obs_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    )
) {
  # gerar a tabela com o numero de observacoes por UC
  n_obs_uc <- dados  |>
    dplyr::count(
      uc_name,
      uc_name_abv
    )

  # grava a tabela n_obs_uc.rds no diretorio inst/extdata/
  # readr::write_rds(
  #   n_obs_uc,
  #   file = paste0(
  #     here::here(),
  #     "/data/n_obs_uc.rds"
  #   )
  # )

  # retorna a tabela com o numero de observacoes por UC
  return(n_obs_uc)
}

# Documentacao da funcao contar_n_obs_validadas() --------------------
#' Gera uma tabela com o numero total de observacoes validadas por nivel taxonomico
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{contar_n_obs_validadas()}
contar_n_obs_validadas <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_completos.rds"
      )
    )
) {
  # gerar tabela com o numero de observacoes validadas para cada nivel
  # taxonomico
  tabela_n_obs_validadas <- dados |>
    dplyr::count(validation)

  # gravar tabela_n_obs_validadas.rds no diretorio inst/extdata
  readr::write_rds(
    tabela_n_obs_validadas,
    file = paste0(
      here::here(),
      "/data/tabela_n_obs_validadas.rds"
    )
  )

  # extrair os numeros de observacoes validadas
  n_obs_validadas <- tabela_n_obs_validadas |>
    dplyr::pull(var = n)

  # retornar os numeros de observacoes validadas
  return(n_obs_validadas)
}

# Documentacao da funcao contar_n_sp() --------------------
#' Fornece o numero total de especies validadas na base de dados selecionados
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{contar_n_sp()}
contar_n_sp <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    )
) {
  #
  n_sp <- dados |>
    dplyr::distinct(sp_name) |>
    nrow()

  return(n_sp)
}

# Documentacao da funcao contar_n_uc_ano() --------------------
#' Gera uma tabela com o numero de UC's amostradas por ano
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{contar_n_uc_ano()}
contar_n_uc_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    )
) {
  # gera o numero de observacoes por especie
  n_uc_ano <- dados |>
    dplyr::count(
      year,
      uc_name
    ) |>
    dplyr::group_by(
      year
    ) |>
    dplyr::count(
      uc_name
    ) |>
    dplyr::summarise(
      n_ucs = sum(n)
    )

  # grava a tabela n_uc_ano.rds no diretorio inst/extdata/
  # readr::write_rds(
  #   n_uc_ano,
  #   file = paste0(
  #     here::here(),
  #     "/data/n_uc_ano.rds"
  #   )
  # )

  # retorna uma tabela com o numero de UCs por ano
  return(n_uc_ano)
}

# Documentacao da funcao contar_n_uc() --------------------
#' Gera uma tabela com o numero de UC's amostradas por ano
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{contar_n_uc()}
contar_n_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    )
) {
  #
  n_ucs <- dados |>
    dplyr::distinct(uc_name) |>
    nrow()

  # retornos n numero total de Uc's
  return(n_ucs)
}


# Documentacao da funcao gerar_caracteristicas_area_estudo_taxa_encontro() --------
#' Title
#'
#' @param dados 
#' @param resultado_selecao_modelos 
#'
#' @return
#' @export
#'
#' @examples
gerar_caracteristicas_area_estudo_taxa_encontro <- function(
    dados,
    resultado_selecao_modelos
) {
  # arae total, densidade estimada, erro padrão da densidade estimada, 
  # coeficiente de variação da densidade destimada, intervalo de
  # confiança inferior e superior do coeficiente de variação, 
  # gruas de liberdade
  caracteristicas_area_estudo_taxa_encontro <- dados |> 
    purrr::map(
      \(.x) .x$dht$individuals$summary[1:9]
    ) |> 
    list_rbind() |> 
    mutate(Modelo = resultado_selecao_modelos$Model) |> # pode ser um argumento da função
    relocate(Modelo, .before = Region) |> 
    rename(
      Regiao = Region,
      `Area coberta` = CoveredArea,
      Esforco = Effort,
      `Taxa de encontro` = ER,
      `ep da Taxa de encontro` = se.ER,
      `cv. da Taxa de encontro` = cv.ER
    )
  
  # área de estudo, tamanho da área de estudo, trilhas ou estações
  # amostrais, esforço total em cada trilha, abundância estimada em cada
  # estação amostral, número de detecções em cada estação amostral, 
  # área total amostrada
  return(caracteristicas_area_estudo_taxa_encontro)
}

# Documentacao da funcao gerar_caracteristicas_densidade() --------
#' Title
#'
#' @param dados 
#' @param resultado_selecao_modelos 
#'
#' @return
#' @export
#'
#' @examples
gerar_caracteristicas_densidade <- function(
    dados,
    resultado_selecao_modelos
) {
  # área de estudo, tamanho da área de estudo, trilhas ou estações
  # amostrais, esforço total em cada trilha, abundância estimada em cada
  # estação amostral, número de detecções em cada estação amostral, 
  # área total amostrada
  caracteristicas_densidade <- dados |> 
    purrr::map(
      \(.x) .x$dht$individuals$Nhat.by.sample[1:8]
    ) |> 
    list_rbind() |> 
    select(!c(Label, CoveredArea)) |> 
    rename(
      Regiao = Region.Label,
      `Estacao amostral` = Sample.Label,
      Esforco = Effort.x,
      `Abundancia estimada` = Nhat,
      `N de deteccoes` = n
    )
  
  caracteristicas_densidade <- caracteristicas_densidade |>
    mutate(Modelo = rep(resultado_selecao_modelos$Model, each = length(unique(caracteristicas_densidade$`Estacao amostral`)))) |> # pode ser um argumento da função
    relocate(Modelo, .before = Regiao)
  
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

# Documentacao da funcao gerar_tabdin_dados_brutos() --------------------
#' Gera uma tabela dinamica dos dados brutos
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#' @param n_linhas Descricao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_dados_brutos()}
gerar_tabdin_dados_brutos <- function(
    dados = readxl::read_excel(
      path = paste0(
        here::here(),
        "/data-raw/monitora_masto_aves_2023_04_04.xlsx"
      ),
      sheet = "dados brutos"
    ),
    n_linhas = 1:1000
) {
  # gerar tabela dinamica dos dados brutos
  dados_brutos <- dados |>
    dplyr::slice(n_linhas) |>
    DT::datatable(filter = list(position = "top"))

  # retornar a tabela dinamica dos dados brutos
  return(dados_brutos)
}

# Documentacao da funcao gerar_tabdin_dados_completos() --------------------
#' Gera uma tabela dinamica dos dados completos
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#' @param n_linhas Descricao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_dados_completos()}
gerar_tabdin_dados_completos <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_completos.rds"
      )
    ),
    n_linhas = 1:4500
) {
  # gerar tabela dinamica dos dados completos
  dados_completos <- dados |>
    dplyr::slice(n_linhas) |>
    DT::datatable(filter = list(position = "top"))

  # retornar a tabela dinamica dos dados completos
  return(dados_completos)
}

# Documentacao da funcao gerar_tabdin_dados_selecionados_distanceR_cov() --------------------
#' Gera uma tabela dinamica dos dados selecionados no formato distance do R com covariaveis
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#' @param n_linhas Descricao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_dados_selecionados_distanceR_cov()}
gerar_tabdin_dados_selecionados_distanceR_cov <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados_transformados_dist_r_cov.rds"
      )
    ),
    n_linhas = 1:4500
) {
  # gerar tabela dinamica dos dados
  dados_transformados_dist_r_cov <- dados |>
    dplyr::slice(
      n_linhas
    ) |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica dos dados_transformados_dist_r_selecionado
  return(dados_transformados_dist_r_cov)
}

# Documentacao da funcao gerar_tabdin_dados_selecionados_distanceR() --------------------
#' Gera uma tabela dinamica dos dados selecionados no formato distance do R sem covariaveis
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#' @param n_linhas Descricao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_dados_selecionados_distanceR()}
gerar_tabdin_dados_selecionados_distanceR <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados_transformados_dist_r.rds"
      )
    ),
    n_linhas = 1:4500
) {
  # gerar tabela dinamica dos dados selecionados no formato distance do R sem covariaveis
  dados_transformados_dist_r_completo <- dados |>
    dplyr::slice(
      n_linhas
    ) |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica dos dados_transformados_dist_r_completo
  return(dados_transformados_dist_r_completo)
}

# Documentacao da funcao gerar_tabdin_dados_filtrados() --------------------
#' Gera uma tabela dinamica dos dados selecionados e filtrados por UC e especie
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#' @param nome_uc Descricao
#' @param nome_sp Descricao
#' @param n_linhas Descricao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_dados_filtrados()}
gerar_tabdin_dados_filtrados <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    ),
    nome_uc = "Resex Tapajos-Arapiuns",
    nome_sp = "Dasyprocta croconota",
    n_linhas = 1:4500
) {
  # gerar tabela dinamica dos dados filtrados por UC e especie
  dados_filtrado <- dados |>
    dplyr::filter(
      uc_name == nome_uc,
      sp_name == nome_sp
    ) |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica dos dados filtrados por UC e especie
  return(dados_filtrado)
}

# Documentacao da funcao gerar_tabdin_dados_selecionados() --------------------
#' Gera uma tabela dinamica dos dados selecionados
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#' @param n_linhas Descricao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_dados_selecionados()}
gerar_tabdin_dados_selecionados <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    ),
    n_linhas = 1:4500
) {
  # gerar tabela dinamica dos dados selecionados
  tabdin_dados_selecionados <- dados |>
    dplyr::slice(
      n_linhas
    ) |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica dos dados selecionados
  return(tabdin_dados_selecionados)
}

# Documentacao da funcao gerar_tabdin_n_ano_uc() --------------------
#' Gera uma tabela dinamica do numero de anos em que cada UC foi amostrada
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_n_ano_uc()}
gerar_tabdin_n_ano_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_ano_uc.rds"
      )
    )
) {
  # gerar tabela dinamica dos dados do numero de anos em que cada UC foi amostrada
  tabdin_n_ano_uc <- dados |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica do numero de anos em que cada UC foi amostrada
  return(tabdin_n_ano_uc)
}

# Documentacao da funcao gerar_tabdin_n_obs_sp_ano() --------------------
#' Gera uma tabela dinamica do numero de observacoes por especie em cada ano
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_n_obs_sp_ano()}
gerar_tabdin_n_obs_sp_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_sp_ano.rds"
      )
    )
) {
  # gerar tabela dinamica dos dados do numero de gerar_tabdin_n_obs_sp_ano
  tabdin_n_obs_sp_ano <- dados |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica do numero de gerar_tabdin_n_obs_sp_ano
  return(tabdin_n_obs_sp_ano)
}

# Documentacao da funcao gerar_tabdin_n_obs_sp_uc_ano() --------------------
#' Gera uma tabela dinamica do numero de observacoes por especie, em cada UC e em cada ano
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_n_obs_sp_uc_ano()}
gerar_tabdin_n_obs_sp_uc_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_sp_uc_ano.rds"
      )
    )
) {
  # gerar tabela dinamica dos dados do numero de observacoes por UC, por especies, por estacao e por ano
  tabdin_n_obs_sp_uc_ano <- dados |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica do numero de observacoes por UC, por especies, por estacao e por ano
  return(tabdin_n_obs_sp_uc_ano)
}

# Documentacao da funcao gerar_tabdin_n_obs_sp_uc_estacao_ano() --------------------
#' Gera uma tabela dinamica do numero de observacoes por especie, em cada UC, estacao e ano
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_n_obs_sp_uc_estacao_ano()}
gerar_tabdin_n_obs_sp_uc_estacao_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_sp_uc_estacao_ano.rds"
      )
    )
) {
  # gerar tabela dinamica dos dados do numero de observacoes por UC, por especies, por estacao e por ano
  tabdin_n_obs_sp_uc_estacao_ano <- dados |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica do numero de observacoes por UC, por especies, por estacao e por ano
  return(tabdin_n_obs_sp_uc_estacao_ano)
}

# Documentacao da funcao gerar_tabdin_n_obs_sp_uc() --------------------
#' Gera uma tabela dinamica do numero de observacoes por especie, em cada UC
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_n_obs_sp_uc()}
gerar_tabdin_n_obs_sp_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_sp_uc.rds"
      )
    )
) {
  # gerar tabela dinamica dos dados do numero de observacoes por sp por UC
  tabdin_n_obs_sp_uc <- dados |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica do numero de observacoes por sp por UC
  return(tabdin_n_obs_sp_uc)
}

# Documentacao da funcao gerar_tabdin_n_obs_sp() --------------------
#' Gera uma tabela dinamica do numero de observacoes por especie
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_n_obs_sp()}
gerar_tabdin_n_obs_sp <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_sp.rds"
      )
    )
) {
  # gerar tabela dinamica dos dados do numero de observacoes por sp
  tabdin_n_obs_sp <- dados |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica do numero de observacoes por sp
  return(tabdin_n_obs_sp)
}

# Documentacao da funcao gerar_tabdin_n_obs_uc_ano() --------------------
#' Gera uma tabela dinamica do numero de observacoes por UC em cada ano
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_n_obs_uc_ano()}
gerar_tabdin_n_obs_uc_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_uc_ano.rds"
      )
    )
) {
  # gerar tabela dinamica dos dados do numero de anos em que cada UC foi amostrada
  tabdin_n_obs_uc_ano <- dados |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica do numero de anos em que cada UC foi amostrada
  return(tabdin_n_obs_uc_ano)
}

# Documentacao da funcao gerar_tabdin_n_obs_uc() --------------------
#' Gera uma tabela dinamica do numero de observacoes por UC
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_n_obs_uc()}
gerar_tabdin_n_obs_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_uc.rds"
      )
    )
) {
  # gerar tabela dinamica dos dados do numero de observacoes por uc
  tabdin_n_obs_uc <- dados |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica do numero de observacoes por uc
  return(tabdin_n_obs_uc)
}

# Documentacao da funcao gerar_tabdin_n_uc_ano() --------------------
#' Gera uma tabela dinamica do numero de UC's amostrada em cada ano
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_n_uc_ano()}
gerar_tabdin_n_uc_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_uc_ano.rds"
      )
    )
) {
  # gerar tabela dinamica dos dados do numero de observacoes por sp
  tabdin_n_uc_ano <- dados |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica do numero de observacoes por sp
  return(tabdin_n_uc_ano)
}

# Documentacao da funcao plotar_distribuicao_distancia_interativo() --------------------
#' Gera tres graficos interativos descrevendo a variacao das distancias perpendiculares observadas
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{...} contendo um grafico de caixa, um grafico de pontos e um histograma da distribuicao de distancias perpendiculares.
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{plotar_distribuicao_distancia_interativo()}
plotar_distribuicao_distancia_interativo <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados_transformados_dist_r.rds"
      )
    ),
    largura_caixa = 1
) {
  # desenha o grafico de caixa
  box <- dados |>
    ggplot2::ggplot() +
    ggplot2::aes(x = distance) +
    ggplot2::geom_boxplot(
      col = "black",
      fill = "chartreuse4"
    ) +
    ggplot2::scale_y_continuous(
      breaks = NULL,
      limits = c(-.8, .8)) +
    ggplot2::labs(
      x = "",
      y = " \n \n",
    ) +
    ggplot2::theme_minimal()

  # transforma em grafico interativo
  box <- plotly::ggplotly(box)

  # desenha o grafico de pontos
  pontos <- dados |>
    dplyr::arrange(dplyr::desc(distance)) |>
    ggplot2::ggplot() +
    ggplot2::aes(y = dplyr::desc(seq_along(distance)),
                 x = distance) +
    ggplot2::geom_point(
      color = "chartreuse4"
    )  +
    ggplot2::labs(x = "Distancia",
                  y = " \n \n") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank())

  # transforma em grafico interativo
  pontos <- plotly::ggplotly(pontos)

  # dessenha o hitograma
  hist <- dados |>
    dplyr::arrange(dplyr::desc(distance)) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = distance) +
    ggplot2::geom_histogram(binwidth = largura_caixa,
                            fill = "chartreuse4",
                            col = "white",
                            center = 1.25) +
    ggplot2::labs(x = "",
                  y = "Frequencia") +
    ggplot2::theme_minimal()

  # tansforma em grafico interativo
  hist <- plotly::ggplotly(hist)

  # organizar os graficos
  fig <-  plotly::subplot(
    hist,
    box,
    pontos,
    nrows = 3,
    titleX = TRUE,
    titleY = TRUE
  )

  # retronar os graficos
  return(fig)
}

# Documentacao da funcao plotar_distribuicao_distancia_estatico() --------------------
#' Gera tres graficos estaticos descrevendo a variacao das distancias perpendiculares observadas
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{...} contendo um grafico de caixa, um grafico de pontos e um histograma da distribuicao de distancias perpendiculares.
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{plotar_distribuicao_distancia_estatico()}
plotar_distribuicao_distancia_estatico <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados_transformados_dist_r.rds"
      )
    )
) {
  # desenha o grafico de caixa
  box <- dados |>
    ggplot2::ggplot() +
    ggplot2::aes(x = distance) +
    ggplot2::geom_boxplot(col = "black",
                          fill = "chartreuse4") +
    ggplot2::scale_y_continuous(
      breaks = NULL,
      limits = c(-.8, .8)) +
    ggplot2::labs(
      x = "",
      y = " \n \n",
    ) +
    ggplot2::theme_minimal()

  # desenha o grafico de pontos
  pontos <- dados |>
    dplyr::arrange(dplyr::desc(distance)) |>
    ggplot2::ggplot() +
    ggplot2::aes(y = dplyr::desc(seq_along(distance)),
                 x = distance) +
    ggplot2::geom_point(
      color = "chartreuse4"
    )  +
    ggplot2::labs(x = "Distancia",
                  y = " \n \n") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank())

  # dessenha o hitograma
  hist <- dados |>
    dplyr::arrange(dplyr::desc(distance)) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = distance) +
    ggplot2::geom_histogram(binwidth = 2.5,
                            fill = "chartreuse4",
                            col = "white",
                            center = 1.25) +
    ggplot2::labs(x = "",
                  y = "Frequencia") +
    ggplot2::theme_minimal()

  # organizar os graficos
  fig <- ggpubr::ggarrange(
    hist,
    box,
    pontos,
    nrow = 3
  )

  # retronar os graficos
  return(fig)
}

# Documentacao da funcao plotar_funcao_deteccao_modelos_selecionad --------
#' Title
#'
#' @param dados 
#'
#' @return
#' @export
#'
#' @examples
plotar_funcao_deteccao_modelos_selecionados <- function(dados) {
  dados |> 
    purrr::map(
      \(.x) plot(
        .x,
        xlab = "Distancia (m)",
        ylab = "Probabilidade de detecacao",
        pl.col = "chartreuse4"
      )
    )
}

# Documentacao da funcao plotar_funcao_deteccao_selecao_distancia_truncamento() --------
#' Title
#'
#' @param dados 
#'
#' @return
#' @export
#'
#' @examples
plotar_funcao_deteccao_selecao_distancia_truncamento <- function(dados) {
  dados$modelos |> 
    purrr::map(
      \(.x) plot(
        .x,
        xlab = "Distancia (m)",
        ylab = "Probabilidade de detecacao",
        pl.col = "chartreuse4"
      )
    )
}


# Documentacao da funcao plotar_n_obs_sp_estatico() --------------------
#' Gera graficos de barras estaticos com o numero de observacoes por especie
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{...} contendo graficos de barras estaticos.
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{plotar_n_obs_sp_estatico()}
plotar_n_obs_sp_estatico <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_sp.rds"
      )
    )
) {
  fig <- dados |>
    dplyr::mutate(
      n_obs = dplyr::case_when(
        n > 1000 ~ "Mais de 1000 observacoes",
        n %in% 501:1000 ~ "Entre 501 e 1000 observacoes",
        n %in% 101:500 ~ "Entre 101 e 500 observacoes",
        n %in% 1:100 ~ "Ate 100 observacoes"
      ),
      sp_name_abv = forcats::fct_reorder(
        sp_name_abv,
        n
      ),
      n_obs = forcats::fct_reorder(
        n_obs,
        dplyr::desc(n)
      )
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(y = sp_name_abv,
                 x = n,
                 label = n) +
    ggplot2::geom_col(
      fill = "chartreuse4",
      #width = .9
    ) +
    ggplot2::geom_label(
      size = 8
    ) +
    ggplot2::labs(y = "Especies",
                  x = "Numero de observacoes") +
    ggplot2::facet_wrap(
      facets = ggplot2::vars(
        n_obs
      ),
      nrow = 4,
      scales = "free"
    ) +
    ggplot2::theme_minimal(14) +
    ggplot2::theme(
      title = ggplot2::element_text(size = 40),
      axis.text = ggplot2::element_text(size = 30),
      # Remove labels from the vertical axis
      #axis.text.y = element_blank(),
      strip.text = ggplot2::element_text(size = 40),
    )


  # retronar os graficos
  return(fig)
}

# Documentacao da funcao plotar_n_obs_sp_interativo() --------------------
#' Gera graficos de barras interativos com o numero de observacoes por especie
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{...} contendo graficos de barras interativos.
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{plotar_n_obs_sp_interativo()}
plotar_n_obs_sp_interativo <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_sp.rds"
      )
    )
) {
  fig <- plotly::ggplotly(
    # gerar figura
    fig <- dados |>
      dplyr::mutate(
        n_obs = dplyr::case_when(
          n > 1000 ~ "Mais de 1000 observacoes",
          n %in% 501:1000 ~ "Entre 501 e 1000 observacoes",
          n %in% 101:500 ~ "Entre 101 e 500 observacoes",
          n %in% 1:100 ~ "Ate 100 observacoes"
        ),
        sp_name = forcats::fct_reorder(
          sp_name,
          n
        ),
        n_obs = forcats::fct_reorder(
          n_obs,
          dplyr::desc(n)
        )
      ) |>
      ggplot2::ggplot() +
      ggplot2::aes(y = sp_name,
                   x = n,
                   label = sp_name_abv) +
      ggplot2::geom_col(
        fill = "chartreuse4",
        #width = .9
      ) +
      ggplot2::labs(y = "Especies",
                    x = "Numero de observacoes") +
      ggplot2::facet_wrap(
        facets = ggplot2::vars(
          n_obs
        ),
        nrow = 4,
        scales = "free"
      ) +
      ggplot2::theme_minimal(14) +
      ggplot2::theme(
        # Remove labels from the vertical axis
        axis.text.y = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(size = 10),
      )
  )

  # retronar os graficos
  return(fig)
}

# Documentacao da funcao plotar_n_obs_sp_uc_interativo() --------------------
#' Gera um grafico de barras interativo com o numero de observacoes por especie e UC
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{...} contendo graficos de barras interativos.
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{plotar_n_obs_sp_uc_interativo()}
plotar_n_obs_sp_uc_interativo <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_sp_uc.rds"
      )
    )
) {
  # desenha o grafico com mais de 1000 observacoes
  mais_mil_obs <- dados |>
    dplyr::filter(n %in% 1001:2497) |>
    dplyr::mutate(
      sp_name_abv = forcats::fct_reorder(
        sp_name_abv,
        dplyr::desc(n)
      )
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = sp_name_abv,
                 y = n,
                 fill = uc_name_abv) +
    ggplot2::geom_col(
      #fill = "chartreuse4",
      position = "dodge"
    ) +
    #ggplot2::geom_label() +
    ggplot2::labs(
      x = "Nome das especies",
      y = "Numero de observacoes"
    ) +
    ggplot2::theme_minimal(14)

  # transforma em grafico interativo
  mais_mil_obs <- plotly::ggplotly(mais_mil_obs)

  # desenha o grafico com 501 a 1000 observacoes
  quinhentos_mil_obs <- dados  |>
    dplyr::filter(n %in% 501:1000) |>
    dplyr::mutate(
      sp_name_abv = forcats::fct_reorder(
        sp_name_abv,
        dplyr::desc(n)
      )
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = sp_name_abv,
                 y = n,
                 fill = uc_name_abv) +
    ggplot2::geom_col(
      #fill = "chartreuse4",
      position = "dodge"
    ) +
    #ggplot2::geom_label() +
    ggplot2::labs(
      x = "Nome das especies",
      y = "Numero de observacoes"
    ) +
    ggplot2::theme_minimal(14)

  # transforma em grafico interativo
  quinhentos_mil_obs <- plotly::ggplotly(quinhentos_mil_obs)

  # desenha o grafico com 101 a 500 observacoes
  cem_quintas_obs <- dados  |>
    dplyr::filter(n %in% 101:500) |>
    dplyr::mutate(
      sp_name_abv = forcats::fct_reorder(
        sp_name_abv,
        dplyr::desc(n)
      )
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = sp_name_abv,
                 y = n,
                 fill = uc_name_abv) +
    ggplot2::geom_col(
      #fill = "chartreuse4",
      position = "dodge"
    ) +
    #ggplot2::geom_label() +
    ggplot2::labs(
      x = "Nome das especies",
      y = "Numero de observacoes"
    ) +
    ggplot2::theme_minimal(14)

  # tansforma em grafico interativo
  cem_quintas_obs <- plotly::ggplotly(cem_quintas_obs)

  # desenha o grafico com menos de 100 observacoes
  uma_cem_obs <- dados  |>
    dplyr::filter(n < 100) |>
    dplyr::mutate(
      sp_name_abv = forcats::fct_reorder(
        sp_name_abv,
        dplyr::desc(n)
      )
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = sp_name_abv,
                 y = n,
                 fill = uc_name_abv) +
    ggplot2::geom_col(
      #fill = "chartreuse4",
      position = "dodge"
    ) +
    #ggplot2::geom_label() +
    ggplot2::labs(
      x = "Nome das especies",
      y = "Numero de observacoes"
    ) +
    ggplot2::theme_minimal(14)

  # tansforma em grafico interativo
  uma_cem_obs <- plotly::ggplotly(uma_cem_obs)

  # organizar os graficos
  fig <-  plotly::subplot(
    mais_mil_obs,
    quinhentos_mil_obs,
    cem_quintas_obs,
    uma_cem_obs,
    nrows = 4,
    titleX = FALSE,
    titleY = TRUE,
    shareX = FALSE,
    shareY = TRUE
  )

  # retronar os graficos
  return(fig)
}

# Documentacao da funcao plotar_n_obs_uc_estatico() --------------------
#' Gera um grafico de barras estatico com o numero de observacoes por UC
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{...} contendo graficos de barras interativos.
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{plotar_n_obs_uc_estatico()}
plotar_n_obs_uc_estatico <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_uc.rds"
      )
    )
) {
  # desenha os graficos com mais de 1000, de 501 a 1000, de 101 a 500 e ate 100 observacoes
  fig <- dados |>
    dplyr::mutate(
      n_obs = dplyr::case_when(
        n > 1000 ~ "Mais de 1000 observacoes",
        n %in% 501:1000 ~ "Entre 501 e 1000 observacoes",
        n %in% 101:500 ~ "Entre 101 e 500 observacoes",
        n %in% 1:100 ~ "Ate 100 observacoes"
      ),
      uc_name = forcats::fct_reorder(
        uc_name,
        n
      ),
      n_obs = forcats::fct_reorder(
        n_obs,
        dplyr::desc(n)
      )
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(y = uc_name,
                 x = n,
                 label = n) +
    ggplot2::geom_col(
      fill = "chartreuse4",
      #width = .9
    ) +
    ggplot2::geom_label(
      size = 8
    ) +
    ggplot2::geom_text(
      ggplot2::aes(0, y = uc_name, label = uc_name),
      hjust = 0,
      nudge_x = 0.3,
      colour = "black",
      family = "Econ Sans Cnd",
      size = 7
    ) +
    ggplot2::labs(y = "Unidades de Conservacao",
                  x = "Numero de observacoes") +
    ggplot2::facet_wrap(
      facets = ggplot2::vars(
        n_obs
      ),
      nrow = 4,
      scales = "free"
    ) +
    ggplot2::theme_minimal(14) +
    ggplot2::theme(
      title = ggplot2::element_text(size = 40),
      axis.text = ggplot2::element_text(size = 30),
      # Remove labels from the vertical axis
      axis.text.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 40),
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )

  # retronar os graficos
  return(fig)
}

# Documentacao da funcao plotar_n_obs_uc_interativo() --------------------
#' Gera um grafico de barras interativo com o numero de observacoes por UC
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{...} contendo graficos de barras interativos.
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{plotar_n_obs_uc_interativo()}
# desenha um grafico de barras intereativo com o numero de observacoes por UC
plotar_n_obs_uc_interativo <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_uc.rds"
      )
    )
) {
  # desenha o grafico com mais de 1000 observacoes
  fig <-
    plotly::ggplotly(dados |>
                       dplyr::mutate(
                         n_obs = dplyr::case_when(
                           n %in% 1:100 ~ "Ate 100 observacoes",
                           n %in% 101:500 ~ "Entre 101 e 500 observacoes",
                           n %in% 501:1000 ~ "Entre 501 e 1000 observacoes",
                           n > 1000 ~ "Mais de 1000 observacoes"
                         ),
                         uc_name_abv = forcats::fct_reorder(
                           uc_name_abv,
                           dplyr::desc(n)
                         ),
                         n_obs = forcats::fct_reorder(
                           n_obs,
                           dplyr::desc(n)
                         )
                       ) |>
                       ggplot2::ggplot() +
                       ggplot2::aes(x = uc_name_abv,
                                    y = n,
                                    label = uc_name) +
                       ggplot2::geom_col(fill = "chartreuse4") +
                       ggplot2::labs(x = "Unidades de Conservacao",
                                     y = "Numero de observacoes") +
                       ggplot2::facet_wrap(
                         facets = ggplot2::vars(
                           n_obs
                         ),
                         nrow = 4,
                         scales = "free"
                       ) +
                       ggplot2::theme_minimal(14)
    )

  # retronar os graficos
  return(fig)
}

# Documentacao da funcao plotar_n_obs_validadas_estatico() --------------------
#' Gera um grafico de barras estatico com o numero de observacoes validadas para cada nivel taxonomico
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{...} contendo graficos de barras interativos.
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{plotar_n_obs_validadas_estatico()}
plotar_n_obs_validadas_estatico <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/tabela_n_obs_validadas.rds"
      )
    )
) {
  #
  grafico_n_sp_validada_estatico <- dados  |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = validation,
      y = n,
      label = n
    ) +
    ggplot2::geom_col(
      fill = "chartreuse4"
    ) +
    ggplot2::geom_label(
      ggplot2::aes(
        label = n
      )
    ) +
    ggplot2::labs(
      title = "Numero de obs. validadas para \ncada nivel taxonomico",
      x = "Nivel taxonomico",
      y = "Contagem"
    ) +
    ggplot2::theme_minimal(14)

  # retornar grafico estatico
  return(grafico_n_sp_validada_estatico)
}

# Documentacao da funcao plotar_n_obs_validadas_interativo() --------------------
#' Gera um grafico de barras interativo com o numero de observacoes validadas para cada nivel taxonomico
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{...} contendo graficos de barras interativos.
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{plotar_n_obs_validadas_interativo()}
plotar_n_obs_validadas_interativo <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/tabela_n_obs_validadas.rds"
      )
    )
) {
  #
  grafico_n_sp_validada <- dados  |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = validation,
      y = n,
    ) +
    ggplot2::geom_col(
      fill = "chartreuse4"
    ) +
    ggplot2::labs(
      title = "Numero de obs. validadas para \ncada nivel taxonomico",
      x = "Nivel taxonomico",
      y = "Contagem"
    ) +
    ggplot2::theme_minimal(14)

  # gerar grafico interativo
  grafico_n_sp_validada <- plotly::ggplotly(
    grafico_n_sp_validada
  )

  return(grafico_n_sp_validada)
}

# Documentacao da funcao selecionar_distancia_truncamento() ------------------
#' Title
#'
#' @param dados 
#' @param dist_truncamento 
#'
#' @return
#' @export
#'
#' @examples
selecionar_distancia_truncamento <- function(
    dados,
    dist_truncamento = list(
      `25%` = "25%",
      `20%` = "20%", 
      `15%` = "15%", 
      `10%` = "10%",
      `5%` = "5%"
    )
) {
  
  # ajustar modelos com funcao de deteccao do tipo Half-normal e sem termos 
  # de ajuste a diferentes distancias de truncamento 
  modelos_hn_diferentes_dist_truncamento <- purrr::map(
    dist_truncamento,
    \(.x) Distance::ds(
      dados,
      key = NULL, 
      adjustment = NULL,
      truncation = .x
    )
  )
  
  # performa a selecao de modelos e gera um data.frame com os resultados
  tabela_selecao_dist_truncamento <- modelos_hn_diferentes_dist_truncamento |> 
    purrr::map(
      \(x) Distance::summarize_ds_models(x, delta_only = FALSE)
    ) |> 
    list_rbind() |> 
    mutate(AIC = round(AIC, 3))
  
  # gerar uma tabela com os valores da AIC e as distancias de truncamento
  # correspondentes
  aic <- modelos_hn_diferentes_dist_truncamento |> 
    purrr::map(\(x) AIC(x)) |> 
    list_rbind() |> 
    mutate(dist_truncamento = names(dist_truncamento),
           AIC = round(AIC, 3))
  
  # corrigir a coluna com as distancias de truncamento adequadas
  tabela_selecao_dist_truncamento <- tabela_selecao_dist_truncamento |> 
    left_join(aic, join_by(AIC)) |> 
    mutate(Model = dist_truncamento) |> 
    select(!df:dist_truncamento)
  
  # retorna um data.frame com a selecao da melhor distancia de truncamento
  return(list(
    modelos = modelos_hn_diferentes_dist_truncamento,
    selecao = tabela_selecao_dist_truncamento
  ))
  
}

# Documentacao da funcao selecionar_funcao_deteccao_termo_ajuste() ------------------
#' Title
#'
#' @param dados 
#'
#' @return
#' @export
#'
#' @examples
selecionar_funcao_deteccao_termo_ajuste <- function(
    dados
) {
  
  # gerar rank das funcoes de deteccao e termos de ajuste
  selecao_funcao_deteccao_termo_ajuste <- Distance::summarize_ds_models(
    dados$`half-normal`$`Sem termo`,
    dados$`half-normal`$Cosseno,
    dados$`half-normal`$`Hermite polinomial`,
    dados$`hazard-rate`$`Sem termo`,
    dados$`hazard-rate`$Cosseno,
    dados$`hazard-rate`$`Polinomial simples`,
    dados$uniforme$Cosseno,
    dados$uniforme$`Polinomial simples`, 
    delta_only = FALSE
  )
  
  # 
  tabela_selecao_funcao_deteccao_termo_ajuste <- selecao_funcao_deteccao_termo_ajuste |> 
    select(!1) |> 
    rename(Model = `Key function`) |> 
    distinct()
  
  # retorna um data.frame com a selecao das funcoes de deteccao e termos
  # de ajuste
  return(tabela_selecao_funcao_deteccao_termo_ajuste)
  
}

# Documentacao da funcao testar_bondade_ajuste() --------------------------
#' Title
#'
#' @param dados 
#'
#' @return
#' @export
#'
#' @examples
testar_bondade_ajuste <- function(
    dados, 
    plot = FALSE,
    chisq = FALSE,
    nc = NULL
    ) {
  
  # gera uma lista com os resultados dos testes de bondade de ajuste
  bondade_ajuste <- dados |> 
    purrr::map(
      \(x) gof_ds(
        x, 
        plot = plot, 
        chisq = chisq,
        nc = nc
      )
    ) |> 
    # gerar o data.frame com os resultados dos testes de bondade de ajuste
    purrr::map(
      \(x) data.frame(x$dsgof$CvM)
    ) |> 
    list_rbind() |> 
    mutate(Modelo = names(dados)) |> 
    relocate(Modelo, .before = W)
  
  # retornar o data.frame com o resultado dos testes de bondade de ajuste
  return(bondade_ajuste)
}

# Documentacao da funcao transformar_para_distanceR_covariaveis() --------------------
#' Gera uma tabela no formato para analise no pacote Distance do R, com duas covariaveis, a partir dos dados selecionados
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{tibble}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{transformar_para_distanceR_covariaveis()}
# transforma para o formato para analise no pacote distance do R
# adicionando duas covariaveis
transformar_para_distanceR_covariaveis <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    )
) {
  dados_transformados_dist_r_cov <- dados |>
    dplyr::select(
      Region.Label = uc_name,
      Sample.Label = `ea_name`,
      Effort = total_effort,
      sampling_day,
      uc_name_abv,
      sp_name,
      sp_name_abv,
      distance,
      year,
      size = group_size,
      cense_time
    ) |>
    dplyr::mutate(
      Area = 0,
      # Sample.Label = lubridate::date(Sample.Label),
      object = 1:nrow(dados)
    ) |>
    dplyr::relocate(
      Area,
      .before = Sample.Label
    )

  # garvar no diretorio inst/extdata/ arquivo dados_transformados_dist_r_cov.rds
  # readr::write_rds(
  #   dados_transformados_dist_r_cov,
  #   file = paste0(
  #     here::here(),
  #     "/data/dados_selecionados_transformados_dist_r_cov.rds"
  #   )
  # )
  
  # retorna o data.frame
  return(dados_transformados_dist_r_cov)
}

# Documentacao da funcao transformar_para_distanceR_covariaveis() --------------------
#' Gera uma tabela no formato para analise no pacote Distance do R, com duas covariaveis, a partir dos dados selecionados
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{tibble}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{transformar_para_distanceR_covariaveis_sem_repeticao()}
# transforma para o formato para analise no pacote distance do R
# adicionando duas covariaveis e mantendo o esforco sem contar as repeticoes
transformar_para_distanceR_covariaveis_sem_repeticao <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    )
) {
  dados_transformados_dist_r_cov_sem_repeticao <- dados |>
    dplyr::select(
      Region.Label = uc_name,
      Sample.Label = `ea_name`,
      Effort = day_effort,
      sampling_day,
      uc_name_abv,
      sp_name,
      sp_name_abv,
      distance,
      season = season,
      year,
      size = group_size,
      cense_time
    ) |>
    dplyr::mutate(
      Area = 0,
      # Sample.Label = lubridate::date(Sample.Label),
      object = 1:nrow(dados)
    ) |>
    dplyr::relocate(
      Area,
      .before = Sample.Label
    )
  
  # # garvar no diretorio inst/extdata/ arquivo dados_transformados_dist_r_cov.rds
  # readr::write_rds(
  #   dados_transformados_dist_r_cov,
  #   file = paste0(
  #     here::here(),
  #     "/data/dados_selecionados_transformados_dist_r_cov.rds"
  #   )
  # )
  # retorna o data.frame
  return(dados_transformados_dist_r_cov_sem_repeticao)
}

# Documentacao da funcao transformar_para_distanceR() --------------------
#' Gera uma tabela no formato para analise no pacote Distance do R, sem covariaveis, a partir dos dados selecionados
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descricao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{tibble}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{transformar_para_distanceR()}
transforma_para_distanceR_com_repeticao_filtra_uc_sp <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    ),
    nome_uc = "Resex Tapajos-Arapiuns",
    nome_sp = "Dasyprocta croconota"
) {
  # transforma dados para formato distance R
  dados_transformados_dist_r_com_repeticao_filtra_uc_sp <- dados |>
    dplyr::select(
      Region.Label = uc_name,
      Sample.Label = `ea_name`,
      Effort = total_effort,
      distance,
      sp_name,
      sampling_day,
      year,
      season
    ) |>
    dplyr::mutate(
      Area = 0
    ) |>
    dplyr::relocate(
      Area,
      .before = Sample.Label
    ) |> 
    filter(
      Region.Label == nome_uc,
      sp_name == nome_sp
    )
  
  # gerar coluna object
  dados_transformados_dist_r_com_repeticao_filtra_uc_sp <- dados_transformados_dist_r_com_repeticao_filtra_uc_sp |> 
    mutate(object = seq_along(dados_transformados_dist_r_com_repeticao_filtra_uc_sp$Region.Label))
  
  # # grava uma versao dados_transformados_dist_r.rds no diretorio data
  # readr::write_rds(
  #   dados_transformados_dist_r,
  #   file = paste0(
  #     here::here(),
  #     "/data/dados_selecionados_transformados_dist_r.rds"
  #   )
  # )

  # retorna o data.frame
  return(dados_transformados_dist_r_com_repeticao_filtra_uc_sp)
}

# Ducumentacao para transforma_para_dsitanceR_quase_sem_repeticao_ --------
#' Title
#'
#' @param dados 
#' @param nome_uc 
#' @param nome_sp 
#'
#' @return
#' @export
#'
#' @examples
transforma_para_dsitanceR_quase_sem_repeticao_filtra_uc_sp <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    ),
    nome_uc = "Resex Tapajos-Arapiuns",
    nome_sp = "Dasyprocta croconota"
) {
  # transforma dados para formato distance R
  dados_transformados_dist_r_quase_sem_repeticao_filtra_uc_sp <- dados |>
    dplyr::select(
      Region.Label = uc_name,
      Sample.Label = `ea_name`,
      Effort = day_effort,
      distance,
      sp_name,
      sampling_day,
      year,
      season,
    ) |>
    dplyr::mutate(
      Area = 0
    ) |>
    dplyr::relocate(
      Area,
      .before = Sample.Label
    ) |>   filter(
      Region.Label == nome_uc, 
      sp_name == nome_sp
    )
  
  # gerar filtro para eliminar amostras repetidas mantendo o dia com o maior n de obs
  # gerar o n de obs por data de amostragem
  n_obs_data <- dados_transformados_dist_r_quase_sem_repeticao_filtra_uc_sp |> 
    group_by(Sample.Label, sampling_day, year, season) |> 
    count(sampling_day) |> 
    ungroup()
  
  # gerar as datas com maior n de obs em cada estacao e ano
  data_com_maior_n_obs <- dados_transformados_dist_r_quase_sem_repeticao_filtra_uc_sp |> 
    group_by(Sample.Label, year, season) |> 
    count(sampling_day) |> 
    reframe(n_max = max(n)) |> 
    ungroup()
  
  # juntar as duas data.frames para obter as datas com maior n de obs
  # em cada ano e excluir datas de amostragem repitidas na mesma estacao
  # e ano
  dados_para_filtrar_por_data_quase_sem_repeticao <- n_obs_data |> 
    semi_join(
      data_com_maior_n_obs, 
      join_by(Sample.Label, year, season, n == n_max),
    ) |> 
    distinct(sampling_day, year, season)
  
  # gerar o filtro de datas
  filtro_datas_quase_sem_repeticao <- dados_para_filtrar_por_data_quase_sem_repeticao$sampling_day
  
  # eliminar amostras repetidas
  dados_transformados_dist_r_quase_sem_repeticao_filtra_uc_sp <- dados_transformados_dist_r_quase_sem_repeticao_filtra_uc_sp |> 
    filter(sampling_day %in% filtro_datas_quase_sem_repeticao)
  
  # gerar coluna object
  dados_transformados_dist_r_quase_sem_repeticao_filtra_uc_sp <- dados_transformados_dist_r_quase_sem_repeticao_filtra_uc_sp |> 
    mutate(object = seq_along(dados_transformados_dist_r_quase_sem_repeticao_filtra_uc_sp$Region.Label))
  
  # # grava uma versao dados_transformados_dist_r.rds no diretorio data
  # readr::write_rds(
  #   dados_transformados_dist_r,
  #   file = paste0(
  #     here::here(),
  #     "/data/dados_selecionados_transformados_dist_r.rds"
  #   )
  # )
  
  # retorna o data.frame
  return(dados_transformados_dist_r_quase_sem_repeticao_filtra_uc_sp)
}



