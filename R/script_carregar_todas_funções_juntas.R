
# script para carregar todas as funções juntas ----------------------------

# carregar função corrigir_diretorio.R
# Descrição
# A função recebe o diretorio de trabalho atual e altera para o diretório 
# correto
corrigir_diretorio <- function(
    diretorio_atual = getwd(),   
    padrao_alterar = "doc",
    corrige = "/R/"
) {
  # substituir o pdarão atual pelo correto
  novo_diretorio <- paste0(
    stringr::str_remove(
      diretorio_atual, 
      padrao_alterar
    ),
    corrige
  )
  
  # retorno o novo diretorio 
  return(novo_diretorio)
}

# carregar função carregar_dados_brutos_xlsx.R
# Descrição
# carrega os dados brutos em formato .xlsx diretamente da pasta raw-data
# funciona apenas no ambiente do arquivo nome_do_arquivo.rmd
carregar_dados_brutos_xlsx <- function(
    dados = readxl::read_excel(
      path = paste0(
        stringr::str_remove(
          getwd(), 
          "doc"
        ),
        "/data-raw/Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP.xlsx"
      ),
      sheet = "dados brutos"
    )
) {
  # retorna os dados butos
  return(dados)
}

# carregar função gerar_tabdin_dados_brutos
# Descrição
# gera tabela dinâmica dos dados brutos
gerar_tabdin_dados_brutos <- function(
    dados = readxl::read_excel(
      path = paste0(
        stringr::str_remove(
          getwd(), 
          "doc"
        ),
        "/data-raw/Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP.xlsx"
      ),
      sheet = "dados brutos"
    ),
    n_linhas = 1:1000
) {
  # gerar tabela dinâmica dos dados completos
  dados_brutos <- dados |> 
    slice(n_linhas) |> 
    DT::datatable(filter = list(position = "top"))
  
  # retornar a tabela dinamica dos dados brutos
  return(dados_brutos)
}

# carregar a função carregar_dados_completos
# Descrição
# a função carregar_dados1() carrega toda a planilha de dados 
# selecionando automaticamente as colunas CDU, Local - Nome da Unidade de 
# Conservação
# deve carregar uma planilha que esteja no formato do arquivo:
# Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP
# enviada pelo Gerson por whatsapp no dia 08/03/2023

carregar_dados_completos <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data-raw/dados_brutos.rds'
      )
    )
) {
  # padronizar separadores
  # gerar o data.frame desejado
  dados_completos <- dados |>  
    dplyr::select(
      uc_code = CDUC,
      uc_name = `Local - Nome da Unidade de Conservação`,
      ea_number = `Número da Estação Amostral`,
      ea_name = `Nome da EA`,
      season = `Estação do ano`,
      sampling_day = `data da amostragem`,
      day_effort = `Esforço de amostragem tamanho da trilha (m)`,
      class = Classe,
      order = Ordem,
      family = Família,
      genus = Gênero,
      sp = `Espécies validadas para análise do ICMBio`,
      validation = `Clasificação taxonômica validada`, 
      distance = `distância (m)     do animal em relação a trilha`,
      group_size = `n° de animais`,
      observadores = `nome dos observadores`,
      cense_started_at = `horário de início  (h:mm)`,
      cense_stoped_at = `horário de término (h:mm)`
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
          "PSM", "PSC", "PSD", "PSP", "PSO", "PPN", "PCO", "PI", "PJaú", "PJur",
          "PMR", "PS", "PV", "PCA", "PMT", "RG", "RJ", "RTap", "RU", "RG",
          "RTrom", "RAT", "RBA", "RCI", "RCM", "RRC", "RROP", "RIA", "RRA", "RTA"
        )
      ),
      validation = forcats::fct_recode(
        validation,
        "Espécie" = "E",
        "Espécie" = "e",
        "Família" = "F",
        "Gênero" = "G",
        "Gênero" = "g",
        "Ordem" = "O"
      ),
      year = lubridate::year(sampling_day),
      across(
        where(is.character),
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
      sp:number_observers,
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
    )
  
  # grava uma versão dados_completos.rds no diretório data
  readr::write_rds(
    dados_completos,
    file = paste0(
      stringr::str_remove(
        getwd(), 
        "doc"
      ),
      "/data/dados_completos.rds"
    )
  )
  # retornar o data.frame
  return(dados_completos)
}

# carregar função gerar_tabdin_dados_completos
# Descrição
# gera tabela dinâmica dos dados completos
gerar_tabdin_dados_completos <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data/dados_completos.rds'
      )
    ),
    n_linhas = 1:4500
) {
  # gerar tabela dinâmica dos dados completos
  dados_completos <- dados |> 
    slice(n_linhas) |> 
    DT::datatable(filter = list(position = "top"))
  
  # retornar a tabela dinamica dos dados completos
  return(dados_completos)
}

# carregar a função carregar_dados_filtrados
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
  dados_completos <- dados |>  
    dplyr::filter(
      uc_name == nome_uc,
      sp == nome_sp
    )
  
  # retornar o data.frame
  return(dados_completos)
}

# Descrição
# gera tabela dinâmica dos dados filtrado
gerar_tabdin_dados_filtrados <- function(
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
    nome_sp = "Dasyprocta croconota",
    n_linhas = 1:4500
) {
  # gerar tabela dinâmica dos dados filtrado
  dados_filtrado <- dados |>  
    dplyr::filter(
      uc_name == nome_uc,
      sp == nome_sp
    ) |> 
    datatable(
      filter = list(
        position = "top"
      )
    )
  
  # retornar a tabela dinamica dos dados filtrado
  return(dados_filtrado)
}

# Descrição
# recebe um conjunto de dados gerados a partir da função
# carregar_dados_completo() retorna um tibble no formato para análise no pacote 
# Distance do R
transformar_para_distanceR <- function(
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
  dados_transformados_dist_r <- dados_completos |>  
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
  return(dados_transformados_dist_r)
}

# Descrição
# gera tabela dinâmica dos dados completos
gerar_tabdin_dados_distanceR_completo <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data/dados_transformados_dist_r_completo.rds'
      )
    ),
    n_linhas = 1:4500
) {
  # gerar tabela dinâmica dos dados completos
  dados_transformados_dist_r_completo <- dados |> 
    slice(
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

# Descrição
# gera tabela dinâmica dos dados completos

gerar_tabdin_dados_distanceR_completo_cov <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data/dados_transformados_dist_r_cov.rds'
      )
    ),
    n_linhas = 1:4500
) {
  # gerar tabela dinâmica dos dados 
  dados_transformados_dist_r_cov <- dados |> 
    slice(
      n_linhas
    ) |> 
    DT::datatable(
      filter = list(
        position = "top"
      )
    )
  
  # retornar a tabela dinamica dos dados_transformados_dist_r_completo
  return(dados_transformados_dist_r_cov)
}

# Descrição
# fornece o número total de observações validadas para cada nível
# taxonômico na base de dados.

contar_n_obs_validadas <- function(
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
  # gerar tabela com o número de observações validadas para cada nível 
  # taxonomico
  tabela_n_obs_validadas <- dados_completos |> 
    count(validation) 
  
  # gravar tabela_n_obs_validadas.rds no diretório data
  readr::write_rds(
    tabela_n_obs_validadas,
    file = paste0(
      stringr::str_remove(
        getwd(), 
        pattern = "doc"
      ),
      "/data/tabela_n_obs_validadas.rds"
    )
  )
  
  # extrair os números de observações validadas 
  n_obs_validadas <- tabela_n_obs_validadas |> 
    pull(var = n)
  
  # retornar os números de observações validadas 
  return(n_obs_validadas)
}

# Descrição
# gera gráfico com número de observações validadas para cada nível taxonômico

plotar_n_obs_validadas <- function(
    dados = readr::read_rds(
      stringr::str_replace_all(
        getwd(),
        pattern = "doc",
        replacement = "data/tabela_n_obs_validadas.rds"
      )
    )
) {
  #
  grafico_n_sp_validada <- dados  |>  
    ggplot2::ggplot() +
    ggplot2::aes(
      x = validation,
      y = n,
      label = n
    ) +
    ggplot2::geom_col(
      fill = "chartreuse4"
    ) +
    ggplot2::geom_label() +
    ggplot2::labs(
      title = "Número de obs. validadas para \ncada nível taxonômico",
      x = "Nível taxonômico",
      y = "Contagem"
    ) +
    ggplot2::theme_minimal(14)
  
  return(grafico_n_sp_validada)
}

# Descrição
# a função carregar_dados_selecionados() carrega os dados_completos para
# gerar a tabel dados_selecionados
carregar_dados_selecionados <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          "doc"
        ),
        "/data/dados_completos.rds"
      )
    )
) {
  # gerar o data.frame desejado
  dados_selecionados <- dados |> 
    filter(validation == "Espécie")
  
  # grava uma versão dados_completos.rds no diretório data
  readr::write_rds(
    dados_selecionados,
    file = paste0(
      stringr::str_remove(
        getwd(), 
        "doc"
      ),
      "/data/dados_selecionados.rds"
    )
  )
  
  # retornar o data.frame
  return(dados_selecionados)
}

# Descrição
# fornece o número total de UCs na base de dados.

contar_n_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          "doc"
        ),
        "/data/dados_completos.rds"
      )
    )
) {
  #
  n_ucs <- dados |> 
    distinct(uc_name) |> 
    nrow()
  
  return(n_ucs)
}

# Descrição
# fornece o número total de UCs na base de dados.

contar_n_sp <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          "doc"
        ),
        "/data/dados_completos.rds"
      )
    )
) {
  #
  n_sp <- dados |> 
    distinct(sp) |> 
    nrow()
  
  return(n_sp)
}

# Descrição

plotar_n_obs_uc_interativo <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data/dados_selecionados.rds'
      )
    )
) {
  # carregar pacote necessário
  #library(patchwork)
  
  # desenha o gráfico com mais de 1000 observações
  mais_mil_obs <- dados  |>  
    dplyr::count(uc_name_abv) |> 
    dplyr::filter(n > 1000) |> 
    mutate(
      uc_name_abv = forcats::fct_reorder(
        uc_name_abv,
        dplyr::desc(n)
      )
    ) |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = uc_name_abv,
                 y = n,
                 label = n) +
    ggplot2::geom_col(fill = "chartreuse4") +
    ggplot2::labs(y = "Número de observações") +
    ggplot2::theme_minimal(14)
  
  # transforma em grafico interativo
  mais_mil_obs <- plotly::ggplotly(mais_mil_obs)
  
  # desenha o gráfico com 501 a 1000 observações
  quinehtos_mil_obs <- dados  |>  
    dplyr::count(uc_name_abv) |> 
    dplyr::filter(n %in% 501:1000) |> 
    mutate(
      uc_name_abv = forcats::fct_reorder(
        uc_name_abv,
        dplyr::desc(n)
      )
    ) |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = uc_name_abv,
                 y = n,
                 label = n) +
    ggplot2::geom_col(fill = "chartreuse4") +
    ggplot2::labs(y = "Número de observações") +
    ggplot2::theme_minimal(14)
  
  # transforma em gráfico interativo
  quinehtos_mil_obs <- plotly::ggplotly(quinehtos_mil_obs)
  
  # desenha o gráfico com 101 a 500 observações
  cem_quintas_obs <- dados  |>  
    dplyr::count(uc_name_abv) |> 
    dplyr::filter(n %in% 501:1000) |> 
    mutate(
      uc_name_abv = forcats::fct_reorder(
        uc_name_abv,
        dplyr::desc(n)
      )
    ) |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = uc_name_abv,
                 y = n,
                 label = n) +
    ggplot2::geom_col(fill = "chartreuse4") +
    ggplot2::labs(y = "Número de observações") +
    ggplot2::theme_minimal(14)
  
  # tansforma em gráfico interativo
  cem_quintas_obs <- plotly::ggplotly(cem_quintas_obs)
  
  # desenha o gráfico com menos de 100 observações
  uma_cem_obs <- dados  |>  
    dplyr::count(uc_name_abv) |> 
    dplyr::filter(n %in% 501:1000) |> 
    mutate(
      uc_name_abv = forcats::fct_reorder(
        uc_name_abv,
        dplyr::desc(n)
      )
    ) |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = uc_name_abv,
                 y = n,
                 label = n) +
    ggplot2::geom_col(fill = "chartreuse4") +
    ggplot2::labs(x = "Nomes das Uc's abreviados",
                  y = "Número de observações") +
    ggplot2::theme_minimal(14)
  
  # tansforma em gráfico interativo
  uma_cem_obs <- plotly::ggplotly(uma_cem_obs)
  
  # organizar os gráficos
  # fig <- ggpubr::ggarrange(
  #  hist,
  # box, 
  #pontos,
  #nrow = 3
  #)
  fig <-  plotly::subplot(
    mais_mil_obs, 
    quinehtos_mil_obs,
    cem_quintas_obs,
    uma_cem_obs,
    nrows = 4,
    titleX = TRUE,
    titleY = TRUE,
    shareX = TRUE,
    shareY = TRUE
  )
  
  # retronar os gráficos
  return(fig)
}

# Descrição
# gera uma tabela com o número de observações válidas para cada UC.

contar_n_obs_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          "doc"
        ),
        "/data/dados_selecionados.rds"
      )
    )
) {
  # gerar a tabela com o número de observações por UC
  n_obs_uc <- dados  |>  
    dplyr::count(
      uc_name,
      uc_name_abv
    )
  
  # grava a tabela n_obs_uc.rds no diretório data/
  readr::write_rds(
    n_obs_uc,
    file = paste0(
      stringr::str_remove(
        getwd(), 
        "doc"
      ),
      "/data/n_obs_uc.rds"
    )
  )
  
  # retorna a tabela com o número de observações por UC
  return(n_obs_uc)
}

# Descrição
# gera tabela dinâmica dos dados selecionados

gerar_tabdin_n_obs_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data/n_obs_uc.rds'
      )
    )
) {
  # gerar tabela dinâmica dos dados do número de observações por uc
  tabdin_n_ub_uc <- dados |> 
    DT::datatable(
      filter = list(
        position = "top"
      )
    )
  
  # retornar a tabela dinamica do número de observações por uc
  return(tabdin_n_ub_uc)
}

# Descrição
# fornece o número total de observações por espécie.

contar_n_obs_sp <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          "doc"
        ),
        "/data/dados_selecionados.rds"
      )
    )
) {
  # gera o número de observaçẽos por espécie
  n_obs_sp <- dados |> 
    # gerar nome das sp abreviados
    # mutate(sp_abv = ) |> 
    dplyr::count(
      sp#,
      #sp_abv
    )
  
  # grava a tabela n_obs_sp.rds no diretório data/
  readr::write_rds(
    n_obs_sp,
    file = paste0(
      stringr::str_remove(
        getwd(), 
        "doc"
      ),
      "/data/n_obs_sp.rds"
    )
  )
  
  # retorna o número de observaçẽos por espécie
  return(n_obs_sp)
}

# Descrição
# gera tabela dinâmica do número de observações por espécies

gerar_tabdin_n_obs_sp <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          pattern = "doc"
        ),
        '/data/n_obs_sp.rds'
      )
    )
) {
  # gerar tabela dinâmica dos dados do número de observações por sp
  tabdin_n_obs_sp <- dados |> 
    DT::datatable(
      filter = list(
        position = "top"
      )
    )
  
  # retornar a tabela dinamica do número de observações por sp
  return(tabdin_n_obs_sp)
}

# Descrição

plotar_n_sp_uc_interativo <- function(
    dados = readr::read_rds(
      file = paste0(
        stringr::str_remove(
          getwd(), 
          "doc"
        ),
        "/data/n_obs_sp.rds"
      )
    )
) {
  # carregar pacote necessário
  #library(patchwork)
  
  # desenha o gráfico com mais de 1000 observações
  mais_mil_obs <- dados |>  
    dplyr::filter(n %in% 1001:2497) |> 
    dplyr::mutate(
      sp = forcats::fct_reorder(
        sp,
        dplyr::desc(n)
      )
    ) |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = sp, # substituir por sp_abv
                 y = n) +
    ggplot2::geom_col(fill = "chartreuse4") +
    #ggplot2::geom_label() +
    ggplot2::labs(
      x = "Nome das espécies",
      y = "Número de observações"
    ) +
    ggplot2::theme_minimal(14)
  
  # transforma em grafico interativo
  mais_mil_obs <- plotly::ggplotly(mais_mil_obs)
  
  # desenha o gráfico com 501 a 1000 observações
  quinehtos_mil_obs <- dados  |>  
    dplyr::mutate(
      sp = forcats::fct_reorder(
        sp,
        dplyr::desc(n)
      )
    ) |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = sp, # substituir por sp_abv
                 y = n) +
    ggplot2::geom_col(fill = "chartreuse4") +
    #ggplot2::geom_label() +
    ggplot2::labs(
      x = "Nome das espécies",
      y = "Número de observações"
    ) +
    ggplot2::theme_minimal(14)
  
  # transforma em gráfico interativo
  quinehtos_mil_obs <- plotly::ggplotly(quinehtos_mil_obs)
  
  # desenha o gráfico com 101 a 500 observações
  cem_quintas_obs <- dados  |>  
    dplyr::filter(n %in% 101:500) |> 
    dplyr::mutate(
      sp = forcats::fct_reorder(
        sp,
        dplyr::desc(n)
      )
    ) |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = sp, # substituir por sp_abv
                 y = n) +
    ggplot2::geom_col(fill = "chartreuse4") +
    #ggplot2::geom_label() +
    ggplot2::labs(
      x = "Nome das espécies",
      y = "Número de observações"
    ) +
    ggplot2::theme_minimal(14)
  
  # tansforma em gráfico interativo
  cem_quintas_obs <- plotly::ggplotly(cem_quintas_obs)
  
  # desenha o gráfico com menos de 100 observações
  uma_cem_obs <- dados  |>  
    dplyr::filter(n < 100) |> 
    dplyr::mutate(
      sp = forcats::fct_reorder(
        sp,
        dplyr::desc(n)
      )
    ) |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = sp, # substituir por sp_abv
                 y = n) +
    ggplot2::geom_col(fill = "chartreuse4") +
    #ggplot2::geom_label() +
    ggplot2::labs(
      x = "Nome das espécies",
      y = "Número de observações"
    ) +
    ggplot2::theme_minimal(14)
  
  # tansforma em gráfico interativo
  uma_cem_obs <- plotly::ggplotly(uma_cem_obs)
  
  # organizar os gráficos
  # fig <- ggpubr::ggarrange(
  #  hist,
  # box, 
  #pontos,
  #nrow = 3
  #)
  fig <-  plotly::subplot(
    mais_mil_obs, 
    quinehtos_mil_obs,
    cem_quintas_obs,
    uma_cem_obs,
    nrows = 4,
    titleX = TRUE,
    titleY = TRUE,
    shareX = TRUE,
    shareY = TRUE
  )
  
  # retronar os gráficos
  return(fig)
}

