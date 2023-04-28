\name{carregar_dados_brutos_xlsx}
\alias{carregar_dados_brutos_xlsx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Carrega os dados brutos originais em formato .xlsx 
}
\description{
A função `carregar_dados_brutos_xlsx()` carrega o arquivo da planilha de dados originais, `"Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP.xlsx"` e salva na pasta data-raw um novo arquivo dados_brutos.rds. 
}
\usage{
carregar_dados_brutos_xlsx(dados = "/data-raw/monitora_masto_aves_2023_04_04.xlsx")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dados}{
Carrega da pasta data-raw um arquivo em formato rds. Por configruação, carrega o arquivo monitora_masto_ave_2023_04_04.xlsx
}
}
\details{
Para aplicar a função é necessário utilizar a versão R 4.2.2 e estar com as versões atualizadas dos pacotes
}
\author{
Vitor N. T. Borges-Júnior
}

\examples{
dados_brutos <- carregar_dados_brutos_rds()
dados_brutos

## The function is currently defined as
carregar_dados_brutos_xlsx <- function(
  dados = readxl::read_excel(
    path = paste0(
      here::here(),
      "/data-raw/monitora_masto_aves_2023_04_04.xlsx"
    ),
    sheet = "dados brutos"
    )
  ) {
  # grava uma versão dados_brutos.rds no diretório data-raw
  readr::write_rds(
    dados,
    file = paste0(
      here::here(),
      "/data-raw/dados_brutos.rds"
    )
  )
  
    # retorna os dados butos
  return(dados)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
