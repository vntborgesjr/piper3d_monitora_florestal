\name{carregar_dados_brutos_rds}
\alias{carregar_dados_brutos_rds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Carrega os dados brutos em formato .rsd 
}
\description{
A função `carregar_dados_brutos_rds()` carrega o arquivo `dados_brutos.rds` diretamente da pasta `data-raw`. Os dados carregados equivalem a planilha de dados originais, `"Planilha Oficial consolidada de Masto-aves 2014-21 Validada CEMAVE CPB CENAP.xlsx"`. 
}
\usage{
carregar_dados_brutos_rds(dados = "data-raw/dados_brutos.rds")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dados}{
        Carrega da pasta data-raw um arquivo em formato rds. Por configruação, carrega o arquivo dados_brutos.rds.
}
}
\details{
Para aplicar a função é necessário utilizar a versão R 4.2.2 e estar com as versões atualizadas dos pacotes readr e here.
}
\author{
Vitor N. T. Borges-Júnior
}
\examples{
dados_brutos <- carregar_dados_brutos_rds()
dados_brutos

## The function is currently defined as
carregar_dados_brutos_rds <- function(
  dados = readr::read_rds(
    file = paste0(here::here(),
      "/data-raw/dados_brutos.rds"
    )
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
