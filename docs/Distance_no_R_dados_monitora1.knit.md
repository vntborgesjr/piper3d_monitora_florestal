
<!-- rnb-text-begin -->

---
title: "Distance no R com dados 'modelo'"
output: html_notebook
---

Retirado do artigo Miller et al. (2019). Distance sampling in R. Journal of Statistical Sofware 89(1)


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBjYXJyZWdhciBwYWNvdGVzIFxubGlicmFyeShEaXN0YW5jZSlcbmxpYnJhcnkoZHBseXIpXG5saWJyYXJ5KERUKVxubGlicmFyeShmbGV4dGFibGUpXG5saWJyYXJ5KGdncGxvdDIpXG5saWJyYXJ5KGx1YnJpZGF0ZSlcbmxpYnJhcnkocGxvdGx5KVxubGlicmFyeShyZWFkcilcbmxpYnJhcnkocmVhZHhsKVxubGlicmFyeShzdHJpbmdyKVxubGlicmFyeSh0aWJibGUpXG5saWJyYXJ5KHRpZHlyKVxuXG4jIGNhcnJlZ2FyIGFzIGZ1bsOnw7VlcyBkYSBwYXN0YSBSXG4jIGNhcnJlZ2FyIGZ1bsOnw6NvIHNjcmlwdF9jYXJyZWdhcl9mdW7Dp8O1ZXNfcGFzdGFfci5SXG5zb3VyY2UoXG4gIHBhc3RlMChcbiAgICBoZXJlOjpoZXJlKCksXG4gICAgXCIvUi9taW5oYXNfZnVuY29lcy5SXCJcbiAgKVxuKVxuYGBgIn0= -->

```r
# carregar pacotes 
library(Distance)
library(dplyr)
library(DT)
library(flextable)
library(ggplot2)
library(lubridate)
library(plotly)
library(readr)
library(readxl)
library(stringr)
library(tibble)
library(tidyr)

# carregar as funções da pasta R
# carregar função script_carregar_funções_pasta_r.R
source(
  paste0(
    here::here(),
    "/R/minhas_funcoes.R"
  )
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Formatação do conjunto de dados

Variáveis necessárias para o `data.frame`:

-   `Region.Label`: vetor fator com o estrato contendo o transecto (pode ser uma estratificação pré-amostragem - UCs - ou pós-amostragem - ex. região, estado, bioma)

-   `Area`: vetor numérico contendo a área do estrato;

-   `Sample.Label`: vetor númerico contendo a identidade (ID) do transecto

-   `object`: nome adicional, ver seção 6;

-   `detected`: nome adicional, ver seção 6;

-   `Effort`: vetor númerico contendo o esforço do transecto (para linhas seu comprimento, para pontos o número de vezes que o ponto foi visitado)

-   `size`: vetor numérico copntendo o tamanho do grupo observado;

-   `distance`: vetor numérico de distâncias observadas;

-   `Month`:

-   `OBs`:

-   `Sp`:

-   `mas`:

-   `HAS`:

-   `Study.Area`:

Transectos que foram amostrados, mas que não tiveram observações (n = 0) devem ser incluídos no conjunto de dados com `NA` nas observações de distância e qualquer outra covariael para a qual não se tenha observação.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBjdXRpYV90YXBfYXJhcCB8PiBcbiMgICBjb21wbGV0ZShSZWdpb24uTGFiZWwsIFNhbXBsZS5MYWJlbCwgc3BfbmFtZSkgfD4gXG4jICAgZGF0YXRhYmxlKGZpbHRlciA9IGxpc3QocG9zaXRpb24gPSBcInRvcFwiKSlcbmBgYCJ9 -->

```r
# cutia_tap_arap |> 
#   complete(Region.Label, Sample.Label, sp_name) |> 
#   datatable(filter = list(position = "top"))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Jogar a imputacao de `NA`s pra dentro da funcao carregar dados completos.

# Primeira espécie para dados com repetição

## *Dasyprocta croconota* na **Resex Tapajós-Arapiuns**

### Carregar dados


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBjYXJyZWdhciBkYWRvc1xuY3V0aWFfdGFwX2FyYXAgPC0gdHJhbnNmb3JtYV9wYXJhX2Rpc3RhbmNlUl9jb21fcmVwZXRpY2FvX2ZpbHRyYV91Y19zcChcbiAgbm9tZV91YyA9IFwiUmVzZXggVGFwYWpvcy1BcmFwaXVuc1wiLFxuICBub21lX3NwID0gXCJEYXN5cHJvY3RhIGNyb2Nvbm90YVwiXG4pIFxuXG5jdXRpYV90YXBfYXJhcFxuYGBgIn0= -->

```r
# carregar dados
cutia_tap_arap <- transforma_para_distanceR_com_repeticao_filtra_uc_sp(
  nome_uc = "Resex Tapajos-Arapiuns",
  nome_sp = "Dasyprocta croconota"
) 

cutia_tap_arap
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Dsitribuição das distâncias perpendiculares


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY3V0aWFfdGFwX2FyYXAgfD4gXG4gIGRyb3BfbmEoZGlzdGFuY2UpIHw+IFxucGxvdGFyX2Rpc3RyaWJ1aWNhb19kaXN0YW5jaWFfaW50ZXJhdGl2byhsYXJndXJhX2NhaXhhID0gMSlcbmBgYCJ9 -->

```r
cutia_tap_arap |> 
  drop_na(distance) |> 
plotar_distribuicao_distancia_interativo(largura_caixa = 1)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Determinando a distância para truncar os dados


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBjb25kdXogYSBzZWxlY2FvIGRhIG1lbGhvciBkaXN0YW5jaWEgZGUgdHJ1bmNhbWVudG8gYSBwYXJ0aXIgZG8gYWpzdXRlIGRlIG1vZGVsb3MgY29tIGZ1bmNhbyBkZSBkZXRlY2NhbyBoYWxmLW5vcm1hbCBzZW0gdGVybW9zIGRlIGFqdXN0ZVxuY3V0aWFfdGFwX2FyYXBfZGlzdF90cnVuYyA8LSBjdXRpYV90YXBfYXJhcCB8PiBcbiAgc2VsZWNpb25hcl9kaXN0YW5jaWFfdHJ1bmNhbWVudG8oKVxuXG5jdXRpYV90YXBfYXJhcF9kaXN0X3RydW5jJHNlbGVjYW9cbmBgYCJ9 -->

```r
# conduz a selecao da melhor distancia de truncamento a partir do ajsute de modelos com funcao de deteccao half-normal sem termos de ajuste
cutia_tap_arap_dist_trunc <- cutia_tap_arap |> 
  selecionar_distancia_truncamento()

cutia_tap_arap_dist_trunc$selecao
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucGxvdGFyX2Z1bmNhb19kZXRlY2Nhb19zZWxlY2FvX2Rpc3RhbmNpYV90cnVuY2FtZW50byhjdXRpYV90YXBfYXJhcF9kaXN0X3RydW5jKVxuYGBgIn0= -->

```r
plotar_funcao_deteccao_selecao_distancia_truncamento(cutia_tap_arap_dist_trunc)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Ajustando funções de detecção no R com a melhor distância de truncamento

#### *Half-Normal* sem termos de ajuste e com termos de ajuste Cosseno e Polinomial de Hermite truncando os dados de distância em 25%


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBhanVzdGFuZG8gYSBmdW7Dp8OjbyBkZSBkZXRlY8Onw6NvIGhhbGYtbm9ybWFsIHBhcmEgdW0gdHJ1bmNhbWVudG8gZGUgMjUlIGRvcyBkYWRvc1xuY3V0aWFfdGFwX2FyYXBfaG4gPC0gY3V0aWFfdGFwX2FyYXAgfD4gXG4gIGFqdXN0ZV9tb2RlbG9zX2Rpc3RhbmNlX2huKHRydW5jYW1lbnRvID0gXCIyNSVcIilcbmBgYCJ9 -->

```r
# ajustando a função de detecção half-normal para um truncamento de 25% dos dados
cutia_tap_arap_hn <- cutia_tap_arap |> 
  ajuste_modelos_distance_hn(truncamento = "25%")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


#### *Hazard-Rate* sem termos de ajuste e com termos de ajuste Cosseno e Polinomial de Hermite


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBhanVzdGFuZG8gYSBmdW7Dp8OjbyBkZSBkZXRlY8Onw6NvIGhhemFyZC1yYXRlIHBhcmEgdW0gdHJ1bmNhbWVudG8gZGUgMjUlIGRvcyBkYWRvc1xuY3V0aWFfdGFwX2FyYXBfaHIgPC0gY3V0aWFfdGFwX2FyYXAgfD4gXG4gIGFqdXN0ZV9tb2RlbG9zX2Rpc3RhbmNlX2hyKHRydW5jYW1lbnRvID0gXCIyNSVcIilcbmBgYCJ9 -->

```r
# ajustando a função de detecção hazard-rate para um truncamento de 25% dos dados
cutia_tap_arap_hr <- cutia_tap_arap |> 
  ajuste_modelos_distance_hr(truncamento = "25%")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


#### *Uniform* sem termos de ajuste e com termos de ajuste Cosseno e Polinomial de Hermite


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBhanVzdGFuZG8gYSBmdW7Dp8OjbyBkZSBkZXRlY8Onw6NvIHVuaWZvcm1lIHBhcmEgdW0gdHJ1bmNhbWVudG8gZGUgMjUlIGRvcyBkYWRvc1xuY3V0aWFfdGFwX2FyYXBfdW5pZiA8LSBjdXRpYV90YXBfYXJhcCB8PiBcbiAgYWp1c3RlX21vZGVsb3NfZGlzdGFuY2VfdW5pZih0cnVuY2FtZW50byA9IFwiMjUlXCIpXG5gYGAifQ== -->

```r
# ajustando a função de detecção uniforme para um truncamento de 25% dos dados
cutia_tap_arap_unif <- cutia_tap_arap |> 
  ajuste_modelos_distance_unif(truncamento = "25%")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


###### Seleção de modelos com funções de detecção e termos de ajuste com melhor ajuste aos dados
 

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubGlzdGFfbW9kZWxvc19hanVzdGFkb3MgPC0gbGlzdChcbiAgYGhhbGYtbm9ybWFsYCA9IGN1dGlhX3RhcF9hcmFwX2huLCBcbiAgYGhhemFyZC1yYXRlYCA9IGN1dGlhX3RhcF9hcmFwX2hyLCBcbiAgdW5pZm9ybWUgPSBjdXRpYV90YXBfYXJhcF91bmlmXG4pXG5cbnNlbGVjYW9fZnVuY2FvX2RldGVjY2FvX3Rlcm1vX2Fqc3V0ZSA8LSBzZWxlY2lvbmFyX2Z1bmNhb19kZXRlY2Nhb190ZXJtb19hanVzdGUobGlzdGFfbW9kZWxvc19hanVzdGFkb3MpXG5cbnNlbGVjYW9fZnVuY2FvX2RldGVjY2FvX3Rlcm1vX2Fqc3V0ZVxuYGBgIn0= -->

```r
lista_modelos_ajustados <- list(
  `half-normal` = cutia_tap_arap_hn, 
  `hazard-rate` = cutia_tap_arap_hr, 
  uniforme = cutia_tap_arap_unif
)

selecao_funcao_deteccao_termo_ajsute <- selecionar_funcao_deteccao_termo_ajuste(lista_modelos_ajustados)

selecao_funcao_deteccao_termo_ajsute
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


#### Resumir o resultado dos modelos

O que tem que ter?

Os gráficos (probabilidade de detecção pela distância, com a curva ajustada, exemplo abaixo, fazer no ggplot), resultado do goodness of fit (gof_ds()), cada modelo vai ter que ter um nome diferente numa tabela(?)


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubW9kZWxvc19zZWxlY2lvbmFkb3MgPC0gbGlzdChcbiAgY3V0aWFfdGFwX2FyYXBfaHIkYFNlbSB0ZXJtb2AsXG4gIGN1dGlhX3RhcF9hcmFwX3VuaWYkQ29zc2VubyxcbiAgY3V0aWFfdGFwX2FyYXBfaG4kQ29zc2VubyxcbiAgY3V0aWFfdGFwX2FyYXBfdW5pZiRgUG9saW5vbWlhbCBzaW1wbGVzYCxcbiAgY3V0aWFfdGFwX2FyYXBfaG4kYFNlbSB0ZXJtb2BcbilcblxucGxvdGFyX2Z1bmNhb19kZXRlY2Nhb19tb2RlbG9zX3NlbGVjaW9uYWRvcyhtb2RlbG9zX3NlbGVjaW9uYWRvcylcbmBgYCJ9 -->

```r
modelos_selecionados <- list(
  cutia_tap_arap_hr$`Sem termo`,
  cutia_tap_arap_unif$Cosseno,
  cutia_tap_arap_hn$Cosseno,
  cutia_tap_arap_unif$`Polinomial simples`,
  cutia_tap_arap_hn$`Sem termo`
)

plotar_funcao_deteccao_modelos_selecionados(modelos_selecionados)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->

### Bondade de ajuste

Para visualizar quão bem a função de detecção se ajusta aos dados quanto temos as distâncias exatas podemos usar um plot de quantis empíricos x teóricos (Q-Q plot). Ele compara a função de distribuição cumulativa (CDF) dos valores ajustados da função detecção a distribuição empírica dos dados (EDF).

Também podemos usar o teste de Cramér-von Mises para testar se os pontos da EDF e da CDF tem origem na mesma distribuição. O teste usa a soma de todas as distâncias entre um ponto e a linha y = x para formar a estatística a ser testada. Um resultado significativo fornece evidência contra a hiipótese nula, sugerindo que o modelo não se ajusta bem aos dados.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBjb25kdXppbmRvIG8gdGVzdGUgZGZlIGJvbmRhZGVkZSBhanVzdGUgZGUgQ3JhbWVyLXZvbiBNaXNlc1xuZ29mX2RzKGN1dGlhX3RhcF9hcmFwX2huJGBTZW0gdGVybW9gKVxuYGBgIn0= -->

```r
# conduzindo o teste dfe bondadede ajuste de Cramer-von Mises
gof_ds(cutia_tap_arap_hn$`Sem termo`)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


O resutlado do teste aponta que o modelo *Half-normal* deve ser descartado.

Testes de bondade de ajuste de chi-quadrado são gerados usando a função `gof_ds` quando as distâncias forneceidas estão categorizadas.

### Seleção de Modelos

Uma vez que temos um conjunto de modelos plausíveis, podemos utilizar o cirtériode informaçãode Akaike (AIC) para selecionar entre os modelos o que melhor se ajusta aos dados utilizando a função `summarize_ds_models`.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBnZXJhbmRvIHVtYSB0YWJlbGEgZGUgc2VsZcOnw6NvIGRlIG1vZGVsb3MgdXNhbmRvIEFJQ1xuc3VtbWFyaXplX2RzX21vZGVscyhjdXRpYV9obiwgY3V0aWFfaHJfdGltZSwgY3V0aWFfaHJfdGltZV9zaXplKVxuYGBgIn0= -->

```r
# gerando uma tabela de seleção de modelos usando AIC
summarize_ds_models(cutia_hn, cutia_hr_time, cutia_hr_time_size)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


O melhor modelo é o Hazard-rate com tempo de senso e tamanho do grupo como covariáveis.

## Estimando a abundância e a variância

### Estimando abundância e variância no R

Para obter a abundância na região de estudo, primeiro calculamos a abundância na área amostrada para obter $N_c$ e em seguida escalonamos esse valor para toda a área de estudo multiplicando $N_c$ pela razão entre a área amostrada e a área da região. Para estimar a abundância na área amostrada, utilizamos as estimativas de probabilidade de detecção no estimador de Horvitz-Thompson.

Quando fornecemos os dados no formato correto ("flatfile") `ds` irá automaticamente calcular as estimativas de abundância baseado nas informações de amostragem presenta nos dados.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VtbWFyeShjdXRpYV9obilcbmBgYCJ9 -->

```r
summary(cutia_hn)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


1.  Summary statistics: fornece as áreas, aŕea de amostragem, esforço, número de observações, número de transectos, taxa de encontro, seus erros padrões e coeficientes de variação para cada estrato;

2.  Abundance: fornece estimativas, erros padrões, coeficientesde variação, intervalos de confiança inferior e superior, graus de liberdade para a estimativa de abundância de cada estrato;

3.  Densidade: lista as mesmas estatísticas de Abundance, só que para densidade.

## **Funções Exploratórias Adicionais**

`contar_n_repeticoes_trilha()` - conta o número de vezes que cada trilha foi visitada

## Adicionando covariavel

Ajuste *Hermite pollynomial* usa od código `"herm"` e polinomial simples `"poly"`.

Podemos incluir covariáveis utilizando o argumento `formula = ~ ...`. Abaixo, está especificado um modelo "Hazard-rate" para os dados de cutia q ue inclui o tempo de senso como covariável e uma distância limite de 20m.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY3V0aWFfaHJfdGltZSA8LSBjdXRpYV90YXBfYXJhcF8xNSB8PiBcbiAgZHModHJ1bmNhdGlvbiA9IDIwLFxuICAgICBrZXkgPSBcImhyXCIsXG4gICAgIGZvcm11bGEgPSB+IGNlbnNlX3RpbWUpXG5gYGAifQ== -->

```r
cutia_hr_time <- cutia_tap_arap_15 |> 
  ds(truncation = 20,
     key = "hr",
     formula = ~ cense_time)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Adicionando uma segunda covariável: tamanho do grupo.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY3V0aWFfaHJfdGltZV9zaXplIDwtIGRzKGRhdGEgPSBjdXRpYV90YXBfYXJhcF8xNSxcbiAgICAgICAgICAgICAgICAgICAgIHRydW5jYXRpb24gPSAyMCxcbiAgICAgICAgICAgICAgICAgICAgIHRyYW5zZWN0ID0gXCJsaW5lXCIsXG4gICAgICAgICAgICAgICAgICAgICBrZXkgPSBcImhyXCIsXG4gICAgICAgICAgICAgICAgICAgICBmb3JtdWxhID0gfiBjZW5zZV90aW1lICsgc2l6ZSlcbmBgYCJ9 -->

```r
cutia_hr_time_size <- ds(data = cutia_tap_arap_15,
                     truncation = 20,
                     transect = "line",
                     key = "hr",
                     formula = ~ cense_time + size)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucGxvdChjdXRpYV9ocl90aW1lKVxucGxvdChjdXRpYV9ocl90aW1lX3NpemUpXG5gYGAifQ== -->

```r
plot(cutia_hr_time)
plot(cutia_hr_time_size)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Cutias da ESEC Terra do Meio para diferentes distâncias de truncamento


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY3V0aWFfZXNlY190ZXJyYV9tZWlvIDwtIHRyYW5zZm9ybWFyX3BhcmFfZGlzdGFuY2VSX2NvdmFyaWF2ZWlzKCkgfD4gXG4gIGZpbHRlcihcbiAgICBSZWdpb24uTGFiZWwgPT0gXCJFc2VjIGRhIFRlcnJhIGRvIE1laW9cIixcbiAgICBzcF9uYW1lID09IFwiRGFzeXByb2N0YSBjcm9jb25vdGFcIlxuICApIHw+IFxuICBkcm9wX25hKGRpc3RhbmNlKVxuICBcbmBgYCJ9 -->

```r
cutia_esec_terra_meio <- transformar_para_distanceR_covariaveis() |> 
  filter(
    Region.Label == "Esec da Terra do Meio",
    sp_name == "Dasyprocta croconota"
  ) |> 
  drop_na(distance)
  
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBkZXNlbmhhIG8gZ3JhZmljbyBjb20gYSBkaXN0cmlidWljYW8gZGUgZGlzdGFuY2lhcyBwZXJwZW5kaWN1bGFyZXNcbmN1dGlhX2VzZWNfdGVycmFfbWVpbyB8PiBcbiAgZmlsdGVyKGRpc3RhbmNlID49IDEsXG4gICAgICAgICBkaXN0YW5jZSA8IDE1KSB8PiBcbiAgcGxvdGFyX2Rpc3RyaWJ1aWNhb19kaXN0YW5jaWFfaW50ZXJhdGl2byhsYXJndXJhX2NhaXhhID0gMSlcbmBgYCJ9 -->

```r
# desenha o grafico com a distribuicao de distancias perpendiculares
cutia_esec_terra_meio |> 
  filter(distance >= 1,
         distance < 15) |> 
  plotar_distribuicao_distancia_interativo(largura_caixa = 1)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


#### *Half-Normal* sem termos de ajuste e com termos de ajuste Cosseno e Polinomial de Hermite

Ajustando um modelo ao dados das cutias *Dasyprocta croconota*, configurando uma distância limite de 20m e usando *Half-normal* como *key function* usando o argumento `key`, sem termo de ajuste.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY3V0aWFfZXNlY190ZXJyYV9tZWlvX2ZpbHRyYWRvXG5gYGAifQ== -->

```r
cutia_esec_terra_meio_filtrado
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY3V0aWFfZXNlY190ZXJyYV9tZWlvX2ZpbHRyYWRvIDwtIGN1dGlhX2VzZWNfdGVycmFfbWVpbyB8PiBcbiAgZmlsdGVyKGRpc3RhbmNlID49IDEsXG4gICAgICAgICBkaXN0YW5jZSA8IDE1KVxuIyBhanVzdGFuZG8gYSBmdW7Dp8OjbyBkZSBkZXRlY8Onw6NvIHBhcmEgdW1hIGRpc3RhbmNpYSBkZSB0cnVuY2FtZW50byBkZSAyMCwgMTUsIDEwIGUgNSBtZXRyb3NcbiMgZHNpdGFuY2lhcyBkZSB0cnVuY2FtZW50b1xuZGlzdF90cnVuY2FtZW50byA8LSBsaXN0KFxuICAjYDIwIG1ldHJvc2AgPSAyMCwgXG4gIGAxNSBtZXRyb3NgID0gMTUsIFxuICBgMTIgbWV0cm9zYCA9IDEyLFxuICBgMTAgbWV0cm9zYCA9IDEwXG4pXG5cbiMgS2V5IGZ1bmN0aW9uIC0gSGFsZi1ub3JtYWwgXG5jdXRpYV9lc2VjX3RlcnJhX21laW9faG4gPC0gcHVycnI6Om1hcChcbiAgZGlzdF90cnVuY2FtZW50byxcbiAgXFwoLngpIGFqdXN0ZV9tb2RlbG9zX2Rpc3RhbmNlX2huKFxuICAgIGN1dGlhX2VzZWNfdGVycmFfbWVpb19maWx0cmFkbywgXG4gICAgdHJ1bmNhbWVudG8gPSAueFxuICApXG4gIClcbmBgYCJ9 -->

```r
cutia_esec_terra_meio_filtrado <- cutia_esec_terra_meio |> 
  filter(distance >= 1,
         distance < 15)
# ajustando a função de detecção para uma distancia de truncamento de 20, 15, 10 e 5 metros
# dsitancias de truncamento
dist_truncamento <- list(
  #`20 metros` = 20, 
  `15 metros` = 15, 
  `12 metros` = 12,
  `10 metros` = 10
)

# Key function - Half-normal 
cutia_esec_terra_meio_hn <- purrr::map(
  dist_truncamento,
  \(.x) ajuste_modelos_distance_hn(
    cutia_esec_terra_meio_filtrado, 
    truncamento = .x
  )
  )
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY3V0aWFfZXNlY190ZXJyYV9tZWlvX2huXG5gYGAifQ== -->

```r
cutia_esec_terra_meio_hn
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


#### *Hazard-Rate* sem termos de ajuste e com termos de ajuste Cosseno e Polinomial de Hermite

Ajustando um modelo ao dados da cutia *Dasyprocta croconota*, configurando uma distância limite de 20m e usando *Hazard rate* como *key function* usando o argumento `key`.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBhanVzdGFuZG8gYSBmdW7Dp8OjbyBkZSBkZXRlY8Onw6NvIHBhcmEgdW1hIGRpc3RhbmNpYSBkZSB0cnVuY2FtZW50byBkZSAyMCwgMTUsIDEwIGUgNSBtZXRyb3NcbiMgS2V5IGZ1bmN0aW9uIC0gSGF6YXJkLXJhdGVcbmN1dGlhX2VzZWNfdGVycmFfbWVpb19ociA8LSBwdXJycjo6bWFwKFxuICBkaXN0X3RydW5jYW1lbnRvLFxuICBcXCgueCkgYWp1c3RlX21vZGVsb3NfZGlzdGFuY2VfaHIoXG4gICAgY3V0aWFfZXNlY190ZXJyYV9tZWlvX2ZpbHRyYWRvLCBcbiAgICB0cnVuY2FtZW50byA9IC54XG4gIClcbiAgKVxuYGBgIn0= -->

```r
# ajustando a função de detecção para uma distancia de truncamento de 20, 15, 10 e 5 metros
# Key function - Hazard-rate
cutia_esec_terra_meio_hr <- purrr::map(
  dist_truncamento,
  \(.x) ajuste_modelos_distance_hr(
    cutia_esec_terra_meio_filtrado, 
    truncamento = .x
  )
  )
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


#### *Uniform* sem termos de ajuste e com termos de ajuste Cosseno e Polinomial de Hermite

Ajustando um modelo ao dados das cutias *Dasyprocta croconota*, configurando uma distância limite de 20m e usando *Uniform* como *key function* usando o argumento `key`, sem termo de ajuste.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBhanVzdGFuZG8gYSBmdW7Dp8OjbyBkZSBkZXRlY8Onw6NvIHBhcmEgdW1hIGRpc3RhbmNpYSBkZSB0cnVuY2FtZW50byBkZSAyMCwgMTUsIDEwIGUgNSBtZXRyb3NcbiMgS2V5IGZ1bmN0aW9uIC0gVW5pZm9ybVxuY3V0aWFfZXNlY190ZXJyYV9tZWlvX3VuaWYgPC0gcHVycnI6Om1hcChcbiAgZGlzdF90cnVuY2FtZW50byxcbiAgXFwoLngpIGFqdXN0ZV9tb2RlbG9zX2Rpc3RhbmNlX3VuaWYoXG4gICAgY3V0aWFfdGFwX2FyYXAsIFxuICAgIHRydW5jYW1lbnRvID0gLnhcbiAgKVxuICApXG5gYGAifQ== -->

```r
# ajustando a função de detecção para uma distancia de truncamento de 20, 15, 10 e 5 metros
# Key function - Uniform
cutia_esec_terra_meio_unif <- purrr::map(
  dist_truncamento,
  \(.x) ajuste_modelos_distance_unif(
    cutia_tap_arap, 
    truncamento = .x
  )
  )
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


###### Seleção de modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VtbWFyaXplX2RzX21vZGVscyhcbiAgY3V0aWFfZXNlY190ZXJyYV9tZWlvX2huJGAyMCBtZXRyb3NgJGBTZW0gdGVybW9gLFxuY3V0aWFfZXNlY190ZXJyYV9tZWlvX2huJGAyMCBtZXRyb3NgJENvc3Nlbm8sXG4gIGN1dGlhX2VzZWNfdGVycmFfbWVpb19obiRgMjAgbWV0cm9zYCRgSGVybWl0ZSBwb2xpbm9taWFsYCxcbmN1dGlhX2VzZWNfdGVycmFfbWVpb19ociRgMjAgbWV0cm9zYCRgU2VtIHRlcm1vYCxcbmN1dGlhX2VzZWNfdGVycmFfbWVpb19ociRgMjAgbWV0cm9zYCRDb3NzZW5vLFxuY3V0aWFfZXNlY190ZXJyYV9tZWlvX2hyJGAyMCBtZXRyb3NgJGBQb2xpbm9taWFsIHNpbXBsZXNgLFxuY3V0aWFfZXNlY190ZXJyYV9tZWlvX3VuaWYkYDIwIG1ldHJvc2AkQ29zc2VubyxcbmN1dGlhX2VzZWNfdGVycmFfbWVpb191bmlmJGAyMCBtZXRyb3NgJGBQb2xpbm9taWFsIHNpbXBsZXNgXG4pXG5gYGAifQ== -->

```r
summarize_ds_models(
  cutia_esec_terra_meio_hn$`20 metros`$`Sem termo`,
cutia_esec_terra_meio_hn$`20 metros`$Cosseno,
  cutia_esec_terra_meio_hn$`20 metros`$`Hermite polinomial`,
cutia_esec_terra_meio_hr$`20 metros`$`Sem termo`,
cutia_esec_terra_meio_hr$`20 metros`$Cosseno,
cutia_esec_terra_meio_hr$`20 metros`$`Polinomial simples`,
cutia_esec_terra_meio_unif$`20 metros`$Cosseno,
cutia_esec_terra_meio_unif$`20 metros`$`Polinomial simples`
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VtbWFyaXplX2RzX21vZGVscyhcbiAgY3V0aWFfZXNlY190ZXJyYV9tZWlvX2huJGAxNSBtZXRyb3NgJGBTZW0gdGVybW9gLFxuY3V0aWFfZXNlY190ZXJyYV9tZWlvX2huJGAxNSBtZXRyb3NgJENvc3Nlbm8sXG4gIGN1dGlhX2VzZWNfdGVycmFfbWVpb19obiRgMTUgbWV0cm9zYCRgSGVybWl0ZSBwb2xpbm9taWFsYCxcbmN1dGlhX2VzZWNfdGVycmFfbWVpb19ociRgMTUgbWV0cm9zYCRgU2VtIHRlcm1vYCxcbmN1dGlhX2VzZWNfdGVycmFfbWVpb19ociRgMTUgbWV0cm9zYCRDb3NzZW5vLFxuY3V0aWFfZXNlY190ZXJyYV9tZWlvX2hyJGAxNSBtZXRyb3NgJGBQb2xpbm9taWFsIHNpbXBsZXNgLFxuY3V0aWFfZXNlY190ZXJyYV9tZWlvX3VuaWYkYDE1IG1ldHJvc2AkQ29zc2VubyxcbmN1dGlhX2VzZWNfdGVycmFfbWVpb191bmlmJGAxNSBtZXRyb3NgJGBQb2xpbm9taWFsIHNpbXBsZXNgXG4pXG5gYGAifQ== -->

```r
summarize_ds_models(
  cutia_esec_terra_meio_hn$`15 metros`$`Sem termo`,
cutia_esec_terra_meio_hn$`15 metros`$Cosseno,
  cutia_esec_terra_meio_hn$`15 metros`$`Hermite polinomial`,
cutia_esec_terra_meio_hr$`15 metros`$`Sem termo`,
cutia_esec_terra_meio_hr$`15 metros`$Cosseno,
cutia_esec_terra_meio_hr$`15 metros`$`Polinomial simples`,
cutia_esec_terra_meio_unif$`15 metros`$Cosseno,
cutia_esec_terra_meio_unif$`15 metros`$`Polinomial simples`
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VtbWFyaXplX2RzX21vZGVscyhcbiAgY3V0aWFfZXNlY190ZXJyYV9tZWlvX2huJGAxMCBtZXRyb3NgJGBTZW0gdGVybW9gLFxuY3V0aWFfZXNlY190ZXJyYV9tZWlvX2huJGAxMCBtZXRyb3NgJENvc3Nlbm8sXG4gIGN1dGlhX2VzZWNfdGVycmFfbWVpb19obiRgMTAgbWV0cm9zYCRgSGVybWl0ZSBwb2xpbm9taWFsYCxcbmN1dGlhX2VzZWNfdGVycmFfbWVpb19ociRgMTAgbWV0cm9zYCRgU2VtIHRlcm1vYCxcbmN1dGlhX2VzZWNfdGVycmFfbWVpb19ociRgMTAgbWV0cm9zYCRDb3NzZW5vLFxuY3V0aWFfZXNlY190ZXJyYV9tZWlvX2hyJGAxMCBtZXRyb3NgJGBQb2xpbm9taWFsIHNpbXBsZXNgLFxuY3V0aWFfZXNlY190ZXJyYV9tZWlvX3VuaWYkYDEwIG1ldHJvc2AkQ29zc2VubyxcbmN1dGlhX2VzZWNfdGVycmFfbWVpb191bmlmJGAxMCBtZXRyb3NgJGBQb2xpbm9taWFsIHNpbXBsZXNgXG4pXG5gYGAifQ== -->

```r
summarize_ds_models(
  cutia_esec_terra_meio_hn$`10 metros`$`Sem termo`,
cutia_esec_terra_meio_hn$`10 metros`$Cosseno,
  cutia_esec_terra_meio_hn$`10 metros`$`Hermite polinomial`,
cutia_esec_terra_meio_hr$`10 metros`$`Sem termo`,
cutia_esec_terra_meio_hr$`10 metros`$Cosseno,
cutia_esec_terra_meio_hr$`10 metros`$`Polinomial simples`,
cutia_esec_terra_meio_unif$`10 metros`$Cosseno,
cutia_esec_terra_meio_unif$`10 metros`$`Polinomial simples`
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VtbWFyaXplX2RzX21vZGVscyhcbiAgY3V0aWFfZXNlY190ZXJyYV9tZWlvX2huJGAxMiBtZXRyb3NgJGBTZW0gdGVybW9gLFxuY3V0aWFfZXNlY190ZXJyYV9tZWlvX2huJGAxMiBtZXRyb3NgJENvc3Nlbm8sXG4gIGN1dGlhX2VzZWNfdGVycmFfbWVpb19obiRgMTIgbWV0cm9zYCRgSGVybWl0ZSBwb2xpbm9taWFsYCxcbmN1dGlhX2VzZWNfdGVycmFfbWVpb19ociRgMTIgbWV0cm9zYCRgU2VtIHRlcm1vYCxcbmN1dGlhX2VzZWNfdGVycmFfbWVpb19ociRgMTIgbWV0cm9zYCRDb3NzZW5vLFxuY3V0aWFfZXNlY190ZXJyYV9tZWlvX2hyJGAxMiBtZXRyb3NgJGBQb2xpbm9taWFsIHNpbXBsZXNgLFxuY3V0aWFfZXNlY190ZXJyYV9tZWlvX3VuaWYkYDEyIG1ldHJvc2AkQ29zc2VubyxcbmN1dGlhX2VzZWNfdGVycmFfbWVpb191bmlmJGAxMiBtZXRyb3NgJGBQb2xpbm9taWFsIHNpbXBsZXNgXG4pXG5gYGAifQ== -->

```r
summarize_ds_models(
  cutia_esec_terra_meio_hn$`12 metros`$`Sem termo`,
cutia_esec_terra_meio_hn$`12 metros`$Cosseno,
  cutia_esec_terra_meio_hn$`12 metros`$`Hermite polinomial`,
cutia_esec_terra_meio_hr$`12 metros`$`Sem termo`,
cutia_esec_terra_meio_hr$`12 metros`$Cosseno,
cutia_esec_terra_meio_hr$`12 metros`$`Polinomial simples`,
cutia_esec_terra_meio_unif$`12 metros`$Cosseno,
cutia_esec_terra_meio_unif$`12 metros`$`Polinomial simples`
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Cutias da Parna da Serra do Pardo para diferentes distâncias de truncamento


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY3V0aWFfcGFybmFfc2VycmFfcGFyZG8gPC0gdHJhbnNmb3JtYXJfcGFyYV9kaXN0YW5jZVJfY292YXJpYXZlaXMoKSB8PiBcbiAgZmlsdGVyKFxuICAgIFJlZ2lvbi5MYWJlbCA9PSBcIlBhcm5hIGRhIFNlcnJhIGRvIFBhcmRvXCIsXG4gICAgc3BfbmFtZSA9PSBcIkRhc3lwcm9jdGEgY3JvY29ub3RhXCJcbiAgKSB8PiBcbiAgZHJvcF9uYShkaXN0YW5jZSlcbiAgXG5gYGAifQ== -->

```r
cutia_parna_serra_pardo <- transformar_para_distanceR_covariaveis() |> 
  filter(
    Region.Label == "Parna da Serra do Pardo",
    sp_name == "Dasyprocta croconota"
  ) |> 
  drop_na(distance)
  
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBkZXNlbmhhIG8gZ3JhZmljbyBjb20gYSBkaXN0cmlidWljYW8gZGUgZGlzdGFuY2lhcyBwZXJwZW5kaWN1bGFyZXNcbmN1dGlhX3Bhcm5hX3NlcnJhX3BhcmRvIHw+IFxuICBmaWx0ZXIoZGlzdGFuY2UgPCAxNSxcbiAgICAgICAgIGRpc3RhbmNlID4gMCkgfD4gXG4gIHBsb3Rhcl9kaXN0cmlidWljYW9fZGlzdGFuY2lhX2ludGVyYXRpdm8oKVxuYGBgIn0= -->

```r
# desenha o grafico com a distribuicao de distancias perpendiculares
cutia_parna_serra_pardo |> 
  filter(distance < 15,
         distance > 0) |> 
  plotar_distribuicao_distancia_interativo()
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


#### *Half-Normal* sem termos de ajuste e com termos de ajuste Cosseno e Polinomial de Hermite

Ajustando um modelo ao dados das cutias *Dasyprocta croconota*, configurando uma distância limite de 20m e usando *Half-normal* como *key function* usando o argumento `key`, sem termo de ajuste.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBhanVzdGFuZG8gYSBmdW7Dp8OjbyBkZSBkZXRlY8Onw6NvIHBhcmEgdW1hIGRpc3RhbmNpYSBkZSB0cnVuY2FtZW50byBkZSAyMCwgMTUsIDEwIGUgNSBtZXRyb3NcbiMgZHNpdGFuY2lhcyBkZSB0cnVuY2FtZW50b1xuZGlzdF90cnVuY2FtZW50byA8LSBsaXN0KFxuICBgMjAgbWV0cm9zYCA9IDIwLCBcbiAgYDE1IG1ldHJvc2AgPSAxNSwgXG4gIGAxMCBtZXRyb3NgID0gMTAsXG4gIGA1IG1ldHJvc2AgPSA1XG4pXG5cbiMgS2V5IGZ1bmN0aW9uIC0gSGFsZi1ub3JtYWwgXG5jdXRpYV9wYXJuYV9zZXJyYV9wYXJkb19obiA8LSBwdXJycjo6bWFwKFxuICBkaXN0X3RydW5jYW1lbnRvLFxuICBcXCgueCkgYWp1c3RlX21vZGVsb3NfZGlzdGFuY2VfaG4oXG4gICAgY3V0aWFfcGFybmFfc2VycmFfcGFyZG8sIFxuICAgIHRydW5jYW1lbnRvID0gLnhcbiAgKVxuICApXG5gYGAifQ== -->

```r
# ajustando a função de detecção para uma distancia de truncamento de 20, 15, 10 e 5 metros
# dsitancias de truncamento
dist_truncamento <- list(
  `20 metros` = 20, 
  `15 metros` = 15, 
  `10 metros` = 10,
  `5 metros` = 5
)

# Key function - Half-normal 
cutia_parna_serra_pardo_hn <- purrr::map(
  dist_truncamento,
  \(.x) ajuste_modelos_distance_hn(
    cutia_parna_serra_pardo, 
    truncamento = .x
  )
  )
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY3V0aWFfcGFybmFfc2VycmFfcGFyZG9faG5cbmBgYCJ9 -->

```r
cutia_parna_serra_pardo_hn
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


#### *Hazard-Rate* sem termos de ajuste e com termos de ajuste Cosseno e Polinomial de Hermite

Ajustando um modelo ao dados da cutia *Dasyprocta croconota*, configurando uma distância limite de 20m e usando *Hazard rate* como *key function* usando o argumento `key`.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBhanVzdGFuZG8gYSBmdW7Dp8OjbyBkZSBkZXRlY8Onw6NvIHBhcmEgdW1hIGRpc3RhbmNpYSBkZSB0cnVuY2FtZW50byBkZSAyMCwgMTUsIDEwIGUgNSBtZXRyb3NcbiMgS2V5IGZ1bmN0aW9uIC0gSGF6YXJkLXJhdGVcbmN1dGlhX3Bhcm5hX3NlcnJhX3BhcmRvX2hyIDwtIHB1cnJyOjptYXAoXG4gIGRpc3RfdHJ1bmNhbWVudG8sXG4gIFxcKC54KSBhanVzdGVfbW9kZWxvc19kaXN0YW5jZV9ocihcbiAgICBjdXRpYV9wYXJuYV9zZXJyYV9wYXJkbywgXG4gICAgdHJ1bmNhbWVudG8gPSAueFxuICApXG4gIClcbmBgYCJ9 -->

```r
# ajustando a função de detecção para uma distancia de truncamento de 20, 15, 10 e 5 metros
# Key function - Hazard-rate
cutia_parna_serra_pardo_hr <- purrr::map(
  dist_truncamento,
  \(.x) ajuste_modelos_distance_hr(
    cutia_parna_serra_pardo, 
    truncamento = .x
  )
  )
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


#### *Uniform* sem termos de ajuste e com termos de ajuste Cosseno e Polinomial de Hermite

Ajustando um modelo ao dados das cutias *Dasyprocta croconota*, configurando uma distância limite de 20m e usando *Uniform* como *key function* usando o argumento `key`, sem termo de ajuste.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBhanVzdGFuZG8gYSBmdW7Dp8OjbyBkZSBkZXRlY8Onw6NvIHBhcmEgdW1hIGRpc3RhbmNpYSBkZSB0cnVuY2FtZW50byBkZSAyMCwgMTUsIDEwIGUgNSBtZXRyb3NcbiMgS2V5IGZ1bmN0aW9uIC0gVW5pZm9ybVxuY3V0aWFfcGFybmFfc2VycmFfcGFyZG9fdW5pZiA8LSBwdXJycjo6bWFwKFxuICBkaXN0X3RydW5jYW1lbnRvLFxuICBcXCgueCkgYWp1c3RlX21vZGVsb3NfZGlzdGFuY2VfdW5pZihcbiAgICBjdXRpYV9wYXJuYV9zZXJyYV9wYXJkbywgXG4gICAgdHJ1bmNhbWVudG8gPSAueFxuICApXG4pXG5gYGAifQ== -->

```r
# ajustando a função de detecção para uma distancia de truncamento de 20, 15, 10 e 5 metros
# Key function - Uniform
cutia_parna_serra_pardo_unif <- purrr::map(
  dist_truncamento,
  \(.x) ajuste_modelos_distance_unif(
    cutia_parna_serra_pardo, 
    truncamento = .x
  )
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


###### Seleção de modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VtbWFyaXplX2RzX21vZGVscyhcbiAgY3V0aWFfcGFybmFfc2VycmFfcGFyZG9faG4kYDIwIG1ldHJvc2AkYFNlbSB0ZXJtb2AsXG5jdXRpYV9wYXJuYV9zZXJyYV9wYXJkb19obiRgMjAgbWV0cm9zYCRDb3NzZW5vLFxuICBjdXRpYV9wYXJuYV9zZXJyYV9wYXJkb19obiRgMjAgbWV0cm9zYCRgSGVybWl0ZSBwb2xpbm9taWFsYCxcbmN1dGlhX3Bhcm5hX3NlcnJhX3BhcmRvX2hyJGAyMCBtZXRyb3NgJGBTZW0gdGVybW9gLFxuY3V0aWFfcGFybmFfc2VycmFfcGFyZG9faHIkYDIwIG1ldHJvc2AkQ29zc2VubyxcbmN1dGlhX3Bhcm5hX3NlcnJhX3BhcmRvX2hyJGAyMCBtZXRyb3NgJGBQb2xpbm9taWFsIHNpbXBsZXNgLFxuY3V0aWFfcGFybmFfc2VycmFfcGFyZG9fdW5pZiRgMjAgbWV0cm9zYCRDb3NzZW5vLFxuY3V0aWFfcGFybmFfc2VycmFfcGFyZG9fdW5pZiRgMjAgbWV0cm9zYCRgUG9saW5vbWlhbCBzaW1wbGVzYFxuKVxuYGBgIn0= -->

```r
summarize_ds_models(
  cutia_parna_serra_pardo_hn$`20 metros`$`Sem termo`,
cutia_parna_serra_pardo_hn$`20 metros`$Cosseno,
  cutia_parna_serra_pardo_hn$`20 metros`$`Hermite polinomial`,
cutia_parna_serra_pardo_hr$`20 metros`$`Sem termo`,
cutia_parna_serra_pardo_hr$`20 metros`$Cosseno,
cutia_parna_serra_pardo_hr$`20 metros`$`Polinomial simples`,
cutia_parna_serra_pardo_unif$`20 metros`$Cosseno,
cutia_parna_serra_pardo_unif$`20 metros`$`Polinomial simples`
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VtbWFyaXplX2RzX21vZGVscyhcbiAgY3V0aWFfcGFybmFfc2VycmFfcGFyZG9faG4kYDE1IG1ldHJvc2AkYFNlbSB0ZXJtb2AsXG5jdXRpYV9wYXJuYV9zZXJyYV9wYXJkb19obiRgMTUgbWV0cm9zYCRDb3NzZW5vLFxuICBjdXRpYV9wYXJuYV9zZXJyYV9wYXJkb19obiRgMTUgbWV0cm9zYCRgSGVybWl0ZSBwb2xpbm9taWFsYCxcbmN1dGlhX3Bhcm5hX3NlcnJhX3BhcmRvX2hyJGAxNSBtZXRyb3NgJGBTZW0gdGVybW9gLFxuY3V0aWFfcGFybmFfc2VycmFfcGFyZG9faHIkYDE1IG1ldHJvc2AkQ29zc2VubyxcbmN1dGlhX3Bhcm5hX3NlcnJhX3BhcmRvX2hyJGAxNSBtZXRyb3NgJGBQb2xpbm9taWFsIHNpbXBsZXNgLFxuY3V0aWFfcGFybmFfc2VycmFfcGFyZG9fdW5pZiRgMTUgbWV0cm9zYCRDb3NzZW5vLFxuY3V0aWFfcGFybmFfc2VycmFfcGFyZG9fdW5pZiRgMTUgbWV0cm9zYCRgUG9saW5vbWlhbCBzaW1wbGVzYFxuKVxuYGBgIn0= -->

```r
summarize_ds_models(
  cutia_parna_serra_pardo_hn$`15 metros`$`Sem termo`,
cutia_parna_serra_pardo_hn$`15 metros`$Cosseno,
  cutia_parna_serra_pardo_hn$`15 metros`$`Hermite polinomial`,
cutia_parna_serra_pardo_hr$`15 metros`$`Sem termo`,
cutia_parna_serra_pardo_hr$`15 metros`$Cosseno,
cutia_parna_serra_pardo_hr$`15 metros`$`Polinomial simples`,
cutia_parna_serra_pardo_unif$`15 metros`$Cosseno,
cutia_parna_serra_pardo_unif$`15 metros`$`Polinomial simples`
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VtbWFyaXplX2RzX21vZGVscyhcbiAgY3V0aWFfcGFybmFfc2VycmFfcGFyZG9faG4kYDEwIG1ldHJvc2AkYFNlbSB0ZXJtb2AsXG5jdXRpYV9wYXJuYV9zZXJyYV9wYXJkb19obiRgMTAgbWV0cm9zYCRDb3NzZW5vLFxuICBjdXRpYV9wYXJuYV9zZXJyYV9wYXJkb19obiRgMTAgbWV0cm9zYCRgSGVybWl0ZSBwb2xpbm9taWFsYCxcbmN1dGlhX3Bhcm5hX3NlcnJhX3BhcmRvX2hyJGAxMCBtZXRyb3NgJGBTZW0gdGVybW9gLFxuY3V0aWFfcGFybmFfc2VycmFfcGFyZG9faHIkYDEwIG1ldHJvc2AkQ29zc2VubyxcbmN1dGlhX3Bhcm5hX3NlcnJhX3BhcmRvX2hyJGAxMCBtZXRyb3NgJGBQb2xpbm9taWFsIHNpbXBsZXNgLFxuY3V0aWFfcGFybmFfc2VycmFfcGFyZG9fdW5pZiRgMTAgbWV0cm9zYCRgU2VtIHRlcm1vYCxcbmN1dGlhX3Bhcm5hX3NlcnJhX3BhcmRvX3VuaWYkYDEwIG1ldHJvc2AkQ29zc2VubyxcbmN1dGlhX3Bhcm5hX3NlcnJhX3BhcmRvX3VuaWYkYDEwIG1ldHJvc2AkYFBvbGlub21pYWwgc2ltcGxlc2BcbilcbmBgYCJ9 -->

```r
summarize_ds_models(
  cutia_parna_serra_pardo_hn$`10 metros`$`Sem termo`,
cutia_parna_serra_pardo_hn$`10 metros`$Cosseno,
  cutia_parna_serra_pardo_hn$`10 metros`$`Hermite polinomial`,
cutia_parna_serra_pardo_hr$`10 metros`$`Sem termo`,
cutia_parna_serra_pardo_hr$`10 metros`$Cosseno,
cutia_parna_serra_pardo_hr$`10 metros`$`Polinomial simples`,
cutia_parna_serra_pardo_unif$`10 metros`$`Sem termo`,
cutia_parna_serra_pardo_unif$`10 metros`$Cosseno,
cutia_parna_serra_pardo_unif$`10 metros`$`Polinomial simples`
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VtbWFyaXplX2RzX21vZGVscyhcbiAgY3V0aWFfcGFybmFfc2VycmFfcGFyZG9faG4kYDUgbWV0cm9zYCRgU2VtIHRlcm1vYCxcbmN1dGlhX3Bhcm5hX3NlcnJhX3BhcmRvX2huJGA1IG1ldHJvc2AkQ29zc2VubyxcbiAgY3V0aWFfcGFybmFfc2VycmFfcGFyZG9faG4kYDUgbWV0cm9zYCRgSGVybWl0ZSBwb2xpbm9taWFsYCxcbmN1dGlhX3Bhcm5hX3NlcnJhX3BhcmRvX2hyJGA1IG1ldHJvc2AkYFNlbSB0ZXJtb2AsXG5jdXRpYV9wYXJuYV9zZXJyYV9wYXJkb19ociRgNSBtZXRyb3NgJENvc3Nlbm8sXG5jdXRpYV9wYXJuYV9zZXJyYV9wYXJkb19ociRgNSBtZXRyb3NgJGBQb2xpbm9taWFsIHNpbXBsZXNgLFxuY3V0aWFfcGFybmFfc2VycmFfcGFyZG9fdW5pZiRgNSBtZXRyb3NgJGBTZW0gdGVybW9gLFxuY3V0aWFfcGFybmFfc2VycmFfcGFyZG9fdW5pZiRgNSBtZXRyb3NgJENvc3Nlbm8sXG5jdXRpYV9wYXJuYV9zZXJyYV9wYXJkb191bmlmJGA1IG1ldHJvc2AkYFBvbGlub21pYWwgc2ltcGxlc2BcbilcbmBgYCJ9 -->

```r
summarize_ds_models(
  cutia_parna_serra_pardo_hn$`5 metros`$`Sem termo`,
cutia_parna_serra_pardo_hn$`5 metros`$Cosseno,
  cutia_parna_serra_pardo_hn$`5 metros`$`Hermite polinomial`,
cutia_parna_serra_pardo_hr$`5 metros`$`Sem termo`,
cutia_parna_serra_pardo_hr$`5 metros`$Cosseno,
cutia_parna_serra_pardo_hr$`5 metros`$`Polinomial simples`,
cutia_parna_serra_pardo_unif$`5 metros`$`Sem termo`,
cutia_parna_serra_pardo_unif$`5 metros`$Cosseno,
cutia_parna_serra_pardo_unif$`5 metros`$`Polinomial simples`
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


##### Tentativa de fazer a seleçã ode modelos usando purrr - tentar aninhar mais um map


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucHVycnI6Om1hcF9kZihcbiAgbGlzdChcbiAgICBjdXRpYV9lc2VjX3RlcnJhX21laW9faG4kYDIwIG1ldHJvc2AsXG4gICAgY3V0aWFfZXNlY190ZXJyYV9tZWlvX2hyJGAyMCBtZXRyb3NgXG4gICksXG4gIFxcKC54KSBwdXJycjo6bWFwX2RmKC54LCBcXCgueSkgc3VtbWFyaXplX2RzX21vZGVscygueSkpXG4pXG5cbnB1cnJyOjptYXBfZGYoXG4gIGN1dGlhX2VzZWNfdGVycmFfbWVpb19obiRgMTUgbWV0cm9zYCxcbiAgXFwoLngpIHN1bW1hcml6ZV9kc19tb2RlbHMoLngpXG4pXG5cbnB1cnJyOjptYXBfZGYoXG4gIGN1dGlhX2VzZWNfdGVycmFfbWVpb19obiRgMTAgbWV0cm9zYCxcbiAgXFwoLngpIHN1bW1hcml6ZV9kc19tb2RlbHMoLngpXG4pXG5cbnB1cnJyOjptYXBfZGYoXG4gIGN1dGlhX2VzZWNfdGVycmFfbWVpb19obiRgNSBtZXRyb3NgLFxuICBcXCgueCkgc3VtbWFyaXplX2RzX21vZGVscygueClcbilcbmBgYCJ9 -->

```r
purrr::map_df(
  list(
    cutia_esec_terra_meio_hn$`20 metros`,
    cutia_esec_terra_meio_hr$`20 metros`
  ),
  \(.x) purrr::map_df(.x, \(.y) summarize_ds_models(.y))
)

purrr::map_df(
  cutia_esec_terra_meio_hn$`15 metros`,
  \(.x) summarize_ds_models(.x)
)

purrr::map_df(
  cutia_esec_terra_meio_hn$`10 metros`,
  \(.x) summarize_ds_models(.x)
)

purrr::map_df(
  cutia_esec_terra_meio_hn$`5 metros`,
  \(.x) summarize_ds_models(.x)
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Primeira espécie para dados com repetição

## *Saguinus midas* no **Parna Montanhas do Tumucumaque**

![Fonte: datuopinion.com](https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Ftse4.mm.bing.net%2Fth%3Fid%3DOIP.MKXKT9lNOTspnocS9qpgJAHaFO%26pid%3DApi&f=1&ipt=baacdeefff20d2cc79d1cc7beba75bbb85546c4b53a4ba3305f4cca2e1f2e592&ipo=images)

### Carregar dados


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc2FndWlfbW9udF90dW11YyA8LSB0cmFuc2Zvcm1hcl9wYXJhX2Rpc3RhbmNlUl9jb3ZhcmlhdmVpcygpIHw+IFxuICBmaWx0ZXIoXG4gICAgUmVnaW9uLkxhYmVsID09IFwiUGFybmEgTW9udGFuaGFzIGRvIFR1bXVjdW1hcXVlXCIsXG4gICAgc3BfbmFtZSA9PSBcIlNhZ3VpbnVzIG1pZGFzXCJcbiAgKSB8PiBcbiAgZHJvcF9uYShkaXN0YW5jZSlcbmBgYCJ9 -->

```r
sagui_mont_tumuc <- transformar_para_distanceR_covariaveis() |> 
  filter(
    Region.Label == "Parna Montanhas do Tumucumaque",
    sp_name == "Saguinus midas"
  ) |> 
  drop_na(distance)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Distribuição das distâncias perpendiculares


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc2FndWlfbW9udF90dW11YyB8PiBcbiAgcGxvdGFyX2Rpc3RyaWJ1aWNhb19kaXN0YW5jaWFfaW50ZXJhdGl2bygpXG5gYGAifQ== -->

```r
sagui_mont_tumuc |> 
  plotar_distribuicao_distancia_interativo()
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Ajustando modelo distance com função de detecção Half-Normal e distancia de truncamento 10


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc2FndWlfbW9udF90dW11Y19obiA8LSBzYWd1aV9tb250X3R1bXVjIHw+IFxuICBhanVzdGVfbW9kZWxvc19kaXN0YW5jZV9obihsaXN0YV90ZXJtb3NfYWp1c3RlID0gdHJ1bmNhbWVudG8gPSAxMClcbmBgYCJ9 -->

```r
sagui_mont_tumuc_hn <- sagui_mont_tumuc |> 
  ajuste_modelos_distance_hn(lista_termos_ajuste = truncamento = 10)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Ajustando modelo distance com função de detecção Hazard-rate e distancia de truncamento 10


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc2FndWlfbW9udF90dW11Y19ociA8LSBzYWd1aV9tb250X3R1bXVjIHw+IFxuICBhanVzdGVfbW9kZWxvc19kaXN0YW5jZV9ocih0cnVuY2FtZW50byA9IDEwKVxuYGBgIn0= -->

```r
sagui_mont_tumuc_hr <- sagui_mont_tumuc |> 
  ajuste_modelos_distance_hr(truncamento = 10)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Plot dos modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc2FndWlfbW9udF90dW11Y19obiB8PiBcbiAgcHVycnI6Om1hcChcXCgueCkgcGxvdCgueCkpIFxuYGBgIn0= -->

```r
sagui_mont_tumuc_hn |> 
  purrr::map(\(.x) plot(.x)) 
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc2FndWlfbW9udF90dW11Y19ociB8PiBcbiAgcHVycnI6Om1hcChcXCgueCkgcGxvdCgueCkpIFxuYGBgIn0= -->

```r
sagui_mont_tumuc_hr |> 
  purrr::map(\(.x) plot(.x)) 
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Seleção de modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VtbWFyaXplX2RzX21vZGVscyhcbiAgc2FndWlfbW9udF90dW11Y19obiRgU2VtIHRlcm1vYCxcbiAgc2FndWlfbW9udF90dW11Y19obiRDb3NzZW5vLFxuICBzYWd1aV9tb250X3R1bXVjX2huJGBIZXJtaXRlIHBvbGlub21pYWxgLFxuICBzYWd1aV9tb250X3R1bXVjX2hyJGBTZW0gdGVybW9gLFxuICBzYWd1aV9tb250X3R1bXVjX2hyJENvc3Nlbm8sXG4gIHNhZ3VpX21vbnRfdHVtdWNfaHIkYFBvbGlub21pYWwgc2ltcGxlc2BcbilcbmBgYCJ9 -->

```r
summarize_ds_models(
  sagui_mont_tumuc_hn$`Sem termo`,
  sagui_mont_tumuc_hn$Cosseno,
  sagui_mont_tumuc_hn$`Hermite polinomial`,
  sagui_mont_tumuc_hr$`Sem termo`,
  sagui_mont_tumuc_hr$Cosseno,
  sagui_mont_tumuc_hr$`Polinomial simples`
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Teste de bondade de ajuste


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc2FndWlfbW9udF90dW11Y19obiB8PiBcbiAgcHVycnI6Om1hcChcXCgueCkgZ29mX2RzKG1vZGVsID0gLngpKVxuYGBgIn0= -->

```r
sagui_mont_tumuc_hn |> 
  purrr::map(\(.x) gof_ds(model = .x))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc2FndWlfbW9udF90dW11Y19ociB8PiBcbiAgcHVycnI6Om1hcChcXCgueCkgZ29mX2RzKG1vZGVsID0gLngpKVxuYGBgIn0= -->

```r
sagui_mont_tumuc_hr |> 
  purrr::map(\(.x) gof_ds(model = .x))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimando a abundancia


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCBhcmVhIGNvYmVydGEgcGVsbyBlc2ZvcsOnbyBhbW9zdHJhbCwgZXNmb3LDp28gYW1vc3RyYWwgZW0gbWV0cm9zLCBuw7ptZXJvIGRlIGRldGVjw6fDtWVzLCBuw7ptZXJvIGRlIHRyYW5zZWN0b3MgKGVhKSwgdGF4YSBkZSBlbmNvbnRybywgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSB0YXhhIGRlIGVuY29udHJvICBcbnNhZ3VpX21vbnRfdHVtdWNfaG4kYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJHN1bW1hcnlbMTo5XVxuYGBgIn0= -->

```r
# área de estudo, tamanho da área de estudo, area coberta pelo esforço amostral, esforço amostral em metros, número de detecções, número de transectos (ea), taxa de encontro, coeficiente de variação da taxa de encontro  
sagui_mont_tumuc_hn$`Sem termo`$dht$individuals$summary[1:9]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCB0cmlsaGFzIG91IGVzdGHDp8O1ZXMgYW1vc3RyYWlzLCBlc2ZvcsOnbyB0b3RhbCBlbSBjYWRhIHRyaWxoYSwgYWJ1bmTDom5jaWEgZXN0aW1hZGEgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIG7Dum1lcm8gZGUgZGV0ZWPDp8O1ZXMgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIMOhcmVhIHRvdGFsIGFtb3N0cmFkYVxuc2FndWlfbW9udF90dW11Y19obiRgU2VtIHRlcm1vYCRkaHQkaW5kaXZpZHVhbHMkTmhhdC5ieS5zYW1wbGVbMTo4XVxuYGBgIn0= -->

```r
# área de estudo, tamanho da área de estudo, trilhas ou estações amostrais, esforço total em cada trilha, abundância estimada em cada estação amostral, número de detecções em cada estação amostral, área total amostrada
sagui_mont_tumuc_hn$`Sem termo`$dht$individuals$Nhat.by.sample[1:8]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyB0b3RhbCwgZGVuc2lkYWRlIGVzdGltYWRhLCBlcnJvIHBhZHLDo28gZGEgZGVuc2lkYWRlIGRlc3RpbWFkYSwgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSBkZW5zaWRhZGUgZGVzdGltYWRhLCBpbnRlcnZhbG8gZGUgY29uZmlhbsOnYSBpbmZlcmlvciBlIHN1cGVyaW9yIGRvIGNvZWZpY2llbnRlIGRlIHZhcmlhw6fDo28sIGdydWFzIGRlIGxpYmVyZGFkZVxuc2FndWlfbW9udF90dW11Y19obiRgU2VtIHRlcm1vYCRkaHQkaW5kaXZpZHVhbHMkRFxuXG5gYGAifQ== -->

```r
# total, densidade estimada, erro padrão da densidade destimada, coeficiente de variação da densidade destimada, intervalo de confiança inferior e superior do coeficiente de variação, gruas de liberdade
sagui_mont_tumuc_hn$`Sem termo`$dht$individuals$D

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCBhcmVhIGNvYmVydGEgcGVsbyBlc2ZvcsOnbyBhbW9zdHJhbCwgZXNmb3LDp28gYW1vc3RyYWwgZW0gbWV0cm9zLCBuw7ptZXJvIGRlIGRldGVjw6fDtWVzLCBuw7ptZXJvIGRlIHRyYW5zZWN0b3MgKGVhKSwgdGF4YSBkZSBlbmNvbnRybywgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSB0YXhhIGRlIGVuY29udHJvICBcbnNhZ3VpX21vbnRfdHVtdWNfaHIkYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJHN1bW1hcnlbMTo5XVxuYGBgIn0= -->

```r
# área de estudo, tamanho da área de estudo, area coberta pelo esforço amostral, esforço amostral em metros, número de detecções, número de transectos (ea), taxa de encontro, coeficiente de variação da taxa de encontro  
sagui_mont_tumuc_hr$`Sem termo`$dht$individuals$summary[1:9]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCB0cmlsaGFzIG91IGVzdGHDp8O1ZXMgYW1vc3RyYWlzLCBlc2ZvcsOnbyB0b3RhbCBlbSBjYWRhIHRyaWxoYSwgYWJ1bmTDom5jaWEgZXN0aW1hZGEgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIG7Dum1lcm8gZGUgZGV0ZWPDp8O1ZXMgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIMOhcmVhIHRvdGFsIGFtb3N0cmFkYVxuc2FndWlfbW9udF90dW11Y19ociRgU2VtIHRlcm1vYCRkaHQkaW5kaXZpZHVhbHMkTmhhdC5ieS5zYW1wbGVbMTo4XVxuYGBgIn0= -->

```r
# área de estudo, tamanho da área de estudo, trilhas ou estações amostrais, esforço total em cada trilha, abundância estimada em cada estação amostral, número de detecções em cada estação amostral, área total amostrada
sagui_mont_tumuc_hr$`Sem termo`$dht$individuals$Nhat.by.sample[1:8]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyB0b3RhbCwgZGVuc2lkYWRlIGVzdGltYWRhLCBlcnJvIHBhZHLDo28gZGEgZGVuc2lkYWRlIGRlc3RpbWFkYSwgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSBkZW5zaWRhZGUgZGVzdGltYWRhLCBpbnRlcnZhbG8gZGUgY29uZmlhbsOnYSBpbmZlcmlvciBlIHN1cGVyaW9yIGRvIGNvZWZpY2llbnRlIGRlIHZhcmlhw6fDo28sIGdydWFzIGRlIGxpYmVyZGFkZVxuc2FndWlfbW9udF90dW11Y19ociRgU2VtIHRlcm1vYCRkaHQkaW5kaXZpZHVhbHMkRFxuYGBgIn0= -->

```r
# total, densidade estimada, erro padrão da densidade destimada, coeficiente de variação da densidade destimada, intervalo de confiança inferior e superior do coeficiente de variação, gruas de liberdade
sagui_mont_tumuc_hr$`Sem termo`$dht$individuals$D
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Segunda espécie para dados com repetição

## *Myoprocta pratti* na **Resex Alto Tarauacá**

![Fonte: zoochat.com](https://external-content.duckduckgo.com/iu/?u=https%253A%252F%252Ftse3.mm.bing.net%252Fth%253Fid%253DOIP.hqCNuSZSYZYfF_hTuoFlWAHaE8%2526pid%253DApi&f=1&ipt=6708c08de380fff37267c88563c4958653de6bc88c44979b20f2fd7ffcbbad4a&ipo=images)

### Carregar dados


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY3V0aWFfYWx0b190YXJhdSA8LSB0cmFuc2Zvcm1hcl9wYXJhX2Rpc3RhbmNlUl9jb3ZhcmlhdmVpcygpIHw+IFxuICBmaWx0ZXIoXG4gICAgUmVnaW9uLkxhYmVsID09IFwiUmVzZXggQWx0byBUYXJhdWFjw6FcIixcbiAgICBzcF9uYW1lID09IFwiTXlvcHJvY3RhIHByYXR0aVwiXG4gICkgfD4gXG4gIGRyb3BfbmEoZGlzdGFuY2UpXG5gYGAifQ== -->

```r
cutia_alto_tarau <- transformar_para_distanceR_covariaveis() |> 
  filter(
    Region.Label == "Resex Alto Tarauacá",
    sp_name == "Myoprocta pratti"
  ) |> 
  drop_na(distance)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Distribuição das distâncias perpendiculares


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY3V0aWFfYWx0b190YXJhdSB8PiBcbiAgcGxvdGFyX2Rpc3RyaWJ1aWNhb19kaXN0YW5jaWFfaW50ZXJhdGl2bygpXG5gYGAifQ== -->

```r
cutia_alto_tarau |> 
  plotar_distribuicao_distancia_interativo()
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Ajustando modelo distance com função de detecção Half-Normal e distancia de truncamento 10


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY3V0aWFfYWx0b190YXJhdV9obiA8LSBjdXRpYV9hbHRvX3RhcmF1IHw+IFxuICBhanVzdGVfbW9kZWxvc19kaXN0YW5jZV9obih0cnVuY2FtZW50byA9IDEwKVxuYGBgIn0= -->

```r
cutia_alto_tarau_hn <- cutia_alto_tarau |> 
  ajuste_modelos_distance_hn(truncamento = 10)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Ajustando modelo distance com função de detecção Hazard-rate e distancia de truncamento 10


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY3V0aWFfYWx0b190YXJhdV9ociA8LSBjdXRpYV9hbHRvX3RhcmF1IHw+IFxuICBhanVzdGVfbW9kZWxvc19kaXN0YW5jZV9ocih0cnVuY2FtZW50byA9IDEwKVxuYGBgIn0= -->

```r
cutia_alto_tarau_hr <- cutia_alto_tarau |> 
  ajuste_modelos_distance_hr(truncamento = 10)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Plot dos modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY3V0aWFfYWx0b190YXJhdV9obiB8PiBcbiAgcHVycnI6Om1hcChcXCgueCkgcGxvdCgueCkpIFxuYGBgIn0= -->

```r
cutia_alto_tarau_hn |> 
  purrr::map(\(.x) plot(.x)) 
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY3V0aWFfYWx0b190YXJhdV9ociB8PiBcbiAgcHVycnI6Om1hcChcXCgueCkgcGxvdCgueCkpIFxuYGBgIn0= -->

```r
cutia_alto_tarau_hr |> 
  purrr::map(\(.x) plot(.x)) 
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Seleção de modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VtbWFyaXplX2RzX21vZGVscyhcbiAgY3V0aWFfYWx0b190YXJhdV9obiRgU2VtIHRlcm1vYCxcbiAgY3V0aWFfYWx0b190YXJhdV9obiRDb3NzZW5vLFxuICBjdXRpYV9hbHRvX3RhcmF1X2huJGBIZXJtaXRlIHBvbGlub21pYWxgLFxuICBjdXRpYV9hbHRvX3RhcmF1X2hyJGBTZW0gdGVybW9gLFxuICBjdXRpYV9hbHRvX3RhcmF1X2hyJENvc3Nlbm8sXG4gIGN1dGlhX2FsdG9fdGFyYXVfaHIkYFBvbGlub21pYWwgc2ltcGxlc2BcbilcbmBgYCJ9 -->

```r
summarize_ds_models(
  cutia_alto_tarau_hn$`Sem termo`,
  cutia_alto_tarau_hn$Cosseno,
  cutia_alto_tarau_hn$`Hermite polinomial`,
  cutia_alto_tarau_hr$`Sem termo`,
  cutia_alto_tarau_hr$Cosseno,
  cutia_alto_tarau_hr$`Polinomial simples`
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Teste de bondade de ajuste


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY3V0aWFfYWx0b190YXJhdV9obiB8PiBcbiAgcHVycnI6Om1hcChcXCgueCkgZ29mX2RzKG1vZGVsID0gLngpKVxuYGBgIn0= -->

```r
cutia_alto_tarau_hn |> 
  purrr::map(\(.x) gof_ds(model = .x))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY3V0aWFfYWx0b190YXJhdV9ociB8PiBcbiAgcHVycnI6Om1hcChcXCgueCkgZ29mX2RzKG1vZGVsID0gLngpKVxuYGBgIn0= -->

```r
cutia_alto_tarau_hr |> 
  purrr::map(\(.x) gof_ds(model = .x))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimando a abundancia


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCBhcmVhIGNvYmVydGEgcGVsbyBlc2ZvcsOnbyBhbW9zdHJhbCwgZXNmb3LDp28gYW1vc3RyYWwgZW0gbWV0cm9zLCBuw7ptZXJvIGRlIGRldGVjw6fDtWVzLCBuw7ptZXJvIGRlIHRyYW5zZWN0b3MgKGVhKSwgdGF4YSBkZSBlbmNvbnRybywgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSB0YXhhIGRlIGVuY29udHJvICBcbmN1dGlhX2FsdG9fdGFyYXVfaG4kYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJHN1bW1hcnlbMTo5XVxuYGBgIn0= -->

```r
# área de estudo, tamanho da área de estudo, area coberta pelo esforço amostral, esforço amostral em metros, número de detecções, número de transectos (ea), taxa de encontro, coeficiente de variação da taxa de encontro  
cutia_alto_tarau_hn$`Sem termo`$dht$individuals$summary[1:9]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCB0cmlsaGFzIG91IGVzdGHDp8O1ZXMgYW1vc3RyYWlzLCBlc2ZvcsOnbyB0b3RhbCBlbSBjYWRhIHRyaWxoYSwgYWJ1bmTDom5jaWEgZXN0aW1hZGEgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIG7Dum1lcm8gZGUgZGV0ZWPDp8O1ZXMgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIMOhcmVhIHRvdGFsIGFtb3N0cmFkYVxuY3V0aWFfYWx0b190YXJhdV9obiRgU2VtIHRlcm1vYCRkaHQkaW5kaXZpZHVhbHMkTmhhdC5ieS5zYW1wbGVbMTo4XVxuYGBgIn0= -->

```r
# área de estudo, tamanho da área de estudo, trilhas ou estações amostrais, esforço total em cada trilha, abundância estimada em cada estação amostral, número de detecções em cada estação amostral, área total amostrada
cutia_alto_tarau_hn$`Sem termo`$dht$individuals$Nhat.by.sample[1:8]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyB0b3RhbCwgZGVuc2lkYWRlIGVzdGltYWRhLCBlcnJvIHBhZHLDo28gZGEgZGVuc2lkYWRlIGRlc3RpbWFkYSwgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSBkZW5zaWRhZGUgZGVzdGltYWRhLCBpbnRlcnZhbG8gZGUgY29uZmlhbsOnYSBpbmZlcmlvciBlIHN1cGVyaW9yIGRvIGNvZWZpY2llbnRlIGRlIHZhcmlhw6fDo28sIGdydWFzIGRlIGxpYmVyZGFkZVxuY3V0aWFfYWx0b190YXJhdV9obiRgU2VtIHRlcm1vYCRkaHQkaW5kaXZpZHVhbHMkRFxuXG5gYGAifQ== -->

```r
# total, densidade estimada, erro padrão da densidade destimada, coeficiente de variação da densidade destimada, intervalo de confiança inferior e superior do coeficiente de variação, gruas de liberdade
cutia_alto_tarau_hn$`Sem termo`$dht$individuals$D

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCBhcmVhIGNvYmVydGEgcGVsbyBlc2ZvcsOnbyBhbW9zdHJhbCwgZXNmb3LDp28gYW1vc3RyYWwgZW0gbWV0cm9zLCBuw7ptZXJvIGRlIGRldGVjw6fDtWVzLCBuw7ptZXJvIGRlIHRyYW5zZWN0b3MgKGVhKSwgdGF4YSBkZSBlbmNvbnRybywgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSB0YXhhIGRlIGVuY29udHJvICBcbmN1dGlhX2FsdG9fdGFyYXVfaHIkYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJHN1bW1hcnlbMTo5XVxuYGBgIn0= -->

```r
# área de estudo, tamanho da área de estudo, area coberta pelo esforço amostral, esforço amostral em metros, número de detecções, número de transectos (ea), taxa de encontro, coeficiente de variação da taxa de encontro  
cutia_alto_tarau_hr$`Sem termo`$dht$individuals$summary[1:9]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCB0cmlsaGFzIG91IGVzdGHDp8O1ZXMgYW1vc3RyYWlzLCBlc2ZvcsOnbyB0b3RhbCBlbSBjYWRhIHRyaWxoYSwgYWJ1bmTDom5jaWEgZXN0aW1hZGEgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIG7Dum1lcm8gZGUgZGV0ZWPDp8O1ZXMgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIMOhcmVhIHRvdGFsIGFtb3N0cmFkYVxuY3V0aWFfYWx0b190YXJhdV9ociRgU2VtIHRlcm1vYCRkaHQkaW5kaXZpZHVhbHMkTmhhdC5ieS5zYW1wbGVbMTo4XVxuYGBgIn0= -->

```r
# área de estudo, tamanho da área de estudo, trilhas ou estações amostrais, esforço total em cada trilha, abundância estimada em cada estação amostral, número de detecções em cada estação amostral, área total amostrada
cutia_alto_tarau_hr$`Sem termo`$dht$individuals$Nhat.by.sample[1:8]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyB0b3RhbCwgZGVuc2lkYWRlIGVzdGltYWRhLCBlcnJvIHBhZHLDo28gZGEgZGVuc2lkYWRlIGRlc3RpbWFkYSwgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSBkZW5zaWRhZGUgZGVzdGltYWRhLCBpbnRlcnZhbG8gZGUgY29uZmlhbsOnYSBpbmZlcmlvciBlIHN1cGVyaW9yIGRvIGNvZWZpY2llbnRlIGRlIHZhcmlhw6fDo28sIGdydWFzIGRlIGxpYmVyZGFkZVxuY3V0aWFfYWx0b190YXJhdV9ociRgU2VtIHRlcm1vYCRkaHQkaW5kaXZpZHVhbHMkRFxuXG5gYGAifQ== -->

```r
# total, densidade estimada, erro padrão da densidade destimada, coeficiente de variação da densidade destimada, intervalo de confiança inferior e superior do coeficiente de variação, gruas de liberdade
cutia_alto_tarau_hr$`Sem termo`$dht$individuals$D

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Terceira espécie para dados com repetição

## *Lagothrix cana* na **Rebio do Jaru**

![Fonte: zoochat.com](https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Ftse1.mm.bing.net%2Fth%3Fid%3DOIP.wNKxS8e_CGO89TOOoTy-ywHaE8%26pid%3DApi&f=1&ipt=43306d74ae7f05cf0c98b98bfac7d113fbb4031c267e5efae616fb999dc64dd0&ipo=images)

### Carregar dados


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubWFjYWNvX2phcnUgPC0gdHJhbnNmb3JtYXJfcGFyYV9kaXN0YW5jZVJfY292YXJpYXZlaXMoKSB8PiBcbiAgZmlsdGVyKFxuICAgIFJlZ2lvbi5MYWJlbCA9PSBcIlJlYmlvIGRvIEphcnVcIixcbiAgICBzcF9uYW1lID09IFwiTGFnb3Rocml4IGNhbmFcIlxuICApIHw+IFxuICBkcm9wX25hKGRpc3RhbmNlKVxuYGBgIn0= -->

```r
macaco_jaru <- transformar_para_distanceR_covariaveis() |> 
  filter(
    Region.Label == "Rebio do Jaru",
    sp_name == "Lagothrix cana"
  ) |> 
  drop_na(distance)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Distribuição das distâncias perpendiculares


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubWFjYWNvX2phcnUgfD4gXG4gIHBsb3Rhcl9kaXN0cmlidWljYW9fZGlzdGFuY2lhX2ludGVyYXRpdm8oKVxuYGBgIn0= -->

```r
macaco_jaru |> 
  plotar_distribuicao_distancia_interativo()
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Ajustando modelo distance com função de detecção Half-Normal e distancia de truncamento 20


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubWFjYWNvX2phcnVfaG4gPC0gbWFjYWNvX2phcnUgfD4gXG4gIGFqdXN0ZV9tb2RlbG9zX2Rpc3RhbmNlX2huKHRydW5jYW1lbnRvID0gMjApXG5gYGAifQ== -->

```r
macaco_jaru_hn <- macaco_jaru |> 
  ajuste_modelos_distance_hn(truncamento = 20)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Ajustando modelo distance com função de detecção Hazard-rate e distancia de truncamento 20


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubWFjYWNvX2phcnVfaHIgPC0gbWFjYWNvX2phcnUgfD4gXG4gIGFqdXN0ZV9tb2RlbG9zX2Rpc3RhbmNlX2hyKHRydW5jYW1lbnRvID0gMjApXG5gYGAifQ== -->

```r
macaco_jaru_hr <- macaco_jaru |> 
  ajuste_modelos_distance_hr(truncamento = 20)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Plot dos modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubWFjYWNvX2phcnVfaG4gfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIHBsb3QoLngpKSBcbmBgYCJ9 -->

```r
macaco_jaru_hn |> 
  purrr::map(\(.x) plot(.x)) 
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubWFjYWNvX2phcnVfaHIgfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIHBsb3QoLngpKSBcbmBgYCJ9 -->

```r
macaco_jaru_hr |> 
  purrr::map(\(.x) plot(.x)) 
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Seleção de modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VtbWFyaXplX2RzX21vZGVscyhcbiAgbWFjYWNvX2phcnVfaG4kYFNlbSB0ZXJtb2AsXG4gIG1hY2Fjb19qYXJ1X2huJENvc3Nlbm8sXG4gIG1hY2Fjb19qYXJ1X2huJGBIZXJtaXRlIHBvbGlub21pYWxgLFxuICBtYWNhY29famFydV9ociRgU2VtIHRlcm1vYCxcbiAgbWFjYWNvX2phcnVfaHIkQ29zc2VubyxcbiAgbWFjYWNvX2phcnVfaHIkYFBvbGlub21pYWwgc2ltcGxlc2BcbilcbmBgYCJ9 -->

```r
summarize_ds_models(
  macaco_jaru_hn$`Sem termo`,
  macaco_jaru_hn$Cosseno,
  macaco_jaru_hn$`Hermite polinomial`,
  macaco_jaru_hr$`Sem termo`,
  macaco_jaru_hr$Cosseno,
  macaco_jaru_hr$`Polinomial simples`
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Teste de bondade de ajuste


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubWFjYWNvX2phcnVfaG4gfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIGdvZl9kcyhtb2RlbCA9IC54KSlcbmBgYCJ9 -->

```r
macaco_jaru_hn |> 
  purrr::map(\(.x) gof_ds(model = .x))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubWFjYWNvX2phcnVfaHIgfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIGdvZl9kcyhtb2RlbCA9IC54KSlcbmBgYCJ9 -->

```r
macaco_jaru_hr |> 
  purrr::map(\(.x) gof_ds(model = .x))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimando a abundancia


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCBhcmVhIGNvYmVydGEgcGVsbyBlc2ZvcsOnbyBhbW9zdHJhbCwgZXNmb3LDp28gYW1vc3RyYWwgZW0gbWV0cm9zLCBuw7ptZXJvIGRlIGRldGVjw6fDtWVzLCBuw7ptZXJvIGRlIHRyYW5zZWN0b3MgKGVhKSwgdGF4YSBkZSBlbmNvbnRybywgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSB0YXhhIGRlIGVuY29udHJvICBcbm1hY2Fjb19qYXJ1X2huJGBTZW0gdGVybW9gJGRodCRpbmRpdmlkdWFscyRzdW1tYXJ5WzE6OV1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, area coberta pelo esforço amostral, esforço amostral em metros, número de detecções, número de transectos (ea), taxa de encontro, coeficiente de variação da taxa de encontro  
macaco_jaru_hn$`Sem termo`$dht$individuals$summary[1:9]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCB0cmlsaGFzIG91IGVzdGHDp8O1ZXMgYW1vc3RyYWlzLCBlc2ZvcsOnbyB0b3RhbCBlbSBjYWRhIHRyaWxoYSwgYWJ1bmTDom5jaWEgZXN0aW1hZGEgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIG7Dum1lcm8gZGUgZGV0ZWPDp8O1ZXMgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIMOhcmVhIHRvdGFsIGFtb3N0cmFkYVxubWFjYWNvX2phcnVfaG4kYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJE5oYXQuYnkuc2FtcGxlWzE6OF1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, trilhas ou estações amostrais, esforço total em cada trilha, abundância estimada em cada estação amostral, número de detecções em cada estação amostral, área total amostrada
macaco_jaru_hn$`Sem termo`$dht$individuals$Nhat.by.sample[1:8]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyB0b3RhbCwgZGVuc2lkYWRlIGVzdGltYWRhLCBlcnJvIHBhZHLDo28gZGEgZGVuc2lkYWRlIGRlc3RpbWFkYSwgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSBkZW5zaWRhZGUgZGVzdGltYWRhLCBpbnRlcnZhbG8gZGUgY29uZmlhbsOnYSBpbmZlcmlvciBlIHN1cGVyaW9yIGRvIGNvZWZpY2llbnRlIGRlIHZhcmlhw6fDo28sIGdydWFzIGRlIGxpYmVyZGFkZVxubWFjYWNvX2phcnVfaG4kYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJERcblxuYGBgIn0= -->

```r
# total, densidade estimada, erro padrão da densidade destimada, coeficiente de variação da densidade destimada, intervalo de confiança inferior e superior do coeficiente de variação, gruas de liberdade
macaco_jaru_hn$`Sem termo`$dht$individuals$D

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCBhcmVhIGNvYmVydGEgcGVsbyBlc2ZvcsOnbyBhbW9zdHJhbCwgZXNmb3LDp28gYW1vc3RyYWwgZW0gbWV0cm9zLCBuw7ptZXJvIGRlIGRldGVjw6fDtWVzLCBuw7ptZXJvIGRlIHRyYW5zZWN0b3MgKGVhKSwgdGF4YSBkZSBlbmNvbnRybywgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSB0YXhhIGRlIGVuY29udHJvICBcbm1hY2Fjb19qYXJ1X2hyJGBTZW0gdGVybW9gJGRodCRpbmRpdmlkdWFscyRzdW1tYXJ5WzE6OV1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, area coberta pelo esforço amostral, esforço amostral em metros, número de detecções, número de transectos (ea), taxa de encontro, coeficiente de variação da taxa de encontro  
macaco_jaru_hr$`Sem termo`$dht$individuals$summary[1:9]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCB0cmlsaGFzIG91IGVzdGHDp8O1ZXMgYW1vc3RyYWlzLCBlc2ZvcsOnbyB0b3RhbCBlbSBjYWRhIHRyaWxoYSwgYWJ1bmTDom5jaWEgZXN0aW1hZGEgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIG7Dum1lcm8gZGUgZGV0ZWPDp8O1ZXMgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIMOhcmVhIHRvdGFsIGFtb3N0cmFkYVxubWFjYWNvX2phcnVfaHIkYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJE5oYXQuYnkuc2FtcGxlWzE6OF1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, trilhas ou estações amostrais, esforço total em cada trilha, abundância estimada em cada estação amostral, número de detecções em cada estação amostral, área total amostrada
macaco_jaru_hr$`Sem termo`$dht$individuals$Nhat.by.sample[1:8]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyB0b3RhbCwgZGVuc2lkYWRlIGVzdGltYWRhLCBlcnJvIHBhZHLDo28gZGEgZGVuc2lkYWRlIGRlc3RpbWFkYSwgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSBkZW5zaWRhZGUgZGVzdGltYWRhLCBpbnRlcnZhbG8gZGUgY29uZmlhbsOnYSBpbmZlcmlvciBlIHN1cGVyaW9yIGRvIGNvZWZpY2llbnRlIGRlIHZhcmlhw6fDo28sIGdydWFzIGRlIGxpYmVyZGFkZVxubWFjYWNvX2phcnVfaHIkYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJERcblxuYGBgIn0= -->

```r
# total, densidade estimada, erro padrão da densidade destimada, coeficiente de variação da densidade destimada, intervalo de confiança inferior e superior do coeficiente de variação, gruas de liberdade
macaco_jaru_hr$`Sem termo`$dht$individuals$D

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Quarta espécie para dados com repetição

## *Mazama americana* na **Resex Tapajos-Arapiuns**

![Fonte: zoochat.com](https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Ftse2.mm.bing.net%2Fth%3Fid%3DOIP.V7M0mCIe5bFtJMv4O04jgAHaE8%26pid%3DApi&f=1&ipt=81f27ed3e2882badba619d12c798f2c29c896bd70cb71f6da91902c13dfbb06f&ipo=images)

### Carregar dados


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY2Vydm9fdGFwX2FyYXAgPC0gdHJhbnNmb3JtYXJfcGFyYV9kaXN0YW5jZVJfY292YXJpYXZlaXMoKSB8PiBcbiAgZmlsdGVyKFxuICAgIFJlZ2lvbi5MYWJlbCA9PSBcIlJlc2V4IFRhcGFqb3MtQXJhcGl1bnNcIixcbiAgICBzcF9uYW1lID09IFwiTWF6YW1hIGFtZXJpY2FuYVwiXG4gICkgfD4gXG4gIGRyb3BfbmEoZGlzdGFuY2UpXG5gYGAifQ== -->

```r
cervo_tap_arap <- transformar_para_distanceR_covariaveis() |> 
  filter(
    Region.Label == "Resex Tapajos-Arapiuns",
    sp_name == "Mazama americana"
  ) |> 
  drop_na(distance)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Distribuição das distâncias perpendiculares


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY2Vydm9fdGFwX2FyYXAgfD4gXG4gIHBsb3Rhcl9kaXN0cmlidWljYW9fZGlzdGFuY2lhX2ludGVyYXRpdm8oKVxuYGBgIn0= -->

```r
cervo_tap_arap |> 
  plotar_distribuicao_distancia_interativo()
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Ajustando modelo distance com função de detecção Half-Normal e distancia de truncamento 13


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY2Vydm9fdGFwX2FyYXBfaG4gPC0gY2Vydm9fdGFwX2FyYXAgfD4gXG4gIGFqdXN0ZV9tb2RlbG9zX2Rpc3RhbmNlX2huKHRydW5jYW1lbnRvID0gMTMpXG5gYGAifQ== -->

```r
cervo_tap_arap_hn <- cervo_tap_arap |> 
  ajuste_modelos_distance_hn(truncamento = 13)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Ajustando modelo distance com função de detecção Hazard-rate e distancia de truncamento 13


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY2Vydm9fdGFwX2FyYXBfaHIgPC0gY2Vydm9fdGFwX2FyYXAgfD4gXG4gIGFqdXN0ZV9tb2RlbG9zX2Rpc3RhbmNlX2hyKHRydW5jYW1lbnRvID0gMTMpXG5gYGAifQ== -->

```r
cervo_tap_arap_hr <- cervo_tap_arap |> 
  ajuste_modelos_distance_hr(truncamento = 13)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Plot dos modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY2Vydm9fdGFwX2FyYXBfaG4gfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIHBsb3QoLngpKSBcbmBgYCJ9 -->

```r
cervo_tap_arap_hn |> 
  purrr::map(\(.x) plot(.x)) 
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY2Vydm9fdGFwX2FyYXBfaHIgfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIHBsb3QoLngpKSBcbmBgYCJ9 -->

```r
cervo_tap_arap_hr |> 
  purrr::map(\(.x) plot(.x)) 
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Seleção de modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VtbWFyaXplX2RzX21vZGVscyhcbiAgY2Vydm9fdGFwX2FyYXBfaG4kYFNlbSB0ZXJtb2AsXG4gIGNlcnZvX3RhcF9hcmFwX2huJENvc3Nlbm8sXG4gIGNlcnZvX3RhcF9hcmFwX2huJGBIZXJtaXRlIHBvbGlub21pYWxgLFxuICBjZXJ2b190YXBfYXJhcF9ociRgU2VtIHRlcm1vYCxcbiAgY2Vydm9fdGFwX2FyYXBfaHIkQ29zc2VubyxcbiAgY2Vydm9fdGFwX2FyYXBfaHIkYFBvbGlub21pYWwgc2ltcGxlc2BcbilcbmBgYCJ9 -->

```r
summarize_ds_models(
  cervo_tap_arap_hn$`Sem termo`,
  cervo_tap_arap_hn$Cosseno,
  cervo_tap_arap_hn$`Hermite polinomial`,
  cervo_tap_arap_hr$`Sem termo`,
  cervo_tap_arap_hr$Cosseno,
  cervo_tap_arap_hr$`Polinomial simples`
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Teste de bondade de ajuste


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY2Vydm9fdGFwX2FyYXBfaG4gfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIGdvZl9kcyhtb2RlbCA9IC54KSlcbmBgYCJ9 -->

```r
cervo_tap_arap_hn |> 
  purrr::map(\(.x) gof_ds(model = .x))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY2Vydm9fdGFwX2FyYXBfaHIgfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIGdvZl9kcyhtb2RlbCA9IC54KSlcbmBgYCJ9 -->

```r
cervo_tap_arap_hr |> 
  purrr::map(\(.x) gof_ds(model = .x))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimando a abundancia


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCBhcmVhIGNvYmVydGEgcGVsbyBlc2ZvcsOnbyBhbW9zdHJhbCwgZXNmb3LDp28gYW1vc3RyYWwgZW0gbWV0cm9zLCBuw7ptZXJvIGRlIGRldGVjw6fDtWVzLCBuw7ptZXJvIGRlIHRyYW5zZWN0b3MgKGVhKSwgdGF4YSBkZSBlbmNvbnRybywgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSB0YXhhIGRlIGVuY29udHJvICBcbmNlcnZvX3RhcF9hcmFwX2huJGBTZW0gdGVybW9gJGRodCRpbmRpdmlkdWFscyRzdW1tYXJ5WzE6OV1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, area coberta pelo esforço amostral, esforço amostral em metros, número de detecções, número de transectos (ea), taxa de encontro, coeficiente de variação da taxa de encontro  
cervo_tap_arap_hn$`Sem termo`$dht$individuals$summary[1:9]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCB0cmlsaGFzIG91IGVzdGHDp8O1ZXMgYW1vc3RyYWlzLCBlc2ZvcsOnbyB0b3RhbCBlbSBjYWRhIHRyaWxoYSwgYWJ1bmTDom5jaWEgZXN0aW1hZGEgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIG7Dum1lcm8gZGUgZGV0ZWPDp8O1ZXMgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIMOhcmVhIHRvdGFsIGFtb3N0cmFkYVxuY2Vydm9fdGFwX2FyYXBfaG4kYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJE5oYXQuYnkuc2FtcGxlWzE6OF1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, trilhas ou estações amostrais, esforço total em cada trilha, abundância estimada em cada estação amostral, número de detecções em cada estação amostral, área total amostrada
cervo_tap_arap_hn$`Sem termo`$dht$individuals$Nhat.by.sample[1:8]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyB0b3RhbCwgZGVuc2lkYWRlIGVzdGltYWRhLCBlcnJvIHBhZHLDo28gZGEgZGVuc2lkYWRlIGRlc3RpbWFkYSwgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSBkZW5zaWRhZGUgZGVzdGltYWRhLCBpbnRlcnZhbG8gZGUgY29uZmlhbsOnYSBpbmZlcmlvciBlIHN1cGVyaW9yIGRvIGNvZWZpY2llbnRlIGRlIHZhcmlhw6fDo28sIGdydWFzIGRlIGxpYmVyZGFkZVxuY2Vydm9fdGFwX2FyYXBfaG4kYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJERcblxuYGBgIn0= -->

```r
# total, densidade estimada, erro padrão da densidade destimada, coeficiente de variação da densidade destimada, intervalo de confiança inferior e superior do coeficiente de variação, gruas de liberdade
cervo_tap_arap_hn$`Sem termo`$dht$individuals$D

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCBhcmVhIGNvYmVydGEgcGVsbyBlc2ZvcsOnbyBhbW9zdHJhbCwgZXNmb3LDp28gYW1vc3RyYWwgZW0gbWV0cm9zLCBuw7ptZXJvIGRlIGRldGVjw6fDtWVzLCBuw7ptZXJvIGRlIHRyYW5zZWN0b3MgKGVhKSwgdGF4YSBkZSBlbmNvbnRybywgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSB0YXhhIGRlIGVuY29udHJvICBcbmNlcnZvX3RhcF9hcmFwX2hyJGBTZW0gdGVybW9gJGRodCRpbmRpdmlkdWFscyRzdW1tYXJ5WzE6OV1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, area coberta pelo esforço amostral, esforço amostral em metros, número de detecções, número de transectos (ea), taxa de encontro, coeficiente de variação da taxa de encontro  
cervo_tap_arap_hr$`Sem termo`$dht$individuals$summary[1:9]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCB0cmlsaGFzIG91IGVzdGHDp8O1ZXMgYW1vc3RyYWlzLCBlc2ZvcsOnbyB0b3RhbCBlbSBjYWRhIHRyaWxoYSwgYWJ1bmTDom5jaWEgZXN0aW1hZGEgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIG7Dum1lcm8gZGUgZGV0ZWPDp8O1ZXMgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIMOhcmVhIHRvdGFsIGFtb3N0cmFkYVxuY2Vydm9fdGFwX2FyYXBfaHIkYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJE5oYXQuYnkuc2FtcGxlWzE6OF1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, trilhas ou estações amostrais, esforço total em cada trilha, abundância estimada em cada estação amostral, número de detecções em cada estação amostral, área total amostrada
cervo_tap_arap_hr$`Sem termo`$dht$individuals$Nhat.by.sample[1:8]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyB0b3RhbCwgZGVuc2lkYWRlIGVzdGltYWRhLCBlcnJvIHBhZHLDo28gZGEgZGVuc2lkYWRlIGRlc3RpbWFkYSwgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSBkZW5zaWRhZGUgZGVzdGltYWRhLCBpbnRlcnZhbG8gZGUgY29uZmlhbsOnYSBpbmZlcmlvciBlIHN1cGVyaW9yIGRvIGNvZWZpY2llbnRlIGRlIHZhcmlhw6fDo28sIGdydWFzIGRlIGxpYmVyZGFkZVxuY2Vydm9fdGFwX2FyYXBfaHIkYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJERcblxuYGBgIn0= -->

```r
# total, densidade estimada, erro padrão da densidade destimada, coeficiente de variação da densidade destimada, intervalo de confiança inferior e superior do coeficiente de variação, gruas de liberdade
cervo_tap_arap_hr$`Sem termo`$dht$individuals$D

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Quinta espécie para dados com repetição

## *Tinamus major* na **Parna Montanhas do Tumucumaque**

![Fonte: pinterest.com](https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Ftse1.mm.bing.net%2Fth%3Fid%3DOIP.3mG-yVZq9vN0aOITB7ltpgHaEr%26pid%3DApi&f=1&ipt=bdec34d393bcaddd9cedcedb81daa6496680619f13d704b6d419ff516e5b5210&ipo=images)

### Carregar dados


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuaW5hbWJ1X21vbnRfdHVtdWMgPC0gdHJhbnNmb3JtYXJfcGFyYV9kaXN0YW5jZVJfY292YXJpYXZlaXMoKSB8PiBcbiAgZmlsdGVyKFxuICAgIFJlZ2lvbi5MYWJlbCA9PSBcIlBhcm5hIE1vbnRhbmhhcyBkbyBUdW11Y3VtYXF1ZVwiLFxuICAgIHNwX25hbWUgPT0gXCJUaW5hbXVzIG1ham9yXCJcbiAgKSB8PiBcbiAgZHJvcF9uYShkaXN0YW5jZSlcbmBgYCJ9 -->

```r
inambu_mont_tumuc <- transformar_para_distanceR_covariaveis() |> 
  filter(
    Region.Label == "Parna Montanhas do Tumucumaque",
    sp_name == "Tinamus major"
  ) |> 
  drop_na(distance)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Distribuição das distâncias perpendiculares


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuaW5hbWJ1X21vbnRfdHVtdWMgfD4gXG4gIHBsb3Rhcl9kaXN0cmlidWljYW9fZGlzdGFuY2lhX2ludGVyYXRpdm8oKVxuYGBgIn0= -->

```r
inambu_mont_tumuc |> 
  plotar_distribuicao_distancia_interativo()
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Ajustando modelo distance com função de detecção Half-Normal e distancia de truncamento 15


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuaW5hbWJ1X21vbnRfdHVtdWNfaG4gPC0gaW5hbWJ1X21vbnRfdHVtdWMgfD4gXG4gIGFqdXN0ZV9tb2RlbG9zX2Rpc3RhbmNlX2huKHRydW5jYW1lbnRvID0gMTUpXG5gYGAifQ== -->

```r
inambu_mont_tumuc_hn <- inambu_mont_tumuc |> 
  ajuste_modelos_distance_hn(truncamento = 15)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Ajustando modelo distance com função de detecção Hazard-rate e distancia de truncamento 15


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuaW5hbWJ1X21vbnRfdHVtdWNfaHIgPC0gaW5hbWJ1X21vbnRfdHVtdWMgfD4gXG4gIGFqdXN0ZV9tb2RlbG9zX2Rpc3RhbmNlX2hyKHRydW5jYW1lbnRvID0gMTUpXG5gYGAifQ== -->

```r
inambu_mont_tumuc_hr <- inambu_mont_tumuc |> 
  ajuste_modelos_distance_hr(truncamento = 15)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Plot dos modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuaW5hbWJ1X21vbnRfdHVtdWNfaG4gfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIHBsb3QoLngpKSBcbmBgYCJ9 -->

```r
inambu_mont_tumuc_hn |> 
  purrr::map(\(.x) plot(.x)) 
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuaW5hbWJ1X21vbnRfdHVtdWNfaHIgfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIHBsb3QoLngpKSBcbmBgYCJ9 -->

```r
inambu_mont_tumuc_hr |> 
  purrr::map(\(.x) plot(.x)) 
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Seleção de modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VtbWFyaXplX2RzX21vZGVscyhcbiAgaW5hbWJ1X21vbnRfdHVtdWNfaG4kYFNlbSB0ZXJtb2AsXG4gIGluYW1idV9tb250X3R1bXVjX2huJENvc3Nlbm8sXG4gIGluYW1idV9tb250X3R1bXVjX2huJGBIZXJtaXRlIHBvbGlub21pYWxgLFxuICBpbmFtYnVfbW9udF90dW11Y19ociRgU2VtIHRlcm1vYCxcbiAgaW5hbWJ1X21vbnRfdHVtdWNfaHIkQ29zc2VubyxcbiAgaW5hbWJ1X21vbnRfdHVtdWNfaHIkYFBvbGlub21pYWwgc2ltcGxlc2BcbilcbmBgYCJ9 -->

```r
summarize_ds_models(
  inambu_mont_tumuc_hn$`Sem termo`,
  inambu_mont_tumuc_hn$Cosseno,
  inambu_mont_tumuc_hn$`Hermite polinomial`,
  inambu_mont_tumuc_hr$`Sem termo`,
  inambu_mont_tumuc_hr$Cosseno,
  inambu_mont_tumuc_hr$`Polinomial simples`
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Teste de bondade de ajuste


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuaW5hbWJ1X21vbnRfdHVtdWNfaG4gfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIGdvZl9kcyhtb2RlbCA9IC54KSlcbmBgYCJ9 -->

```r
inambu_mont_tumuc_hn |> 
  purrr::map(\(.x) gof_ds(model = .x))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuaW5hbWJ1X21vbnRfdHVtdWNfaHIgfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIGdvZl9kcyhtb2RlbCA9IC54KSlcbmBgYCJ9 -->

```r
inambu_mont_tumuc_hr |> 
  purrr::map(\(.x) gof_ds(model = .x))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimando a abundancia


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCBhcmVhIGNvYmVydGEgcGVsbyBlc2ZvcsOnbyBhbW9zdHJhbCwgZXNmb3LDp28gYW1vc3RyYWwgZW0gbWV0cm9zLCBuw7ptZXJvIGRlIGRldGVjw6fDtWVzLCBuw7ptZXJvIGRlIHRyYW5zZWN0b3MgKGVhKSwgdGF4YSBkZSBlbmNvbnRybywgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSB0YXhhIGRlIGVuY29udHJvICBcbmluYW1idV9tb250X3R1bXVjX2huJGBTZW0gdGVybW9gJGRodCRpbmRpdmlkdWFscyRzdW1tYXJ5WzE6OV1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, area coberta pelo esforço amostral, esforço amostral em metros, número de detecções, número de transectos (ea), taxa de encontro, coeficiente de variação da taxa de encontro  
inambu_mont_tumuc_hn$`Sem termo`$dht$individuals$summary[1:9]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCB0cmlsaGFzIG91IGVzdGHDp8O1ZXMgYW1vc3RyYWlzLCBlc2ZvcsOnbyB0b3RhbCBlbSBjYWRhIHRyaWxoYSwgYWJ1bmTDom5jaWEgZXN0aW1hZGEgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIG7Dum1lcm8gZGUgZGV0ZWPDp8O1ZXMgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIMOhcmVhIHRvdGFsIGFtb3N0cmFkYVxuaW5hbWJ1X21vbnRfdHVtdWNfaG4kYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJE5oYXQuYnkuc2FtcGxlWzE6OF1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, trilhas ou estações amostrais, esforço total em cada trilha, abundância estimada em cada estação amostral, número de detecções em cada estação amostral, área total amostrada
inambu_mont_tumuc_hn$`Sem termo`$dht$individuals$Nhat.by.sample[1:8]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyB0b3RhbCwgZGVuc2lkYWRlIGVzdGltYWRhLCBlcnJvIHBhZHLDo28gZGEgZGVuc2lkYWRlIGRlc3RpbWFkYSwgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSBkZW5zaWRhZGUgZGVzdGltYWRhLCBpbnRlcnZhbG8gZGUgY29uZmlhbsOnYSBpbmZlcmlvciBlIHN1cGVyaW9yIGRvIGNvZWZpY2llbnRlIGRlIHZhcmlhw6fDo28sIGdydWFzIGRlIGxpYmVyZGFkZVxuaW5hbWJ1X21vbnRfdHVtdWNfaG4kYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJERcblxuYGBgIn0= -->

```r
# total, densidade estimada, erro padrão da densidade destimada, coeficiente de variação da densidade destimada, intervalo de confiança inferior e superior do coeficiente de variação, gruas de liberdade
inambu_mont_tumuc_hn$`Sem termo`$dht$individuals$D

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCBhcmVhIGNvYmVydGEgcGVsbyBlc2ZvcsOnbyBhbW9zdHJhbCwgZXNmb3LDp28gYW1vc3RyYWwgZW0gbWV0cm9zLCBuw7ptZXJvIGRlIGRldGVjw6fDtWVzLCBuw7ptZXJvIGRlIHRyYW5zZWN0b3MgKGVhKSwgdGF4YSBkZSBlbmNvbnRybywgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSB0YXhhIGRlIGVuY29udHJvICBcbmluYW1idV9tb250X3R1bXVjX2hyJGBTZW0gdGVybW9gJGRodCRpbmRpdmlkdWFscyRzdW1tYXJ5WzE6OV1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, area coberta pelo esforço amostral, esforço amostral em metros, número de detecções, número de transectos (ea), taxa de encontro, coeficiente de variação da taxa de encontro  
inambu_mont_tumuc_hr$`Sem termo`$dht$individuals$summary[1:9]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCB0cmlsaGFzIG91IGVzdGHDp8O1ZXMgYW1vc3RyYWlzLCBlc2ZvcsOnbyB0b3RhbCBlbSBjYWRhIHRyaWxoYSwgYWJ1bmTDom5jaWEgZXN0aW1hZGEgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIG7Dum1lcm8gZGUgZGV0ZWPDp8O1ZXMgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIMOhcmVhIHRvdGFsIGFtb3N0cmFkYVxuaW5hbWJ1X21vbnRfdHVtdWNfaHIkYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJE5oYXQuYnkuc2FtcGxlWzE6OF1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, trilhas ou estações amostrais, esforço total em cada trilha, abundância estimada em cada estação amostral, número de detecções em cada estação amostral, área total amostrada
inambu_mont_tumuc_hr$`Sem termo`$dht$individuals$Nhat.by.sample[1:8]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyB0b3RhbCwgZGVuc2lkYWRlIGVzdGltYWRhLCBlcnJvIHBhZHLDo28gZGEgZGVuc2lkYWRlIGRlc3RpbWFkYSwgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSBkZW5zaWRhZGUgZGVzdGltYWRhLCBpbnRlcnZhbG8gZGUgY29uZmlhbsOnYSBpbmZlcmlvciBlIHN1cGVyaW9yIGRvIGNvZWZpY2llbnRlIGRlIHZhcmlhw6fDo28sIGdydWFzIGRlIGxpYmVyZGFkZVxuaW5hbWJ1X21vbnRfdHVtdWNfaHIkYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJERcblxuYGBgIn0= -->

```r
# total, densidade estimada, erro padrão da densidade destimada, coeficiente de variação da densidade destimada, intervalo de confiança inferior e superior do coeficiente de variação, gruas de liberdade
inambu_mont_tumuc_hr$`Sem termo`$dht$individuals$D

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Tetnativa de filtragem e seleção de dados para eliminar repetições

Na primeira tentativa de ajuste do modelo distance para dados sem repetição, os dados da cutia *Dasyprocta croconota*, sem estratificação, para o Parna da Serra do Pardo, Esec da Terra do Meio e Resex Riozinho do Anfrísio. Em seguida, os dados da mesma espécie serão analisados para Resex Tapajós-Arapiuns estratificados por ano.

## *Dasyprocta croconota* na **Parna da Serra do Pardo**

![Fonte: biolib.cz](https://www.biolib.cz/IMG/GAL/BIG/205849.jpg)

### Carregar dados


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3NlcnJhX3BhcmRvX3F1YXNlX3NlbV9yZXBldGljYW8gPC0gdHJhbnNmb3JtYV9wYXJhX2RzaXRhbmNlUl9xdWFzZV9zZW1fcmVwZXRpY2FvX2ZpbHRyYV91Y19zcChcbiAgZGFkb3MgPSBkYWRvc19zZWxlY2lvbmFkb3MsXG4gIG5vbWVfdWMgPSBcIlBhcm5hIGRhIFNlcnJhIGRvIFBhcmRvXCIsXG4gIG5vbWVfc3AgPSBcIkRhc3lwcm9jdGEgY3JvY29ub3RhXCJcbilcblxuZGFzeV9jcm9jX3NlcnJhX3BhcmRvX3F1YXNlX3NlbV9yZXBldGljYW9cbmBgYCJ9 -->

```r
dasy_croc_serra_pardo_quase_sem_repeticao <- transforma_para_dsitanceR_quase_sem_repeticao_filtra_uc_sp(
  dados = dados_selecionados,
  nome_uc = "Parna da Serra do Pardo",
  nome_sp = "Dasyprocta croconota"
)

dasy_croc_serra_pardo_quase_sem_repeticao
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Distribuição das distâncias perpendiculares


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3NlcnJhX3BhcmRvX3F1YXNlX3NlbV9yZXBldGljYW8gfD4gXG4gIHBsb3Rhcl9kaXN0cmlidWljYW9fZGlzdGFuY2lhX2ludGVyYXRpdm8obGFyZ3VyYV9jYWl4YSA9IDEpXG5gYGAifQ== -->

```r
dasy_croc_serra_pardo_quase_sem_repeticao |> 
  plotar_distribuicao_distancia_interativo(largura_caixa = 1)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Ajustando modelo distance com função de detecção Half-Normal e distancia de truncamento 11


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3NlcnJhX3BhcmRvX3F1YXNlX3NlbV9yZXBldGljYW9faG4gPC0gZGFzeV9jcm9jX3NlcnJhX3BhcmRvX3F1YXNlX3NlbV9yZXBldGljYW8gfD4gXG4gIGFqdXN0ZV9tb2RlbG9zX2Rpc3RhbmNlX2huKHRydW5jYW1lbnRvID0gMTEpXG5gYGAifQ== -->

```r
dasy_croc_serra_pardo_quase_sem_repeticao_hn <- dasy_croc_serra_pardo_quase_sem_repeticao |> 
  ajuste_modelos_distance_hn(truncamento = 11)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Ajustando modelo distance com função de detecção Hazard-rate e distancia de truncamento 11


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3NlcnJhX3BhcmRvX3F1YXNlX3NlbV9yZXBldGljYW9faHIgPC0gZGFzeV9jcm9jX3NlcnJhX3BhcmRvX3F1YXNlX3NlbV9yZXBldGljYW8gfD4gXG4gIGFqdXN0ZV9tb2RlbG9zX2Rpc3RhbmNlX2hyKHRydW5jYW1lbnRvID0gMTEpXG5gYGAifQ== -->

```r
dasy_croc_serra_pardo_quase_sem_repeticao_hr <- dasy_croc_serra_pardo_quase_sem_repeticao |> 
  ajuste_modelos_distance_hr(truncamento = 11)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Plot dos modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3NlcnJhX3BhcmRvX3F1YXNlX3NlbV9yZXBldGljYW9faG4gfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIHBsb3QoLngpKSBcbmBgYCJ9 -->

```r
dasy_croc_serra_pardo_quase_sem_repeticao_hn |> 
  purrr::map(\(.x) plot(.x)) 
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3NlcnJhX3BhcmRvX3F1YXNlX3NlbV9yZXBldGljYW9faHIgfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIHBsb3QoLngpKSBcbmBgYCJ9 -->

```r
dasy_croc_serra_pardo_quase_sem_repeticao_hr |> 
  purrr::map(\(.x) plot(.x)) 
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Seleção de modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VtbWFyaXplX2RzX21vZGVscyhcbiAgZGFzeV9jcm9jX3NlcnJhX3BhcmRvX3F1YXNlX3NlbV9yZXBldGljYW9faG4kYFNlbSB0ZXJtb2AsXG4gIGRhc3lfY3JvY19zZXJyYV9wYXJkb19xdWFzZV9zZW1fcmVwZXRpY2FvX2huJENvc3Nlbm8sXG4gIGRhc3lfY3JvY19zZXJyYV9wYXJkb19xdWFzZV9zZW1fcmVwZXRpY2FvX2huJGBIZXJtaXRlIHBvbGlub21pYWxgLFxuICBkYXN5X2Nyb2Nfc2VycmFfcGFyZG9fcXVhc2Vfc2VtX3JlcGV0aWNhb19ociRgU2VtIHRlcm1vYCxcbiAgZGFzeV9jcm9jX3NlcnJhX3BhcmRvX3F1YXNlX3NlbV9yZXBldGljYW9faHIkQ29zc2VubyxcbiAgZGFzeV9jcm9jX3NlcnJhX3BhcmRvX3F1YXNlX3NlbV9yZXBldGljYW9faHIkYFBvbGlub21pYWwgc2ltcGxlc2BcbilcbmBgYCJ9 -->

```r
summarize_ds_models(
  dasy_croc_serra_pardo_quase_sem_repeticao_hn$`Sem termo`,
  dasy_croc_serra_pardo_quase_sem_repeticao_hn$Cosseno,
  dasy_croc_serra_pardo_quase_sem_repeticao_hn$`Hermite polinomial`,
  dasy_croc_serra_pardo_quase_sem_repeticao_hr$`Sem termo`,
  dasy_croc_serra_pardo_quase_sem_repeticao_hr$Cosseno,
  dasy_croc_serra_pardo_quase_sem_repeticao_hr$`Polinomial simples`
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Teste de bondade de ajuste


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3NlcnJhX3BhcmRvX3F1YXNlX3NlbV9yZXBldGljYW9faG4gfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIGdvZl9kcyhtb2RlbCA9IC54KSlcbmBgYCJ9 -->

```r
dasy_croc_serra_pardo_quase_sem_repeticao_hn |> 
  purrr::map(\(.x) gof_ds(model = .x))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3NlcnJhX3BhcmRvX3F1YXNlX3NlbV9yZXBldGljYW9faHIgfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIGdvZl9kcyhtb2RlbCA9IC54KSlcbmBgYCJ9 -->

```r
dasy_croc_serra_pardo_quase_sem_repeticao_hr |> 
  purrr::map(\(.x) gof_ds(model = .x))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimando a abundancia


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCBhcmVhIGNvYmVydGEgcGVsbyBlc2ZvcsOnbyBhbW9zdHJhbCwgZXNmb3LDp28gYW1vc3RyYWwgZW0gbWV0cm9zLCBuw7ptZXJvIGRlIGRldGVjw6fDtWVzLCBuw7ptZXJvIGRlIHRyYW5zZWN0b3MgKGVhKSwgdGF4YSBkZSBlbmNvbnRybywgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSB0YXhhIGRlIGVuY29udHJvICBcbmRhc3lfY3JvY19zZXJyYV9wYXJkb19xdWFzZV9zZW1fcmVwZXRpY2FvX2huJENvc3Nlbm8kZGh0JGluZGl2aWR1YWxzJHN1bW1hcnlbMTo5XVxuYGBgIn0= -->

```r
# área de estudo, tamanho da área de estudo, area coberta pelo esforço amostral, esforço amostral em metros, número de detecções, número de transectos (ea), taxa de encontro, coeficiente de variação da taxa de encontro  
dasy_croc_serra_pardo_quase_sem_repeticao_hn$Cosseno$dht$individuals$summary[1:9]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCB0cmlsaGFzIG91IGVzdGHDp8O1ZXMgYW1vc3RyYWlzLCBlc2ZvcsOnbyB0b3RhbCBlbSBjYWRhIHRyaWxoYSwgYWJ1bmTDom5jaWEgZXN0aW1hZGEgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIG7Dum1lcm8gZGUgZGV0ZWPDp8O1ZXMgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIMOhcmVhIHRvdGFsIGFtb3N0cmFkYVxuZGFzeV9jcm9jX3NlcnJhX3BhcmRvX3F1YXNlX3NlbV9yZXBldGljYW9faG4kQ29zc2VubyRkaHQkaW5kaXZpZHVhbHMkTmhhdC5ieS5zYW1wbGVbMTo4XVxuYGBgIn0= -->

```r
# área de estudo, tamanho da área de estudo, trilhas ou estações amostrais, esforço total em cada trilha, abundância estimada em cada estação amostral, número de detecções em cada estação amostral, área total amostrada
dasy_croc_serra_pardo_quase_sem_repeticao_hn$Cosseno$dht$individuals$Nhat.by.sample[1:8]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyB0b3RhbCwgZGVuc2lkYWRlIGVzdGltYWRhLCBlcnJvIHBhZHLDo28gZGEgZGVuc2lkYWRlIGVzdGltYWRhLCBjb2VmaWNpZW50ZSBkZSB2YXJpYcOnw6NvIGRhIGRlbnNpZGFkZSBlc3RpbWFkYSwgaW50ZXJ2YWxvIGRlIGNvbmZpYW7Dp2EgaW5mZXJpb3IgZSBzdXBlcmlvciBkbyBjb2VmaWNpZW50ZSBkZSB2YXJpYcOnw6NvLCBncnVhcyBkZSBsaWJlcmRhZGVcbmRhc3lfY3JvY19zZXJyYV9wYXJkb19xdWFzZV9zZW1fcmVwZXRpY2FvX2huJENvc3Nlbm8kZGh0JGluZGl2aWR1YWxzJERcblxuYGBgIn0= -->

```r
# total, densidade estimada, erro padrão da densidade estimada, coeficiente de variação da densidade estimada, intervalo de confiança inferior e superior do coeficiente de variação, gruas de liberdade
dasy_croc_serra_pardo_quase_sem_repeticao_hn$Cosseno$dht$individuals$D

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCBhcmVhIGNvYmVydGEgcGVsbyBlc2ZvcsOnbyBhbW9zdHJhbCwgZXNmb3LDp28gYW1vc3RyYWwgZW0gbWV0cm9zLCBuw7ptZXJvIGRlIGRldGVjw6fDtWVzLCBuw7ptZXJvIGRlIHRyYW5zZWN0b3MgKGVhKSwgdGF4YSBkZSBlbmNvbnRybywgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSB0YXhhIGRlIGVuY29udHJvICBcbmRhc3lfY3JvY19zZXJyYV9wYXJkb19xdWFzZV9zZW1fcmVwZXRpY2FvX2hyJGBQb2xpbm9taWFsIHNpbXBsZXNgJGRodCRpbmRpdmlkdWFscyRzdW1tYXJ5WzE6OV1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, area coberta pelo esforço amostral, esforço amostral em metros, número de detecções, número de transectos (ea), taxa de encontro, coeficiente de variação da taxa de encontro  
dasy_croc_serra_pardo_quase_sem_repeticao_hr$`Polinomial simples`$dht$individuals$summary[1:9]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCB0cmlsaGFzIG91IGVzdGHDp8O1ZXMgYW1vc3RyYWlzLCBlc2ZvcsOnbyB0b3RhbCBlbSBjYWRhIHRyaWxoYSwgYWJ1bmTDom5jaWEgZXN0aW1hZGEgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIG7Dum1lcm8gZGUgZGV0ZWPDp8O1ZXMgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIMOhcmVhIHRvdGFsIGFtb3N0cmFkYVxuZGFzeV9jcm9jX3NlcnJhX3BhcmRvX3F1YXNlX3NlbV9yZXBldGljYW9faHIkYFBvbGlub21pYWwgc2ltcGxlc2AkZGh0JGluZGl2aWR1YWxzJE5oYXQuYnkuc2FtcGxlWzE6OF1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, trilhas ou estações amostrais, esforço total em cada trilha, abundância estimada em cada estação amostral, número de detecções em cada estação amostral, área total amostrada
dasy_croc_serra_pardo_quase_sem_repeticao_hr$`Polinomial simples`$dht$individuals$Nhat.by.sample[1:8]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyB0b3RhbCwgZGVuc2lkYWRlIGVzdGltYWRhLCBlcnJvIHBhZHLDo28gZGEgZGVuc2lkYWRlIGRlc3RpbWFkYSwgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSBkZW5zaWRhZGUgZGVzdGltYWRhLCBpbnRlcnZhbG8gZGUgY29uZmlhbsOnYSBpbmZlcmlvciBlIHN1cGVyaW9yIGRvIGNvZWZpY2llbnRlIGRlIHZhcmlhw6fDo28sIGdydWFzIGRlIGxpYmVyZGFkZVxuZGFzeV9jcm9jX3NlcnJhX3BhcmRvX3F1YXNlX3NlbV9yZXBldGljYW9faHIkYFBvbGlub21pYWwgc2ltcGxlc2AkZGh0JGluZGl2aWR1YWxzJERcblxuYGBgIn0= -->

```r
# total, densidade estimada, erro padrão da densidade destimada, coeficiente de variação da densidade destimada, intervalo de confiança inferior e superior do coeficiente de variação, gruas de liberdade
dasy_croc_serra_pardo_quase_sem_repeticao_hr$`Polinomial simples`$dht$individuals$D

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## *Dasyprocta croconota* na **Esec da Terra do Meio**

![Fonte: biolib.cz](https://www.biolib.cz/IMG/GAL/BIG/205849.jpg)

### Carregar dados


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3RlcnJhX21laW9fcXVhc2Vfc2VtX3JlcGV0aWNhbyA8LSB0cmFuc2Zvcm1hX3BhcmFfZHNpdGFuY2VSX3F1YXNlX3NlbV9yZXBldGljYW9fZmlsdHJhX3VjX3NwKFxuICBkYWRvcyA9IGRhZG9zX3NlbGVjaW9uYWRvcyxcbiAgbm9tZV91YyA9IFwiRXNlYyBkYSBUZXJyYSBkbyBNZWlvXCIsXG4gIG5vbWVfc3AgPSBcIkRhc3lwcm9jdGEgY3JvY29ub3RhXCJcbilcblxuZGFzeV9jcm9jX3RlcnJhX21laW9fcXVhc2Vfc2VtX3JlcGV0aWNhb1xuYGBgIn0= -->

```r
dasy_croc_terra_meio_quase_sem_repeticao <- transforma_para_dsitanceR_quase_sem_repeticao_filtra_uc_sp(
  dados = dados_selecionados,
  nome_uc = "Esec da Terra do Meio",
  nome_sp = "Dasyprocta croconota"
)

dasy_croc_terra_meio_quase_sem_repeticao
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Distribuição das distâncias perpendiculares


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3RlcnJhX21laW9fcXVhc2Vfc2VtX3JlcGV0aWNhbyB8PiBcbiAgcGxvdGFyX2Rpc3RyaWJ1aWNhb19kaXN0YW5jaWFfaW50ZXJhdGl2byhsYXJndXJhX2NhaXhhID0gMSlcbmBgYCJ9 -->

```r
dasy_croc_terra_meio_quase_sem_repeticao |> 
  plotar_distribuicao_distancia_interativo(largura_caixa = 1)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Ajustando modelo distance com função de detecção Half-Normal e distancia de truncamento 10


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3RlcnJhX21laW9fcXVhc2Vfc2VtX3JlcGV0aWNhb19obiA8LSBkYXN5X2Nyb2NfdGVycmFfbWVpb19xdWFzZV9zZW1fcmVwZXRpY2FvIHw+IFxuICBhanVzdGVfbW9kZWxvc19kaXN0YW5jZV9obih0cnVuY2FtZW50byA9IDEwKVxuYGBgIn0= -->

```r
dasy_croc_terra_meio_quase_sem_repeticao_hn <- dasy_croc_terra_meio_quase_sem_repeticao |> 
  ajuste_modelos_distance_hn(truncamento = 10)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Ajustando modelo distance com função de detecção Hazard-rate e distancia de truncamento 10


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3RlcnJhX21laW9fcXVhc2Vfc2VtX3JlcGV0aWNhb19ociA8LSBkYXN5X2Nyb2NfdGVycmFfbWVpb19xdWFzZV9zZW1fcmVwZXRpY2FvIHw+IFxuICBhanVzdGVfbW9kZWxvc19kaXN0YW5jZV9ocih0cnVuY2FtZW50byA9IDEwKVxuYGBgIn0= -->

```r
dasy_croc_terra_meio_quase_sem_repeticao_hr <- dasy_croc_terra_meio_quase_sem_repeticao |> 
  ajuste_modelos_distance_hr(truncamento = 10)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Plot dos modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3RlcnJhX21laW9fcXVhc2Vfc2VtX3JlcGV0aWNhb19obiB8PiBcbiAgcHVycnI6Om1hcChcXCgueCkgcGxvdCgueCkpIFxuYGBgIn0= -->

```r
dasy_croc_terra_meio_quase_sem_repeticao_hn |> 
  purrr::map(\(.x) plot(.x)) 
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3RlcnJhX21laW9fcXVhc2Vfc2VtX3JlcGV0aWNhb19ociB8PiBcbiAgcHVycnI6Om1hcChcXCgueCkgcGxvdCgueCkpIFxuYGBgIn0= -->

```r
dasy_croc_terra_meio_quase_sem_repeticao_hr |> 
  purrr::map(\(.x) plot(.x)) 
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Seleção de modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VtbWFyaXplX2RzX21vZGVscyhcbiAgZGFzeV9jcm9jX3RlcnJhX21laW9fcXVhc2Vfc2VtX3JlcGV0aWNhb19obiRgU2VtIHRlcm1vYCxcbiAgZGFzeV9jcm9jX3RlcnJhX21laW9fcXVhc2Vfc2VtX3JlcGV0aWNhb19obiRDb3NzZW5vLFxuICBkYXN5X2Nyb2NfdGVycmFfbWVpb19xdWFzZV9zZW1fcmVwZXRpY2FvX2huJGBIZXJtaXRlIHBvbGlub21pYWxgLFxuICBkYXN5X2Nyb2NfdGVycmFfbWVpb19xdWFzZV9zZW1fcmVwZXRpY2FvX2hyJGBTZW0gdGVybW9gLFxuICBkYXN5X2Nyb2NfdGVycmFfbWVpb19xdWFzZV9zZW1fcmVwZXRpY2FvX2hyJENvc3Nlbm8sXG4gIGRhc3lfY3JvY190ZXJyYV9tZWlvX3F1YXNlX3NlbV9yZXBldGljYW9faHIkYFBvbGlub21pYWwgc2ltcGxlc2BcbilcbmBgYCJ9 -->

```r
summarize_ds_models(
  dasy_croc_terra_meio_quase_sem_repeticao_hn$`Sem termo`,
  dasy_croc_terra_meio_quase_sem_repeticao_hn$Cosseno,
  dasy_croc_terra_meio_quase_sem_repeticao_hn$`Hermite polinomial`,
  dasy_croc_terra_meio_quase_sem_repeticao_hr$`Sem termo`,
  dasy_croc_terra_meio_quase_sem_repeticao_hr$Cosseno,
  dasy_croc_terra_meio_quase_sem_repeticao_hr$`Polinomial simples`
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Teste de bondade de ajuste


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3RlcnJhX21laW9fcXVhc2Vfc2VtX3JlcGV0aWNhb19obiB8PiBcbiAgcHVycnI6Om1hcChcXCgueCkgZ29mX2RzKG1vZGVsID0gLngpKVxuYGBgIn0= -->

```r
dasy_croc_terra_meio_quase_sem_repeticao_hn |> 
  purrr::map(\(.x) gof_ds(model = .x))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3RlcnJhX21laW9fcXVhc2Vfc2VtX3JlcGV0aWNhb19ociB8PiBcbiAgcHVycnI6Om1hcChcXCgueCkgZ29mX2RzKG1vZGVsID0gLngpKVxuYGBgIn0= -->

```r
dasy_croc_terra_meio_quase_sem_repeticao_hr |> 
  purrr::map(\(.x) gof_ds(model = .x))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimando a abundancia


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCBhcmVhIGNvYmVydGEgcGVsbyBlc2ZvcsOnbyBhbW9zdHJhbCwgZXNmb3LDp28gYW1vc3RyYWwgZW0gbWV0cm9zLCBuw7ptZXJvIGRlIGRldGVjw6fDtWVzLCBuw7ptZXJvIGRlIHRyYW5zZWN0b3MgKGVhKSwgdGF4YSBkZSBlbmNvbnRybywgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSB0YXhhIGRlIGVuY29udHJvICBcbmRhc3lfY3JvY190ZXJyYV9tZWlvX3F1YXNlX3NlbV9yZXBldGljYW9faG4kYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJHN1bW1hcnlbMTo5XVxuYGBgIn0= -->

```r
# área de estudo, tamanho da área de estudo, area coberta pelo esforço amostral, esforço amostral em metros, número de detecções, número de transectos (ea), taxa de encontro, coeficiente de variação da taxa de encontro  
dasy_croc_terra_meio_quase_sem_repeticao_hn$`Sem termo`$dht$individuals$summary[1:9]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCB0cmlsaGFzIG91IGVzdGHDp8O1ZXMgYW1vc3RyYWlzLCBlc2ZvcsOnbyB0b3RhbCBlbSBjYWRhIHRyaWxoYSwgYWJ1bmTDom5jaWEgZXN0aW1hZGEgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIG7Dum1lcm8gZGUgZGV0ZWPDp8O1ZXMgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIMOhcmVhIHRvdGFsIGFtb3N0cmFkYVxuZGFzeV9jcm9jX3RlcnJhX21laW9fcXVhc2Vfc2VtX3JlcGV0aWNhb19obiRDb3NzZW5vJGRodCRpbmRpdmlkdWFscyROaGF0LmJ5LnNhbXBsZVsxOjhdXG5gYGAifQ== -->

```r
# área de estudo, tamanho da área de estudo, trilhas ou estações amostrais, esforço total em cada trilha, abundância estimada em cada estação amostral, número de detecções em cada estação amostral, área total amostrada
dasy_croc_terra_meio_quase_sem_repeticao_hn$Cosseno$dht$individuals$Nhat.by.sample[1:8]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyB0b3RhbCwgZGVuc2lkYWRlIGVzdGltYWRhLCBlcnJvIHBhZHLDo28gZGEgZGVuc2lkYWRlIGVzdGltYWRhLCBjb2VmaWNpZW50ZSBkZSB2YXJpYcOnw6NvIGRhIGRlbnNpZGFkZSBlc3RpbWFkYSwgaW50ZXJ2YWxvIGRlIGNvbmZpYW7Dp2EgaW5mZXJpb3IgZSBzdXBlcmlvciBkbyBjb2VmaWNpZW50ZSBkZSB2YXJpYcOnw6NvLCBncnVhcyBkZSBsaWJlcmRhZGVcbmRhc3lfY3JvY190ZXJyYV9tZWlvX3F1YXNlX3NlbV9yZXBldGljYW9faG4kQ29zc2VubyRkaHQkaW5kaXZpZHVhbHMkRFxuXG5gYGAifQ== -->

```r
# total, densidade estimada, erro padrão da densidade estimada, coeficiente de variação da densidade estimada, intervalo de confiança inferior e superior do coeficiente de variação, gruas de liberdade
dasy_croc_terra_meio_quase_sem_repeticao_hn$Cosseno$dht$individuals$D

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCBhcmVhIGNvYmVydGEgcGVsbyBlc2ZvcsOnbyBhbW9zdHJhbCwgZXNmb3LDp28gYW1vc3RyYWwgZW0gbWV0cm9zLCBuw7ptZXJvIGRlIGRldGVjw6fDtWVzLCBuw7ptZXJvIGRlIHRyYW5zZWN0b3MgKGVhKSwgdGF4YSBkZSBlbmNvbnRybywgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSB0YXhhIGRlIGVuY29udHJvICBcbmRhc3lfY3JvY190ZXJyYV9tZWlvX3F1YXNlX3NlbV9yZXBldGljYW9faHIkYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJHN1bW1hcnlbMTo5XVxuYGBgIn0= -->

```r
# área de estudo, tamanho da área de estudo, area coberta pelo esforço amostral, esforço amostral em metros, número de detecções, número de transectos (ea), taxa de encontro, coeficiente de variação da taxa de encontro  
dasy_croc_terra_meio_quase_sem_repeticao_hr$`Sem termo`$dht$individuals$summary[1:9]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCB0cmlsaGFzIG91IGVzdGHDp8O1ZXMgYW1vc3RyYWlzLCBlc2ZvcsOnbyB0b3RhbCBlbSBjYWRhIHRyaWxoYSwgYWJ1bmTDom5jaWEgZXN0aW1hZGEgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIG7Dum1lcm8gZGUgZGV0ZWPDp8O1ZXMgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIMOhcmVhIHRvdGFsIGFtb3N0cmFkYVxuZGFzeV9jcm9jX3RlcnJhX21laW9fcXVhc2Vfc2VtX3JlcGV0aWNhb19ociRgU2VtIHRlcm1vYCRkaHQkaW5kaXZpZHVhbHMkTmhhdC5ieS5zYW1wbGVbMTo4XVxuYGBgIn0= -->

```r
# área de estudo, tamanho da área de estudo, trilhas ou estações amostrais, esforço total em cada trilha, abundância estimada em cada estação amostral, número de detecções em cada estação amostral, área total amostrada
dasy_croc_terra_meio_quase_sem_repeticao_hr$`Sem termo`$dht$individuals$Nhat.by.sample[1:8]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyB0b3RhbCwgZGVuc2lkYWRlIGVzdGltYWRhLCBlcnJvIHBhZHLDo28gZGEgZGVuc2lkYWRlIGRlc3RpbWFkYSwgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSBkZW5zaWRhZGUgZGVzdGltYWRhLCBpbnRlcnZhbG8gZGUgY29uZmlhbsOnYSBpbmZlcmlvciBlIHN1cGVyaW9yIGRvIGNvZWZpY2llbnRlIGRlIHZhcmlhw6fDo28sIGdydWFzIGRlIGxpYmVyZGFkZVxuZGFzeV9jcm9jX3RlcnJhX21laW9fcXVhc2Vfc2VtX3JlcGV0aWNhb19ociRgU2VtIHRlcm1vYCRkaHQkaW5kaXZpZHVhbHMkRFxuXG5gYGAifQ== -->

```r
# total, densidade estimada, erro padrão da densidade destimada, coeficiente de variação da densidade destimada, intervalo de confiança inferior e superior do coeficiente de variação, gruas de liberdade
dasy_croc_terra_meio_quase_sem_repeticao_hr$`Sem termo`$dht$individuals$D

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## *Dasyprocta croconota* na **Resex Riozinho do Anfrísio**

![Fonte: biolib.cz](https://www.biolib.cz/IMG/GAL/BIG/205849.jpg)

### Carregar dados


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3Jpb19hbmZyX3F1YXNlX3NlbV9yZXBldGljYW8gPC0gdHJhbnNmb3JtYV9wYXJhX2RzaXRhbmNlUl9xdWFzZV9zZW1fcmVwZXRpY2FvX2ZpbHRyYV91Y19zcChcbiAgZGFkb3MgPSBkYWRvc19zZWxlY2lvbmFkb3MsXG4gIG5vbWVfdWMgPSBcIlJlc2V4IFJpb3ppbmhvIGRvIEFuZnLDrXNpb1wiLCBcbiAgbm9tZV9zcCA9IFwiRGFzeXByb2N0YSBjcm9jb25vdGFcIlxuICApIFxuXG5kYXN5X2Nyb2NfcmlvX2FuZnJfcXVhc2Vfc2VtX3JlcGV0aWNhb1xuYGBgIn0= -->

```r
dasy_croc_rio_anfr_quase_sem_repeticao <- transforma_para_dsitanceR_quase_sem_repeticao_filtra_uc_sp(
  dados = dados_selecionados,
  nome_uc = "Resex Riozinho do Anfrísio", 
  nome_sp = "Dasyprocta croconota"
  ) 

dasy_croc_rio_anfr_quase_sem_repeticao
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Distribuição das distâncias perpendiculares


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3Jpb19hbmZyX3F1YXNlX3NlbV9yZXBldGljYW8gfD4gXG4gIHBsb3Rhcl9kaXN0cmlidWljYW9fZGlzdGFuY2lhX2ludGVyYXRpdm8obGFyZ3VyYV9jYWl4YSA9IDEpXG5gYGAifQ== -->

```r
dasy_croc_rio_anfr_quase_sem_repeticao |> 
  plotar_distribuicao_distancia_interativo(largura_caixa = 1)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Ajustando modelo distance com função de detecção Half-Normal e distancia de truncamento 11


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3Jpb19hbmZyX3F1YXNlX3NlbV9yZXBldGljYW9faG4gPC0gZGFzeV9jcm9jX3Jpb19hbmZyX3F1YXNlX3NlbV9yZXBldGljYW8gfD4gXG4gIGFqdXN0ZV9tb2RlbG9zX2Rpc3RhbmNlX2huKHRydW5jYW1lbnRvID0gMTEpXG5gYGAifQ== -->

```r
dasy_croc_rio_anfr_quase_sem_repeticao_hn <- dasy_croc_rio_anfr_quase_sem_repeticao |> 
  ajuste_modelos_distance_hn(truncamento = 11)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Ajustando modelo distance com função de detecção Hazard-rate e distancia de truncamento 11


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3Jpb19hbmZyX3F1YXNlX3NlbV9yZXBldGljYW9faHIgPC0gZGFzeV9jcm9jX3Jpb19hbmZyX3F1YXNlX3NlbV9yZXBldGljYW8gfD4gXG4gIGFqdXN0ZV9tb2RlbG9zX2Rpc3RhbmNlX2hyKHRydW5jYW1lbnRvID0gMTEpXG5gYGAifQ== -->

```r
dasy_croc_rio_anfr_quase_sem_repeticao_hr <- dasy_croc_rio_anfr_quase_sem_repeticao |> 
  ajuste_modelos_distance_hr(truncamento = 11)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Plot dos modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3Jpb19hbmZyX3F1YXNlX3NlbV9yZXBldGljYW9faG4gfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIHBsb3QoLngpKSBcbmBgYCJ9 -->

```r
dasy_croc_rio_anfr_quase_sem_repeticao_hn |> 
  purrr::map(\(.x) plot(.x)) 
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3Jpb19hbmZyX3F1YXNlX3NlbV9yZXBldGljYW9faHIgfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIHBsb3QoLngpKSBcbmBgYCJ9 -->

```r
dasy_croc_rio_anfr_quase_sem_repeticao_hr |> 
  purrr::map(\(.x) plot(.x)) 
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Seleção de modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VtbWFyaXplX2RzX21vZGVscyhcbiAgZGFzeV9jcm9jX3Jpb19hbmZyX3F1YXNlX3NlbV9yZXBldGljYW9faG4kYFNlbSB0ZXJtb2AsXG4gIGRhc3lfY3JvY19yaW9fYW5mcl9xdWFzZV9zZW1fcmVwZXRpY2FvX2huJENvc3Nlbm8sXG4gIGRhc3lfY3JvY19yaW9fYW5mcl9xdWFzZV9zZW1fcmVwZXRpY2FvX2huJGBIZXJtaXRlIHBvbGlub21pYWxgLFxuICBkYXN5X2Nyb2NfcmlvX2FuZnJfcXVhc2Vfc2VtX3JlcGV0aWNhb19ociRgU2VtIHRlcm1vYCxcbiAgZGFzeV9jcm9jX3Jpb19hbmZyX3F1YXNlX3NlbV9yZXBldGljYW9faHIkQ29zc2VubyxcbiAgZGFzeV9jcm9jX3Jpb19hbmZyX3F1YXNlX3NlbV9yZXBldGljYW9faHIkYFBvbGlub21pYWwgc2ltcGxlc2BcbilcbmBgYCJ9 -->

```r
summarize_ds_models(
  dasy_croc_rio_anfr_quase_sem_repeticao_hn$`Sem termo`,
  dasy_croc_rio_anfr_quase_sem_repeticao_hn$Cosseno,
  dasy_croc_rio_anfr_quase_sem_repeticao_hn$`Hermite polinomial`,
  dasy_croc_rio_anfr_quase_sem_repeticao_hr$`Sem termo`,
  dasy_croc_rio_anfr_quase_sem_repeticao_hr$Cosseno,
  dasy_croc_rio_anfr_quase_sem_repeticao_hr$`Polinomial simples`
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Teste de bondade de ajuste


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3Jpb19hbmZyX3F1YXNlX3NlbV9yZXBldGljYW9faG4gfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIGdvZl9kcyhtb2RlbCA9IC54KSlcbmBgYCJ9 -->

```r
dasy_croc_rio_anfr_quase_sem_repeticao_hn |> 
  purrr::map(\(.x) gof_ds(model = .x))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3Jpb19hbmZyX3F1YXNlX3NlbV9yZXBldGljYW9faHIgfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIGdvZl9kcyhtb2RlbCA9IC54KSlcbmBgYCJ9 -->

```r
dasy_croc_rio_anfr_quase_sem_repeticao_hr |> 
  purrr::map(\(.x) gof_ds(model = .x))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimando a abundancia


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCBhcmVhIGNvYmVydGEgcGVsbyBlc2ZvcsOnbyBhbW9zdHJhbCwgZXNmb3LDp28gYW1vc3RyYWwgZW0gbWV0cm9zLCBuw7ptZXJvIGRlIGRldGVjw6fDtWVzLCBuw7ptZXJvIGRlIHRyYW5zZWN0b3MgKGVhKSwgdGF4YSBkZSBlbmNvbnRybywgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSB0YXhhIGRlIGVuY29udHJvICBcbmRhc3lfY3JvY19yaW9fYW5mcl9xdWFzZV9zZW1fcmVwZXRpY2FvX2huJGBTZW0gdGVybW9gJGRodCRpbmRpdmlkdWFscyRzdW1tYXJ5WzE6OV1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, area coberta pelo esforço amostral, esforço amostral em metros, número de detecções, número de transectos (ea), taxa de encontro, coeficiente de variação da taxa de encontro  
dasy_croc_rio_anfr_quase_sem_repeticao_hn$`Sem termo`$dht$individuals$summary[1:9]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCB0cmlsaGFzIG91IGVzdGHDp8O1ZXMgYW1vc3RyYWlzLCBlc2ZvcsOnbyB0b3RhbCBlbSBjYWRhIHRyaWxoYSwgYWJ1bmTDom5jaWEgZXN0aW1hZGEgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIG7Dum1lcm8gZGUgZGV0ZWPDp8O1ZXMgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIMOhcmVhIHRvdGFsIGFtb3N0cmFkYVxuZGFzeV9jcm9jX3Jpb19hbmZyX3F1YXNlX3NlbV9yZXBldGljYW9faG4kYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJE5oYXQuYnkuc2FtcGxlWzE6OF1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, trilhas ou estações amostrais, esforço total em cada trilha, abundância estimada em cada estação amostral, número de detecções em cada estação amostral, área total amostrada
dasy_croc_rio_anfr_quase_sem_repeticao_hn$`Sem termo`$dht$individuals$Nhat.by.sample[1:8]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyB0b3RhbCwgZGVuc2lkYWRlIGVzdGltYWRhLCBlcnJvIHBhZHLDo28gZGEgZGVuc2lkYWRlIGVzdGltYWRhLCBjb2VmaWNpZW50ZSBkZSB2YXJpYcOnw6NvIGRhIGRlbnNpZGFkZSBlc3RpbWFkYSwgaW50ZXJ2YWxvIGRlIGNvbmZpYW7Dp2EgaW5mZXJpb3IgZSBzdXBlcmlvciBkbyBjb2VmaWNpZW50ZSBkZSB2YXJpYcOnw6NvLCBncnVhcyBkZSBsaWJlcmRhZGVcbmRhc3lfY3JvY19yaW9fYW5mcl9xdWFzZV9zZW1fcmVwZXRpY2FvX2huJGBTZW0gdGVybW9gJGRodCRpbmRpdmlkdWFscyREXG5cbmBgYCJ9 -->

```r
# total, densidade estimada, erro padrão da densidade estimada, coeficiente de variação da densidade estimada, intervalo de confiança inferior e superior do coeficiente de variação, gruas de liberdade
dasy_croc_rio_anfr_quase_sem_repeticao_hn$`Sem termo`$dht$individuals$D

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCBhcmVhIGNvYmVydGEgcGVsbyBlc2ZvcsOnbyBhbW9zdHJhbCwgZXNmb3LDp28gYW1vc3RyYWwgZW0gbWV0cm9zLCBuw7ptZXJvIGRlIGRldGVjw6fDtWVzLCBuw7ptZXJvIGRlIHRyYW5zZWN0b3MgKGVhKSwgdGF4YSBkZSBlbmNvbnRybywgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSB0YXhhIGRlIGVuY29udHJvICBcbmRhc3lfY3JvY19yaW9fYW5mcl9xdWFzZV9zZW1fcmVwZXRpY2FvX2hyJGBTZW0gdGVybW9gJGRodCRpbmRpdmlkdWFscyRzdW1tYXJ5WzE6OV1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, area coberta pelo esforço amostral, esforço amostral em metros, número de detecções, número de transectos (ea), taxa de encontro, coeficiente de variação da taxa de encontro  
dasy_croc_rio_anfr_quase_sem_repeticao_hr$`Sem termo`$dht$individuals$summary[1:9]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCB0cmlsaGFzIG91IGVzdGHDp8O1ZXMgYW1vc3RyYWlzLCBlc2ZvcsOnbyB0b3RhbCBlbSBjYWRhIHRyaWxoYSwgYWJ1bmTDom5jaWEgZXN0aW1hZGEgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIG7Dum1lcm8gZGUgZGV0ZWPDp8O1ZXMgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIMOhcmVhIHRvdGFsIGFtb3N0cmFkYVxuZGFzeV9jcm9jX3Jpb19hbmZyX3F1YXNlX3NlbV9yZXBldGljYW9faHIkYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJE5oYXQuYnkuc2FtcGxlWzE6OF1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, trilhas ou estações amostrais, esforço total em cada trilha, abundância estimada em cada estação amostral, número de detecções em cada estação amostral, área total amostrada
dasy_croc_rio_anfr_quase_sem_repeticao_hr$`Sem termo`$dht$individuals$Nhat.by.sample[1:8]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyB0b3RhbCwgZGVuc2lkYWRlIGVzdGltYWRhLCBlcnJvIHBhZHLDo28gZGEgZGVuc2lkYWRlIGRlc3RpbWFkYSwgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSBkZW5zaWRhZGUgZGVzdGltYWRhLCBpbnRlcnZhbG8gZGUgY29uZmlhbsOnYSBpbmZlcmlvciBlIHN1cGVyaW9yIGRvIGNvZWZpY2llbnRlIGRlIHZhcmlhw6fDo28sIGdydWFzIGRlIGxpYmVyZGFkZVxuZGFzeV9jcm9jX3Jpb19hbmZyX3F1YXNlX3NlbV9yZXBldGljYW9faHIkYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJERcblxuYGBgIn0= -->

```r
# total, densidade estimada, erro padrão da densidade destimada, coeficiente de variação da densidade destimada, intervalo de confiança inferior e superior do coeficiente de variação, gruas de liberdade
dasy_croc_rio_anfr_quase_sem_repeticao_hr$`Sem termo`$dht$individuals$D

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## *Dasyprocta croconota* na **Resex Tapajós Arapiuns** - estratificado por ano

![Fonte: biolib.cz](https://www.biolib.cz/IMG/GAL/BIG/205849.jpg)

### Carregar dados


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3RhcF9hcmFwX3F1YXNlX3NlbV9yZXBldGljYW8gPC0gdHJhbnNmb3JtYV9wYXJhX2RzaXRhbmNlUl9xdWFzZV9zZW1fcmVwZXRpY2FvX2ZpbHRyYV91Y19zcChcbiAgZGFkb3MgPSBkYWRvc19zZWxlY2lvbmFkb3MsXG4gIG5vbWVfdWMgPSBcIlJlc2V4IFRhcGFqb3MtQXJhcGl1bnNcIiwgXG4gIG5vbWVfc3AgPSBcIkRhc3lwcm9jdGEgY3JvY29ub3RhXCJcbiAgKSBcblxuZGFzeV9jcm9jX3RhcF9hcmFwX3F1YXNlX3NlbV9yZXBldGljYW9cbmBgYCJ9 -->

```r
dasy_croc_tap_arap_quase_sem_repeticao <- transforma_para_dsitanceR_quase_sem_repeticao_filtra_uc_sp(
  dados = dados_selecionados,
  nome_uc = "Resex Tapajos-Arapiuns", 
  nome_sp = "Dasyprocta croconota"
  ) 

dasy_croc_tap_arap_quase_sem_repeticao
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Distribuição das distâncias perpendiculares


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3RhcF9hcmFwX3F1YXNlX3NlbV9yZXBldGljYW8gfD4gXG4gIHBsb3Rhcl9kaXN0cmlidWljYW9fZGlzdGFuY2lhX2ludGVyYXRpdm8obGFyZ3VyYV9jYWl4YSA9IDEpXG5gYGAifQ== -->

```r
dasy_croc_tap_arap_quase_sem_repeticao |> 
  plotar_distribuicao_distancia_interativo(largura_caixa = 1)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Ajustando modelo distance com função de detecção Half-Normal e distancia de truncamento 11


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3RhcF9hcmFwX3F1YXNlX3NlbV9yZXBldGljYW9faG4gPC0gZGFzeV9jcm9jX3RhcF9hcmFwX3F1YXNlX3NlbV9yZXBldGljYW8gfD4gXG4gIGFqdXN0ZV9tb2RlbG9zX2Rpc3RhbmNlX2huKHRydW5jYW1lbnRvID0gMTEpXG5gYGAifQ== -->

```r
dasy_croc_tap_arap_quase_sem_repeticao_hn <- dasy_croc_tap_arap_quase_sem_repeticao |> 
  ajuste_modelos_distance_hn(truncamento = 11)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Ajustando modelo distance com função de detecção Hazard-rate e distancia de truncamento 11


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3RhcF9hcmFwX3F1YXNlX3NlbV9yZXBldGljYW9faHIgPC0gZGFzeV9jcm9jX3RhcF9hcmFwX3F1YXNlX3NlbV9yZXBldGljYW8gfD4gXG4gIGFqdXN0ZV9tb2RlbG9zX2Rpc3RhbmNlX2hyKHRydW5jYW1lbnRvID0gMTEpXG5gYGAifQ== -->

```r
dasy_croc_tap_arap_quase_sem_repeticao_hr <- dasy_croc_tap_arap_quase_sem_repeticao |> 
  ajuste_modelos_distance_hr(truncamento = 11)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Plot dos modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3RhcF9hcmFwX3F1YXNlX3NlbV9yZXBldGljYW9faG4gfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIHBsb3QoLngpKSBcbmBgYCJ9 -->

```r
dasy_croc_tap_arap_quase_sem_repeticao_hn |> 
  purrr::map(\(.x) plot(.x)) 
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3RhcF9hcmFwX3F1YXNlX3NlbV9yZXBldGljYW9faHIgfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIHBsb3QoLngpKSBcbmBgYCJ9 -->

```r
dasy_croc_tap_arap_quase_sem_repeticao_hr |> 
  purrr::map(\(.x) plot(.x)) 
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Seleção de modelos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VtbWFyaXplX2RzX21vZGVscyhcbiAgZGFzeV9jcm9jX3RhcF9hcmFwX3F1YXNlX3NlbV9yZXBldGljYW9faG4kYFNlbSB0ZXJtb2AsXG4gIGRhc3lfY3JvY190YXBfYXJhcF9xdWFzZV9zZW1fcmVwZXRpY2FvX2huJENvc3Nlbm8sXG4gIGRhc3lfY3JvY190YXBfYXJhcF9xdWFzZV9zZW1fcmVwZXRpY2FvX2huJGBIZXJtaXRlIHBvbGlub21pYWxgLFxuICBkYXN5X2Nyb2NfdGFwX2FyYXBfcXVhc2Vfc2VtX3JlcGV0aWNhb19ociRgU2VtIHRlcm1vYCxcbiAgZGFzeV9jcm9jX3RhcF9hcmFwX3F1YXNlX3NlbV9yZXBldGljYW9faHIkQ29zc2VubyxcbiAgZGFzeV9jcm9jX3RhcF9hcmFwX3F1YXNlX3NlbV9yZXBldGljYW9faHIkYFBvbGlub21pYWwgc2ltcGxlc2BcbilcbmBgYCJ9 -->

```r
summarize_ds_models(
  dasy_croc_tap_arap_quase_sem_repeticao_hn$`Sem termo`,
  dasy_croc_tap_arap_quase_sem_repeticao_hn$Cosseno,
  dasy_croc_tap_arap_quase_sem_repeticao_hn$`Hermite polinomial`,
  dasy_croc_tap_arap_quase_sem_repeticao_hr$`Sem termo`,
  dasy_croc_tap_arap_quase_sem_repeticao_hr$Cosseno,
  dasy_croc_tap_arap_quase_sem_repeticao_hr$`Polinomial simples`
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Teste de bondade de ajuste


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3RhcF9hcmFwX3F1YXNlX3NlbV9yZXBldGljYW9faG4gfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIGdvZl9kcyhtb2RlbCA9IC54KSlcbmBgYCJ9 -->

```r
dasy_croc_tap_arap_quase_sem_repeticao_hn |> 
  purrr::map(\(.x) gof_ds(model = .x))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGFzeV9jcm9jX3RhcF9hcmFwX3F1YXNlX3NlbV9yZXBldGljYW9faHIgfD4gXG4gIHB1cnJyOjptYXAoXFwoLngpIGdvZl9kcyhtb2RlbCA9IC54KSlcbmBgYCJ9 -->

```r
dasy_croc_tap_arap_quase_sem_repeticao_hr |> 
  purrr::map(\(.x) gof_ds(model = .x))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimando a abundancia


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCBhcmVhIGNvYmVydGEgcGVsbyBlc2ZvcsOnbyBhbW9zdHJhbCwgZXNmb3LDp28gYW1vc3RyYWwgZW0gbWV0cm9zLCBuw7ptZXJvIGRlIGRldGVjw6fDtWVzLCBuw7ptZXJvIGRlIHRyYW5zZWN0b3MgKGVhKSwgdGF4YSBkZSBlbmNvbnRybywgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSB0YXhhIGRlIGVuY29udHJvICBcbmRhc3lfY3JvY190YXBfYXJhcF9xdWFzZV9zZW1fcmVwZXRpY2FvX2huJGBTZW0gdGVybW9gJGRodCRpbmRpdmlkdWFscyRzdW1tYXJ5WzE6OV1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, area coberta pelo esforço amostral, esforço amostral em metros, número de detecções, número de transectos (ea), taxa de encontro, coeficiente de variação da taxa de encontro  
dasy_croc_tap_arap_quase_sem_repeticao_hn$`Sem termo`$dht$individuals$summary[1:9]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCB0cmlsaGFzIG91IGVzdGHDp8O1ZXMgYW1vc3RyYWlzLCBlc2ZvcsOnbyB0b3RhbCBlbSBjYWRhIHRyaWxoYSwgYWJ1bmTDom5jaWEgZXN0aW1hZGEgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIG7Dum1lcm8gZGUgZGV0ZWPDp8O1ZXMgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIMOhcmVhIHRvdGFsIGFtb3N0cmFkYVxuZGFzeV9jcm9jX3RhcF9hcmFwX3F1YXNlX3NlbV9yZXBldGljYW9faG4kYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJE5oYXQuYnkuc2FtcGxlWzE6OF1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, trilhas ou estações amostrais, esforço total em cada trilha, abundância estimada em cada estação amostral, número de detecções em cada estação amostral, área total amostrada
dasy_croc_tap_arap_quase_sem_repeticao_hn$`Sem termo`$dht$individuals$Nhat.by.sample[1:8]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyB0b3RhbCwgZGVuc2lkYWRlIGVzdGltYWRhLCBlcnJvIHBhZHLDo28gZGEgZGVuc2lkYWRlIGVzdGltYWRhLCBjb2VmaWNpZW50ZSBkZSB2YXJpYcOnw6NvIGRhIGRlbnNpZGFkZSBlc3RpbWFkYSwgaW50ZXJ2YWxvIGRlIGNvbmZpYW7Dp2EgaW5mZXJpb3IgZSBzdXBlcmlvciBkbyBjb2VmaWNpZW50ZSBkZSB2YXJpYcOnw6NvLCBncnVhcyBkZSBsaWJlcmRhZGVcbmRhc3lfY3JvY190YXBfYXJhcF9xdWFzZV9zZW1fcmVwZXRpY2FvX2huJGBTZW0gdGVybW9gJGRodCRpbmRpdmlkdWFscyREXG5cbmBgYCJ9 -->

```r
# total, densidade estimada, erro padrão da densidade estimada, coeficiente de variação da densidade estimada, intervalo de confiança inferior e superior do coeficiente de variação, gruas de liberdade
dasy_croc_tap_arap_quase_sem_repeticao_hn$`Sem termo`$dht$individuals$D

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCBhcmVhIGNvYmVydGEgcGVsbyBlc2ZvcsOnbyBhbW9zdHJhbCwgZXNmb3LDp28gYW1vc3RyYWwgZW0gbWV0cm9zLCBuw7ptZXJvIGRlIGRldGVjw6fDtWVzLCBuw7ptZXJvIGRlIHRyYW5zZWN0b3MgKGVhKSwgdGF4YSBkZSBlbmNvbnRybywgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSB0YXhhIGRlIGVuY29udHJvICBcbmRhc3lfY3JvY190YXBfYXJhcF9xdWFzZV9zZW1fcmVwZXRpY2FvX2hyJGBTZW0gdGVybW9gJGRodCRpbmRpdmlkdWFscyRzdW1tYXJ5WzE6OV1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, area coberta pelo esforço amostral, esforço amostral em metros, número de detecções, número de transectos (ea), taxa de encontro, coeficiente de variação da taxa de encontro  
dasy_croc_tap_arap_quase_sem_repeticao_hr$`Sem termo`$dht$individuals$summary[1:9]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyDDoXJlYSBkZSBlc3R1ZG8sIHRhbWFuaG8gZGEgw6FyZWEgZGUgZXN0dWRvLCB0cmlsaGFzIG91IGVzdGHDp8O1ZXMgYW1vc3RyYWlzLCBlc2ZvcsOnbyB0b3RhbCBlbSBjYWRhIHRyaWxoYSwgYWJ1bmTDom5jaWEgZXN0aW1hZGEgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIG7Dum1lcm8gZGUgZGV0ZWPDp8O1ZXMgZW0gY2FkYSBlc3Rhw6fDo28gYW1vc3RyYWwsIMOhcmVhIHRvdGFsIGFtb3N0cmFkYVxuZGFzeV9jcm9jX3RhcF9hcmFwX3F1YXNlX3NlbV9yZXBldGljYW9faHIkYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJE5oYXQuYnkuc2FtcGxlWzE6OF1cbmBgYCJ9 -->

```r
# área de estudo, tamanho da área de estudo, trilhas ou estações amostrais, esforço total em cada trilha, abundância estimada em cada estação amostral, número de detecções em cada estação amostral, área total amostrada
dasy_croc_tap_arap_quase_sem_repeticao_hr$`Sem termo`$dht$individuals$Nhat.by.sample[1:8]
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyB0b3RhbCwgZGVuc2lkYWRlIGVzdGltYWRhLCBlcnJvIHBhZHLDo28gZGEgZGVuc2lkYWRlIGRlc3RpbWFkYSwgY29lZmljaWVudGUgZGUgdmFyaWHDp8OjbyBkYSBkZW5zaWRhZGUgZGVzdGltYWRhLCBpbnRlcnZhbG8gZGUgY29uZmlhbsOnYSBpbmZlcmlvciBlIHN1cGVyaW9yIGRvIGNvZWZpY2llbnRlIGRlIHZhcmlhw6fDo28sIGdydWFzIGRlIGxpYmVyZGFkZVxuZGFzeV9jcm9jX3RhcF9hcmFwX3F1YXNlX3NlbV9yZXBldGljYW9faHIkYFNlbSB0ZXJtb2AkZGh0JGluZGl2aWR1YWxzJERcblxuYGBgIn0= -->

```r
# total, densidade estimada, erro padrão da densidade destimada, coeficiente de variação da densidade destimada, intervalo de confiança inferior e superior do coeficiente de variação, gruas de liberdade
dasy_croc_tap_arap_quase_sem_repeticao_hr$`Sem termo`$dht$individuals$D

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->

