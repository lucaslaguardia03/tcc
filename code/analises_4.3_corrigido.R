library(dplyr)
library(readxl)
library(openxlsx)
library(ineq)



# ============================================================
# 1. Base
# ============================================================

base_final <- read_excel("data/base_final.xlsx")


# ============================================================
# 2. Preparação dos dados
# ============================================================

base_gini <- base_final %>%
  filter(ano >= 2018, ano <= 2025) %>%
  mutate(
    rectot_fundeb = coalesce(rectot_fundeb, 0),
    rectot_educ = coalesce(rectot_educ, 0),
    compl_uni = coalesce(compl_uni, 0),
    mat_censo = coalesce(mat_censo, 0)
  ) %>%
  filter(mat_censo > 0)


# ============================================================
# 3. Gini por ano — Receita por matrícula
# ============================================================

base_gini_anual <- base_gini %>%
  group_by(ano) %>%
  summarise(
    
    # Fundeb — atual
    gini_fundeb = ineq::Gini(
      rectot_fundeb / mat_censo
    ),
    
    # Fundeb — contrafactual sem complementação da União
    gini_fundeb_sem_compl = ineq::Gini(
      (rectot_fundeb - compl_uni) / mat_censo
    ),
    
    # Educação — atual
    gini_educ = ineq::Gini(
      rectot_educ / mat_censo
    ),
    
    # Educação — contrafactual sem complementação da União
    gini_educ_sem_compl = ineq::Gini(
      (rectot_educ - compl_uni) / mat_censo
    ),
    
    .groups = "drop"
  ) %>%
  arrange(ano)


# ============================================================
# 4. Exportar para Excel
# ============================================================

write.xlsx(
  base_gini_anual,
  "data/gini_temporal_2018_2025.xlsx",
  rowNames = FALSE
)











#--------------------------------------------------------------------------




#Gini desagregado - Receitas Fundeb

# ============================================================
# 1. Base
# ============================================================

base_final <- read_excel("data/base_final.xlsx")


# ============================================================
# 2. Base de 2025
# ============================================================

base_2025 <- base_final %>%
  filter(ano == 2025) %>%
  mutate(
    rectot_fundeb = coalesce(rectot_fundeb, 0),
    rectot_educ = coalesce(rectot_educ, 0),
    compl_vaat = coalesce(compl_vaat, 0),
    compl_vaar = coalesce(compl_vaar, 0),
    compl_eti = coalesce(compl_eti, 0),
    transf_fundeb = coalesce(transf_fundeb, 0),
    mat_censo = coalesce(mat_censo, 0)
  ) %>%
  filter(mat_censo > 0)


# ============================================================
# 3. Receita por matrícula e variáveis de ordenação
# ============================================================
# Todas as curvas tratam o município como unidade de análise, com
# o mesmo peso para todos (eixo x = proporção acumulada de municípios;
# eixo y = proporção acumulada da receita por matrícula do componente).
#
# Gráfico 12: municípios ordenados pela receita do Fundeb por matrícula.
# Gráfico 13: municípios ordenados pela receita total vinculada à
#             educação por matrícula ANTES das complementações da União
#             (evita que o recebimento da complementação altere a
#             posição do município no ordenamento).

base_2025 <- base_2025 %>%
  mutate(
    est_mat  = transf_fundeb / mat_censo,
    vaat_mat = compl_vaat / mat_censo,
    vaar_mat = compl_vaar / mat_censo,
    eti_mat  = compl_eti / mat_censo,
    disp_fundeb = rectot_fundeb / mat_censo,
    disp_educ_antes_compl =
      (rectot_educ - compl_vaat - compl_vaar - compl_eti) / mat_censo
  )


# ============================================================
# 4. Função: curvas de concentração por componente
# ============================================================

curvas_concentracao <- function(base, var_ordem) {
  
  base <- base %>% arrange(.data[[var_ordem]])
  
  n <- nrow(base)
  prop_mun <- c(0, seq_len(n) / n)
  
  acumular <- function(x) c(0, cumsum(x) / sum(x))
  
  data.frame(
    `% acumulado de municípios` = prop_mun,
    `Linha de perfeita igualdade` = prop_mun,
    `Fundo Estadual` = acumular(base$est_mat),
    `Complementação-VAAT` = acumular(base$vaat_mat),
    `Complementação-VAAR` = acumular(base$vaar_mat),
    `Complementação-ETI` = acumular(base$eti_mat),
    check.names = FALSE
  )
}


# ============================================================
# 5. Gráficos 12 e 13
# ============================================================

grafico_12 <- curvas_concentracao(base_2025, "disp_fundeb")
grafico_13 <- curvas_concentracao(base_2025, "disp_educ_antes_compl")


# As curvas (grafico_12 e grafico_13) são exportadas junto com as
# tabelas de decomposição, no arquivo data/gini_decomposto.xlsx
# (ver seção "Exportar para Excel" mais abaixo).




#--------------------------------------------------------------------------
# ============================================================
# Gini desagregado por componentes — 2025
# Indicador: receita por matrícula
# ============================================================

library(dplyr)
library(readxl)
library(openxlsx)


# ============================================================
# 1. Base
# ============================================================

base_final <- read_excel("data/base_final.xlsx")


# ============================================================
# 2. Base de 2025
# ============================================================

base_2025 <- base_final %>%
  filter(ano == 2025) %>%
  mutate(
    rectot_educ = coalesce(rectot_educ, 0),
    rectot_fundeb = coalesce(rectot_fundeb, 0),
    transf_fundeb = coalesce(transf_fundeb, 0),
    compl_vaat = coalesce(compl_vaat, 0),
    compl_vaar = coalesce(compl_vaar, 0),
    compl_eti = coalesce(compl_eti, 0),
    mat_censo = coalesce(mat_censo, 0)
  ) %>%
  filter(mat_censo > 0) %>%
  mutate(
    # Receitas educacionais que não vêm do Fundeb
    rec_nao_fundeb = rectot_educ - rectot_fundeb
  )


# ============================================================
# 3. Função para Gini ponderado
# ============================================================
#
# Calcula o Gini utilizando mat_censo como peso.
#
# Isso é necessário porque o indicador é receita por matrícula
# e a curva de concentração utiliza a proporção acumulada
# das matrículas.
#

gini_ponderado <- function(x, w) {
  
  # Ordenar pelo valor de x
  ordem <- order(x)
  
  x <- x[ordem]
  w <- w[ordem]
  
  # Média ponderada
  media <- sum(w * x) / sum(w)
  
  # Diferenças absolutas ponderadas
  numerador <- sum(
    outer(x, x, FUN = function(a, b) abs(a - b)) *
      outer(w, w)
  )
  
  G <- numerador / (2 * sum(w)^2 * media)
  
  return(G)
}


# ============================================================
# 4. Função de decomposição
# ============================================================

decompor_gini <- function(base, receita_total, componentes) {
  
  
  # ----------------------------------------------------------
  # Receita total por matrícula
  # ----------------------------------------------------------
  
  y <- base[[receita_total]] / base$mat_censo
  
  # Cada município tem o mesmo peso (unidade de análise = município).
  # O número de matrículas é usado apenas para calcular a receita por
  # matrícula, e não como peso.
  w <- rep(1, nrow(base))
  
  
  # ----------------------------------------------------------
  # Gini da disponibilidade financeira
  # ----------------------------------------------------------
  
  G <- gini_ponderado(y, w)
  
  
  # ----------------------------------------------------------
  # Componentes
  # ----------------------------------------------------------
  
  # (os componentes são passados como argumento da função)
  
  
  # ----------------------------------------------------------
  # Ordenação pela disponibilidade financeira total
  # ----------------------------------------------------------
  
  ordem <- order(y)
  
  y_ord <- y[ordem]
  w_ord <- w[ordem]
  
  
  # ----------------------------------------------------------
  # Posto fracionário ponderado
  # ----------------------------------------------------------
  #
  # R_i representa a posição do município na distribuição
  # acumulada das matrículas.
  #
  # Utilizamos o ponto médio do intervalo de cada município.
  #
  
  peso_acumulado_anterior <- c(
    0,
    head(cumsum(w_ord), -1)
  )
  
  R <- (
    peso_acumulado_anterior +
      w_ord / 2
  ) / sum(w_ord)
  
  
  # ----------------------------------------------------------
  # Cálculo para cada componente
  # ----------------------------------------------------------
  
  resultado <- lapply(seq_along(componentes), function(i) {
    
    nome <- names(componentes)[i]
    variavel <- componentes[i]
    
    
    # --------------------------------------------------------
    # Receita do componente por matrícula
    # --------------------------------------------------------
    
    x <- base[[variavel]] / base$mat_censo
    
    x_ord <- x[ordem]
    
    
    # --------------------------------------------------------
    # Participação do componente
    # --------------------------------------------------------
    #
    # Razão entre a média municipal da receita por matrícula do
    # componente e a média municipal da receita por matrícula do
    # agregado. Com isso, a soma das contribuições é igual ao Gini.
    #
    
    s <- mean(x) / mean(y)
    
    
    # --------------------------------------------------------
    # Gini ponderado do componente
    # --------------------------------------------------------
    
    g <- gini_ponderado(x, w)
    
    
    # --------------------------------------------------------
    # Grau de concentração
    # --------------------------------------------------------
    #
    # C_k = 2 * Cov_w(x_k, R) / média_w(x_k)
    #
    # Como R é o posto fracionário ponderado, calculamos
    # diretamente a covariância ponderada.
    #
    
    media_x <- sum(w_ord * x_ord) / sum(w_ord)
    
    media_R <- sum(w_ord * R) / sum(w_ord)
    
    cov_x_R <- sum(
      w_ord *
        (x_ord - media_x) *
        (R - media_R)
    ) / sum(w_ord)
    
    c <- 2 * cov_x_R / media_x
    
    
    # --------------------------------------------------------
    # Correlação de Gini
    # --------------------------------------------------------
    
    r <- c / g
    
    
    # --------------------------------------------------------
    # Contribuição absoluta
    # --------------------------------------------------------
    
    contribuicao <- s * g * r
    
    
    # --------------------------------------------------------
    # % Contribuição
    # --------------------------------------------------------
    
    contribuicao_relativa <- contribuicao / G
    
    
    # --------------------------------------------------------
    # Elasticidade
    # --------------------------------------------------------
    
    elasticidade <- contribuicao_relativa - s
    
    
    # --------------------------------------------------------
    # Resultado
    # --------------------------------------------------------
    
    data.frame(
      Componente = nome,
      `Participação (s)` = s,
      `Gini (g)` = g,
      `Correlação de Gini (r)` = r,
      `Grau de Concentração (c)` = c,
      `Contribuição (s×g×r) %` = contribuicao * 100,
      `% Contribuição (s×g×r/G)` =
        contribuicao_relativa * 100,
      `Elasticidade (s×g×r/G-s)` = elasticidade,
      check.names = FALSE
    )
  })
  
  
  # ----------------------------------------------------------
  # Retornar tabela
  # ----------------------------------------------------------
  
  bind_rows(resultado)
}


# ============================================================
# 5. Decomposição — Receita Total do Fundeb
# ============================================================

componentes_fundeb <- c(
  "Fundo Estadual" = "transf_fundeb",
  "Complementação-VAAT" = "compl_vaat",
  "Complementação-VAAR" = "compl_vaar",
  "Complementação-ETI" = "compl_eti"
)

tabela_fundeb <- decompor_gini(
  base = base_2025,
  receita_total = "rectot_fundeb",
  componentes = componentes_fundeb
)


# ============================================================
# 6. Decomposição — Receita Total da Educação
# ============================================================

componentes_educ <- c(componentes_fundeb, "Receitas não-Fundeb" = "rec_nao_fundeb")

tabela_educ <- decompor_gini(
  base = base_2025,
  receita_total = "rectot_educ",
  componentes = componentes_educ
)


# ============================================================
# 7. Verificações
# ============================================================

cat(
  "Soma da participação dos componentes — Fundeb:",
  sum(tabela_fundeb$`Participação (s)`),
  "\n"
)

cat(
  "Soma da contribuição — Fundeb:",
  sum(tabela_fundeb$`% Contribuição (s×g×r/G)`),
  "%\n"
)

cat(
  "Soma da participação dos componentes — Educação:",
  sum(tabela_educ$`Participação (s)`),
  "\n"
)

cat(
  "Soma da contribuição — Educação:",
  sum(tabela_educ$`% Contribuição (s×g×r/G)`),
  "%\n"
)


# ============================================================
# 8. Exportar para Excel
# ============================================================

# Linha de total (G = soma das contribuições)
adicionar_total <- function(tabela) {
  G <- sum(tabela$`Contribuição (s×g×r) %`) / 100
  bind_rows(
    tabela,
    data.frame(
      Componente = "Total",
      `Participação (s)` = sum(tabela$`Participação (s)`),
      `Gini (g)` = G,
      `Correlação de Gini (r)` = NA_real_,
      `Grau de Concentração (c)` = NA_real_,
      `Contribuição (s×g×r) %` = sum(tabela$`Contribuição (s×g×r) %`),
      `% Contribuição (s×g×r/G)` = sum(tabela$`% Contribuição (s×g×r/G)`),
      `Elasticidade (s×g×r/G-s)` = NA_real_,
      check.names = FALSE
    )
  )
}

# Ajusta nomes e escala para o Excel (% Contribuição como fração,
# formatada como porcentagem)
preparar_tabela <- function(tabela) {
  t <- adicionar_total(tabela)
  t[["% Contribuição (s×g×r/G)"]] <- t[["% Contribuição (s×g×r/G)"]] / 100
  names(t)[names(t) == "Grau de Concentração (c)"] <- "Grau de concentração (c)"
  names(t)[names(t) == "Contribuição (s×g×r) %"]  <- "Contribuição (s×g×r)"
  t
}

# Arquivo único: Tabela 7, Tabela 8, Gráfico 12 e Gráfico 13
abas <- list(
  "Tabela 7"   = preparar_tabela(tabela_fundeb),
  "Tabela 8"   = preparar_tabela(tabela_educ),
  "Gráfico 12" = grafico_12,
  "Gráfico 13" = grafico_13
)

wb <- createWorkbook()
estilo_pct <- createStyle(numFmt = "0.00%")
estilo_n4  <- createStyle(numFmt = "0.0000")

for (nome in names(abas)) {
  addWorksheet(wb, nome)
  writeData(wb, nome, abas[[nome]])
  n_lin <- nrow(abas[[nome]]) + 1
  
  if (grepl("^Tabela", nome)) {
    setColWidths(wb, nome, cols = 1:8, widths = c(24, 16, 16, 20, 22, 20, 24, 24))
    addStyle(wb, nome, estilo_pct, rows = 2:n_lin, cols = c(2, 7), gridExpand = TRUE)
    addStyle(wb, nome, estilo_n4,  rows = 2:n_lin, cols = c(3, 4, 5, 6, 8), gridExpand = TRUE)
  } else {
    setColWidths(wb, nome, cols = 1:6, widths = 26)
    addStyle(wb, nome, estilo_pct, rows = 2:n_lin, cols = 1:6, gridExpand = TRUE)
  }
}

saveWorkbook(wb, "data/gini_decomposto.xlsx", overwrite = TRUE)



#--------------------------------------------------------------------------------------------


# ============================================================
# Distribuição das complementações por decis da receita
# educacional não-Fundeb por matrícula — 2025
# ============================================================

library(dplyr)
library(readxl)
library(openxlsx)


# ============================================================
# 1. Base
# ============================================================

base_final <- read_excel("data/base_final.xlsx")


# ============================================================
# 2. Base de 2025
# ============================================================

base_2025 <- base_final %>%
  filter(ano == 2025) %>%
  mutate(
    recadd_educ = coalesce(recadd_educ, 0),
    mat_censo = coalesce(mat_censo, 0),
    compl_vaat = coalesce(compl_vaat, 0),
    compl_vaar = coalesce(compl_vaar, 0),
    compl_eti = coalesce(compl_eti, 0)
  ) %>%
  filter(mat_censo > 0)


# ============================================================
# 3. Disponibilidade de recursos educacionais não-Fundeb
# ============================================================

base_2025 <- base_2025 %>%
  mutate(
    recadd_edu_mat = recadd_educ / mat_censo
  )


# ============================================================
# 4. Ordenação e definição dos decis
# ============================================================
#
# Os municípios são ordenados pela receita educacional
# não-Fundeb por matrícula.
#
# 1º decil = 10% dos municípios com menor disponibilidade
# 10º decil = 10% dos municípios com maior disponibilidade
#

base_2025 <- base_2025 %>%
  arrange(recadd_edu_mat) %>%
  mutate(
    decil = ntile(recadd_edu_mat, 10)
  )


# ============================================================
# 5. Valor das complementações por matrícula
# ============================================================

base_decis <- base_2025 %>%
  group_by(decil) %>%
  summarise(
    
    # Disponibilidade de recursos educacionais não-Fundeb
    # por matrícula
    recadd_edu_mat = sum(recadd_educ) / sum(mat_censo),
    
    # Complementações da União por matrícula
    compl_vaat_mat = sum(compl_vaat) / sum(mat_censo),
    compl_vaar_mat = sum(compl_vaar) / sum(mat_censo),
    compl_eti_mat = sum(compl_eti) / sum(mat_censo),
    
    .groups = "drop"
  ) %>%
  mutate(
    decil = paste0(decil, "º decil")
  )


# ============================================================
# 6. Exportar para Excel
# ============================================================

write.xlsx(
  base_decis,
  "data/complementacoes_decis_recadd_edu_2025.xlsx",
  rowNames = FALSE
)


#--------------------------------------------------------------------------------------------------------------------

# ============================================================
# Distribuição das receitas do Fundeb por decis da receita
# educacional não-Fundeb por matrícula — 2025
# ============================================================

library(dplyr)
library(readxl)
library(openxlsx)


# ============================================================
# 1. Base
# ============================================================

base_final <- read_excel("data/base_final.xlsx")


# ============================================================
# 2. Base de 2025
# ============================================================

base_2025 <- base_final %>%
  filter(ano == 2025) %>%
  mutate(
    recadd_educ = coalesce(recadd_educ, 0),
    mat_censo = coalesce(mat_censo, 0),
    transf_fundeb = coalesce(transf_fundeb, 0),
    compl_uni = coalesce(compl_uni, 0),
    rectot_fundeb = coalesce(rectot_fundeb, 0)
  ) %>%
  filter(mat_censo > 0)


# ============================================================
# 3. Receita educacional não-Fundeb por matrícula
# ============================================================

base_2025 <- base_2025 %>%
  mutate(
    recadd_educ_mat = recadd_educ / mat_censo
  )


# ============================================================
# 4. Ordenação e definição dos decis
# ============================================================
#
# 1º decil = municípios com menor disponibilidade de recursos
# educacionais não-Fundeb por matrícula.
#
# 10º decil = municípios com maior disponibilidade.
#

base_2025 <- base_2025 %>%
  arrange(recadd_educ_mat) %>%
  mutate(
    decil = ntile(recadd_educ_mat, 10)
  )


# ============================================================
# 5. Receitas do Fundeb por matrícula em cada decil
# ============================================================

base_decis <- base_2025 %>%
  group_by(decil) %>%
  summarise(
    
    # Receita educacional não-Fundeb por matrícula
    recadd_educ_mat =
      sum(recadd_educ) / sum(mat_censo),
    
    # Fundo Estadual por matrícula
    transf_fundeb_mat =
      sum(transf_fundeb) / sum(mat_censo),
    
    # Complementação da União por matrícula
    compl_uni_mat =
      sum(compl_uni) / sum(mat_censo),
    
    # Receita total do Fundeb por matrícula
    rectot_fundeb_mat =
      sum(rectot_fundeb) / sum(mat_censo),
    
    .groups = "drop"
  ) %>%
  mutate(
    decil = paste0(decil, "º decil")
  )


# ============================================================
# 6. Exportar para Excel
# ============================================================

write.xlsx(
  base_decis,
  "data/receitas_fundeb_decis_recadd_educ_2025.xlsx",
  rowNames = FALSE
)




#-----------------------------------------------------------------------

# ============================================================
# Boxplot temporal — Complementações da União por matrícula
# Valores deflacionados — 2018–2025
# ============================================================

library(dplyr)
library(readxl)
library(openxlsx)


# ============================================================
# 1. Base
# ============================================================

base_final <- read_excel("data/base_final.xlsx")


# ============================================================
# 2. Preparação da base
# ============================================================

base_boxplot <- base_final %>%
  filter(
    ano >= 2018,
    ano <= 2025
  ) %>%
  mutate(
    compl_vaat = coalesce(compl_vaat, 0),
    compl_vaar = coalesce(compl_vaar, 0),
    compl_eti = coalesce(compl_eti, 0),
    mat_censo = coalesce(mat_censo, 0),
    defla_ipca = coalesce(defla_ipca, 1)
  ) %>%
  filter(mat_censo > 0)


# ============================================================
# 3. Complementações por matrícula — valores reais
# ============================================================
#
# Os valores nominais são atualizados pelo deflator do IPCA
# e, posteriormente, divididos pelas matrículas.
#

base_boxplot <- base_boxplot %>%
  transmute(
    Ano = ano,
    
    VAAT = (compl_vaat * defla_ipca) / mat_censo,
    
    VAAR = (compl_vaar * defla_ipca) / mat_censo,
    
    ETI = (compl_eti * defla_ipca) / mat_censo
  )


# ============================================================
# 4. Ordenação
# ============================================================

base_boxplot <- base_boxplot %>%
  arrange(Ano)


# ============================================================
# 5. Exportar para Excel
# ============================================================

write.xlsx(
  base_boxplot,
  "data/boxplot_complementacoes_por_matricula_2018_2025.xlsx",
  rowNames = FALSE
)



#-------------------------------------------------------------------
