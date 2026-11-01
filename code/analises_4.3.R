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
    compl_vaat = coalesce(compl_vaat, 0),
    compl_vaar = coalesce(compl_vaar, 0),
    compl_eti = coalesce(compl_eti, 0),
    transf_fundeb = coalesce(transf_fundeb, 0),
    mat_censo = coalesce(mat_censo, 0)
  ) %>%
  filter(mat_censo > 0)


# ============================================================
# 3. Ordenação dos municípios
# ============================================================
# Os municípios são ordenados pela disponibilidade financeira
# do Fundeb por matrícula.
#
# Essa mesma ordenação é utilizada para todas as curvas.

base_2025 <- base_2025 %>%
  mutate(
    disponibilidade_fundeb = rectot_fundeb / mat_censo
  ) %>%
  arrange(disponibilidade_fundeb) %>%
  mutate(
    proporcao_matriculas =
      cumsum(mat_censo) / sum(mat_censo)
  )


# ============================================================
# 4. Curva — Complementação VAAT
# ============================================================

curva_vaat <- base_2025 %>%
  mutate(
    proporcao_receita =
      cumsum(compl_vaat) / sum(compl_vaat),
    curva = "Complementação-VAAT"
  ) %>%
  select(
    proporcao_matriculas,
    proporcao_receita,
    curva
  )


# ============================================================
# 5. Curva — Complementação VAAR
# ============================================================

curva_vaar <- base_2025 %>%
  mutate(
    proporcao_receita =
      cumsum(compl_vaar) / sum(compl_vaar),
    curva = "Complementação-VAAR"
  ) %>%
  select(
    proporcao_matriculas,
    proporcao_receita,
    curva
  )


# ============================================================
# 6. Curva — Complementação ETI
# ============================================================

curva_eti <- base_2025 %>%
  mutate(
    proporcao_receita =
      cumsum(compl_eti) / sum(compl_eti),
    curva = "Complementação-ETI"
  ) %>%
  select(
    proporcao_matriculas,
    proporcao_receita,
    curva
  )


# ============================================================
# 7. Curva — Fundo Estadual
# ============================================================

curva_fundo_estadual <- base_2025 %>%
  mutate(
    proporcao_receita =
      cumsum(transf_fundeb) / sum(transf_fundeb),
    curva = "Fundo Estadual"
  ) %>%
  select(
    proporcao_matriculas,
    proporcao_receita,
    curva
  )


# ============================================================
# 8. Linha de igualdade perfeita
# ============================================================

curva_igualdade <- base_2025 %>%
  transmute(
    proporcao_matriculas,
    proporcao_receita = proporcao_matriculas,
    curva = "Igualdade"
  )


# ============================================================
# 9. Base final para o Excel
# ============================================================

base_concentracao_2025 <- bind_rows(
  curva_igualdade,
  curva_vaat,
  curva_vaar,
  curva_eti,
  curva_fundo_estadual
)


# ============================================================
# 10. Exportar
# ============================================================

write.xlsx(
  base_concentracao_2025,
  "data/curvas_concentracao_fundeb_2025.xlsx",
  rowNames = FALSE
)




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
  filter(mat_censo > 0)


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

decompor_gini <- function(base, receita_total) {
  
  
  # ----------------------------------------------------------
  # Receita total por matrícula
  # ----------------------------------------------------------
  
  y <- base[[receita_total]] / base$mat_censo
  
  w <- base$mat_censo
  
  
  # ----------------------------------------------------------
  # Gini da disponibilidade financeira
  # ----------------------------------------------------------
  
  G <- gini_ponderado(y, w)
  
  
  # ----------------------------------------------------------
  # Componentes
  # ----------------------------------------------------------
  
  componentes <- c(
    "Fundo Estadual" = "transf_fundeb",
    "Complementação-VAAT" = "compl_vaat",
    "Complementação-VAAR" = "compl_vaar",
    "Complementação-ETI" = "compl_eti"
  )
  
  
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
    # Participação na receita total, não na média simples
    # dos municípios.
    #
    
    s <- sum(
      base[[variavel]]
    ) / sum(
      base[[receita_total]]
    )
    
    
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

tabela_fundeb <- decompor_gini(
  base = base_2025,
  receita_total = "rectot_fundeb"
)


# ============================================================
# 6. Decomposição — Receita Total da Educação
# ============================================================

tabela_educ <- decompor_gini(
  base = base_2025,
  receita_total = "rectot_educ"
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

write.xlsx(
  list(
    "Receita Educação" = tabela_educ,
    "Receita Fundeb" = tabela_fundeb
  ),
  "data/gini_desagregado_2025.xlsx",
  rowNames = FALSE
)



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
