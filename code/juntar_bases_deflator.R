library(data.table)
library(dplyr)
library(readODS)
library(readxl)
library(openxlsx)


# ============================================================
# COLUNAS NUMÉRICAS
# ============================================================

cols_numericas <- c(
  "rectot_fundeb",
  "transf_fundeb",
  "compl_uni",
  "compl_vaaf",
  "compl_vaat",
  "compl_vaar",
  "compl_eti",
  "royalty_educ",
  "contrib_fundeb",
  "impadd_fundeb",
  "impadd_mde",
  "sal_educ",
  "prog_fnde",
  "outrec_educ",
  "recadd_educ",
  "rectot_educ",
  "mat_censo",
  "defla_ipca"
)


# ============================================================
# COLUNAS MONETÁRIAS
# ============================================================

cols_monetarias <- c(
  "rectot_fundeb",
  "transf_fundeb",
  "compl_uni",
  "compl_vaaf",
  "compl_vaat",
  "compl_vaar",
  "compl_eti",
  "royalty_educ",
  "contrib_fundeb",
  "impadd_fundeb",
  "impadd_mde",
  "sal_educ",
  "prog_fnde",
  "outrec_educ",
  "recadd_educ",
  "rectot_educ"
)


# ============================================================
# IMPORTA BASE 2025
# ============================================================

base_2025 <- fread(
  "data/base_2025.csv",
  sep = ";",
  colClasses = "character"
)


# ============================================================
# CORRIGE TIPOS DA BASE 2025
# ============================================================

# Ano
base_2025[, ano := as.numeric(ano)]

# Valores monetários
base_2025[, (cols_monetarias) := lapply(.SD, function(x) {
  
  x <- trimws(x)
  
  # Remove separador de milhar
  x <- gsub("\\.", "", x)
  
  # Converte vírgula decimal para ponto
  x <- gsub(",", ".", x)
  
  as.numeric(x)
  
}), .SDcols = cols_monetarias]

# Matrículas
base_2025[, mat_censo := as.numeric(mat_censo)]


# ============================================================
# IMPORTA BASE IPEA
# ============================================================

ipea <- read_ods("data_raw/ipea_ate_2024.ods") %>%
  filter(
    ente == "Municipal",
    cod_uf == 31,
    ano >= 2018
  )


# ============================================================
# IMPORTA IPCA
# ============================================================

ipca <- read_excel("data_raw/ipca.xlsx") %>%
  select(ano, defla_ipca) %>%
  mutate(ano = as.numeric(ano))


# ============================================================
# JUNTA IPEA + BASE 2025
# ============================================================

base_final <- rbindlist(
  list(
    as.data.table(ipea),
    base_2025
  ),
  use.names = TRUE,
  fill = TRUE
)


# ============================================================
# SUBSTITUI O DEFLATOR PELO IPCA
# ============================================================

base_final <- base_final %>%
  select(-defla_ipca) %>%
  left_join(
    ipca,
    by = "ano"
  )


# ============================================================
# GARANTE TIPOS NUMÉRICOS
# ============================================================

base_final[, (cols_numericas) := lapply(.SD, as.numeric),
           .SDcols = cols_numericas]


# ============================================================
# CALCULA OS VALORES ANUAIS POR MATRÍCULA
# ============================================================

base_final <- base_final %>%
  mutate(
    vam_fundeb = (rectot_fundeb * defla_ipca) / mat_censo,
    vam_total  = (rectot_educ * defla_ipca) / mat_censo
  )


# ============================================================
# EXPORTA BASE FINAL
# ============================================================

write.xlsx(
  base_final,
  "data/base_final.xlsx",
  overwrite = TRUE
)