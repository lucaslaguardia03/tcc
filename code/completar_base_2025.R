#Script que completa a base do IPEA, a partir da mesma metodologia, com o ano de 2025

library(vroom)
library(dplyr)
library(data.table)

siope_2025 <- vroom(
  "data_raw/siope_receitas.txt",
  delim = ";"
) %>%
  filter(NUM_ANO == 2025 & SIG_UF == "MG" & TIPO == "Municipal") %>% rename (cod_ibge = COD_MUNI)

base <- fread("data_raw/municipios_cod.csv", sep = ";")

royalties <- readxl::read_excel("data_raw/royalties_2025.xlsx", sheet = "MG")

stn_transferencias <- fread("data_raw/stn_transferencias.csv", sep = ";", encoding = "Latin-1") %>% mutate(cod_ibge = substr(cod_ibge, 1, nchar(cod_ibge) - 1))

# Receita Total Fundeb
rectot_fundeb <- stn_transferencias %>%
  group_by(cod_ibge) %>%
  summarise(
    rectot_fundeb = sum(VL_CONSOLIDADO, na.rm = TRUE),
    .groups = "drop"
  )

# Transferência Fundeb Participação Fundo
transf_fundeb <- stn_transferencias %>%
  filter(
    !transferencia %in% c(
      "FUNDEB - COUN VAAT",
      "AJUSTE FUNDEB - AJUSTE FUNDEB VAAT",
      "FUNDEB - COUN VAAR",
      "AJUSTE FUNDEB - AJUSTE FUNDEB VAAR",
      "FUNDEB - COUN VAAF",
      "AJUSTE FUNDEB - AJUSTE FUNDEB VAAF"
    )
  ) %>%
  group_by(cod_ibge) %>%
  summarise(
    transf_fundeb = sum(VL_CONSOLIDADO, na.rm = TRUE),
    .groups = "drop"
  )

# Complementações da União
compl_uni<- stn_transferencias %>%
  filter(
    transferencia %in% c(
      "FUNDEB - COUN VAAT",
      "AJUSTE FUNDEB - AJUSTE FUNDEB VAAT",
      "FUNDEB - COUN VAAR",
      "AJUSTE FUNDEB - AJUSTE FUNDEB VAAR",
      "FUNDEB - COUN VAAF",
      "AJUSTE FUNDEB - AJUSTE FUNDEB VAAF"
    )
  ) %>%
  group_by(cod_ibge) %>%
  summarise(
    compl_uni = sum(VL_CONSOLIDADO, na.rm = TRUE),
    .groups = "drop"
  )

# Complementações VAAT
compl_vaat <- stn_transferencias %>%
  filter(
    transferencia %in% c(
      "FUNDEB - COUN VAAT",
      "AJUSTE FUNDEB - AJUSTE FUNDEB VAAT")
  ) %>%
  group_by(cod_ibge) %>%
  summarise(
    compl_vaat = sum(VL_CONSOLIDADO, na.rm = TRUE),
    .groups = "drop"
  )

# Complementações VAAF
compl_vaaf <- stn_transferencias %>%
  filter(
    transferencia %in% c(
      "FUNDEB - COUN VAAF",
      "AJUSTE FUNDEB - AJUSTE FUNDEB VAAF")
  ) %>%
  group_by(cod_ibge) %>%
  summarise(
    compl_vaaf = sum(VL_CONSOLIDADO, na.rm = TRUE),
    .groups = "drop"
  )

# Complementações VAAR
compl_vaar <- stn_transferencias %>%
  filter(
    transferencia %in% c(
      "FUNDEB - COUN VAAR",
      "AJUSTE FUNDEB - AJUSTE FUNDEB VAAR")
  ) %>%
  group_by(cod_ibge) %>%
  summarise(
    compl_vaar = sum(VL_CONSOLIDADO, na.rm = TRUE),
    .groups = "drop"
  )

# Royalties Educação (75% do total)
royalty_educ <- royalties %>% filter(!is.na(cod_ibge)) %>% select(royalty_educ, cod_ibge)

# Contribuições ao Fundeb

contrib_fundeb <- siope_2025 %>%  filter (NUM_PERI == 6, COD_EXIB_FORMATADO == "10000000", IDN_CLAS == "DF") %>% group_by(cod_ibge) %>%
  summarise(
    contrib_fundeb = sum(VAL_DECL, na.rm = TRUE),
    .groups = "drop"
  )

# Receita vinculada à MDE: 5% Cesta Fundeb (Ou 25% das Deduções do Fundeb, que representam 20% da Cesta - 25% de 20% = 5%)

impadd_fundeb <- siope_2025 %>%  filter (NUM_PERI == 6, COD_EXIB_FORMATADO == "10000000", IDN_CLAS == "DF") %>% group_by(cod_ibge) %>%
  summarise(
    impadd_fundeb = sum(VAL_DECL, na.rm = TRUE) * 0.25,
    .groups = "drop"
  )

# Receita vinculada à MDE: 25% Outros impostos e transferências:

impadd_mde <- siope_2025 %>%  filter (NUM_PERI == 6, COD_EXIB_FORMATADO %in% c( 
                                        "11125000",
                                        "11145100",
                                        "11125300",
                                        "11130300",
                                        "17115500"), IDN_CLAS == "RR") %>% group_by(cod_ibge) %>%
  summarise(
    impadd_mde = sum(VAL_DECL, na.rm = TRUE) * 0.25,
    .groups = "drop"
  )

# Salário Educação

sal_educ <- siope_2025 %>%  filter (NUM_PERI == 6, COD_EXIB_FORMATADO == 17145000, IDN_CLAS == "RR") %>% group_by(cod_ibge) %>%
  summarise(
    sal_educ = sum(VAL_DECL, na.rm = TRUE),
    .groups = "drop"
  )

# Transferências Programas FNDE

prog_fnde <- siope_2025 %>%
  filter(
    NUM_PERI == 6,
    COD_EXIB_FORMATADO %in% c(17140000, 17145000, 24120000),
    IDN_CLAS == "RR"
  ) %>%
  group_by(cod_ibge) %>%
  summarise(
    prog_fnde =
      sum(VAL_DECL[COD_EXIB_FORMATADO %in% c(17140000, 24120000)], na.rm = TRUE) -
      sum(VAL_DECL[COD_EXIB_FORMATADO == 17145000], na.rm = TRUE),
    .groups = "drop"
  )

# Outras receitas educação

outrec_educ<- siope_2025 %>%  filter (NUM_PERI == 6, COD_EXIB_FORMATADO %in% c(17295200), IDN_CLAS == "RR") %>% group_by(cod_ibge) %>%
  summarise(
    outrec_educ = sum(VAL_DECL[COD_EXIB_FORMATADO %in% c(17295200)]),
    .groups = "drop"
  )


##JOIN BASE ÚNICA

rectot_fundeb <- mutate(rectot_fundeb, cod_ibge = as.character(cod_ibge))
transf_fundeb <- mutate(transf_fundeb, cod_ibge = as.character(cod_ibge))
compl_uni <- mutate(compl_uni, cod_ibge = as.character(cod_ibge))
compl_vaat <- mutate(compl_vaat, cod_ibge = as.character(cod_ibge))
compl_vaaf <- mutate(compl_vaaf, cod_ibge = as.character(cod_ibge))
compl_vaar <- mutate(compl_vaar, cod_ibge = as.character(cod_ibge))
royalty_educ <- mutate(royalty_educ, cod_ibge = as.character(cod_ibge))
contrib_fundeb <- mutate(contrib_fundeb, cod_ibge = as.character(cod_ibge))
impadd_fundeb <- mutate(impadd_fundeb, cod_ibge = as.character(cod_ibge))
impadd_mde <- mutate(impadd_mde, cod_ibge = as.character(cod_ibge))
sal_educ <- mutate(sal_educ, cod_ibge = as.character(cod_ibge))
prog_fnde <- mutate(prog_fnde, cod_ibge = as.character(cod_ibge))
outrec_educ <- mutate(outrec_educ, cod_ibge = as.character(cod_ibge))

base_2025 <- rectot_fundeb %>%
  left_join(transf_fundeb, by = "cod_ibge") %>%
  left_join(compl_uni, by = "cod_ibge") %>%
  left_join(compl_vaat, by = "cod_ibge") %>%
  left_join(compl_vaaf, by = "cod_ibge") %>%
  left_join(compl_vaar, by = "cod_ibge") %>%
  left_join(royalty_educ, by = "cod_ibge") %>%
  left_join(contrib_fundeb, by = "cod_ibge") %>%
  left_join(impadd_fundeb, by = "cod_ibge") %>%
  left_join(impadd_mde, by = "cod_ibge") %>%
  left_join(sal_educ, by = "cod_ibge") %>%
  left_join(prog_fnde, by = "cod_ibge") %>%
  left_join(outrec_educ, by = "cod_ibge")



#Colunas de descrição
base_2025 <- base_2025 %>%
  mutate(
    ano = 2025,
    ente = "Municipal",
    cod_uf = 31,
    sig_uf = "MG",
    defla_ipca = 1
  )

codigos <- fread("data_raw/municipios_cod.csv", sep = ";") %>% select(cod_ibge, nom_ente)

base_2025 <- base_2025 %>%
  mutate(
    recadd_educ = rowSums(across(c(impadd_fundeb, impadd_mde, sal_educ, prog_fnde, outrec_educ)), na.rm = TRUE),
    rectot_educ = rectot_fundeb + recadd_educ
  )

#Matrículas

matriculas <- readxl::read_excel("data_raw/matriculas_censo2025.xlsx")

matriculas <- matriculas %>%
  mutate(cod_ibge = substr(cod_ibge, 1, nchar(cod_ibge) - 1))

base_2025 <- base_2025 %>%
  mutate(cod_ibge = as.character(cod_ibge)) %>%
  left_join(matriculas, by = "cod_ibge")



#Ordena colunas igual base do IPEA

base_2025 <- base_2025 %>%
  select(
    ano, ente, cod_uf, sig_uf, cod_ibge, nom_ente,
    rectot_fundeb, transf_fundeb, compl_uni, compl_vaaf, compl_vaat, compl_vaar,
    royalty_educ, contrib_fundeb, impadd_fundeb, impadd_mde, sal_educ, prog_fnde,
    outrec_educ, recadd_educ, rectot_educ, defla_ipca, mat_censo
  )

base_2025 <- base_2025 %>%
  mutate(
    across(c(ente, cod_uf, sig_uf, cod_ibge, nom_ente), as.character),
    across(-c(ente, cod_uf, sig_uf, cod_ibge, nom_ente), as.numeric)
  )

base_2025 <- base_2025 %>%
  mutate(
    across(
      -c(ente, cod_uf, sig_uf, cod_ibge, nom_ente),
      ~ as.numeric(ifelse(is.na(.x), 0, .x))
    )
  )

base_2025 <- base_2025 %>%
  mutate(
    across(
      -c(ente, cod_uf, sig_uf, cod_ibge, nom_ente),
      ~ as.numeric(.x)
    )
  )

fwrite(base_2025, "data/base_2025.csv", sep = ";", dec = ",", bom = TRUE)
