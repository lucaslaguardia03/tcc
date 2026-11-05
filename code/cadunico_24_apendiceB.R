pkgs <- c("readxl", "dplyr", "openxlsx", "sandwich", "lmtest", "tibble")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(openxlsx); library(sandwich); library(lmtest)
})

arq_entrada <- "data_raw/base_correlacoes.xlsx"
arq_saida   <- "data/cadunico_24_apendiceB.xlsx"
dir.create("data", showWarnings = FALSE)
NO <- 6.5

raw <- read_excel(arq_entrada, sheet = "Planilha2", .name_repair = "minimal")
d_all <- tibble(
  cod_ibge = as.character(raw$cod_ibge),
  var      = as.numeric(raw[["variacao_recfundeb_18-25"]]),
  rec      = as.numeric(raw$rec_educ_por_mat) / 1000,
  cad      = as.numeric(raw$pct_pop_cadunico),
  cad24    = as.numeric(raw$pct_pop_cadunico_2024),
  rural    = as.numeric(raw$pct_mat_rural),
  creche   = as.numeric(raw$pct_mat_creche),
  especial = as.numeric(raw$pct_mat_especial),
  qi       = as.numeric(raw$pct_mat_quilombola_indigena),
  eja      = as.numeric(raw$pct_mat_eja),
  integral = as.numeric(raw$pct_mat_integral)
)
cat("Linhas na base:", nrow(d_all), "\n")
cat("Sem CadÚnico 2024:", sum(is.na(d_all$cad24)), "| sem CadÚnico 2018:", sum(is.na(d_all$cad)), "\n")

d <- d_all %>% select(-cad24) %>% filter(complete.cases(.)) %>%
  left_join(d_all %>% select(cod_ibge, cad24), by = "cod_ibge") %>%
  mutate(razao = 1 + var / 100, ylog = log(razao))
n_principal <- nrow(d)
d25 <- d %>% filter(!is.na(cad24))
cat("n modelo principal:", n_principal, "| n com CadÚnico 2024:", nrow(d25), "\n")

virg <- function(x, k = 2) { r <- ifelse(is.na(x), "–", formatC(x, format = "f", digits = k)); r <- gsub("-", "−", r); gsub("[.]", ",", r) }
fp <- function(p) ifelse(is.na(p), "–", ifelse(p < 0.001, "<0,001", gsub("[.]", ",", formatC(p, format = "f", digits = 3))))
est <- function(p) ifelse(is.na(p), "", ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", ""))))
vcv <- function(ft) vcovHC(ft, type = "HC3")
fator_pp <- function(ft) 100 * mean(exp(fitted(ft)) * mean(exp(residuals(ft))))
rhs <- function(cadvar) paste0("rec + pmax(rec - ", NO, ", 0) + ", cadvar)

resumo <- function(dat, cadvar, rotulo) {
  ft <- lm(as.formula(paste("ylog ~", rhs(cadvar))), data = dat)
  ct <- coeftest(ft, vcov = vcv(ft)); ci <- coefci(ft, vcov. = vcv(ft))
  b <- coef(ft); V <- vcv(ft); fac <- fator_pp(ft); gl <- ft$df.residual
  nm <- names(b); j1 <- which(nm == "rec"); j2 <- grep("pmax", nm); jc <- which(nm == cadvar)
  b_ab <- b[j1]; b_ac <- b[j1] + b[j2]
  e_ab <- sqrt(V[j1, j1]); e_ac <- sqrt(V[j1, j1] + V[j2, j2] + 2 * V[j1, j2])
  p_ab <- 2 * pt(-abs(b_ab / e_ab), gl); p_ac <- 2 * pt(-abs(b_ac / e_ac), gl)
  rs <- resettest(ft, power = 2:3, type = "fitted")
  Rm <- diag(length(b))[-1, , drop = FALSE]
  Fr <- as.numeric(t(Rm %*% b) %*% solve(Rm %*% V %*% t(Rm)) %*% (Rm %*% b)) / nrow(Rm)
  list(
    B1 = tibble(Especificacao = rotulo,
                Abaixo = paste0(virg(b_ab * fac, 1), est(p_ab)), Acima = paste0(virg(b_ac * fac, 1), est(p_ac)),
                CadUnico = paste0(virg(unname(b[jc]) * fac, 2), est(ct[jc, 4])), Porte10 = "–",
                n = nrow(dat), R2aj = virg(summary(ft)$adj.r.squared, 3), reset_p = fp(rs$p.value)),
    detalhe = tibble(
      especificacao = rotulo, n = nrow(dat),
      coef_cad_x100 = 100 * unname(b[jc]), ep_HC3_x100 = 100 * ct[jc, 2],
      ic95_inf_x100 = 100 * ci[jc, 1], ic95_sup_x100 = 100 * ci[jc, 2],
      p_cad = ct[jc, 4], efeito_medio_pp = unname(b[jc]) * fac,
      efeito_pp_abaixo = b_ab * fac, p_abaixo = p_ab, efeito_pp_acima = b_ac * fac, p_acima = p_ac,
      r2 = summary(ft)$r.squared, r2_ajustado = summary(ft)$adj.r.squared,
      F_robusto = Fr, p_F_robusto = pf(Fr, nrow(Rm), gl, lower.tail = FALSE),
      reset_F = unname(rs$statistic), reset_p = rs$p.value,
      ep_residual_x100 = 100 * summary(ft)$sigma))
}

r1  <- resumo(d,   "cad",   "(1) Principal (CadÚnico de 2018)")
r11 <- resumo(d25, "cad24", "(11) CadÚnico de 2024 no lugar do de 2018")
r1c <- resumo(d25, "cad",   "(1) Principal, amostra da linha 11")
ft_ambos <- lm(as.formula(paste("ylog ~", rhs("cad + cad24"))), data = d25)
ct_amb <- coeftest(ft_ambos, vcov = vcv(ft_ambos)); fac_amb <- fator_pp(ft_ambos)
ambos <- tibble(termo = c("cad (2018)", "cad24 (2024)"),
                coef_x100 = 100 * coef(ft_ambos)[c("cad", "cad24")],
                ep_HC3_x100 = 100 * ct_amb[c("cad", "cad24"), 2],
                p = ct_amb[c("cad", "cad24"), 4],
                efeito_medio_pp = coef(ft_ambos)[c("cad", "cad24")] * fac_amb,
                r2_ajustado = summary(ft_ambos)$adj.r.squared)

cobertura <- tibble(
  medida = c("CadÚnico 2018", "CadÚnico 2024"),
  n_validos = c(sum(!is.na(d$cad)), sum(!is.na(d$cad24))),
  media = c(mean(d$cad, na.rm = TRUE), mean(d$cad24, na.rm = TRUE)),
  desvio_padrao = c(sd(d$cad, na.rm = TRUE), sd(d$cad24, na.rm = TRUE)),
  minimo = c(min(d$cad, na.rm = TRUE), min(d$cad24, na.rm = TRUE)),
  maximo = c(max(d$cad, na.rm = TRUE), max(d$cad24, na.rm = TRUE)),
  acima_100 = c(sum(d$cad > 100, na.rm = TRUE), sum(d$cad24 > 100, na.rm = TRUE)))
cor_medidas <- tibble(
  pearson_2018_2024 = cor(d25$cad, d25$cad24),
  spearman_2018_2024 = cor(d25$cad, d25$cad24, method = "spearman"),
  pearson_2024_com_ln_razao = cor(d25$cad24, d25$ylog),
  pearson_2018_com_ln_razao = cor(d25$cad, d25$ylog))

tabB1_linha11 <- bind_rows(r1$B1, r11$B1)
detalhe <- bind_rows(r1$detalhe, r1c$detalhe, r11$detalhe)

wb <- createWorkbook()
add <- function(nome, dados) {
  addWorksheet(wb, nome); writeData(wb, nome, dados)
  setColWidths(wb, nome, cols = seq_len(ncol(dados)), widths = "auto")
  addStyle(wb, nome, createStyle(textDecoration = "bold", fgFill = "#D9E1F2"), rows = 1, cols = seq_len(ncol(dados)))
  freezePane(wb, nome, firstRow = TRUE)
}
add("tabB1_linha11", as.data.frame(tabB1_linha11))
add("detalhe", as.data.frame(detalhe))
add("duas_medidas", as.data.frame(ambos))
add("cobertura_e_correlacao", as.data.frame(cbind(cobertura, cor_medidas[rep(1, 2), ])))
saveWorkbook(wb, arq_saida, overwrite = TRUE)
cat("Arquivo gerado:", arq_saida, "\n")
print(as.data.frame(tabB1_linha11)); print(as.data.frame(detalhe)); print(as.data.frame(ambos))
