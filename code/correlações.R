
# =====================================================================
pkgs <- c("readxl", "dplyr", "openxlsx", "sandwich", "lmtest", "splines", "mgcv", "tibble")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(openxlsx)
  library(sandwich); library(lmtest); library(splines); library(mgcv)
})

arq_entrada <- "data_raw/base_correlacoes.xlsx"
arq_saida   <- "data/correlacoes.xlsx"
dir.create("data", showWarnings = FALSE)
SEMENTE <- 20261031

# ---------------------------------------------------------------- dados
raw <- read_excel(arq_entrada, sheet = "Planilha2", .name_repair = "minimal")
d <- tibble(
  cod_ibge = as.character(raw$cod_ibge),
  var      = as.numeric(raw[["variacao_recfundeb_18-25"]]),     # variação %
  rec      = as.numeric(raw$rec_educ_por_mat) / 1000,           # R$ mil
  rcl      = as.numeric(raw$receita_corrente_liquida_pc) / 1000,# R$ mil
  cad      = as.numeric(raw$pct_pop_cadunico),
  rural    = as.numeric(raw$pct_mat_rural),
  creche   = as.numeric(raw$pct_mat_creche),
  especial = as.numeric(raw$pct_mat_especial),
  qi       = as.numeric(raw$pct_mat_quilombola_indigena),
  eja      = as.numeric(raw$pct_mat_eja),
  integral = as.numeric(raw$pct_mat_integral),
  mat      = as.numeric(raw$mat_total_2018),
  pop      = as.numeric(raw$populacao_2018)
) %>% filter(complete.cases(.))
d <- d %>% mutate(razao = 1 + var / 100, ylog = log(razao),
                  d_qi = as.numeric(qi > 0), d_creche = as.numeric(creche > 0),
                  d_especial = as.numeric(especial > 0), d_eja = as.numeric(eja > 0),
                  d_integral = as.numeric(integral > 0))
n <- nrow(d)
cat("Municípios:", n, "\n")

arred <- function(x, k = 4) ifelse(is.numeric(x), round(x, k), x)

# ------------------------------------------------------------ utilitários
rhs_fiscal_lin <- "rec + rcl + cad"
rhs_comp_lin   <- "rural + creche + especial + qi + eja + integral"

fml <- function(rhs, dep_log) as.formula(paste(if (dep_log) "ylog" else "var", "~", rhs))

# Para comparar ln(razão) com a variação em nível, o AIC/BIC usa o Jacobiano:
# ambos passam a medir a verossimilhança da RAZÃO (receita2025/receita2018).
ic_comp <- function(fit, dep_log) {
  if (dep_log) c(AIC(fit) + 2 * sum(log(d$razao)), BIC(fit) + 2 * sum(log(d$razao)))
  else c(AIC(fit) - 2 * n * log(100), BIC(fit) - 2 * n * log(100))
}
prev_orig <- function(fit, dep_log) {            # previsão na escala da variação %
  if (dep_log) { sm <- mean(exp(residuals(fit))); (exp(fitted(fit)) * sm - 1) * 100 }
  else fitted(fit)
}
cv_rmse <- function(rhs, dep_log, k = 10, reps = 5) {
  set.seed(SEMENTE); out <- numeric(reps)
  for (r in seq_len(reps)) {
    fold <- sample(rep(seq_len(k), length.out = n)); pr <- numeric(n)
    for (f in seq_len(k)) {
      tr <- d[fold != f, ]; te <- d[fold == f, ]
      ft <- lm(fml(rhs, dep_log), data = tr)
      p <- predict(ft, newdata = te)
      if (dep_log) p <- (exp(p) * mean(exp(residuals(ft))) - 1) * 100
      pr[fold == f] <- p
    }
    out[r] <- sqrt(mean((d$var - pr)^2))
  }
  mean(out)
}
avalia <- function(rhs, dep_log, etapa, forma, nest_rhs = NA, cv = TRUE) {
  fit <- lm(fml(rhs, dep_log), data = d)
  ic <- ic_comp(fit, dep_log)
  p_nl <- NA_real_
  if (!is.na(nest_rhs)) {
    f0 <- lm(fml(nest_rhs, dep_log), data = d)
    p_nl <- tryCatch(waldtest(fit, f0, vcov = function(x) vcovHC(x, type = "HC3"))$`Pr(>F)`[2],
                     error = function(e) NA_real_)
  }
  fo <- prev_orig(fit, dep_log)
  linha <- tibble::tibble(
    etapa = etapa, dependente = if (dep_log) "ln(razão)" else "variação % (nível)",
    forma = forma, k_parametros = length(coef(fit)) + 1,
    r2_ajustado = summary(fit)$adj.r.squared,
    r2_escala_original = cor(d$var, fo)^2,
    aic_comparavel = ic[1], bic_comparavel = ic[2],
    rmse_cv_10fold = if (cv) cv_rmse(rhs, dep_log) else NA_real_,
    reset_p = tryCatch(resettest(fit, power = 2:3, type = "fitted")$p.value, error = function(e) NA_real_),
    breusch_pagan_p = tryCatch(bptest(fit)$p.value, error = function(e) NA_real_),
    assimetria_residuos = { e <- residuals(fit); mean((e - mean(e))^3) / sd(e)^3 },
    p_termos_nao_lineares_HC3 = p_nl, rhs = rhs)
  list(fit = fit, linha = linha)
}
# regra de escolha: menor BIC comparável; só abandona a forma linear se o
# RMSE de validação cruzada não piorar (parcimônia).
escolhe <- function(tab) {
  i <- which.min(tab$bic_comparavel)
  # a forma linear (linha 1) só é abandonada se a alternativa tiver BIC pelo menos
  # 2 pontos menor (evidência "positiva", Raftery 1995) e não piorar o RMSE de validação cruzada
  if (i != 1 && (tab$bic_comparavel[1] - tab$bic_comparavel[i] < 2 ||
                 tab$rmse_cv_10fold[i] > tab$rmse_cv_10fold[1])) i <- 1
  i
}

# ============================================================ 1) CORRELAÇÕES
cor_linhas <- list()
xs <- c(rec = "Receita total vinculada/matrícula (R$ mil)", rcl = "RCL per capita (R$ mil)",
        cad = "CadÚnico (%)", rural = "Matrículas rurais (%)", creche = "Matrículas em creche (%)",
        especial = "Matrículas em educação especial (%)", qi = "Matrículas quilombolas/indígenas (%)",
        eja = "Matrículas em EJA (%)", integral = "Matrículas em tempo integral (%)")
for (v in names(xs)) {
  x <- d[[v]]
  formas <- list(nivel = x, quadrado = x^2, ln1p = log1p(x))
  if (all(x > 0)) formas$ln <- log(x)
  for (nm in names(formas)) {
    cor_linhas[[length(cor_linhas) + 1]] <- tibble::tibble(
      variavel = xs[[v]], forma_de_x = nm,
      pearson_var = cor(formas[[nm]], d$var), spearman_var = cor(formas[[nm]], d$var, method = "spearman"),
      pearson_ln_razao = cor(formas[[nm]], d$ylog))
  }
}
correlacoes <- bind_rows(cor_linhas)

# ================================================== 2) FORMA DA DEPENDENTE
s2 <- list(); s2fit <- list()
for (dl in c(FALSE, TRUE)) {
  a <- avalia(rhs_fiscal_lin, dl, "Dependente: bloco fiscal", "linear")
  b <- avalia(rhs_comp_lin,   dl, "Dependente: bloco composição", "linear")
  c <- avalia(paste(rhs_fiscal_lin, "+", rhs_comp_lin), dl, "Dependente: modelo conjunto", "linear")
  s2[[length(s2) + 1]] <- bind_rows(a$linha, b$linha, c$linha)
}
s2 <- bind_rows(s2)
# Box-Cox sobre a razão (modelo conjunto)
bc <- tryCatch({
  fit_bc <- lm(razao ~ rec + rcl + cad + rural + creche + especial + qi + eja + integral, data = d)
  b <- MASS::boxcox(fit_bc, lambda = seq(-2, 2, 0.01), plotit = FALSE)
  lam <- b$x[which.max(b$y)]
  ic <- b$x[b$y > max(b$y) - qchisq(0.95, 1) / 2]
  tibble::tibble(lambda_otimo = lam, ic95_inf = min(ic), ic95_sup = max(ic),
                 leitura = "lambda=1: nível; lambda=0: logaritmo; o IC mostra qual é compatível")
}, error = function(e) tibble::tibble(lambda_otimo = NA, ic95_inf = NA, ic95_sup = NA, leitura = "erro"))
tot_bic <- s2 %>% group_by(dependente) %>% summarise(bic_total = sum(bic_comparavel), .groups = "drop")
DEP_LOG <- tot_bic$bic_total[tot_bic$dependente == "ln(razão)"] < tot_bic$bic_total[tot_bic$dependente == "variação % (nível)"]
cat("Dependente escolhida:", if (DEP_LOG) "ln(razão)" else "nível", "\n")

# =========================================== 3) FORMA DO BLOCO FISCAL
# 3a) receita total vinculada por matrícula (RCL e CadÚnico lineares)
best_knot <- function(dep_log) {
  g <- seq(5, 9, by = 0.25)
  bics <- sapply(g, function(k) BIC(lm(fml(paste("rec + pmax(rec -", k, ", 0) + rcl + cad"), dep_log), data = d)))
  g[which.min(bics)]
}
KNOT <- best_knot(DEP_LOG)
rec_formas <- list(
  linear = list("rec", NA),
  ln = list("log(rec)", NA),
  quadratica = list("rec + I(rec^2)", "rec"),
  ln_quadratica = list("log(rec) + I(log(rec)^2)", "log(rec)"),
  spline_linear_nó_6.5 = list("rec + pmax(rec - 6.5, 0)", "rec"),
  spline_linear_nó_buscado = list(paste0("rec + pmax(rec - ", KNOT, ", 0)"), "rec"),
  spline_natural_3gl = list("ns(rec, df = 3)", "rec"),
  spline_natural_4gl = list("ns(rec, df = 4)", "rec"),
  faixas_ate6.5_6.5a10_acima10 = list("cut(rec, c(0, 6.5, 10, Inf))", NA)
)
s3a <- list(); fits3a <- list()
for (nm in names(rec_formas)) {
  rhs <- paste(rec_formas[[nm]][[1]], "+ rcl + cad")
  nest <- if (is.na(rec_formas[[nm]][[2]])) NA else paste(rec_formas[[nm]][[2]], "+ rcl + cad")
  r <- avalia(rhs, DEP_LOG, "Fiscal: forma da receita", nm, nest)
  s3a[[nm]] <- r$linha; fits3a[[nm]] <- r$fit
}
s3a <- bind_rows(s3a)
i <- escolhe(s3a); REC_ESC <- s3a$forma[i]; REC_RHS <- rec_formas[[REC_ESC]][[1]]
cat("Forma da receita escolhida:", REC_ESC, "\n")

# GAM como referência flexível (graus de liberdade efetivos)
gam_fit <- gam(as.formula(paste(if (DEP_LOG) "ylog" else "var", "~ s(rec) + rcl + cad")), data = d, method = "REML")
gs <- summary(gam_fit)
gam_tab <- tibble::tibble(
  termo = "s(receita total vinculada/matrícula)", edf = gs$s.table[1, "edf"], p = gs$s.table[1, "p-value"],
  leitura = "edf próximo de 1 = relação aproximadamente linear; edf > 2 = curva", r2_ajustado = gs$r.sq)

# 3b) RCL
rcl_formas <- list(linear = list("rcl", NA), ln = list("log(rcl)", NA), quadratica = list("rcl + I(rcl^2)", "rcl"))
s3b <- list()
for (nm in names(rcl_formas)) {
  r <- avalia(paste(REC_RHS, "+", rcl_formas[[nm]][[1]], "+ cad"), DEP_LOG, "Fiscal: forma da RCL", nm,
              if (is.na(rcl_formas[[nm]][[2]])) NA else paste(REC_RHS, "+", rcl_formas[[nm]][[2]], "+ cad"))
  s3b[[nm]] <- r$linha
}
s3b <- bind_rows(s3b); i <- escolhe(s3b); RCL_ESC <- s3b$forma[i]; RCL_RHS <- rcl_formas[[RCL_ESC]][[1]]
cat("Forma da RCL escolhida:", RCL_ESC, "\n")

# 3c) CadÚnico
cad_formas <- list(linear = list("cad", NA), ln = list("log(cad)", NA), quadratica = list("cad + I(cad^2)", "cad"))
s3c <- list()
for (nm in names(cad_formas)) {
  r <- avalia(paste(REC_RHS, "+", RCL_RHS, "+", cad_formas[[nm]][[1]]), DEP_LOG, "Fiscal: forma do CadÚnico", nm,
              if (is.na(cad_formas[[nm]][[2]])) NA else paste(REC_RHS, "+", RCL_RHS, "+", cad_formas[[nm]][[2]]))
  s3c[[nm]] <- r$linha
}
s3c <- bind_rows(s3c); i <- escolhe(s3c); CAD_ESC <- s3c$forma[i]; CAD_RHS <- cad_formas[[CAD_ESC]][[1]]
cat("Forma do CadÚnico escolhida:", CAD_ESC, "\n")
RHS_FISCAL_FINAL <- paste(REC_RHS, "+", RCL_RHS, "+", CAD_RHS)

# ==================================== 4) FORMA DO BLOCO COMPOSIÇÃO
sq <- "I(rural^2) + I(creche^2) + I(especial^2) + I(qi^2) + I(eja^2) + I(integral^2)"
ln1 <- "log1p(rural) + log1p(creche) + log1p(especial) + log1p(qi) + log1p(eja) + log1p(integral)"
dum <- "d_creche + d_especial + d_qi + d_eja + d_integral"
comp_formas <- list(
  linear = list(rhs_comp_lin, NA),
  linear_mais_quadrados = list(paste(rhs_comp_lin, "+", sq), rhs_comp_lin),
  ln1p = list(ln1, NA),
  linear_mais_dummies_de_zero = list(paste(rhs_comp_lin, "+", dum), rhs_comp_lin),
  ln1p_mais_dummies_de_zero = list(paste(ln1, "+", dum), ln1)
)
s4 <- list()
for (nm in names(comp_formas)) {
  r <- avalia(comp_formas[[nm]][[1]], DEP_LOG, "Composição: forma das variáveis", nm, comp_formas[[nm]][[2]])
  s4[[nm]] <- r$linha
}
s4 <- bind_rows(s4); i <- escolhe(s4); COMP_ESC <- s4$forma[i]; COMP_RHS <- comp_formas[[COMP_ESC]][[1]]
cat("Forma da composição escolhida:", COMP_ESC, "\n")

# ====================================================== 5) MODELOS FINAIS
ame_log <- function(fit, term) {            # efeito médio, em p.p. de variação, de uma variável linear
  b <- coef(fit)[term]; sm <- mean(exp(residuals(fit))); unname(b * 100 * mean(exp(fitted(fit)) * sm))
}
tabela_coef <- function(fit, nome_modelo, dep_log) {
  ct <- coeftest(fit, vcov = vcovHC(fit, type = "HC3")); ci <- coefci(fit, vcov. = vcovHC(fit, type = "HC3"))
  out <- tibble::tibble(modelo = nome_modelo, termo = rownames(ct), coef = ct[, 1], ep_hc3 = ct[, 2],
                        t = ct[, 3], p = ct[, 4], ic95_inf = ci[, 1], ic95_sup = ci[, 2])
  out$efeito_medio_pp <- NA_real_
  if (dep_log) for (j in seq_len(nrow(out))) if (out$termo[j] != "(Intercept)" && !grepl("[(]", out$termo[j]))
    out$efeito_medio_pp[j] <- ame_log(fit, out$termo[j])
  out
}
fit_fisc_novo <- lm(fml(RHS_FISCAL_FINAL, DEP_LOG), data = d)
fit_comp_novo <- lm(fml(COMP_RHS, DEP_LOG), data = d)
fit_conj_novo <- lm(fml(paste(RHS_FISCAL_FINAL, "+", COMP_RHS), DEP_LOG), data = d)
fit_fisc_velho <- lm(var ~ rec + rcl + cad, data = d)
fit_comp_velho <- lm(var ~ rural + creche + especial + qi + eja + integral, data = d)
fit_conj_velho <- lm(var ~ rec + rcl + cad + rural + creche + especial + qi + eja + integral, data = d)
cat("Conferência modelo atual (receita, deve ser -1,27 por R$ mil):",
    round(coef(fit_fisc_velho)["rec"], 3), "\n")

coefs <- bind_rows(
  tabela_coef(fit_fisc_novo, "Fiscal NOVO", DEP_LOG), tabela_coef(fit_comp_novo, "Composição NOVO", DEP_LOG),
  tabela_coef(fit_conj_novo, "Conjunto NOVO", DEP_LOG),
  tabela_coef(fit_fisc_velho, "Fiscal ATUAL (Tabela 17)", FALSE), tabela_coef(fit_comp_velho, "Composição ATUAL (Tabela 18)", FALSE),
  tabela_coef(fit_conj_velho, "Conjunto ATUAL", FALSE))

diag_linha <- function(fit, nome, dep_log) {
  ct <- NULL; e <- residuals(fit); cd <- cooks.distance(fit)
  tibble::tibble(modelo = nome, r2 = summary(fit)$r.squared, r2_ajustado = summary(fit)$adj.r.squared,
                 r2_escala_original = cor(d$var, prev_orig(fit, dep_log))^2,
                 reset_p = resettest(fit, power = 2:3, type = "fitted")$p.value,
                 breusch_pagan_p = bptest(fit)$p.value, shapiro_p = shapiro.test(e)$p.value,
                 assimetria = mean((e - mean(e))^3) / sd(e)^3, curtose = mean((e - mean(e))^4) / sd(e)^4,
                 n_cook_acima_4n = sum(cd > 4 / n), maior_vif = tryCatch(max(car_vif(fit)), error = function(e) NA_real_))
}
car_vif <- function(fit) {                 # VIF simples (somente termos lineares/centrais)
  X <- model.matrix(fit)[, -1, drop = FALSE]
  sapply(seq_len(ncol(X)), function(j) 1 / (1 - summary(lm(X[, j] ~ X[, -j, drop = FALSE]))$r.squared))
}
diags <- bind_rows(diag_linha(fit_fisc_novo, "Fiscal NOVO", DEP_LOG), diag_linha(fit_comp_novo, "Composição NOVO", DEP_LOG),
                   diag_linha(fit_conj_novo, "Conjunto NOVO", DEP_LOG),
                   diag_linha(fit_fisc_velho, "Fiscal ATUAL", FALSE), diag_linha(fit_comp_velho, "Composição ATUAL", FALSE),
                   diag_linha(fit_conj_velho, "Conjunto ATUAL", FALSE))

# curva da receita: variação prevista (%), demais variáveis na média
grade <- tibble::tibble(rec = seq(4.5, 14, by = 0.5), rcl = mean(d$rcl), cad = mean(d$cad))
for (v in c("rural", "creche", "especial", "qi", "eja", "integral")) grade[[v]] <- mean(d[[v]])
pp <- function(fit, dep_log) {
  p <- predict(fit, newdata = grade)
  if (dep_log) (exp(p) * mean(exp(residuals(fit))) - 1) * 100 else p
}
curva <- tibble::tibble(receita_2018_R_mil = grade$rec,
                        previsto_modelo_ATUAL_linear = pp(fit_fisc_velho, FALSE),
                        previsto_modelo_NOVO = pp(fit_fisc_novo, DEP_LOG))

# comparação sinal / significância: atual x novo
termo_p <- function(fit, padrao) {
  ct <- coeftest(fit, vcov = vcovHC(fit, type = "HC3")); ix <- grep(padrao, rownames(ct), fixed = FALSE)
  if (length(ix) == 1) c(coef = ct[ix, 1], p = ct[ix, 4])
  else c(coef = NA, p = tryCatch(waldtest(fit, update(fit, as.formula(paste(". ~ . -", paste(rownames(ct)[ix], collapse = " - ")))),
                                          vcov = function(x) vcovHC(x, type = "HC3"))$`Pr(>F)`[2], error = function(e) NA))
}
comp_tab <- list()
mk <- function(bloco, var, pad, f_velho, f_novo) {
  a <- termo_p(f_velho, paste0("^", var, "$")); b <- termo_p(f_novo, pad)
  tibble::tibble(bloco = bloco, variavel = var, coef_atual = a["coef"], p_atual = a["p"],
                 coef_novo = b["coef"], p_novo_ou_conjunto = b["p"],
                 mesma_conclusao_a_5pct = (a["p"] < 0.05) == (b["p"] < 0.05))
}
comp_tab[[1]] <- mk("Fiscal", "rec", "rec|ns\\(rec|cut\\(rec", fit_fisc_velho, fit_fisc_novo)
comp_tab[[2]] <- mk("Fiscal", "rcl", "rcl", fit_fisc_velho, fit_fisc_novo)
comp_tab[[3]] <- mk("Fiscal", "cad", "cad", fit_fisc_velho, fit_fisc_novo)
for (v in c("rural", "creche", "especial", "qi", "eja", "integral"))
  comp_tab[[length(comp_tab) + 1]] <- mk("Composição", v, paste0("(^|[(])", v, "([)]|$|\\^)"), fit_comp_velho, fit_comp_novo)
comparacao <- bind_rows(comp_tab)


# ====================== 6) INCLINAÇÃO ABAIXO/ACIMA DO NÓ E SENSIBILIDADE
# Efeito de R$ 1.000 a mais de receita em 2018, em p.p. de variação (na dependente escolhida),
# abaixo e acima do nó, com a estrutura final do bloco fiscal.
sens <- list()
rodar_spline <- function(dados, knot, rotulo, dep_log = DEP_LOG, dep_col = NULL) {
  dd <- dados; dd$yy <- if (dep_log) dd$ylog else dd$var
  if (!is.null(dep_col)) dd$yy <- dep_col
  ft <- lm(yy ~ rec + pmax(rec - knot, 0) + log(rcl) + cad, data = dd)
  V <- vcovHC(ft, type = "HC3"); b <- coef(ft)
  i1 <- "rec"; i2 <- "pmax(rec - knot, 0)"
  nm <- names(b); j1 <- which(nm == "rec"); j2 <- grep("pmax", nm)
  sl_abaixo <- b[j1]; sl_acima <- b[j1] + b[j2]
  ep_acima <- sqrt(V[j1, j1] + V[j2, j2] + 2 * V[j1, j2])
  fator <- if (dep_log) 100 * mean(exp(fitted(ft)) * mean(exp(residuals(ft)))) else 1
  tibble::tibble(especificacao = rotulo, no_R_mil = knot, n = nrow(dd),
                 municipios_abaixo_do_no = sum(dd$rec < knot),
                 efeito_abaixo_pp_por_R1000 = unname(sl_abaixo * fator),
                 p_abaixo = unname(2 * pnorm(-abs(sl_abaixo / sqrt(V[j1, j1])))),
                 efeito_acima_pp_por_R1000 = unname(sl_acima * fator),
                 p_acima = unname(2 * pnorm(-abs(sl_acima / ep_acima))))
}
fit_tmp <- lm(fml(RHS_FISCAL_FINAL, DEP_LOG), data = d); cd_tmp <- cooks.distance(fit_tmp)
sens[[1]] <- rodar_spline(d, 6.5, "Principal (nó 6,5)")
sens[[2]] <- rodar_spline(d, 6.0, "Nó 6,0")
sens[[3]] <- rodar_spline(d, 7.0, "Nó 7,0")
sens[[4]] <- rodar_spline(d[cd_tmp <= 4 / n, ], 6.5, "Sem influentes (Cook > 4/n)")
dw <- d; lim <- quantile(d$var, c(.01, .99)); dw$var <- pmin(pmax(dw$var, lim[1]), lim[2]); dw$ylog <- log(1 + dw$var / 100)
sens[[5]] <- rodar_spline(dw, 6.5, "Dependente winsorizada (1% e 99%)")
sens[[6]] <- rodar_spline(d, 6.5, "Dependente em nível (variação %)", dep_log = FALSE)
sens_tab <- bind_rows(sens)
# o mesmo modelo, mas com o bloco de composição junto
fit_conj_tmp <- lm(fml(paste(RHS_FISCAL_FINAL, "+", COMP_RHS), DEP_LOG), data = d)


# ============================================ 7) TABELAS PRONTAS PARA O TEXTO
# Tudo abaixo reaproveita as escolhas feitas acima (DEP_LOG, REC_RHS, RCL_RHS, CAD_RHS, COMP_RHS).
stopifnot(DEP_LOG, grepl("pmax", REC_RHS), RCL_RHS == "log(rcl)", CAD_RHS == "cad", COMP_ESC == "linear")
virg <- function(x, k = 2) { ifelse(is.na(x), "–", gsub("[.]", ",", formatC(round(x, k), format = "f", digits = k, big.mark = ""))) }
virg <- function(x, k = 2) { r <- ifelse(is.na(x), "–", formatC(x, format = "f", digits = k)); r <- gsub("-", "−", r); gsub("[.]", ",", r) }
fp <- function(p) ifelse(is.na(p), "–", ifelse(p < 0.001, "<0,001", gsub("[.]", ",", formatC(p, format = "f", digits = 3))))
est <- function(p) ifelse(is.na(p), "", ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", ""))))
fnum <- function(x) format(round(x), big.mark = ".", decimal.mark = ",", scientific = FALSE)

DEPN <- "ylog"                     # dependente escolhida
RHS_F_K <- function(k, rcl = "log(rcl)") paste0("rec + pmax(rec - ", k, ", 0) + ", rcl, " + cad")
fator_pp <- function(ft) 100 * mean(exp(fitted(ft)) * mean(exp(residuals(ft))))  # converte coef (log) em p.p. médios
vcv <- function(ft) vcovHC(ft, type = "HC3")

# F robusto global
f_robusto <- function(ft) {
  nm <- names(coef(ft))[-1]
  R <- diag(length(coef(ft)))[-1, , drop = FALSE]
  b <- coef(ft); V <- vcv(ft)
  W <- t(R %*% b) %*% solve(R %*% V %*% t(R)) %*% (R %*% b)
  q <- nrow(R); F <- as.numeric(W) / q
  c(F = F, p = pf(F, q, ft$df.residual, lower.tail = FALSE))
}
cat("Conferência F robusto do modelo fiscal ATUAL (esperado 4,03):", round(f_robusto(fit_fisc_velho)["F"], 2), "\n")

# ---- Tabela 17 (fiscal) e Tabela 18 (composição): modelos finais
tab_coef_log <- function(ft, termos, rotulos, dat = d, beta = TRUE) {
  ct <- coeftest(ft, vcov = vcv(ft)); ci <- coefci(ft, vcov. = vcv(ft)); fac <- fator_pp(ft)
  sdy <- sd(model.response(model.frame(ft)))
  out <- list()
  for (i in seq_along(termos)) {
    j <- which(rownames(ct) == termos[i])
    b <- ct[j, 1]; e <- ct[j, 2]; p <- ct[j, 4]; li <- ci[j, 1]; ls <- ci[j, 2]
    x <- model.matrix(ft)[, termos[i]]
    out[[i]] <- tibble::tibble(variavel = rotulos[i], coef = 100 * b, ep = 100 * e, ic_inf = 100 * li, ic_sup = 100 * ls,
                               p = p, beta = if (beta) b * sd(x) / sdy else NA_real_, efeito_pp = b * fac)
  }
  bind_rows(out)
}
fit_F <- lm(fml(RHS_F_K(6.5), TRUE), data = d)
fit_C <- lm(fml(COMP_RHS, TRUE), data = d)
fit_J <- lm(fml(paste(RHS_F_K(6.5), "+", COMP_RHS), TRUE), data = d)

# segmentos da receita: abaixo do nó = b1; acima do nó = b1 + b2 (combinação linear)
segmentos <- function(ft, knot) {
  nm <- names(coef(ft)); j1 <- which(nm == "rec"); j2 <- grep("pmax", nm)
  b <- coef(ft); V <- vcv(ft); fac <- fator_pp(ft)
  bb <- c(b[j1], b[j1] + b[j2]); ee <- c(sqrt(V[j1, j1]), sqrt(V[j1, j1] + V[j2, j2] + 2 * V[j1, j2]))
  tt <- bb / ee; pp <- 2 * pt(-abs(tt), ft$df.residual)
  tibble::tibble(variavel = c("abaixo", "acima"), coef = 100 * bb, ep = 100 * ee,
                 ic_inf = 100 * (bb - qt(.975, ft$df.residual) * ee), ic_sup = 100 * (bb + qt(.975, ft$df.residual) * ee),
                 p = pp, beta = NA_real_, efeito_pp = bb * fac)
}
t17 <- bind_rows(
  segmentos(fit_F, 6.5) %>% mutate(variavel = c("Receita total vinculada, até R$ 6.500 (R$ 1.000)", "Receita total vinculada, acima de R$ 6.500 (R$ 1.000)")),
  tab_coef_log(fit_F, c("log(rcl)", "cad"), c("RCL per capita (ln)", "População no CadÚnico (p.p.)")))
t17$efeito_pp[3] <- coef(fit_F)["log(rcl)"] * log(1.1) * fator_pp(fit_F)   # efeito de +10% de RCL
rodape17 <- tibble::tibble(constante = coef(fit_F)[1] * 100, r2 = summary(fit_F)$r.squared, r2aj = summary(fit_F)$adj.r.squared,
                           F = f_robusto(fit_F)["F"], p_F = f_robusto(fit_F)["p"], ep_res = 100 * summary(fit_F)$sigma)
t18 <- tab_coef_log(fit_C, c("rural", "creche", "especial", "qi", "eja", "integral"),
                    c("Rural", "Creche", "Educação especial", "Educação quilombola/indígena", "EJA", "Tempo integral"))
rodape18 <- tibble::tibble(constante = coef(fit_C)[1] * 100, r2 = summary(fit_C)$r.squared, r2aj = summary(fit_C)$adj.r.squared,
                           F = f_robusto(fit_C)["F"], p_F = f_robusto(fit_C)["p"], ep_res = 100 * summary(fit_C)$sigma)
fmt_coef_tab <- function(t, nome_efeito = "Efeito médio (p.p.)") {
  data.frame(Variavel = t$variavel, Coeficiente = virg(t$coef, 2), EP_HC3 = virg(t$ep, 2),
             IC95 = paste0("[", virg(t$ic_inf, 2), "; ", virg(t$ic_sup, 2), "]"), p = fp(t$p),
             beta = virg(t$beta, 3), Efeito = virg(t$efeito_pp, 2), check.names = FALSE)
}
tab17_txt <- fmt_coef_tab(t17); tab18_txt <- fmt_coef_tab(t18)

# ---- Tabela A.1: diagnósticos dos três modelos finais
jb <- function(e) { n_ <- length(e); S <- mean((e - mean(e))^3) / sd(e)^3 * ((n_ - 1) / n_)^1.5
  K <- mean((e - mean(e))^4) / (mean((e - mean(e))^2))^2; S <- mean((e - mean(e))^3) / (mean((e - mean(e))^2))^1.5
  x <- n_ / 6 * (S^2 + (K - 3)^2 / 4); c(x, pchisq(x, 2, lower.tail = FALSE)) }
vif_niveis <- function(vars) sapply(vars, function(v) 1 / (1 - summary(lm(reformulate(setdiff(vars, v), v), data = d))$r.squared))
diag_novo <- function(ft, vars) {
  e <- residuals(ft); cd <- cooks.distance(ft); rs <- rstudent(ft); r <- resettest(ft, power = 2:3, type = "fitted")
  bp <- bptest(ft); sw <- shapiro.test(e); j <- jb(e)
  tibble::tibble(vif = max(vif_niveis(vars)), reset_F = unname(r$statistic), reset_p = r$p.value,
                 bp_chi = unname(bp$statistic), bp_p = bp$p.value, sw_W = unname(sw$statistic), sw_p = sw$p.value,
                 jb = j[1], jb_p = j[2], assim = mean((e - mean(e))^3) / (mean((e - mean(e))^2))^1.5,
                 curt = mean((e - mean(e))^4) / (mean((e - mean(e))^2))^2,
                 cook = sum(cd > 4 / n), stud = sum(abs(rs) > 3), max_cook = max(cd), cod_max_cook = d$cod_ibge[which.max(cd)],
                 lev_max_cook = hatvalues(ft)[which.max(cd)], cod_max_stud = d$cod_ibge[which.max(abs(rs))],
                 max_stud = rs[which.max(abs(rs))])
}
vf <- c("rec", "rcl", "cad"); vc <- c("rural", "creche", "especial", "qi", "eja", "integral")
dg <- bind_rows(Fiscal = diag_novo(fit_F, vf), Composicao = diag_novo(fit_C, vc), Conjunto = diag_novo(fit_J, c(vf, vc)), .id = "modelo")
vif_porte <- max(vif_niveis(c(vf, vc)), 1 / (1 - summary(lm(log(mat) ~ rec + rcl + cad + rural + creche + especial + qi + eja + integral, data = d))$r.squared))
fm <- function(x, k = 2) virg(x, k)
tabA1 <- data.frame(
  Diagnostico = c("Maior VIF", "RESET (potências 2 e 3): F (p)", "Breusch-Pagan: χ² (p)", "Shapiro-Wilk: W (p)",
                  "Jarque-Bera: estatística (p)", "Assimetria dos resíduos", "Curtose dos resíduos (normal = 3)",
                  "Municípios com distância de Cook > 4/n", "Municípios com resíduo studentizado acima de 3 (em módulo)"),
  Fiscal = NA, Composicao = NA, Conjunto = NA, check.names = FALSE)
for (m in 1:3) {
  g <- dg[m, ]
  tabA1[, m + 1] <- c(fm(g$vif), paste0(fm(g$reset_F), " (", fp(g$reset_p), ")"), paste0(fm(g$bp_chi), " (", fp(g$bp_p), ")"),
                      paste0(fm(g$sw_W, 3), " (", fp(g$sw_p), ")"), paste0(format(round(g$jb, 1), big.mark = ".", decimal.mark = ","), " (", fp(g$jb_p), ")"),
                      fm(g$assim), fm(g$curt), as.character(g$cook), as.character(g$stud))
}

# ---- Tabelas B.1 e B.2: robustez do modelo final
efeitos_fiscal <- function(ft, knot, dep_log, porte = NULL) {
  ct <- coeftest(ft, vcov = vcv(ft)); seg <- segmentos(ft, knot); b <- coef(ft)
  fac <- if (dep_log) fator_pp(ft) else 1
  if (!dep_log) { nm <- names(b); j1 <- which(nm == "rec"); j2 <- grep("pmax", nm); V <- vcv(ft)
    seg$efeito_pp <- c(b[j1], b[j1] + b[j2]) }
  pr <- function(nome) { j <- which(rownames(ct) == nome); list(ef = unname(b[j] * log(1.1) * fac), p = unname(ct[j, 4])) }
  rc <- pr("log(rcl)"); cj <- which(rownames(ct) == "cad")
  out <- list(ab = seg$efeito_pp[1], p_ab = seg$p[1], ac = seg$efeito_pp[2], p_ac = seg$p[2], rcl = rc$ef, p_rcl = rc$p,
              cad = unname(b[cj] * fac), p_cad = unname(ct[cj, 4]), porte = NA_real_, p_porte = NA_real_)
  if (!is.null(porte)) { q <- pr(porte); out$porte <- q$ef; out$p_porte <- q$p }
  out
}
spec_lin <- function(dat, dep, rhs) lm(as.formula(paste(dep, "~", rhs)), data = dat)
dw <- d; lim <- quantile(d$var, c(.01, .99)); dw$var <- pmin(pmax(dw$var, lim[1]), lim[2]); dw$ylog <- log(1 + dw$var / 100)
cdF <- cooks.distance(fit_F); cdC <- cooks.distance(fit_C); cdJ <- cooks.distance(fit_J)
fiscal_specs <- list(
  list("(1) Principal", d, "ylog", RHS_F_K(6.5), 6.5, NULL),
  list("(2) Dependente em nível (variação %)", d, "var", RHS_F_K(6.5), 6.5, NULL),
  list("(3) Winsorização da dependente", dw, "ylog", RHS_F_K(6.5), 6.5, NULL),
  list("(4) Sem municípios influentes", d[cdF <= 4 / n, ], "ylog", RHS_F_K(6.5), 6.5, NULL),
  list("(5) Nó em R$ 6.000", d, "ylog", RHS_F_K(6.0), 6.0, NULL),
  list("(6) Nó em R$ 7.000", d, "ylog", RHS_F_K(7.0), 7.0, NULL),
  list("(7) + ln matrículas totais", d, "ylog", paste(RHS_F_K(6.5), "+ log(mat)"), 6.5, "log(mat)"),
  list("(8) + ln população", d, "ylog", paste(RHS_F_K(6.5), "+ log(pop)"), 6.5, "log(pop)"),
  list("(9) Modelo conjunto", d, "ylog", paste(RHS_F_K(6.5), "+", COMP_RHS), 6.5, NULL),
  list("(10) Conjunto + ln matrículas totais", d, "ylog", paste(RHS_F_K(6.5), "+", COMP_RHS, "+ log(mat)"), 6.5, "log(mat)"))
fisc_rows <- lapply(fiscal_specs, function(sp) {
  ft <- spec_lin(sp[[2]], sp[[3]], sp[[4]]); ef <- efeitos_fiscal(ft, sp[[5]], sp[[3]] == "ylog", sp[[6]])
  tibble::tibble(Especificacao = sp[[1]],
                 Abaixo = paste0(virg(ef$ab, 1), est(ef$p_ab)), Acima = paste0(virg(ef$ac, 1), est(ef$p_ac)),
                 RCL10 = paste0(virg(ef$rcl, 2), est(ef$p_rcl)), CadUnico = paste0(virg(ef$cad, 2), est(ef$p_cad)),
                 Porte10 = ifelse(is.na(ef$porte), "–", paste0(virg(ef$porte, 2), est(ef$p_porte))),
                 n = nrow(sp[[2]]), R2aj = virg(summary(ft)$adj.r.squared, 3), reset_p = fp(resettest(ft, power = 2:3, type = "fitted")$p.value),
                 p_abaixo_num = ef$p_ab, p_rcl_num = ef$p_rcl, ab_num = ef$ab, ac_num = ef$ac, p_ac_num = ef$p_ac)
}) %>% bind_rows()
tabB1 <- fisc_rows %>% dplyr::select(Especificacao, Abaixo, Acima, RCL10, CadUnico, Porte10, n, R2aj, reset_p)

comp_term_fmt <- function(ft, fac) {
  ct <- coeftest(ft, vcov = vcv(ft)); b <- coef(ft)
  sapply(c("rural", "creche", "especial", "qi", "eja", "integral"), function(v) {
    if (!(v %in% rownames(ct))) return("–"); paste0(virg(b[v] * fac, 2), est(ct[v, 4])) })
}
comp_specs <- list(
  list("(1) Principal", d, "ylog", COMP_RHS, NULL, NULL),
  list("(2) Dependente em nível (variação %)", d, "var", COMP_RHS, NULL, NULL),
  list("(3) Winsorização da dependente", dw, "ylog", COMP_RHS, NULL, NULL),
  list("(4) Sem municípios influentes", d[cdC <= 4 / n, ], "ylog", COMP_RHS, NULL, NULL),
  list("(5) + ln matrículas totais", d, "ylog", paste(COMP_RHS, "+ log(mat)"), "log(mat)", NULL),
  list("(6) + ln população", d, "ylog", paste(COMP_RHS, "+ log(pop)"), "log(pop)", NULL),
  list("(7) Quilombola/indígena como binária", d, "ylog", "rural + creche + especial + d_qi + eja + integral", NULL, "d_qi"),
  list("(8) Modelo conjunto", d, "ylog", paste(RHS_F_K(6.5), "+", COMP_RHS), NULL, NULL),
  list("(9) Conjunto + ln matrículas totais", d, "ylog", paste(RHS_F_K(6.5), "+", COMP_RHS, "+ log(mat)"), "log(mat)", NULL),
  list("(10) Conjunto, com quilombola/indígena binária", d, "ylog", paste(RHS_F_K(6.5), "+ rural + creche + especial + d_qi + eja + integral"), NULL, "d_qi"))
comp_rows <- lapply(comp_specs, function(sp) {
  ft <- spec_lin(sp[[2]], sp[[3]], sp[[4]]); fac <- if (sp[[3]] == "ylog") fator_pp(ft) else 1
  ct <- coeftest(ft, vcov = vcv(ft)); b <- coef(ft)
  v <- sapply(c("rural", "creche", "especial", "qi", "eja", "integral"), function(nm) {
    nm2 <- if (!is.null(sp[[6]]) && nm == "qi") "d_qi" else nm
    paste0(virg(b[nm2] * fac, 2), est(ct[nm2, 4])) })
  porte <- if (is.null(sp[[5]])) "–" else paste0(virg(b[sp[[5]]] * log(1.1) * fac, 2), est(ct[sp[[5]], 4]))
  tibble::tibble(Especificacao = sp[[1]], Rural = v[1], Creche = v[2], Especial = v[3], QI = v[4], EJA = v[5], Integral = v[6],
                 Porte10 = porte, n = nrow(sp[[2]]), R2aj = virg(summary(ft)$adj.r.squared, 3),
                 pr = ct["rural", 4], pc = ct["creche", 4], pe = ct["eja", 4])
}) %>% bind_rows()
tabB2 <- comp_rows %>% dplyr::select(-pr, -pc, -pe)
# faixas dos coeficientes de rural/EJA/creche nas especificações (para o texto)
fx <- function(v) range(as.numeric(gsub(",", ".", gsub("−", "-", gsub("[*]", "", v)))))

# ---- Tabelas C.1 e C.2: seleção da forma funcional
rot <- c(linear = "R", ln = "ln R", quadratica = "R + R²", ln_quadratica = "ln R + (ln R)²",
         `spline_linear_nó_6.5` = "R + máx(R − 6,5; 0)", spline_natural_3gl = "Spline natural, 3 g.l.",
         spline_natural_4gl = "Spline natural, 4 g.l.", faixas_ate6.5_6.5a10_acima10 = "Faixas: até 6,5; 6,5 a 10; acima de 10")
linhaC <- function(tab, rotulos, bloco, sel) {
  tab$delta_bic <- tab$bic_comparavel - tab$bic_comparavel[1]
  data.frame(Bloco = bloco, Forma = rotulos[tab$forma], R2aj = virg(tab$r2_ajustado, 3), dBIC = virg(tab$delta_bic, 1),
             RMSE = virg(tab$rmse_cv_10fold, 2), RESET = fp(tab$reset_p), pNL = fp(tab$p_termos_nao_lineares_HC3),
             escolhida = tab$forma == sel, check.names = FALSE)
}
s2_conj <- s2[s2$etapa == "Dependente: modelo conjunto", ]
s2_conj$forma_rot <- c("Variação % (nível)", "ln(1 + variação/100)")
C1_dep <- data.frame(Bloco = "Variável dependente (modelo conjunto, formas lineares)", Forma = s2_conj$forma_rot,
                     R2aj = virg(s2_conj$r2_ajustado, 3), dBIC = virg(s2_conj$bic_comparavel - s2_conj$bic_comparavel[1], 1),
                     RMSE = virg(s2_conj$rmse_cv_10fold, 2), RESET = fp(s2_conj$reset_p), pNL = "–",
                     escolhida = c(FALSE, TRUE), check.names = FALSE)
s3a_u <- s3a[s3a$forma != "spline_linear_nó_buscado", ]
C1_rec <- linhaC(s3a_u, rot, "Receita total vinculada por matrícula (R, em R$ mil)", REC_ESC)
rot2 <- c(linear = "RCL", ln = "ln RCL", quadratica = "RCL + RCL²")
C2_rcl <- linhaC(s3b, rot2, "RCL per capita", RCL_ESC)
rot3 <- c(linear = "CadÚnico", ln = "ln CadÚnico", quadratica = "CadÚnico + CadÚnico²")
C2_cad <- linhaC(s3c, rot3, "População no CadÚnico", CAD_ESC)
rot4 <- c(linear = "Percentuais lineares", linear_mais_quadrados = "Lineares + quadrados", ln1p = "ln(1 + percentual)",
          linear_mais_dummies_de_zero = "Lineares + dummies de zero", ln1p_mais_dummies_de_zero = "ln(1 + percentual) + dummies de zero")
C2_comp <- linhaC(s4, rot4, "Composição das matrículas", COMP_ESC)
tabC1 <- bind_rows(C1_dep, C1_rec); tabC2 <- bind_rows(C2_rcl, C2_cad, C2_comp)

# ---- números auxiliares para o texto
aux <- tibble::tibble(
  item = c("n_abaixo_6.5", "pct_abaixo_6.5", "gam_edf", "gam_p", "boxcox_lambda", "boxcox_ic_inf", "boxcox_ic_sup",
           "rural_min_rob", "rural_max_rob", "eja_min_rob", "eja_max_rob", "creche_min_rob", "creche_max_rob",
           "cook_fiscal", "cook_comp", "cook_conj", "vif_com_porte", "p_beta_receita_conj_abaixo", "p_acima_conj", "efeito_ab_pp", "efeito_ac_pp"),
  valor = c(sum(d$rec < 6.5), 100 * mean(d$rec < 6.5), gam_tab$edf, gam_tab$p, bc$lambda_otimo, bc$ic95_inf, bc$ic95_sup,
            fx(comp_rows$Rural)[1], fx(comp_rows$Rural)[2], fx(comp_rows$EJA)[1], fx(comp_rows$EJA)[2],
            fx(comp_rows$Creche)[1], fx(comp_rows$Creche)[2], dg$cook[1], dg$cook[2], dg$cook[3], vif_porte,
            fisc_rows$p_abaixo_num[9], fisc_rows$p_ac_num[9], fisc_rows$ab_num[1], fisc_rows$ac_num[1]))

# ================================================================ SAÍDA
escolhas <- tibble::tibble(
  decisao = c("Variável dependente", "Forma da receita (bloco fiscal)", "Forma da RCL", "Forma do CadÚnico",
              "Forma da composição", "Nó do spline buscado (R$ mil)"),
  escolha = c(if (DEP_LOG) "ln(razão) = ln(1 + variação/100)" else "variação % em nível",
              REC_ESC, RCL_ESC, CAD_ESC, COMP_ESC, as.character(KNOT)),
  regra = c("menor BIC comparável (com Jacobiano) somado nos 3 modelos",
            rep("menor BIC; só abandona o linear se o BIC cair >= 2 e o RMSE de validação cruzada não piorar", 4),
            "menor BIC em grade de 5 a 9"))
leia <- data.frame(`Leia-me` = c(
  "Seleção da forma funcional e escolha dos modelos (Tabelas 17 e 18).",
  "Ordem: (1) correlações em formas alternativas; (2) forma da dependente; (3) forma da receita, RCL e CadÚnico; (4) forma das variáveis de composição; (5) modelos finais e comparação com os atuais.",
  "BIC/AIC 'comparáveis': para comparar ln(razão) com a variação em nível, a verossimilhança de ambos é expressa na escala da razão receita2025/receita2018 (ajuste de Jacobiano).",
  "RMSE de validação cruzada: erro de previsão fora da amostra (10 partes x 5 repetições), sempre na escala da variação %.",
  "r2_escala_original: R² entre a variação % observada e a prevista pelo modelo, para comparar modelos com dependentes diferentes.",
  "Regra de escolha: menor BIC; permanece na forma linear (parcimônia) se a alternativa reduz o BIC em menos de 2 pontos ou piora o RMSE da validação cruzada.",
  "A escolha é feita nos mesmos dados em que se testam as hipóteses; os p-valores dos modelos finais devem ser lidos com essa ressalva.",
  "efeito_medio_pp: com dependente em log, converte o coeficiente em p.p. de variação da receita (efeito médio na amostra).",
  "Os coeficientes monetários estão em R$ 1.000, como nas Tabelas 17 e 18.",
  "Erros-padrão: HC3. Teste de termos não lineares: Wald com HC3 (modelos aninhados).",
  "VIF alto na aba 5_Diagnosticos para a receita é artefato do spline (rec e pmax(rec-6,5) são colineares por construção), não multicolinearidade entre variáveis."),
  check.names = FALSE)

wb <- createWorkbook()
add <- function(nome, dados) {
  addWorksheet(wb, nome); writeData(wb, nome, dados)
  setColWidths(wb, nome, cols = seq_len(ncol(dados)), widths = "auto")
  addStyle(wb, nome, createStyle(textDecoration = "bold", fgFill = "#D9E1F2"), rows = 1, cols = seq_len(ncol(dados)))
  freezePane(wb, nome, firstRow = TRUE)
}
rd <- function(x) { x[] <- lapply(x, function(c) if (is.numeric(c)) round(c, 5) else c); x }
add("Leia-me", leia); add("Escolhas", escolhas)
add("1_Correlacoes", rd(correlacoes))
add("2_Dependente", rd(s2)); add("2b_BoxCox", rd(bc))
add("3a_Receita", rd(s3a)); add("3a_GAM", rd(gam_tab)); add("3b_RCL", rd(s3b)); add("3c_CadUnico", rd(s3c))
add("4_Composicao", rd(s4))
add("5_Coeficientes", rd(coefs)); add("5_Diagnosticos", rd(diags))
add("5_Curva_receita", rd(curva)); add("5_Comparacao", rd(comparacao)); add("6_Sensibilidade_no", rd(sens_tab))
add("T17_Fiscal", tab17_txt); add("T17_rodape", rd(rodape17)); add("T18_Composicao", tab18_txt); add("T18_rodape", rd(rodape18))
add("TA1_Diagnosticos", tabA1); add("TA1_influentes", rd(dg[, c("modelo","max_cook","cod_max_cook","lev_max_cook","cod_max_stud","max_stud")]))
add("TB1_Robustez_fiscal", tabB1); add("TB2_Robustez_composicao", tabB2)
add("TC1_Dependente_receita", tabC1); add("TC2_RCL_Cad_Composicao", tabC2); add("Auxiliares_texto", rd(aux))
saveWorkbook(wb, arq_saida, overwrite = TRUE)
cat("Arquivo gerado:", arq_saida, "\n")
