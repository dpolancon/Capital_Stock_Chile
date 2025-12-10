############################################################
## 06_audit_sfc.R — Auditor del panel SFC (Ig, Kg, Kn)
## Proyecto: Reconstrucción SFC del acervo de capital (Chile)
##
## Tareas:
##  - Cargar panel_sfc_2003.rds
##  - Revisar estructura, NAs, panel rectangular
##  - Verificar identidad SFC:
##        (D - R) ?= (dKg - dKn)
##  - Resumir residuales eps_joint y r_joint_*
##  - Generar gráficos de diagnóstico vía plot_sfc_diagnostics()
############################################################

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(here)

dir_int <- here("data","interim")
dir_fig <- here("outputs","figures","sfc")

if (!dir.exists(dir_fig)) dir.create(dir_fig, recursive = TRUE)

panel_file <- file.path(dir_int, "panel_sfc_2003.rds")

if (!file.exists(panel_file)) {
  stop("panel_sfc_2003.rds no encontrado en data/interim/. Ejecuta 06_sfc_engine.R primero.")
}

panel_sfc <- readRDS(panel_file)

cat("\n==============================\n")
cat("   AUDITORÍA 06: panel_sfc_2003\n")
cat("==============================\n\n")

############################################################
## 1. Estructura básica
############################################################

cat("Filas:", nrow(panel_sfc), "\n")
cat("Años :", min(panel_sfc$year), "→", max(panel_sfc$year), "\n")

assets <- sort(unique(panel_sfc$asset))
cat("Activos:", paste(assets, collapse = ", "), "\n\n")

req_cols <- c(
  "year","asset",
  "Ig","Kg","Kn",
  "dKg","dKn","D","R",
  "delta","z",
  "eps_joint","r_joint_Ig","r_joint_Kg","r_joint_Kn",
  "period","full_SFC_window","full_ME_window"
)

missing <- setdiff(req_cols, names(panel_sfc))
if (length(missing) == 0) {
  cat("✔ Todas las columnas requeridas están presentes.\n\n")
} else {
  cat("❌ Faltan columnas en panel_sfc_2003:\n")
  print(missing)
  stop("Estructura incompleta, revisar 06_sfc_engine.R.")
}

############################################################
## 2. NA y estructura numérica básica
############################################################

num_vars <- c(
  "Ig","Kg","Kn",
  "dKg","dKn","D","R",
  "delta","z",
  "eps_joint","r_joint_Ig","r_joint_Kg","r_joint_Kn"
)

na_summary <- panel_sfc %>%
  summarise(
    across(all_of(num_vars), ~ sum(is.na(.x)))
  )

cat("--- NAs por variable numérica ---\n")
print(na_summary)
cat("\n")

if (any(na_summary > 0)) {
  cat("⚠ WARNING: existen NAs en variables numéricas (esperables en el primer año de cada activo por derivadas).\n\n")
} else {
  cat("✔ Sin NAs en variables numéricas (excepto posiblemente filas no definidas por diseño).\n\n")
}

############################################################
## 3. Panel rectangular por (year, asset)
############################################################

panel_test <- panel_sfc %>%
  count(year, asset) %>%
  tidyr::pivot_wider(names_from = asset, values_from = n, values_fill = 0)

incomplete_years <- panel_test %>%
  filter(dplyr::if_any(-year, ~ .x == 0)) %>%
  pull(year)

if (length(incomplete_years) == 0) {
  cat("✔ Panel rectangular completo (todos los activos presentes en todos los años).\n\n")
} else {
  cat("⚠ WARNING: años con activos faltantes:\n")
  print(incomplete_years)
  cat("\n")
}

############################################################
## 4. Identidad SFC: (D - R) vs (dKg - dKn)
############################################################

cat("--- Chequeo identidad SFC: (D - R) vs (dKg - dKn) ---\n")

sfc_check <- panel_sfc %>%
  mutate(
    diff_SFC = (D - R) - (dKg - dKn)
  ) %>%
  summarise(
    max_abs_diff   = max(abs(diff_SFC), na.rm = TRUE),
    max_abs_eps    = max(abs(eps_joint), na.rm = TRUE),
    max_abs_diff_e = max(abs(diff_SFC - eps_joint), na.rm = TRUE)
  )

print(sfc_check)
cat("\n")

if (sfc_check$max_abs_diff_e < 1e-9) {
  cat("✔ eps_joint coincide numéricamente con (D - R) - (dKg - dKn).\n")
} else {
  cat("❌ eps_joint NO coincide con la identidad SFC calculada. Revisar 06_sfc_engine.R.\n")
}

if (sfc_check$max_abs_diff < 1e-6) {
  cat("✔ Violaciones de identidad SFC numéricamente despreciables.\n\n")
} else {
  cat("⚠ WARNING: violaciones de identidad SFC NO triviales.\n\n")
}

############################################################
## 5. Resumen de residuales normalizados
############################################################

cat("--- Resumen residuales normalizados (full_SFC_window) ---\n")

resid_summary <- panel_sfc %>%
  filter(full_SFC_window) %>%
  summarise(
    max_abs_r_Ig = max(abs(r_joint_Ig), na.rm = TRUE),
    max_abs_r_Kg = max(abs(r_joint_Kg), na.rm = TRUE),
    max_abs_r_Kn = max(abs(r_joint_Kn), na.rm = TRUE)
  )

print(resid_summary)
cat("\n")

############################################################
## 6. Diagnóstico de tasas delta y z
############################################################

cat("--- Rango de tasas implícitas delta (D/Kn) y z (R/Kg) ---\n")

rate_summary <- panel_sfc %>%
  filter(full_SFC_window) %>%
  summarise(
    delta_min = min(delta, na.rm = TRUE),
    delta_max = max(delta, na.rm = TRUE),
    z_min     = min(z,     na.rm = TRUE),
    z_max     = max(z,     na.rm = TRUE)
  )

print(rate_summary)
cat("\n")

############################################################
## 7. Función de gráficos: plot_sfc_diagnostics()
############################################################

plot_sfc_diagnostics <- function(df, out_dir = NULL) {
  ## df: panel_sfc
  ## out_dir: carpeta para guardar png (opcional)
  
  # 1) Trayectorias de Ig, Kg, Kn por activo (ventana SFC)
  df_levels <- df %>%
    filter(full_SFC_window) %>%
    select(year, asset, Ig, Kg, Kn) %>%
    pivot_longer(cols = c(Ig, Kg, Kn),
                 names_to = "stock_type",
                 values_to = "value")
  
  p_levels <- ggplot(df_levels, aes(x = year, y = value, colour = stock_type)) +
    geom_line() +
    facet_wrap(~ asset, scales = "free_y") +
    labs(
      title = "Ig, Kg, Kn (2003 CLP) — Ventana SFC 1950–1994",
      x = "Año",
      y = "Miles de millones de pesos 2003",
      colour = "Variable"
    ) +
    theme_minimal()
  
  print(p_levels)
  
  if (!is.null(out_dir)) {
    ggsave(
      filename = file.path(out_dir, "sfc_levels_Ig_Kg_Kn.png"),
      plot     = p_levels,
      width    = 10, height = 6
    )
  }
  
  # 2) Tasas delta y z por activo
  df_rates <- df %>%
    filter(full_SFC_window) %>%
    select(year, asset, delta, z) %>%
    pivot_longer(cols = c(delta, z),
                 names_to = "rate_type",
                 values_to = "value")
  
  p_rates <- ggplot(df_rates, aes(x = year, y = value, colour = rate_type)) +
    geom_line() +
    facet_wrap(~ asset, scales = "free_y") +
    labs(
      title = "Tasas implícitas de depreciación (delta) y retiro (z)",
      x = "Año",
      y = "Tasa anual",
      colour = "Tasa"
    ) +
    theme_minimal()
  
  print(p_rates)
  
  if (!is.null(out_dir)) {
    ggsave(
      filename = file.path(out_dir, "sfc_rates_delta_z.png"),
      plot     = p_rates,
      width    = 10, height = 6
    )
  }
  
  # 3) Residuo SFC eps_joint por activo
  df_eps <- df %>%
    filter(full_SFC_window) %>%
    select(year, asset, eps_joint)
  
  p_eps <- ggplot(df_eps, aes(x = year, y = eps_joint)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line() +
    facet_wrap(~ asset, scales = "free_y") +
    labs(
      title = "Residuo SFC eps_joint (D−R − (dKg−dKn))",
      x = "Año",
      y = "eps_joint (2003 CLP)"
    ) +
    theme_minimal()
  
  print(p_eps)
  
  if (!is.null(out_dir)) {
    ggsave(
      filename = file.path(out_dir, "sfc_residual_eps_joint.png"),
      plot     = p_eps,
      width    = 10, height = 6
    )
  }
  
  invisible(list(
    levels_plot = p_levels,
    rates_plot  = p_rates,
    eps_plot    = p_eps
  ))
}

############################################################
## 8. Ejecutar gráficos de diagnóstico
############################################################

cat("--- Generando gráficos de diagnóstico SFC (ventana 1950–1994) ---\n")
plot_sfc_diagnostics(panel_sfc, out_dir = dir_fig)
cat("Gráficos guardados en:\n", dir_fig, "\n")

############################################################
## 9. Final
############################################################

cat("\n>>> AUDITORÍA 06 COMPLETADA <<<\n")
############################################################
