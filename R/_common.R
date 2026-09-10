suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(stringr)
  library(ggplot2)
  library(knitr)
  library(kableExtra)
  library(nortest)
  library(scales)
})

knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  fig.align = "center",
  fig.width = 6,
  fig.height = 4
)

# Paleta academica: azul acero como color principal, con acentos para
# categorias secundarias y un rojo terracota reservado siempre para
# "Missing" / "Faltante", de forma que ese significado sea consistente
# en todas las graficas del documento.
eda_colors <- list(
  primary = "#2E6E8E",
  secondary = "#5FA8A0",
  tertiary = "#E8A33D",
  quaternary = "#8C6BB1",
  missing = "#B24C4C",
  text = "#2B2B2B",
  grid = "#E3E3E3"
)

eda_qual_palette <- unname(c(
  eda_colors$primary, eda_colors$secondary, eda_colors$tertiary, eda_colors$quaternary
))

# Asigna colores a los niveles de una variable categorica, reservando
# siempre el mismo rojo terracota para el nivel "Missing" si existe.
eda_fill_for_levels <- function(lvls) {
  non_missing <- setdiff(lvls, "Missing")
  cols <- stats::setNames(eda_qual_palette[seq_along(non_missing)], non_missing)
  if ("Missing" %in% lvls) cols <- c(cols, Missing = eda_colors$missing)
  cols[lvls]
}

theme_eda <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(color = eda_colors$text),
      plot.title = element_text(face = "bold", size = rel(1.05), margin = margin(b = 8)),
      plot.subtitle = element_text(color = "#5A5A5A", margin = margin(b = 10)),
      axis.title = element_text(face = "bold", size = rel(0.95)),
      axis.text = element_text(color = "#3A3A3A"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = eda_colors$grid, linewidth = 0.4),
      axis.line.x = element_line(color = "#B5B5B5", linewidth = 0.4),
      axis.ticks = element_blank(),
      legend.position = "none",
      plot.caption = element_text(color = "#8A8A8A", size = rel(0.75))
    )
}

# Trata cadenas vacias como categoria "Missing" explicita
recode_missing <- function(x) {
  x <- as.character(x)
  dplyr::if_else(is.na(x) | x == "", "Missing", x)
}

# La ruta real del archivo de datos no se versiona (ver .Renviron.example):
# cada quien la define en su propio .Renviron local (gitignorado).
eda_data_path <- Sys.getenv("DATA_PATH", unset = "")
if (eda_data_path == "") {
  stop(
    "No se encontro la variable de entorno DATA_PATH.\n",
    "Copia .Renviron.example a .Renviron en la raiz del proyecto, completa la ",
    "ruta real del archivo de datos, y reinicia la sesion de R."
  )
}
if (!file.exists(eda_data_path)) {
  stop(sprintf(
    "DATA_PATH apunta a '%s', pero ese archivo no existe.",
    eda_data_path
  ))
}

df_raw <- readr::read_csv(eda_data_path, show_col_types = FALSE)

df <- df_raw %>%
  mutate(
    Gender = factor(recode_missing(Gender), levels = c("Female", "Male", "Missing")),
    Ethnicity = factor(recode_missing(Ethnicity),
      levels = c("Mestizo", "Caucasian", "Afrodescendant", "Other", "Missing")
    ),
    Grade = factor(recode_missing(Grade), levels = c("9", "10", "11", "Missing")),
    SSE = factor(recode_missing(SSE), levels = c("Low", "Medium", "High", "Missing")),
    Education = factor(recode_missing(Education),
      levels = c("Primary", "High School", "Undergraduate", "Graduate", "Missing")
    ),
    Municipality = factor(Municipality,
      levels = c("Santo Tomas", "Polo Nuevo", "Puerto Colombia", "Suan")
    )
  )

# Tabla de frecuencia + proporcion para una variable categorica
freq_table <- function(data, var, caption = NULL) {
  data %>%
    count(.data[[var]], .drop = FALSE, name = "Frecuencia") %>%
    mutate(`Proporcion (%)` = round(100 * Frecuencia / sum(Frecuencia), 2)) %>%
    rename(Categoria = 1) %>%
    kable(caption = caption, col.names = c("Categoria", "Frecuencia", "Proporcion (%)")) %>%
    kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
}

# Barras de participantes (%) por categoria, con N sobre cada barra
plot_categorical <- function(data, var, xlab) {
  plot_df <- data %>%
    count(.data[[var]], .drop = FALSE) %>%
    mutate(pct = 100 * n / sum(n))

  cols <- eda_fill_for_levels(levels(data[[var]]))

  ggplot(plot_df, aes(x = .data[[var]], y = pct, fill = .data[[var]])) +
    geom_col(color = "white", linewidth = 0.3, width = 0.65) +
    geom_text(aes(label = n), vjust = -0.6, fontface = "bold", color = eda_colors$text, size = 3.6) +
    scale_fill_manual(values = cols) +
    scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1))) +
    labs(x = xlab, y = "Participantes (%)") +
    theme_eda()
}

# Barras Completo / Faltante para una variable continua
plot_missingness <- function(data, var, xlab = "Estado del dato") {
  plot_df <- tibble(
    status = factor(c("Completo", "Faltante"), levels = c("Completo", "Faltante")),
    n = c(sum(!is.na(data[[var]])), sum(is.na(data[[var]])))
  ) %>%
    mutate(pct = 100 * n / sum(n))

  ggplot(plot_df, aes(x = status, y = pct, fill = status)) +
    geom_col(color = "white", linewidth = 0.3, width = 0.55, show.legend = FALSE) +
    geom_text(aes(label = n), vjust = -0.6, fontface = "bold", color = eda_colors$text, size = 3.6) +
    scale_fill_manual(values = c(Completo = eda_colors$primary, Faltante = eda_colors$missing)) +
    scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1))) +
    labs(x = xlab, y = "Participantes (%)") +
    theme_eda()
}

# Histograma con test de normalidad de Lilliefors anotado
plot_histogram <- function(data, var, xlab, binwidth = NULL) {
  x <- data[[var]]
  x <- x[!is.na(x)]
  lillie <- nortest::lillie.test(x)
  stars <- dplyr::case_when(
    lillie$p.value < 0.001 ~ "***",
    lillie$p.value < 0.01 ~ "**",
    lillie$p.value < 0.05 ~ "*",
    TRUE ~ ""
  )
  stat_label <- sprintf("D = %.3f%s", lillie$statistic, stars)

  ggplot(data.frame(x = x), aes(x = x)) +
    geom_histogram(binwidth = binwidth, fill = eda_colors$primary, color = "white",
                    alpha = 0.9, linewidth = 0.2) +
    annotate("label", x = Inf, y = Inf, label = stat_label, hjust = 1.08, vjust = 1.4,
             fontface = "bold", color = eda_colors$text, fill = "white", label.size = 0,
             size = 3.4) +
    labs(x = xlab, y = "Frecuencia") +
    theme_eda()
}

# Boxplot con umbrales de Hampel (Mediana +/- 3*MAD) y la media marcada
plot_boxplot_hampel <- function(data, var, ylab) {
  x <- data[[var]]
  x <- x[!is.na(x)]
  med <- median(x)
  mad_val <- mad(x)
  lower <- med - 3 * mad_val
  upper <- med + 3 * mad_val

  ggplot(data.frame(x = x), aes(x = "", y = x)) +
    geom_hline(yintercept = c(lower, upper), linetype = "dashed",
               color = eda_colors$missing, linewidth = 0.4) +
    geom_boxplot(fill = eda_colors$primary, alpha = 0.25, color = eda_colors$primary,
                 width = 0.3, outlier.color = eda_colors$missing) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = eda_colors$tertiary) +
    labs(x = NULL, y = ylab) +
    theme_eda() +
    theme(axis.text.x = element_blank())
}

# Mapa de calor de correlaciones (reemplaza corrplot para mantener todo en ggplot2)
plot_corr_heatmap <- function(data, vars) {
  cor_mat <- cor(data[vars], use = "pairwise.complete.obs")
  cor_df <- as.data.frame(as.table(cor_mat))
  names(cor_df) <- c("Var1", "Var2", "r")

  idx1 <- match(as.character(cor_df$Var1), vars)
  idx2 <- match(as.character(cor_df$Var2), vars)
  cor_df <- cor_df[idx1 < idx2, ]
  cor_df$Var1 <- factor(cor_df$Var1, levels = vars)
  cor_df$Var2 <- factor(cor_df$Var2, levels = rev(vars))

  ggplot(cor_df, aes(Var1, Var2, fill = r)) +
    geom_tile(color = "white", linewidth = 1.2) +
    geom_text(aes(label = sprintf("%.2f", r)), color = eda_colors$text,
              fontface = "bold", size = 4) +
    scale_fill_gradient2(
      low = eda_colors$missing, mid = "white", high = eda_colors$primary,
      midpoint = 0, limits = c(-1, 1), name = "r"
    ) +
    coord_fixed() +
    labs(x = NULL, y = NULL) +
    theme_eda() +
    theme(
      panel.grid = element_blank(),
      legend.position = "right"
    )
}

# Resumen n / media / DE / mediana / min / max para una variable continua
summary_stats <- function(data, var) {
  x <- data[[var]]
  tibble(
    n = sum(!is.na(x)),
    faltantes = sum(is.na(x)),
    media = mean(x, na.rm = TRUE),
    de = sd(x, na.rm = TRUE),
    mediana = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
}
