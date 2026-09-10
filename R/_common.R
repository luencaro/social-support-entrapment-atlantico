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

theme_eda <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line = element_line(color = "black"),
      plot.title = element_text(face = "bold")
    )
}

# Trata cadenas vacias como categoria "Missing" explicita
recode_missing <- function(x) {
  x <- as.character(x)
  dplyr::if_else(is.na(x) | x == "", "Missing", x)
}

df_raw <- readr::read_csv("data/df_moderation_2026.csv", show_col_types = FALSE)

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
plot_categorical <- function(data, var, xlab, fill = "grey80") {
  plot_df <- data %>%
    count(.data[[var]], .drop = FALSE) %>%
    mutate(pct = 100 * n / sum(n))

  ggplot(plot_df, aes(x = .data[[var]], y = pct)) +
    geom_col(fill = fill, color = "black", width = 0.6) +
    geom_text(aes(label = n), vjust = -0.5, fontface = "bold") +
    scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.08))) +
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
    geom_col(color = "black", width = 0.5, show.legend = FALSE) +
    geom_text(aes(label = n), vjust = -0.5, fontface = "bold") +
    scale_fill_manual(values = c("Completo" = "grey85", "Faltante" = "black")) +
    scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.08))) +
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
    geom_histogram(binwidth = binwidth, fill = "grey90", color = "black") +
    annotate("text", x = Inf, y = Inf, label = stat_label, hjust = 1.1, vjust = 1.5, fontface = "bold") +
    labs(x = xlab, y = "Frecuencia") +
    theme_eda()
}

# Boxplot con umbrales de Hampel (Mediana +/- 3*MAD)
plot_boxplot_hampel <- function(data, var, ylab) {
  x <- data[[var]]
  x <- x[!is.na(x)]
  med <- median(x)
  mad_val <- mad(x)
  lower <- med - 3 * mad_val
  upper <- med + 3 * mad_val

  ggplot(data.frame(x = x), aes(x = "", y = x)) +
    geom_boxplot(fill = "grey90", width = 0.3) +
    geom_hline(yintercept = c(lower, upper), linetype = "dashed") +
    labs(x = NULL, y = ylab) +
    theme_eda() +
    theme(axis.text.x = element_blank())
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
