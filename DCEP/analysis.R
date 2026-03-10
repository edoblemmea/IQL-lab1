#Libraries
library(tidyverse)
library(ggrepel)
library(scales)
library(patchwork)
library(knitr)
library(kableExtra)

#config

setwd("C:/Users/emman/Documents/MASTER/IQL/IQL Lab 1")

DATA_DIR  <- "data"
PLOT_DIR  <- "plots_r"
TABLE_DIR <- "tables_r"

dir.create(PLOT_DIR,  showWarnings = FALSE)
dir.create(TABLE_DIR, showWarnings = FALSE)

LANG_META <- tribble(
  ~iso,  ~language,    ~family,
  "el",  "Greek",      "IE-Hellenic",
  "en",  "English",    "IE-Germanic",
  "es",  "Spanish",    "IE-Romance",
  "fi",  "Finnish",    "Uralic",
  "hu",  "Hungarian",  "Uralic",
  "it",  "Italian",    "IE-Romance",
  "mt",  "Maltese",    "Semitic",
  "pl",  "Polish",     "IE-Slavic"
)

#data loading
load_language <- function(iso) {
  path <- file.path("output_preprocessed", paste0(iso, "_freq.tsv"))
  read_tsv(
    path,
    col_types = cols(
      word_form = col_character(),
      frequency = col_double(),
      length    = col_double()
    ),
    locale = locale(encoding = "UTF-8"),
    quote  = ""
  ) |>
    mutate(iso = iso)
}

corpus <- LANG_META |>
  pull(iso) |>
  map(load_language) |>
  list_rbind() |>
  left_join(LANG_META, by = "iso")

corpus |>
  group_by(iso, language) |>
  summarise(
    types  = n(),
    tokens = sum(frequency),
    .groups = "drop"
  ) |>
  print()

#Kendall test
kendall_test <- function(df) {
  test <- cor.test(
    df$length,
    df$frequency,
    method      = "kendall",
    alternative = "less",    # longer words tend to be less frequent
    exact       = FALSE
  )
  tibble(
    tau     = test$estimate,
    p_raw   = test$p.value
  )
}

zipf_results <- corpus |>
  group_by(iso, language, family) |>
  summarise(
    tokens = sum(frequency),
    types  = n(),
    kendall_test(pick(everything())),
    .groups = "drop"
  ) |>
  mutate(p_holm = p.adjust(p_raw, method = "holm"))

zipf_results |> print()

#Compression metrics
compression_metrics <- function(df) {
  
  # Probability of each word type
  p <- df$frequency / sum(df$frequency)
  l <- df$length
  
  # L: actual mean word length
  L <- sum(p * l)
  
  # Lr: random baseline
  Lr <- mean(l)
  
  # Lmin
  l_sorted <- sort(l)                    # lengths sorted ascending
  p_sorted <- sort(p, decreasing = TRUE) # probabilities sorted descending
  Lmin <- sum(p_sorted * l_sorted)
  
  # eta: absolute optimality
  eta <- Lmin / L
  
  # Omega: relative optimality
  omega <- (Lr - L) / (Lr - Lmin)
  
  tibble(Lmin = Lmin, L = L, Lr = Lr, eta = eta, omega = omega)
}

summary_df <- corpus |>
  group_by(iso, language, family) |>
  summarise(
    compression_metrics(pick(everything())),
    .groups = "drop"
  ) |>
  left_join(zipf_results, by = c("iso", "language", "family"))

summary_df |>
  select(language, Lmin, L, Lr, eta, omega) |>
  print()

#summary.txt
summary_df |>
  select(iso, language, family, Lmin, L, Lr, eta, omega,
         tokens, types, tau, p_raw, p_holm) |>
  write_tsv("data/summary.txt")


#VISUALIZATION
#Figure 1: word length vs frequency (8 language panels)

panel_data <- corpus |>
  left_join(LANG_META, by = c("iso", "language", "family"))

label_data <- panel_data |>
  group_by(iso) |>
  slice_max(frequency, n = 15) |> #most frequent words
  ungroup()

lang_order <- c(
  "Spanish",   "Italian",
  "English",   "Greek",
  "Finnish",   "Hungarian",
  "Maltese",   "Polish"
)

panel_data <- panel_data |>
  mutate(language = factor(language, levels = lang_order))

label_data <- label_data |>
  mutate(language = factor(language, levels = lang_order))

# Plotting
fig1 <- ggplot(panel_data, aes(x = frequency, y = length)) +
  geom_jitter(alpha = 0.2, size = 0.8, color = "steelblue",
              height = 0.3, width = 0) +
  geom_smooth(method = "lm", color = "red", linewidth = 0.8,
              se = FALSE) +
  geom_text_repel(
    data         = label_data,
    aes(label    = word_form),
    size         = 2.5,
    max.overlaps = 20,
    color        = "black"
  ) +
  scale_x_log10(labels = label_comma(), limits = c(2, NA)) +  # ← aquí
  facet_wrap(~ language, nrow = 4, ncol = 2) +
  labs(
    x     = "Word frequency (log scale)",
    y     = "Word length (characters)",
    title = "Word length as a function of word frequency"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

ggsave(file.path(PLOT_DIR, "fig1_zipf_panels.pdf"),
       fig1, width = 8, height = 12)
ggsave(file.path(PLOT_DIR, "fig1_zipf_panels.png"),
       fig1, width = 8, height = 12, dpi = 300)

fig1


#Figure 2: compression plot
fig2 <- ggplot(summary_df, aes(x = L, y = Lr, color = family)) +
  
  # Diagonal reference line Lr = L (no compression)
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "gray50") +
  
  # Segment from L to Lmin — no legend
  geom_segment(aes(x = L, xend = Lmin, y = Lr, yend = Lr),
               arrow = arrow(length = unit(0.2, "cm"), ends = "last"),
               linewidth = 0.6,
               show.legend = FALSE) +
  
  # Point at observed L — shows in legend
  geom_point(size = 3, show.legend = TRUE) +
  
  # Point at Lmin — no legend
  geom_point(aes(x = Lmin, y = Lr),
             size = 3, shape = 4,
             show.legend = FALSE) +
  
  # Language labels — no legend
  geom_text_repel(aes(label = language),
                  size = 3.5, fontface = "bold",
                  show.legend = FALSE) +
  
  scale_color_manual(values = family_colors, name = "Language family") +
  
  # Force legend to show only plain circles
  guides(color = guide_legend(override.aes = list(
    shape = 16,
    size  = 3
  ))) +
  
  labs(
    x     = "Mean word length L",
    y     = "Random baseline Lr",
    title = "Degree of compression across languages"
  ) +
  theme_bw() +
  theme(
    plot.title   = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title   = element_text(size = 10),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text  = element_text(size = 8)
  )

ggsave(file.path(PLOT_DIR, "fig2_compression.pdf"),
       fig2, width = 7, height = 6)
ggsave(file.path(PLOT_DIR, "fig2_compression.png"),
       fig2, width = 7, height = 6, dpi = 300)

fig2

#TABLES
#Table 1: Zipf's law of abbreviation
table1 <- summary_df |>
  arrange(family, language) |>
  select(language, family, tokens, types, tau, p_raw, p_holm) |>
  mutate(
    tokens = formatC(tokens, format = "f", digits = 0, big.mark = ","),
    tau    = round(tau, 3),
    p_raw  = formatC(p_raw, format = "e", digits = 2),
    p_holm = formatC(p_holm, format = "e", digits = 2)
  )

# Table 2: Compression
table2 <- summary_df |>
  arrange(family, language) |>
  select(language, family, Lmin, L, Lr, eta, omega) |>
  mutate(across(c(Lmin, L, Lr, eta, omega), ~ round(.x, 3)))

# Print to console
cat("Table 1: Zipf's law of abbreviation\n")
print(table1)
cat("\nTable 2: Compression\n")
print(table2)

# Save as LaTeX
table1 |>
  kable(format = "latex", booktabs = TRUE, escape = FALSE,
        col.names = c("Language", "Family", "Tokens", "Types",
                      "$\\tau$", "$p$", "$p_{Holm}$"),
        caption = "Results of Zipf's law of abbreviation analysis.") |>
  kable_styling(latex_options = "hold_position") |>
  writeLines(file.path(TABLE_DIR, "table1_zipf.tex"))

table2 |>
  kable(format = "latex", booktabs = TRUE, escape = FALSE,
        col.names = c("Language", "Family", "$L_{min}$", "$L$", "$L_r$",
                      "$\\eta$", "$\\Omega$"),
        caption = "Results of compression analysis.") |>
  kable_styling(latex_options = "hold_position") |>
  writeLines(file.path(TABLE_DIR, "table2_compression.tex"))

cat("\nTables saved to", TABLE_DIR, "\n")

#Copy of freq files to data
dir.create("data", showWarnings = FALSE)
for (iso in LANG_META$iso) {
  file.copy(
    from      = file.path("output_preprocessed", paste0(iso, "_freq.tsv")),
    to        = file.path("data", paste0(iso, ".txt")),
    overwrite = TRUE
  )
}
cat("Frequency files copied to data/\n")