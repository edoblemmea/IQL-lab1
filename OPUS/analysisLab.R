# 1. LOAD LIBRARIES
library(tidyverse)
library(ggrepel)
library(scales)

# 2. CONFIGURATION
# Set the base path to your Lab folder
BASE_DIR <- "/Users/macbookairm4/Documents/Mac Air M4/Qudsia/MIRI-CNDS/Semester2/IQL/Zipf_Lab"
setwd(BASE_DIR)

# Matches the TSV_OUTPUT_DIR from your preprocessing.ipynb
DATA_DIR  <- "output_preprocessed" 
PLOT_DIR  <- "output/plots_r"
TABLE_DIR <- "output/tables_r"

dir.create(PLOT_DIR,  showWarnings = FALSE)
dir.create(TABLE_DIR, showWarnings = FALSE)

# Language Metadata
LANG_META <- tribble(
  ~iso,  ~language,    ~family,
  "en",  "English",    "IE-Germanic",
  "el",  "Greek",      "IE-Hellenic",
  "it",  "Italian",    "IE-Romance",
  "es",  "Spanish",    "IE-Romance",
  "pl",  "Polish",     "IE-Slavic",
  "mt",  "Maltese",    "Semitic",
  "fi",  "Finnish",    "Uralic",
  "hu",  "Hungarian",  "Uralic",
  "ur",  "Urdu",       "Indo-Iranian",
  "de",  "German",     "IE-Germanic",
  "nl",  "Dutch",      "IE-Germanic"
)

# 3. ANALYSIS FUNCTION (Kendall Tau, Baselines & Optimality)
calculate_zipf_stats <- function(df) {
  N <- sum(df$frequency)
  L <- sum(df$frequency * df$length) / N
  
  # Lmin: Rank-Ordering Baseline (Shortest lengths to highest frequencies)
  df_sorted <- df %>% arrange(desc(frequency))
  sorted_lengths <- sort(df$length)
  L_min <- sum(df_sorted$frequency * sorted_lengths) / N
  
  # Lr: Random baseline
  L_r <- mean(df$length)
  
  # Kendall Tau Correlation
  kt <- cor.test(df$length, df$frequency, method = "kendall")
  
  tibble(
    tokens = N,
    types  = nrow(df),
    tau    = kt$estimate,
    p_val  = kt$p.value,
    L      = L,
    L_min  = L_min,
    L_r    = L_r,
    eta    = L_min / L,
    omega  = (L_r - L) / (L_r - L_min)
  )
}

# 4. DATA LOADING PIPELINE (Updated to match Python "_freq.tsv" naming)
corpus_raw <- LANG_META$iso %>%
  map(~ {
    # Matches the filename pattern in preprocessing.ipynb
    path <- file.path(DATA_DIR, paste0(.x, "_freq.tsv"))
    
    if (!file.exists(path)) {
      message(paste("Skipping missing file:", path))
      return(NULL)
    }
    
    read_tsv(path, col_types = cols(
      word_form = col_character(),
      frequency = col_double(),
      length    = col_double()
    ), quote = "") %>%
      mutate(iso = .x)
  }) %>%
  keep(~ !is.null(.x)) %>%
  list_rbind() %>%
  left_join(LANG_META, by = "iso")

# 5. GENERATE STATISTICAL SUMMARY
summary_results <- corpus_raw %>%
  group_by(iso, language, family) %>%
  group_modify(~ calculate_zipf_stats(.x)) %>%
  ungroup() %>%
  mutate(p_val_corrected = p.adjust(p_val, method = "holm"))

# 6. VISUALIZATION

# --- FIG 1: MULTIPANEL ZIPF PLOT ---
p1 <- ggplot(corpus_raw, aes(x = frequency, y = length)) +
  geom_point(alpha = 0.15, color = "steelblue") +
  scale_x_log10(labels = label_number(scale_cut = cut_short_scale())) +
  facet_wrap(~language, ncol = 2, scales = "free_x") +
  geom_text_repel(data = corpus_raw %>% group_by(language) %>% slice_max(frequency, n = 3),
                  aes(label = word_form), color = "red", size = 3) +
  theme_minimal() +
  labs(title = "Fig 1: Word Length vs Frequency", x = "Frequency (log10)", y = "Length")

# --- FIG 2: COMPRESSION ANALYSIS (MATCHING SAMPLE STYLE) ---

p2 <- ggplot(summary_results, aes(y = L_r)) +
  # Horizontal lines connecting L to Lmin
  geom_segment(aes(x = L, xend = L_min, yend = L_r, color = family), linewidth = 1) +
  # Actual Mean Length (Dot)
  geom_point(aes(x = L, color = family), size = 4) +
  # Minimum Baseline (Cross)
  geom_point(aes(x = L_min, color = family), shape = 4, size = 4, stroke = 1.5) +
  # Language labels
  geom_text(aes(x = L, label = language, color = family), vjust = 1.8, fontface = "bold", show.legend = FALSE) +
  # Identity line L = Lr
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  theme_bw() +
  labs(title = "Degree of compression across languages", 
       x = "Mean word length L", 
       y = "Random baseline Lr", 
       color = "Language family") +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16))

# 7. SAVE OUTPUTS
ggsave(file.path(PLOT_DIR, "Fig1_zipf.png"), p1, width = 10, height = 12)
ggsave(file.path(PLOT_DIR, "Fig2_compression.png"), p2, width = 10, height = 8)
write_csv(summary_results, file.path(TABLE_DIR, "summary_results.csv"))

# 8. CREATE LATEX TABLE FILE [table1_zipf.tex]
latex_file <- file.path(TABLE_DIR, "table1_zipf.tex")
sink(latex_file)
cat("\\begin{table}[!h]\n\\centering\n\\caption{Results of Zipf's law of abbreviation analysis.}\n\\centering\n")
cat("\\begin{tabular}[t]{lllrrll}\n\\toprule\nLanguage & Family & Tokens & Types & $\\tau$ & $p$ & $p_{Holm}\\\\\n\\midrule\n")
for(i in 1:nrow(summary_results)) {
  row <- summary_results[i,]
  cat(sprintf("%s & %s & %s & %d & %.3f & %.2e & %.2e \\\\\n", 
              row$language, row$family, format(row$tokens, big.mark=","), 
              row$types, row$tau, row$p_val, row$p_val_corrected))
}
cat("\\bottomrule\n\\end{tabular}\n\\end{table}")
sink()

print(paste("Completed! LaTeX table created at:", latex_file))
