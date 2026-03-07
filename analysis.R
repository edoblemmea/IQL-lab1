#Libraries
library(tidyverse)
library(ggrepel)
library(scales)
library(patchwork)

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
  path <- file.path(DATA_DIR, paste0(iso, ".txt"))
  read_tsv(path, col_types = cols(
    word_form = col_character(),
    frequency = col_double(),
    length    = col_double()
  )) |>
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