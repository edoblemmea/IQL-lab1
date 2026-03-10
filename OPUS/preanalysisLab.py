#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Mar  9 21:21:09 2026

@author: macbookairm4
"""
# qudsia code
import re
import unicodedata
import spacy
from collections import Counter
from pathlib import Path

# --- 1. CONFIGURATION ---
BASE_PATH = Path("/Users/macbookairm4/Documents/Mac Air M4/Qudsia/MIRI-CNDS/Semester2/IQL/Zipf_Lab")
INPUT_DIR = BASE_PATH / "texts"
OUTPUT_DIR = BASE_PATH / "output"

TEXT_OUT = OUTPUT_DIR / "texts_preprocessed"
DATA_OUT = OUTPUT_DIR / "data"

TEXT_OUT.mkdir(parents=True, exist_ok=True)
DATA_OUT.mkdir(parents=True, exist_ok=True)

# Set your sentence limit
N_SENTENCES = 20000

LANGUAGES = {
    "ur": {"name": "Urdu",     "family": "Indo-Iranian", "model": "xx_ent_wiki_sm"},
    "es": {"name": "Spanish",  "family": "IE-Romance",    "model": "es_core_news_sm"},
    "de": {"name": "German",   "family": "IE-Germanic",   "model": "de_core_news_sm"},
    "it": {"name": "Italian",  "family": "IE-Romance",    "model": "it_core_news_sm"},
    "nl": {"name": "Dutch",    "family": "IE-Germanic",   "model": "nl_core_news_sm"},
    "fi": {"name": "Finnish",  "family": "Uralic",        "model": "fi_core_news_sm"},
}

# --- 2. PREPROCESSING FUNCTIONS ---

def clean_text(text):
    text = re.sub(r'https?://\S+|www\.\S+', '', text) 
    text = re.sub(r'\d+', '', text)                   
    text = re.sub(r'\s+', ' ', text).strip()          
    return text

def is_valid_token(token, iso_code):
    if not any(unicodedata.category(c).startswith("L") for c in token):
        return False
    if len(token) < 2:
        return False
    if iso_code == "ur":
        return bool(re.search(r'[\u0600-\u06FF]', token))
    return True

def process_language(iso_code, info):
    print(f"--- Processing {info['name']} ({iso_code}) ---")
    input_path = INPUT_DIR / f"{iso_code}.txt"
    
    if not input_path.exists():
        print(f"  [!] Error: {input_path} not found.")
        return None

    # Load spaCy Model
    try:
        nlp = spacy.load(info['model'], disable=["parser", "ner", "lemmatizer"])
    except OSError:
        nlp = spacy.blank(iso_code)

    # --- NEW: Read only N_SENTENCES ---
    lines_to_process = []
    with open(input_path, 'r', encoding='utf-8') as f:
        for i, line in enumerate(f):
            if i >= N_SENTENCES:
                break
            clean_line = clean_text(line)
            if clean_line:
                lines_to_process.append(clean_line)
    
    full_text = " ".join(lines_to_process)
    
    # Update length limit (now much smaller due to N_SENTENCES)
    nlp.max_length = len(full_text) + 1000
    
    # Save cleaned text
    (TEXT_OUT / f"{iso_code}_clean.txt").write_text(full_text, encoding="utf-8")

    # Tokenize
    doc = nlp(full_text.lower())
    tokens = [t.text for t in doc if is_valid_token(t.text, iso_code)]
    
    # Frequency table
    counts = Counter(tokens)
    
    output_file = DATA_OUT / f"{iso_code}.txt"
    with open(output_file, "w", encoding="utf-8") as f:
        f.write("word_form\tfrequency\tlength\n")
        for word, freq in counts.most_common():
            f.write(f"{word}\t{freq}\t{len(word)}\n")
    
    print(f"  Success: Processed {len(lines_to_process)} lines. Saved {len(counts)} types.")
    return {"iso": iso_code, "tokens": len(tokens), "types": len(counts)}

# --- 3. MAIN EXECUTION ---
if __name__ == "__main__":
    for iso, info in LANGUAGES.items():
        process_language(iso, info)
    print("\nProcessing complete. Check your 'output/data' folder.")


#%%

import re
import unicodedata
import spacy
from collections import Counter
from pathlib import Path

# --- 1. CONFIGURATION ---
BASE_PATH = Path("/Users/macbookairm4/Documents/Mac Air M4/Qudsia/MIRI-CNDS/Semester2/IQL/Zipf_Lab")
INPUT_DIR = BASE_PATH / "texts"
OUTPUT_DIR = BASE_PATH / "output_2"


# Required folders per lab instructions
TEXT_OUTPUT_DIR  = Path("texts_preprocessed")   # clean texts
TSV_OUTPUT_DIR   = Path("output_preprocessed")  # frequency TSVs
TEXT_OUT.mkdir(parents=True, exist_ok=True)
DATA_OUT.mkdir(parents=True, exist_ok=True)

N_SENTENCES = 15000

# Metadata headers to filter out
JUSTIFICATION_HEADERS = {"justification", "begründung", "justificación", "justificazione", "perustelut"}
AMENDMENT_HEADERS = {"enmienda", "emenda", "emendement", "amendment", "tarkistus"}

LANGUAGES = {
    "ur": {"name": "Urdu",     "family": "Indo-Iranian", "model": "xx_ent_wiki_sm"},
    "es": {"name": "Spanish",  "family": "IE-Romance",    "model": "es_core_news_sm"},
    "de": {"name": "German",   "family": "IE-Germanic",   "model": "de_core_news_sm"},
    "it": {"name": "Italian",  "family": "IE-Romance",    "model": "it_core_news_sm"},
    "nl": {"name": "Dutch",    "family": "IE-Germanic",   "model": "nl_core_news_sm"},
    "fi": {"name": "Finnish",  "family": "Uralic",        "model": "fi_core_news_sm"},
}

# --- 2. ADVANCED CLEANING LOGIC ---

def is_metadata_line(line):
    """Detects legislative/procedural lines that are not natural language."""
    line = line.strip()
    if len(line) < 4: return True
    if line.isupper() and len(line.split()) <= 3: return True
    if re.match(r'^(A\d|B\d|PE\s|COM|DO\s)', line): return True
    return False

def clean_line(line):
    """Deep cleaning of URLs, codes, and symbols."""
    line = line.replace('/', ' ')
    line = re.sub(r'https?://\S+|www\.\S+', '', line)
    line = re.sub(r'\b[A-Z0-9_]+\([0-9]+\)[0-9]+\b', '', line) # Legislative codes
    line = re.sub(r'\b(COD|AVC|CNS|APP|INI|BUD)\b', '', line)  # Procedural codes
    line = re.sub(r'\d+', '', line)                            # Remove numbers
    line = re.sub(r'\*+', '', line)                           # Remove symbols
    return re.sub(r'\s+', ' ', line).strip()

def is_valid_token(token, iso_code):
    """Filters for linguistic word forms."""
    if not any(unicodedata.category(c).startswith("L") for c in token):
        return False
    if len(token) < 2: return False
    if iso_code == "ur" and not re.search(r'[\u0600-\u06FF]', token):
        return False
    return True

# --- 3. PROCESSING PIPELINE ---

def process_language(iso_code, info):
    print(f"--- Processing {info['name']} ({iso_code}) ---")
    input_path = INPUT_DIR / f"{iso_code}.txt"
    
    if not input_path.exists():
        print(f"  [!] Skipping: {input_path} not found.")
        return

    # Load Model
    try:
        nlp = spacy.load(info['model'], disable=["parser", "ner", "lemmatizer"])
    except:
        nlp = spacy.blank(iso_code)

    # 1. Truncate and Filter Metadata
    lines = []
    with open(input_path, 'r', encoding='utf-8') as f:
        count = 0
        for line in f:
            if count >= N_SENTENCES: break
            if not is_metadata_line(line):
                cleaned = clean_line(line)
                if len(cleaned) > 3:
                    lines.append(cleaned)
                    count += 1
    
    full_text = " ".join(lines).lower()
    nlp.max_length = len(full_text) + 1000

    # 2. Save preprocessed text for the 'text' folder requirement
    (TEXT_OUT / f"{iso_code}_preprocessed.txt").write_text(full_text, encoding="utf-8")

    # 3. Tokenize and Compute Frequencies
    doc = nlp(full_text)
    tokens = [t.text for t in doc if is_valid_token(t.text, iso_code)]
    counts = Counter(tokens)
    
    # 4. Save TSV file in required 3-column format
    output_file = DATA_OUT / f"{iso_code}.tsv"
    with open(output_file, "w", encoding="utf-8") as f:
        f.write("word_form\tfrequency\tlength\n")
        for word, freq in counts.most_common():
            f.write(f"{word}\t{freq}\t{len(word)}\n")
    
    print(f"  Success: Generated {output_file.name} ({len(counts)} unique types)")

if __name__ == "__main__":
    for iso, info in LANGUAGES.items():
        process_language(iso, info)