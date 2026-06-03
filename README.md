# lexiExpand <img src="https://img.shields.io/badge/version-0.2.0-blue" align="right"/>

**Automated semantic dictionary expansion for text analysis**

[![R-CMD-check](https://github.com/gvajpai/lexiExpand/workflows/R-CMD-check/badge.svg)](https://github.com/gvajpai/lexiExpand/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Dictionary-based text analysis is widely used across social science, hospitality research, and finance — but building and maintaining a good dictionary is painful. Miss one word and every document containing it is silently under-counted.

`lexiExpand` fixes this. Give it a small set of seed words; it uses pre-trained [GloVe](https://nlp.stanford.edu/projects/glove/) word embeddings to find semantically similar words across a 400 000-word vocabulary, ranks them by cosine similarity, and walks you through a word-by-word review. The result is a richer, more reproducible dictionary — no Python, no API key, no manual synonym searching.

---

## Installation

```r
# Option 1 — Install directly from GitHub
# install.packages("remotes")
remotes::install_github("gvajpai/lexiExpand")

# Option 2 — Download source tarball
# https://github.com/gvajpai/lexiExpand/releases/latest
install.packages("lexiExpand_0.2.0.tar.gz", repos = NULL, type = "source")
```

---

## Quick start

```r
library(lexiExpand)

# Step 1 — Download GloVe vectors once (~160 MB, cached after first run)
download_vectors("glove.6B.50d")

# Step 2 — Load into memory
vecs <- load_vectors("glove.6B.50d")

# Step 3 — Expand and review
result <- expand_dict(c("sad", "angry"), vectors = vecs)

# Step 4 — Export
export_dict(result, name = "negative_emotion", format = "list")
```

---

## Usage examples

### Example 1 — Single seed word

Find words semantically similar to a single term. Returns a ranked data frame.

```r
library(lexiExpand)

vecs <- load_vectors("glove.6B.50d")

candidates <- expand_dict(
  seed        = "aroma",
  n           = 10,           # return top 10 candidates
  threshold   = 0.70,         # minimum cosine similarity
  vectors     = vecs,
  interactive = FALSE         # return data frame, no wizard
)

print(candidates)
#>       word similarity  seed pct_match
#> 1    scent      0.841 aroma       84%
#> 2 fragrant      0.823 aroma       82%
#> 3  perfume      0.809 aroma       81%
#> 4  bouquet      0.796 aroma       80%
#> 5    smell      0.781 aroma       78%
#> 6   savory      0.774 aroma       77%
#> 7  flavour      0.768 aroma       77%
#> 8    spicy      0.759 aroma       76%
#> 9   herbal      0.751 aroma       75%
#> 10  earthy      0.743 aroma       74%
```

Export the results to a named list:

```r
export_dict(candidates, seed = "aroma", name = "sensory", format = "list")
#> $sensory
#>  [1] "aroma"    "scent"    "fragrant" "perfume"  "bouquet" ...
```

---

### Example 2 — A vector of seed words

Expand multiple seed words at once. The `$seed` column shows which seed word drove each candidate — useful for audit trails in published research.

```r
sensory_seeds <- c("aroma", "flavor", "texture", "ambience", "decor")

# individual mode: find neighbours per seed, keep highest similarity per word
sensory <- expand_dict(
  seed        = sensory_seeds,
  n           = 15,
  threshold   = 0.68,
  seed_mode   = "individual",   # default
  vectors     = vecs,
  interactive = FALSE
)

head(sensory, 8)
#>          word similarity     seed pct_match
#> 1       scent      0.841    aroma       84%
#> 2      savory      0.829   flavor       83%
#> 3    fragrant      0.821    aroma       82%
#> 4   succulent      0.814   flavor       81%
#> 5    ambiance      0.803 ambience       80%
#> 6     velvety      0.797  texture       80%
#> 7     piquant      0.788   flavor       79%
#> 8  atmosphere      0.779 ambience       78%

# How many candidates came from each seed?
table(sensory$seed)
#>    aroma ambience     decor   flavor  texture
#>       14       11        9       17       12
```

**Centroid mode** — finds words nearest the semantic *centre* of all seeds jointly. Better when seeds form a tight, coherent concept.

```r
sensory_c <- expand_dict(
  seed        = sensory_seeds,
  seed_mode   = "centroid",
  n           = 15,
  threshold   = 0.68,
  vectors     = vecs,
  interactive = FALSE
)
# $seed column reads "centroid" for every row
```

**Interactive review** (the default in an R session) — launches a word-by-word wizard:

```r
result <- expand_dict(
  seed      = sensory_seeds,
  n         = 15,
  threshold = 0.68,
  vectors   = vecs            # interactive = TRUE by default
)

# ── Semantic Dictionary Expander — Review Wizard ──────────────────────
# Seed words: "aroma", "flavor", "texture", "ambience", and "decor"
# 47 candidates to review, sorted by similarity.
# Keys: y accept · n skip · a accept all remaining · q quit
#
# ── [1/47] ─────────────────── similarity: 84%  |  seed: aroma ───────
# scent
#   > y
# ✔ Added "scent"
# ...

export_dict(result, name = "sensory", format = "list")
```

---

### Example 3 — Custom dictionary (lexicon data frame)

`expand_lexicon()` accepts any two-column data frame — word and dimension label — and expands every dimension in one call. This is the primary workflow when working with an established lexicon.

```r
# Build a small example dictionary
my_dict <- data.frame(
  word      = c("aroma",     "flavor",    "texture",     # sensory
                "delighted", "excited",   "moved",       # affect
                "welcoming", "attentive", "friendly",    # social
                "curious",   "innovative","unique"),     # intellectual
  dimension = c("sensory",   "sensory",   "sensory",
                "affect",    "affect",    "affect",
                "social",    "social",    "social",
                "intellectual","intellectual","intellectual"),
  stringsAsFactors = FALSE
)

candidates <- expand_lexicon(
  lexicon   = my_dict,
  n         = 15,
  threshold = 0.68,
  seed_mode = "individual",
  vectors   = vecs
)

# Results include a $dimension column
head(candidates, 6)
#>            word similarity       seed pct_match    dimension
#> 1         scent      0.841      aroma       84%      sensory
#> 2        savory      0.829     flavor       83%      sensory
#> 3      fragrant      0.821      aroma       82%      sensory
#> 4   exhilarated      0.819  delighted       82%       affect
#> 5     enchanted      0.807  delighted       81%       affect
#> 6    hospitable      0.803  welcoming       80%       social

# Count candidates per dimension
table(candidates$dimension)
#>       affect intellectual      sensory       social
#>           18           14           21           13
```

If your column names differ from `word` / `dimension`, pass them explicitly:

```r
my_dict2 <- data.frame(
  term  = c("delicious", "fragrant", "joyful"),
  group = c("sensory",   "sensory",  "affect"),
  stringsAsFactors = FALSE
)

expand_lexicon(
  my_dict2,
  word_col = "term",
  dim_col  = "group",
  vectors  = vecs,
  threshold = 0.70
)
```

Export the augmented dictionary in any format:

```r
# As a named R list (one entry per dimension)
aug_list <- lapply(
  split(candidates$word, candidates$dimension),
  function(new_words) {
    dim_name <- unique(candidates$dimension[candidates$word %in% new_words])
    unique(c(my_dict$word[my_dict$dimension == dim_name], new_words))
  }
)

# As a quanteda dictionary (requires install.packages("quanteda"))
library(quanteda)
quant_dict <- quanteda::dictionary(aug_list)

# Score a corpus
reviews <- corpus(c(
  r1 = "The aroma was captivating and the flavours were extraordinary.",
  r2 = "Staff were hospitable and the ambience truly enchanting.",
  r3 = "We felt curious exploring every unusual dish on the menu.",
  r4 = "Average food. Nothing particularly noteworthy."
))

dfm(tokens(reviews)) |>
  dfm_lookup(dictionary = quant_dict) |>
  convert(to = "data.frame")
#>   doc_id sensory affect social intellectual
#> 1     r1       3      1      0            0
#> 2     r2       1      1      2            0
#> 3     r3       0      0      0            2
#> 4     r4       0      0      0            0
```

---

## Use with mdeinR

`lexiExpand` integrates directly with the [mdeinR](https://github.com/gvajpai/mdeinR) Memorable Dining Experience package. Pass `mde_words` (a 324-word, 5-dimension lexicon) straight into `expand_lexicon()`:

```r
# install.packages("remotes")
# remotes::install_github("gvajpai/mdeinR")
library(mdeinR)
library(lexiExpand)

vecs <- load_vectors("glove.6B.50d", vocab_size = 100000L)

mde_candidates <- expand_lexicon(
  lexicon   = mde_words,      # 324-row data frame: word + dimension columns
  n         = 25L,
  threshold = 0.68,
  vectors   = vecs
)

table(mde_candidates$dimension)
#>      affect   behavioral intellectual      sensory       social
#>          31           22           18           38           24
```

---

## Vector sources

| Function | Source | Notes |
|---|---|---|
| `download_vectors()` + `load_vectors()` | Stanford NLP (direct download) | Default. Supports 50 / 100 / 200 / 300 dimensions. Download once, cached permanently. |
| `load_vectors_textdata()` | `textdata` package | Alternative. Requires `install.packages("textdata")`. Manages its own consent-and-cache flow. |

```r
# Option A — direct download (recommended)
download_vectors("glove.6B.50d")          # ~160 MB extracted, one-time
vecs <- load_vectors("glove.6B.50d")

# Option B — via textdata
# install.packages("textdata")
vecs <- load_vectors_textdata(dimensions = 300, vocab_size = 50000)
```

---

## Function reference

| Function | Description |
|---|---|
| `download_vectors()` | Download and cache GloVe vectors from Stanford NLP |
| `load_vectors()` | Load cached vectors into a session matrix |
| `load_vectors_textdata()` | Load GloVe vectors via the `textdata` package |
| `expand_dict()` | Expand a seed word or vector of seeds |
| `expand_lexicon()` | Expand a full lexicon data frame by dimension |
| `review_candidates()` | Interactive accept / reject wizard |
| `export_dict()` | Export to `list`, `data.frame`, or `quanteda::dictionary` |

---

## Citation

If you use `lexiExpand` in published research, please cite:

> Vajpai, G. N. (2026). *lexiExpand: Semantic Dictionary Expander for Text Analysis*. R package version 0.2.0. https://github.com/gvajpai/lexiExpand

For research using the MDE dictionary, also cite:

> Vajpai, G. N., Webb, T., & Beldona, S. (2025). Designing a memorable dining experience lexicon based on theory and text mining. *International Journal of Hospitality Management*, 130, 104245. https://doi.org/10.1016/j.ijhm.2025.104245

---

## License

MIT © 2025 Gopi Nath Vajpai
