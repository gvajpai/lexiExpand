# =============================================================================
#  Expanding the MDE (Memorable Dining Experience) Dictionary with lexiExpand
#
#  Vajpai, Webb & Beldona (2025). "Designing a memorable dining experience
#  lexicon based on theory and text mining."
#  International Journal of Hospitality Management, 130, 104245.
#  https://doi.org/10.1016/j.ijhm.2025.104245
#
#  The MDE dictionary covers 5 dimensions (324 words total):
#    sensory      (85)  – taste, smell, sight, sound, touch; food & ambience
#    affect       (82)  – emotions aroused during dining
#    social       (58)  – belonging; interactions with staff/family/friends
#    intellectual (51)  – curiosity, learning, cognitive engagement
#    behavioral   (48)  – physical engagement, activities, involvement
#
#  This script uses lexiExpand to discover semantically similar words that
#  may be missing from each dimension, then exports an augmented dictionary
#  ready for use with mdeinR or quanteda.
# =============================================================================


# ── 0. Setup ─────────────────────────────────────────────────────────────────

# Install packages if needed
# install.packages(c("remotes", "lexiExpand"))
# remotes::install_github("gvajpai/mdeinR")

library(lexiExpand)
library(mdeinR)     # provides mde_dictionary and mde_words objects


# ── 1. One-time vector download (~160 MB, cached after first run) ────────────

download_vectors("glove.6B.50d")


# ── 2. Load vectors once for the whole session ───────────────────────────────

vecs <- load_vectors("glove.6B.50d", vocab_size = 100000L)
# 100k words gives better hospitality-domain coverage than the default 50k


# ── 3. Inspect the existing MDE dictionary ───────────────────────────────────

# mdeinR ships mde_dictionary as a named list and mde_words as a data frame
str(mde_dictionary)
#> List of 5
#>  $ sensory     : chr [1:85]  "aroma" "flavor" "texture" ...
#>  $ affect      : chr [1:82]  "delighted" "excited" "moved" ...
#>  $ social      : chr [1:58]  "welcoming" "attentive" "friendly" ...
#>  $ intellectual: chr [1:51]  "curious" "innovative" "unique" ...
#>  $ behavioral  : chr [1:48]  "engaged" "immersive" "interactive" ...

# How many words per dimension?
sapply(mde_dictionary, length)
#> sensory affect social intellectual behavioral
#>      85     82     58           51         48


# ── 4. Expand each dimension with lexiExpand ─────────────────────────────────
#
# Strategy:
#   • Use seed_mode = "individual" to see which seed words drive each candidate
#     (good for audit trails in academic work)
#   • n = 30 gives a generous candidate pool before thresholding
#   • threshold = 0.68 balances breadth vs precision for hospitality text
#   • interactive = FALSE for a reproducible, scriptable pipeline
# -----------------------------------------------------------------------------

expand_mde_dimension <- function(dimension_name,
                                 vecs,
                                 n         = 30L,
                                 threshold = 0.68) {
  seeds <- mde_dictionary[[dimension_name]]

  message("\n── Expanding: ", dimension_name, " (", length(seeds), " seed words) ──")

  candidates <- expand_dict(
    seed        = seeds,
    n           = n,
    threshold   = threshold,
    seed_mode   = "individual",   # show which seed drove each candidate
    vectors     = vecs,
    interactive = FALSE
  )

  # Remove words already in the current dictionary
  all_existing <- unique(unlist(mde_dictionary))
  candidates   <- candidates[!candidates$word %in% all_existing, ]

  candidates
}

# Expand all five dimensions
mde_expansions <- lapply(
  names(mde_dictionary),
  expand_mde_dimension,
  vecs = vecs
)
names(mde_expansions) <- names(mde_dictionary)


# ── 5. Inspect top candidates per dimension ───────────────────────────────────

# Print top 10 for each dimension
for (dim in names(mde_expansions)) {
  cat("\n══ ", toupper(dim), " ══\n", sep = "")
  top10 <- head(mde_expansions[[dim]], 10)
  print(top10[, c("word", "pct_match", "seed")], row.names = FALSE)
}

# Expected output (approximate — depends on GloVe model):
#
# ══ SENSORY ══
#        word pct_match       seed
#      savory       81%      taste
#     fragrant       79%      aroma
#    succulent       77%     flavor
#      velvety       75%    texture
#      piquant       74%      taste
# ...
#
# ══ AFFECT ══
#        word pct_match          seed
#    exhilarated      82%      excited
#    enchanted       80%    delighted
#    overjoyed       78%    delighted
#    enthralled      77%        moved
# ...
#
# ══ SOCIAL ══
#        word pct_match         seed
#   hospitable      83%    welcoming
#    courteous      81%    attentive
#     gracious      79%     friendly
#       genial      77%     friendly
# ...
#
# ══ INTELLECTUAL ══
#        word pct_match          seed
#   inquisitive      80%        curious
#    imaginative      78%    innovative
#       original      76%         unique
#       creative      75%    innovative
# ...
#
# ══ BEHAVIORAL ══
#        word pct_match          seed
#  participatory      79%       engaged
#      hands-on      77%   interactive
#    stimulating      75%     immersive
#       absorbing      74%     immersive
# ...


# ── 6. Alternative: centroid mode for each dimension ─────────────────────────
#
# Centroid mode finds words nearest the semantic *centre* of all seeds
# simultaneously — useful for checking overall dimension coherence.

mde_centroid <- lapply(names(mde_dictionary), function(dim) {
  expand_dict(
    seed        = mde_dictionary[[dim]],
    n           = 20L,
    threshold   = 0.68,
    seed_mode   = "centroid",
    vectors     = vecs,
    interactive = FALSE
  )
})
names(mde_centroid) <- names(mde_dictionary)

# Compare individual vs centroid for 'affect'
cat("\nAffect — individual mode top 5:\n")
print(head(mde_expansions$affect[, c("word","pct_match","seed")], 5), row.names = FALSE)

cat("\nAffect — centroid mode top 5:\n")
print(head(mde_centroid$affect[, c("word","pct_match")], 5), row.names = FALSE)


# ── 7. Export augmented dictionary ───────────────────────────────────────────
#
# Option A: Keep only top-N candidates per dimension (fully automated)

top_n_per_dim <- 10L

augmented_list <- mapply(
  function(orig_words, candidates) {
    new_words <- head(candidates$word, top_n_per_dim)
    unique(c(orig_words, new_words))
  },
  orig_words = mde_dictionary,
  candidates = mde_expansions,
  SIMPLIFY   = FALSE
)

# How many words were added per dimension?
added <- mapply(
  function(orig, aug) length(aug) - length(orig),
  orig = mde_dictionary,
  aug  = augmented_list
)
cat("\nWords added per dimension:\n")
print(added)
#> sensory      affect       social intellectual   behavioral
#>      10          10           10           10           10


# Option B: Interactive review (recommended for publication-quality work)
#
# Uncomment to run the wizard for a single dimension in an R session:
#
# affect_result <- expand_dict(
#   seed      = mde_dictionary$affect,
#   n         = 25L,
#   threshold = 0.68,
#   seed_mode = "individual",
#   vectors   = vecs,
#   interactive = TRUE          # launches word-by-word wizard
# )
#
# augmented_affect <- c(mde_dictionary$affect, affect_result$accepted)


# ── 8. Export to quanteda dictionary ─────────────────────────────────────────

if (requireNamespace("quanteda", quietly = TRUE)) {
  library(quanteda)

  # Build one quanteda dictionary entry per dimension from augmented lists
  quant_dict <- quanteda::dictionary(augmented_list)
  quant_dict
  #> Dictionary object with 5 key entries.
  #> - [sensory]: aroma, flavor, texture, ..., savory, fragrant, succulent, ...
  #> - [affect]: delighted, excited, ..., exhilarated, enchanted, overjoyed, ...
  #> ...

  # Score a set of reviews
  test_reviews <- c(
    rev1 = "The aroma was captivating and the presentation absolutely stunning.",
    rev2 = "Staff were genuinely warm and we felt truly part of a community.",
    rev3 = "We learned so much about fermentation — a fascinating culinary journey.",
    rev4 = "Very average. Nothing remarkable about the food or the atmosphere."
  )

  scored <- tokens(test_reviews) |>
    dfm() |>
    dfm_lookup(dictionary = quant_dict) |>
    convert(to = "data.frame")

  print(scored)
  #>   doc_id sensory affect social intellectual behavioral
  #> 1   rev1       3      1      0            0          0
  #> 2   rev2       0      1      3            0          0
  #> 3   rev3       0      1      0            3          0
  #> 4   rev4       0      0      0            0          0
}


# ── 9. Save the augmented dictionary for reproducibility ─────────────────────

# Save as RDS (preserves R list structure perfectly)
saveRDS(augmented_list, "mde_dictionary_augmented.rds")

# Save as flat CSV for sharing / methods appendix
aug_df <- do.call(rbind, lapply(names(augmented_list), function(dim) {
  data.frame(
    word      = augmented_list[[dim]],
    dimension = dim,
    is_original = augmented_list[[dim]] %in% mde_dictionary[[dim]],
    stringsAsFactors = FALSE
  )
}))

write.csv(aug_df, "mde_dictionary_augmented.csv", row.names = FALSE)

# Also save the full candidate tables for your methods appendix
for (dim in names(mde_expansions)) {
  write.csv(
    mde_expansions[[dim]],
    paste0("mde_candidates_", dim, ".csv"),
    row.names = FALSE
  )
}

cat("\nDone. Files saved:\n")
cat("  mde_dictionary_augmented.rds  (augmented dictionary)\n")
cat("  mde_dictionary_augmented.csv  (flat format for sharing)\n")
cat("  mde_candidates_*.csv          (full candidate tables)\n")
