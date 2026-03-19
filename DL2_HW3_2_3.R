#HW3 2.3

library(archr)
library(dplyr)

# ==========================================================
# Enumerate decision blocks
# ==========================================================

# D1 Planning Trigger (Down-Selecting, 4 options)
D1 <- enumerate_ds(n = 4, k = 1:4, .did = 1)

# D2 Stakeholder Data Sharing
# 0 = direct, 1 = API gateway, 2 = data hub
D2 <- enumerate_sf(n = c(3, 3, 3), .did = 20) %>%
  rename(
    D2_rideshare   = d20,
    D2_metro_LADOT = d21,
    D2_navigation  = d22
  )

# D3 Traffic Data Source (Down-Selecting, 3 options)
D3 <- enumerate_ds(n = 3, k = 1:3, .did = 3)

# D4 Data Warehouse (Standard Form, 5 options)
D4 <- enumerate_sf(n = 5, .did = 4) %>%
  rename(D4 = d4)

# D5 Decision Engine (Standard Form, 3 options)
D5 <- enumerate_sf(n = 3, .did = 5) %>%
  rename(D5 = d5)

# D6 AI / ML (Standard Form, 3 options)
D6 <- enumerate_sf(n = 3, .did = 6) %>%
  rename(D6 = d6)

# D7 Frontend Framework (Standard Form, 3 options)
D7 <- enumerate_sf(n = 3, .did = 7) %>%
  rename(D7 = d7)

# D8 Profit Model (Down-Selecting, 4 options)
D8 <- enumerate_ds(n = 4, k = 1:4, .did = 8)

blocks <- list(D1 = D1, D2 = D2, D3 = D3, D4 = D4, D5 = D5, D6 = D6, D7 = D7, D8 = D8)

# ==========================================================
# Feasibility rules
# ==========================================================

is_feasible_architecture <- function(df) {
  keep <- rep(TRUE, nrow(df))

  # Example 1:
  keep <- keep & !(df$D5 == 2 & df$D6 != 2)

  # Example 2:
  keep <- keep & !(
    df$D2_rideshare == 0 &
    df$D2_metro_LADOT == 0 &
    df$D2_navigation == 0 &
    df$D4 == 4
  )

  # Example 3:
  d8_cols <- grep("^d8_", names(df), value = TRUE)
  if (length(d8_cols) == 4) {
    keep <- keep & !(
      df$D7 == 0 &
      rowSums(df[, d8_cols, drop = FALSE]) == 4
    )
  }

  keep
}

# ==========================================================
# Sample one random architecture
# ==========================================================

sample_one_architecture <- function(blocks, fixed = list()) {
  row_D1 <- sample(seq_len(nrow(blocks$D1)), 1)
  row_D2 <- sample(seq_len(nrow(blocks$D2)), 1)
  row_D3 <- sample(seq_len(nrow(blocks$D3)), 1)
  row_D4 <- sample(seq_len(nrow(blocks$D4)), 1)
  row_D5 <- if (!is.null(fixed$D5_row)) fixed$D5_row else sample(seq_len(nrow(blocks$D5)), 1)
  row_D6 <- if (!is.null(fixed$D6_row)) fixed$D6_row else sample(seq_len(nrow(blocks$D6)), 1)
  row_D7 <- sample(seq_len(nrow(blocks$D7)), 1)
  row_D8 <- sample(seq_len(nrow(blocks$D8)), 1)

  bind_cols(
    blocks$D1[row_D1, , drop = FALSE],
    blocks$D2[row_D2, , drop = FALSE],
    blocks$D3[row_D3, , drop = FALSE],
    blocks$D4[row_D4, , drop = FALSE],
    blocks$D5[row_D5, , drop = FALSE],
    blocks$D6[row_D6, , drop = FALSE],
    blocks$D7[row_D7, , drop = FALSE],
    blocks$D8[row_D8, , drop = FALSE]
  )
}

# ==========================================================
# Vector representation
# ==========================================================

make_vector_representation <- function(df) {
  d1_cols <- grep("^d1_", names(df), value = TRUE)
  d3_cols <- grep("^d3_", names(df), value = TRUE)
  d8_cols <- grep("^d8_", names(df), value = TRUE)

  df %>%
    rowwise() %>%
    mutate(
      vector_representation = paste0(
        "D1=[", paste(c_across(all_of(d1_cols)), collapse = ","), "]; ",
        "D2=[", D2_rideshare, ",", D2_metro_LADOT, ",", D2_navigation, "]; ",
        "D3=[", paste(c_across(all_of(d3_cols)), collapse = ","), "]; ",
        "D4=", D4, "; ",
        "D5=", D5, "; ",
        "D6=", D6, "; ",
        "D7=", D7, "; ",
        "D8=[", paste(c_across(all_of(d8_cols)), collapse = ","), "]"
      )
    ) %>%
    ungroup()
}

# ==========================================================
# Find feasible strata using D5 x D6
# ==========================================================

find_feasible_strata <- function(blocks, tries_per_stratum = 200) {
  strata <- expand.grid(
    D5_row = seq_len(nrow(blocks$D5)),
    D6_row = seq_len(nrow(blocks$D6))
  )

  feasible_flags <- logical(nrow(strata))

  for (i in seq_len(nrow(strata))) {
    found <- FALSE

    for (t in seq_len(tries_per_stratum)) {
      cand <- sample_one_architecture(
        blocks,
        fixed = list(D5_row = strata$D5_row[i], D6_row = strata$D6_row[i])
      )

      if (is_feasible_architecture(cand)) {
        found <- TRUE
        break
      }
    }

    feasible_flags[i] <- found
  }

  strata[feasible_flags, , drop = FALSE]
}

feasible_strata <- find_feasible_strata(blocks)

# ==========================================================
# Allocate N = 100 across strata
# ==========================================================

allocate_counts <- function(N, n_strata) {
  base_n <- floor(N / n_strata)
  remainder <- N %% n_strata
  alloc <- rep(base_n, n_strata)
  if (remainder > 0) alloc[seq_len(remainder)] <- alloc[seq_len(remainder)] + 1
  alloc
}

N <- 100
feasible_strata$n_target <- allocate_counts(N, nrow(feasible_strata))

# ==========================================================
# Sample feasible random architectures
# ==========================================================

sample_stratified_architectures <- function(blocks, feasible_strata, max_tries_per_needed = 200) {
  collected <- list()
  sample_id <- 1

  for (i in seq_len(nrow(feasible_strata))) {
    target_n <- feasible_strata$n_target[i]
    fixed_vals <- list(
      D5_row = feasible_strata$D5_row[i],
      D6_row = feasible_strata$D6_row[i]
    )

    stratum_rows <- list()
    keys <- character(0)
    tries <- 0

    while (length(stratum_rows) < target_n && tries < target_n * max_tries_per_needed) {
      tries <- tries + 1
      cand <- sample_one_architecture(blocks, fixed = fixed_vals)

      if (is_feasible_architecture(cand)) {
        cand <- make_vector_representation(cand)
        key <- cand$vector_representation[1]

        if (!(key %in% keys)) {
          cand$Arch_ID <- sample_id
          cand$Stratum <- paste0("D5=", cand$D5, "_D6=", cand$D6)
          stratum_rows[[length(stratum_rows) + 1]] <- cand
          keys <- c(keys, key)
          sample_id <- sample_id + 1
        }
      }
    }

    if (length(stratum_rows) > 0) {
      collected[[length(collected) + 1]] <- bind_rows(stratum_rows)
    }
  }

  if (length(collected) == 0) return(data.frame())
  bind_rows(collected)
}

random_architectures <- sample_stratified_architectures(blocks, feasible_strata)

# Top up if slightly short
while (nrow(random_architectures) < N) {
  missing_n <- N - nrow(random_architectures)
  extra_rows <- list()

  for (k in seq_len(missing_n * 10)) {
    stratum_pick <- feasible_strata[sample(seq_len(nrow(feasible_strata)), 1), ]
    cand <- sample_one_architecture(
      blocks,
      fixed = list(D5_row = stratum_pick$D5_row, D6_row = stratum_pick$D6_row)
    )

    if (is_feasible_architecture(cand)) {
      cand <- make_vector_representation(cand)
      cand$Arch_ID <- NA
      cand$Stratum <- paste0("D5=", cand$D5, "_D6=", cand$D6)
      extra_rows[[length(extra_rows) + 1]] <- cand
    }
  }

  extra_df <- bind_rows(extra_rows) %>%
    distinct(vector_representation, .keep_all = TRUE)

  random_architectures <- bind_rows(random_architectures, extra_df) %>%
    distinct(vector_representation, .keep_all = TRUE)

  random_architectures$Arch_ID <- seq_len(nrow(random_architectures))
  random_architectures <- random_architectures[seq_len(min(N, nrow(random_architectures))), , drop = FALSE]
}

# ==========================================================
# show vector representation
# ==========================================================

architecture_vectors <- random_architectures %>%
  select(Arch_ID, vector_representation)

print(architecture_vectors, row.names = FALSE)

# ==========================================================
# verify feasibility
# ==========================================================

all_feasible <- all(is_feasible_architecture(random_architectures))
cat("All generated architectures feasible? ", all_feasible, "\n\n")

# ==========================================================
# 10) Part (c): quantitative justification 
# ==========================================================

# Counts by D5-D6 stratum
stratum_counts <- random_architectures %>%
  count(D5, D6, name = "count") %>%
  arrange(D5, D6)

# Marginal counts
D5_counts <- random_architectures %>%
  count(D5, name = "count") %>%
  arrange(D5)

D6_counts <- random_architectures %>%
  count(D6, name = "count") %>%
  arrange(D6)

D7_counts <- random_architectures %>%
  count(D7, name = "count") %>%
  arrange(D7)

# Add proportions
D5_counts <- D5_counts %>% mutate(prop = count / sum(count))
D6_counts <- D6_counts %>% mutate(prop = count / sum(count))
D7_counts <- D7_counts %>% mutate(prop = count / sum(count))
stratum_counts <- stratum_counts %>% mutate(prop = count / sum(count))

# Balance metrics
balance_metrics <- list(
  D5_min = min(D5_counts$count),
  D5_max = max(D5_counts$count),
  D5_range = max(D5_counts$count) - min(D5_counts$count),
  D5_relative_imbalance = (max(D5_counts$count) - min(D5_counts$count)) / mean(D5_counts$count),

  D6_min = min(D6_counts$count),
  D6_max = max(D6_counts$count),
  D6_range = max(D6_counts$count) - min(D6_counts$count),
  D6_relative_imbalance = (max(D6_counts$count) - min(D6_counts$count)) / mean(D6_counts$count),

  D7_min = min(D7_counts$count),
  D7_max = max(D7_counts$count),
  D7_range = max(D7_counts$count) - min(D7_counts$count),
  D7_relative_imbalance = (max(D7_counts$count) - min(D7_counts$count)) / mean(D7_counts$count),

  Stratum_min = min(stratum_counts$count),
  Stratum_max = max(stratum_counts$count),
  Stratum_range = max(stratum_counts$count) - min(stratum_counts$count),
  Stratum_relative_imbalance = (max(stratum_counts$count) - min(stratum_counts$count)) / mean(stratum_counts$count)
)

cat("Counts by D5 and D6 stratum:\n")
print(stratum_counts)

cat("\nMarginal counts for D5:\n")
print(D5_counts)

cat("\nMarginal counts for D6:\n")
print(D6_counts)

cat("\nMarginal counts for D7:\n")
print(D7_counts)

cat("\nBalance metrics:\n")
print(balance_metrics)

 write.csv(random_architectures, "random_architectures_N100.csv", row.names = FALSE)