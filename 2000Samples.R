# HW3 - Generate feasible architectures

library(archr)
library(dplyr)

set.seed(123)

# ==========================================================
# 1) ENUMERATE DECISION BLOCKS
# ==========================================================

# D1 Planning Trigger (Down-Selecting, 4 options)
D1 <- enumerate_ds(n = 4, k = 1:4, .did = 1)

# D2 Stakeholder Data Sharing (Assigning = DS per stakeholder)
# D2 Stakeholder Data Sharing
D2 <- enumerate_sf(n = c(3, 3, 3), .did = 21) %>%
  mutate(across(c(d21, d22, d23), ~ as.integer(.x) + 1)) %>%
  rename(
    D2_rideshare   = d21,
    D2_metro_LADOT = d22,
    D2_navigation  = d23
  )

# D3 Traffic Data Source (Down-Selecting, 3 options)
D3 <- enumerate_ds(n = 3, k = 1:3, .did = 3)

# D4 Data Warehouse (Standard Form, 5 options)
D4 <- data.frame(D4 = 1:5)
  #rename(D4 = d4)

# D5 Decision Engine (0 = existing, 1 = custom)
D5 <- data.frame(D5 = c(0, 1))

# D6 AI / ML (0 = no AI, 1 = AI)
D6 <- data.frame(D6 = c(0, 1))

# D7 Frontend Framework (Standard Form, 3 options)
D7 <- data.frame(D7 = 1:3)
 # rename(D7 = d7)

# D8 Profit Model (Down-Selecting, 4 options)
D8 <- enumerate_ds(n = 4, k = 1:4, .did = 8)

blocks <- list(D1 = D1, D2 = D2, D3 = D3, D4 = D4, D5 = D5, D6 = D6, D7 = D7, D8 = D8)

# ==========================================================
# 2) FEASIBILITY RULES
# ==========================================================

is_feasible_architecture <- function(df) {
  keep <- rep(TRUE, nrow(df))
  
  # At least one planning trigger
  d1_cols <- grep("^d1_", names(df), value = TRUE)
  keep <- keep & (rowSums(df[, d1_cols]) >= 1)
  
  # At least one traffic data source
  d3_cols <- grep("^d3_", names(df), value = TRUE)
  keep <- keep & (rowSums(df[, d3_cols]) >= 1)
  
  # At least one profit model
  d8_cols <- grep("^d8_", names(df), value = TRUE)
  keep <- keep & (rowSums(df[, d8_cols]) >= 1)
  
  # Logical consistency: custom engine usually requires AI
  keep <- keep & !(df$D5 == 1 & df$D6 == 0)
  
  keep
}

# ==========================================================
# 3) SAMPLE ONE ARCHITECTURE
# ==========================================================
sample_one_architecture <- function(blocks, fixed = list()) {
  
  # Sample rows for archr blocks
  row_D1 <- sample(seq_len(nrow(blocks$D1)), 1)
  row_D2 <- sample(seq_len(nrow(blocks$D2)), 1)
  row_D3 <- sample(seq_len(nrow(blocks$D3)), 1)
  row_D8 <- sample(seq_len(nrow(blocks$D8)), 1)
  
  # Sample scalar decisions correctly
  val_D4 <- sample(blocks$D4$D4, 1)
  val_D5 <- if (!is.null(fixed$D5_val)) fixed$D5_val else sample(blocks$D5$D5, 1)
  val_D6 <- if (!is.null(fixed$D6_val)) fixed$D6_val else sample(blocks$D6$D6, 1)
  val_D7 <- sample(blocks$D7$D7, 1)
  
  # Combine everything
  bind_cols(
    blocks$D1[row_D1, , drop = FALSE],
    blocks$D2[row_D2, , drop = FALSE],
    blocks$D3[row_D3, , drop = FALSE],
    data.frame(D4 = val_D4),
    data.frame(D5 = val_D5),
    data.frame(D6 = val_D6),
    data.frame(D7 = val_D7),
    blocks$D8[row_D8, , drop = FALSE]
  )
}
# ==========================================================
# 4) VECTOR REPRESENTATION
# ==========================================================

make_vector_representation <- function(df) {
  d1_cols <- grep("^d1_", names(df), value = TRUE)
  d3_cols <- grep("^d3_", names(df), value = TRUE)
  d8_cols <- grep("^d8_", names(df), value = TRUE)
  
  df %>%
    rowwise() %>%
    mutate(
      vector_representation = paste0(
        "D1=[", paste(c_across(all_of(d1_cols)), collapse=","), "]; ",
        "D2=[", paste(c_across(starts_with("d2")), collapse=","), "]; ",
        "D3=[", paste(c_across(all_of(d3_cols)), collapse=","), "]; ",
        "D4=", D4, "; ",
        "D5=", D5, "; ",
        "D6=", D6, "; ",
        "D7=", D7, "; ",
        "D8=[", paste(c_across(all_of(d8_cols)), collapse=","), "]"
      )
    ) %>%
    ungroup()
}

# ==========================================================
# 5) FIND FEASIBLE STRATA (D5 x D6)
# ==========================================================

find_feasible_strata <- function(blocks, tries_per_stratum = 500) {
  strata <- expand.grid(
    D5_row = seq_len(nrow(blocks$D5)),
    D6_row = seq_len(nrow(blocks$D6))
  )
  
  feasible_flags <- logical(nrow(strata))
  
  for (i in seq_len(nrow(strata))) {
    for (t in seq_len(tries_per_stratum)) {
      cand <- sample_one_architecture(blocks,
                                      fixed = list(D5_row = strata$D5_row[i],
                                                   D6_row = strata$D6_row[i]))
      
      if (is_feasible_architecture(cand)) {
        feasible_flags[i] <- TRUE
        break
      }
    }
  }
  
  strata[feasible_flags, ]
}

feasible_strata <- find_feasible_strata(blocks)

# ==========================================================
# 6) ALLOCATE 2000 SAMPLES
# ==========================================================

allocate_counts <- function(N, n_strata) {
  base_n <- floor(N / n_strata)
  remainder <- N %% n_strata
  alloc <- rep(base_n, n_strata)
  if (remainder > 0) alloc[1:remainder] <- alloc[1:remainder] + 1
  alloc
}

N <- 2000
feasible_strata$n_target <- allocate_counts(N, nrow(feasible_strata))

# ==========================================================
# 7) SAMPLE ARCHITECTURES
# ==========================================================

sample_stratified_architectures <- function(blocks, feasible_strata) {
  collected <- list()
  sample_id <- 1
  
  for (i in seq_len(nrow(feasible_strata))) {
    target_n <- feasible_strata$n_target[i]
    
    fixed_vals <- list(
      D5_row = feasible_strata$D5_row[i],
      D6_row = feasible_strata$D6_row[i]
    )
    
    rows <- list()
    keys <- character(0)
    
    while (length(rows) < target_n) {
      cand <- sample_one_architecture(blocks, fixed_vals)
      
      if (is_feasible_architecture(cand)) {
        cand <- make_vector_representation(cand)
        key <- cand$vector_representation
        
        if (!(key %in% keys)) {
          cand$Arch_ID <- sample_id
          cand$Stratum <- paste0("D5=", cand$D5, "_D6=", cand$D6)
          
          rows[[length(rows)+1]] <- cand
          keys <- c(keys, key)
          sample_id <- sample_id + 1
        }
      }
    }
    
    collected[[i]] <- bind_rows(rows)
  }
  
  bind_rows(collected)
}

random_architectures <- sample_stratified_architectures(blocks, feasible_strata)

# ==========================================================
# 8) OUTPUT
# ==========================================================

cat("Generated:", nrow(random_architectures), "architectures\n")

# Show vectors
print(random_architectures %>% select(Arch_ID, vector_representation))

# Feasibility check
cat("All feasible:", all(is_feasible_architecture(random_architectures)), "\n")

# Save file
write.csv(random_architectures, "random_architectures_N2000_v3.csv", row.names = FALSE)