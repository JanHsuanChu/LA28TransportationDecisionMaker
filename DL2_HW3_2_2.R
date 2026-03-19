#HW3B 2.2 


library(archr)
library(dplyr)

# 1) Enumerate each decision block with archr


# D1 Planning Trigger (Down-Selecting, 4 options)
# Must select at least 1 option
D1 <- enumerate_ds(n = 4, k = 1:4, .did = 1)

# D2 Stakeholder Data Sharing
# 3 stakeholders, each standard form with 3 alternatives
# 0 = direct, 1 = API gateway, 2 = data hub
D2 <- enumerate_sf(n = c(3, 3, 3), .did = 20) %>%
  rename(
    D2_rideshare   = d20,
    D2_metro_LADOT = d21,
    D2_navigation  = d22
  )

# D3 Traffic Data Source (Down-Selecting, 3 options)
# Must select at least 1 option
D3 <- enumerate_ds(n = 3, k = 1:3, .did = 3)

# D4 Data Warehouse (Standard Form, 5 options)
D4 <- enumerate_sf(n = 5, .did = 4) %>%
  rename(D4 = d4)

# D5 Decision Engine (Standard Form, 3 options)
D5 <- enumerate_sf(n = 3, .did = 5) %>%
  rename(D5 = d5)

# D6 AI / ML (Standard Form, 3 options)
# 0 = rule-based, 1 = statistical, 2 = ML
D6 <- enumerate_sf(n = 3, .did = 6) %>%
  rename(D6 = d6)

# D7 Frontend Framework (Standard Form, 3 options)
D7 <- enumerate_sf(n = 3, .did = 7) %>%
  rename(D7 = d7)

# D8 Profit Model (Down-Selecting, 4 options)
# Must select at least 1 option
D8 <- enumerate_ds(n = 4, k = 1:4, .did = 8)

# Store blocks and sizes

blocks <- list(D1, D2, D3, D4, D5, D6, D7, D8)
block_names <- c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8")
block_sizes <- sapply(blocks, nrow)

print(block_sizes)

# Helper: convert a global index into row indices

global_to_rows <- function(idx, dims) {
  idx0 <- idx - 1
  rows <- integer(length(dims))
  
  for (j in seq_along(dims)) {
    rows[j] <- (idx0 %% dims[j]) + 1
    idx0 <- idx0 %/% dims[j]
  }
  
  rows
}

# Helper: build architectures from sampled global indices


build_architectures_from_indices <- function(sampled_indices, blocks, dims) {
  arch_list <- vector("list", length(sampled_indices))
  
  for (i in seq_along(sampled_indices)) {
    row_ids <- global_to_rows(sampled_indices[i], dims)
    
    arch_row <- bind_cols(
      blocks[[1]][row_ids[1], , drop = FALSE],
      blocks[[2]][row_ids[2], , drop = FALSE],
      blocks[[3]][row_ids[3], , drop = FALSE],
      blocks[[4]][row_ids[4], , drop = FALSE],
      blocks[[5]][row_ids[5], , drop = FALSE],
      blocks[[6]][row_ids[6], , drop = FALSE],
      blocks[[7]][row_ids[7], , drop = FALSE],
      blocks[[8]][row_ids[8], , drop = FALSE]
    ) %>%
      mutate(
        Arch_ID = i,
        Global_Index = sampled_indices[i]
      ) %>%
      relocate(Arch_ID, Global_Index)
    
    arch_list[[i]] <- arch_row
  }
  
  bind_rows(arch_list)
}

# 5) Feasibility check 

is_feasible_row <- function(df) {
  keep <- rep(TRUE, nrow(df))
  
  # Example 1:
  # If D5 is option 2, require D6 also be option 2
  keep <- keep & !(df$D5 == 2 & df$D6 != 2)
  
  # Example 2:
  # If all stakeholder data sharing choices are direct (0),
  # disallow Data Warehouse option 4
  keep <- keep & !(
    df$D2_rideshare == 0 &
      df$D2_metro_LADOT == 0 &
      df$D2_navigation == 0 &
      df$D4 == 4
  )
  
  # Example 3:
  # If frontend is option 0, disallow all four D8 options selected
  d8_cols <- grep("^d8_", names(df), value = TRUE)
  if (length(d8_cols) == 4) {
    keep <- keep & !(
      df$D7 == 0 &
        rowSums(df[, d8_cols, drop = FALSE]) == 4
    )
  }
  
  keep
}

# 6) Deterministic sampling without full expansion


sample_deterministic_architectures <- function(
    n_samples = 50,
    blocks,
    dims,
    oversample_factor = 3,
    max_iter = 10
) {
  total_arch <- prod(dims)
  collected <- list()
  collected_keys <- integer(0)
  n_collected <- 0
  iter <- 1
  
  while (n_collected < n_samples && iter <= max_iter) {
    n_try <- min(total_arch, n_samples * oversample_factor * iter)
    
    candidate_global_idx <- unique(round(seq(1, total_arch, length.out = n_try)))
    candidate_global_idx <- setdiff(candidate_global_idx, collected_keys)
    
    if (length(candidate_global_idx) == 0) break
    
    candidate_arch <- build_architectures_from_indices(
      sampled_indices = candidate_global_idx,
      blocks = blocks,
      dims = dims
    )
    
    feasible_arch <- candidate_arch[is_feasible_row(candidate_arch), , drop = FALSE]
    
    if (nrow(feasible_arch) > 0) {
      collected[[length(collected) + 1]] <- feasible_arch
      collected_keys <- c(collected_keys, feasible_arch$Global_Index)
      n_collected <- sum(sapply(collected, nrow))
    }
    
    iter <- iter + 1
  }
  
  if (length(collected) == 0) {
    return(data.frame())
  }
  
  out <- bind_rows(collected) %>%
    distinct(Global_Index, .keep_all = TRUE) %>%
    arrange(Global_Index)
  
  if (nrow(out) > n_samples) {
    keep_idx <- unique(round(seq(1, nrow(out), length.out = n_samples)))
    out <- out[keep_idx, , drop = FALSE]
  }
  
  out$Arch_ID <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}

# Generate deterministic architectures


det_architectures <- sample_deterministic_architectures(
  n_samples = 50,
  blocks = blocks,
  dims = block_sizes,
  oversample_factor = 3,
  max_iter = 10
)


cat("Block sizes:\n")
print(block_sizes)

cat("\nTotal possible architectures:", prod(block_sizes), "\n")

print(det_architectures)
