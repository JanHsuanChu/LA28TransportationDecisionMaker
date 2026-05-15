# Team DL2 Homework Assignment 2.4 - adjusted for final project 

# install.packages("workshops/archr_1.0.tar.gz", type = "source")

# Load packages
library(dplyr) # data wrangling
library(readr) # read_csv etc.
library(tidyr) # expand_grid(), etc.
library(archr) # enumerate_binary(), etc.

# When D4 is not in scope, warehouse is fixed to this level (first alt.; treat as N/A).
D4_INACTIVE <- 0L

# Next we need to enumerate the different decisions from our ADG. 
# Our ADG consists of 3 Downselect, 2 Assignment, and 3 Standard Form Decisions

# D1 Planning Trigger Decision is a downselect decision. 
# Because the decision options are NOT mutually exclusive, it would be possible to select all. 
# n is the number of choices, k is the MAX number of items you can select.
d1 = enumerate_ds(n = 4, k = 2, .did = 1)

# D2 Stakeholder Data Sharing Decision is an Assigning Decision
# there are three options and one or more could be selected for each data source
d2 = enumerate_assignment(3, 3, 2, .did = 2)

# D3 Traffic Data Source Decision is a downselect decision because we an select multiple
d3 = enumerate_ds(n = 3, k = 2, .did = 3)

# D4 Data Warehouse Solution is a Standard Form Solution because only one is needed
d4 = enumerate_sf(n = c(5), .did = 4)

# D5 Decision Engine Solution is standard form because we only need one solution
d5 = enumerate_sf(n = c(2), .did = 5)


# The next decision is another assigning decision
# D6 is the AI / ML Decision which includes different forms of whether to use these models in our prediction algorithm
d6 = enumerate_sf(n = c(2), .did = 6)

# D7 Frontend Web Layer Solution is standard form because only one is needed
d7 = enumerate_sf(n = c(3), .did = 7)

# Lastly, D8 is another downselect decision for the business profit model, multiple options can be selected/combined
# D8 Business Profit Model (n = 4 so "option 4" is column `d8_4`; k = max number selected)
d8 = enumerate_ds(n = 4, k = 2, .did = 8)

# Combining the enumerated decisions using the samples from each due to the size 
arch = expand_grid(d1, d2, d3, d4, d5, d6, d7, d8, .name_repair = "unique") %>%
  # Constraint 1: D4 only if D2 "option 3" — any of d2_a1_b3, d2_a2_b3, d2_a3_b3 is 1;
  # otherwise warehouse is fixed to D4_INACTIVE.
  filter(
    if_any(ends_with("_b3"), ~ . == 1L) | d4 == D4_INACTIVE,
    # Constraint 2: if D6 selects 1, then D5 must be 0 (i.e. d6 != 1 | d5 == 0).
    d6 != 1L | d5 == 0L,
    # Constraint 3: if D8 includes option 4 (`d8_4 == 1`), D7 has only two encoded levels
    # (0 and 1); disallow the third standard-form level (2) for `d7`.
    d8_4 != 1L | d7 != 2L
  )

# Writing the Code to a CSV File (path is current working directory)
out_file <- file.path(getwd(), "architectures.csv")
readr::write_csv(arch, out_file)
message(
  "Rows after constraints: ",
  format(nrow(arch), big.mark = ",", scientific = FALSE),
  ". Wrote CSV to: ",
  normalizePath(out_file, winslash = "/", mustWork = FALSE)
)



