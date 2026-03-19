# Team DL2 Homework Assignment 2.4 March 18th 2026

# install.packages("workshops/archr_1.0.tar.gz", type = "source")

# Load packages
library(dplyr) # data wrangling
library(readr) # read_csv etc.
library(tidyr) # expand_grid(), etc.
library(archr) # enumerate_binary(), etc.


# Next we need to enumerate the different decisions from our ADG. 
# Our ADG consists of 3 Downselect, 2 Assignment, and 3 Standard Form Decisions

# D1 Planning Trigger Decision is a downselect decision. 
# Because the decision options are NOT mutually exclusive, it would be possible to select all. 
# n is the number of choices, k is the MAX number of items you can select.
d1 = enumerate_ds(n = 4, k = 2)

d1_Sample = sample(d1,1, FALSE)

# D2 Stakeholder Data Sharing Decision is an Assigning Decision
# there are three options and one or more could be selected for each data source

d2 = enumerate_assignment(3, 3, 2)

d2_Sample = sample(d2,1, FALSE)

# D3 Traffic Data Source Decision is a downselect decision because we an select multiple
d3 = enumerate_ds(n = 3, k = 3)
d3_Sample = sample(d3,1, FALSE)

# D4 Data Warehouse Solution is a Standard Form Solution because only one is needed
d4 = enumerate_sf(n = c(5))
d4_Sample = sample(d4,1, FALSE) 

# D5 Decision Engine Solution is standard form because we only need one solution
d5 = enumerate_sf(n = c(2))
d5_Sample = sample(d5,1, FALSE)

# The next decision is another assigning decision
# D6 is the AI / ML Decision which includes different forms of whether to use these models in our prediction algorithm
d6 = enumerate_sf(n = c(2))
d6_Sample = sample(d6,1, FALSE)

# D7 Frontend Web Layer Solution is standard form because only one is needed
d7 = enumerate_sf(n = c(3))
d7_Sample = sample(d7,1, FALSE)

# Lastly, D8 is another downselect decision for the business profit model, multiple options can be selected/combined
# D8 Business Profit Model
d8 = enumerate_ds(n = 3, k = 2)
d8_Sample = sample(d8,1, FALSE)


# Combining the enumerated decisions using the samples from each due to the size 
arch = expand_grid(d1_Sample, d2_Sample, d3_Sample, d4_Sample, d5_Sample, d6_Sample, d7_Sample, d8_Sample, .name_repair = "unique") 

# Writing the Code to a CSV File 
arch %>% readr::write_csv("architectures.csv")  



