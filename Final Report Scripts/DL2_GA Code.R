# SETUP #

library(dplyr)
library(archr)
library(GA)
library(rmoo)

#Bit count by decision
n_d1 <- 4
n_d2 <- 6
n_d3 <- 3
n_d4 <- 3
n_d5 <- 1
n_d6 <- 1
n_d7 <- 2
n_d8 <- 4

total_bits <- sum(n_d1, n_d2, n_d3, n_d4, n_d5, n_d6, n_d7, n_d8)


# CONVERSION TO BITS #

int2bin <- function(xhat){
  c(
    xhat$d1,
    decimal2binary(xhat$d2[1] - 1, length = 2),
    decimal2binary(xhat$d2[2] - 1, length = 2),
    decimal2binary(xhat$d2[3] - 1, length = 2),
    xhat$d3,
    decimal2binary(xhat$d4 - 1, length = 3),
    decimal2binary(xhat$d5, length = 1),
    decimal2binary(xhat$d6, length = 1),
    decimal2binary(xhat$d7 - 1, length = 2),
    xhat$d8
  )
}

bit2int <- function(x){
  idx <- 1
  
  d1 <- x[idx:(idx + n_d1 - 1)]
  idx <- idx + n_d1
  
  d2_bits <- x[idx:(idx + n_d2 - 1)]
  idx <- idx + n_d2
  
  d2 <- c(
    binary2decimal(d2_bits[1:2]) + 1,
    binary2decimal(d2_bits[3:4]) + 1,
    binary2decimal(d2_bits[5:6]) + 1
  )
  
  d3 <- x[idx:(idx + n_d3 - 1)]
  idx <- idx + n_d3
  
  d4 <- binary2decimal(x[idx:(idx + n_d4 - 1)]) + 1
  idx <- idx + n_d4
  
  d5 <- binary2decimal(x[idx:(idx + n_d5 - 1)])
  idx <- idx + n_d5
  
  d6 <- binary2decimal(x[idx:(idx + n_d6 - 1)])
  idx <- idx + n_d6
  
  d7 <- binary2decimal(x[idx:(idx + n_d7 - 1)]) + 1
  idx <- idx + n_d7
  
  d8 <- x[idx:(idx + n_d8 - 1)]
  
  list(d1 = d1, d2 = d2, d3 = d3, d4 = d4, d5 = d5, d6 = d6, d7 = d7, d8 = d8)
}


# REPAIR BITS & CONSTRAINTS #

repair_exact_k <- function(bits, k){
  while(sum(bits) != k){
    bits <- sample(c(0,1), size = length(bits), replace = TRUE)
  }
  bits
}

repair_bits <- function(x){
  xhat <- bit2int(x)
  
  d1 <- repair_exact_k(xhat$d1, 2)
  d2 <- xhat$d2
  d3 <- repair_exact_k(xhat$d3, 2)
  d4 <- xhat$d4
  d5 <- xhat$d5
  d6 <- xhat$d6
  d7 <- xhat$d7
  d8 <- repair_exact_k(xhat$d8, 2)
  
  d2[d2 < 1 | d2 > 3] <- sample(1:3, size = sum(d2 < 1 | d2 > 3), replace = TRUE)
  if(!(d4 %in% 1:5)) d4 <- sample(1:5, 1)
  if(!(d5 %in% 0:1)) d5 <- sample(0:1, 1)
  if(!(d6 %in% 0:1)) d6 <- sample(0:1, 1)
  if(!(d7 %in% 1:3)) d7 <- sample(1:3, 1)
  
  if(any(d2 == 3)) d2 <- c(3,3,3)
  if(!any(d2 == 3)) d4 <- 5
  if(d6 == 1) d5 <- 1
  if(d8[4] == 1 && !(d7 %in% c(1,2))) d7 <- sample(c(1,2), 1)
  
  int2bin(list(d1 = d1, d2 = d2, d3 = d3, d4 = d4, d5 = d5, d6 = d6, d7 = d7, d8 = d8))
}

constrain <- function(x){
  repair_bits(x)
}


# METRICS #

get_travel_time_reduction <- function(d3, d5, d6){
  A <- c(`1` = 0.3, `2` = 0.6, `3` = 1.0)[as.character(d3)]
  Q <- c(`0` = 0.5, `1` = 1.0)[as.character(d5)]
  M <- c(`0` = 0.4, `1` = 1.0)[as.character(d6)]
  30 * (0.40*A + 0.30*Q + 0.30*M)
}

get_prediction_accuracy <- function(d2, d3, d6){
  Q <- c(`1` = 0.3, `2` = 0.6, `3` = 1.0)[as.character(d3)]
  Q_bonus <- c(`1` = 0.00, `2` = 0.10, `3` = 0.20)[as.character(d2)]
  M <- c(`0` = 0.4, `1` = 1.0)[as.character(d6)]
  raw <- 0.55 * min(Q + Q_bonus, 1.0) + 0.45 * M
  raw * (0.99 - 0.50) + 0.50
}

get_response_time <- function(d2, d5, d6){
  I <- c(`1` = 0.2, `2` = 0.5, `3` = 1.0)[as.character(d2)]
  C_engine <- c(`0` = 0.4, `1` = 0.8)[as.character(d5)]
  C_ai <- c(`0` = 0.0, `1` = 0.8)[as.character(d6)]
  C <- min(C_engine + C_ai, 1.0)
  raw <- 0.35 * I + 0.65 * C
  raw * (60 - 1) + 1
}

get_data_coverage <- function(d2, d3){
  source_cov <- c(`1` = 0.30, `2` = 0.60, `3` = 1.00)[as.character(d3)]
  sharing_mult <- c(`1` = 0.70, `2` = 0.85, `3` = 1.00)[as.character(d2)]
  100 * source_cov * sharing_mult
}


# Cost / risk constants
ab_semi <- 3.0
bb_semi <- 1.12
hours_per_person_month <- 120
hourly_rate <- 150
k_complex <- 0.03
discount_rate_annual <- 0.07
n_months_horizon <- 24
RECURRING_KUSD_TO_USD_PER_MONTH <- 1000

theta_on_schedule <- c(d5 = 0.4, d6 = 0.2, d7 = 0.4)
theta_reliability <- c(d3 = 0.5, d5 = 0.5)
total_profit <- 1000000

sum_selected_options <- function(selected_levels, level_to_cost){
  if(length(selected_levels) == 0) return(0)
  sum(unname(level_to_cost[as.character(selected_levels)]))
}

get_kloc_total <- function(d5, d6, d7, d8){
  k5 <- case_when(d5 == 0 ~ 5, d5 == 1 ~ 15, TRUE ~ NA_real_)
  k6 <- case_when(d6 == 0 ~ 0, d6 == 1 ~ 10, TRUE ~ NA_real_)
  k7 <- case_when(d7 == 1 ~ 5, d7 == 2 ~ 7, d7 == 3 ~ 4, TRUE ~ NA_real_)
  k8 <- ifelse(d8[4] == 1, 2, 0)
  k5 + k6 + k7 + k8
}

get_programming_cost <- function(d5, d6, d7, d8){
  kloc <- get_kloc_total(d5, d6, d7, d8)
  effort_pm <- ab_semi * kloc^bb_semi
  effort_pm * hours_per_person_month * hourly_rate * (1 + k_complex)
}

get_computing_cost <- function(d1_levels, d2_levels, d6){
  m1 <- sum_selected_options(d1_levels, c(`1` = 1, `2` = 1.5, `3` = 1.2, `4` = 2))
  m2 <- sum_selected_options(d2_levels, c(`1` = 1, `2` = 1.1, `3` = 1.2))
  m6 <- case_when(d6 == 0 ~ 0, d6 == 1 ~ 1, TRUE ~ NA_real_)
  m1 + m2 + m6
}

get_third_party_cost <- function(d3_levels, d4, d5){
  m3 <- sum_selected_options(d3_levels, c(`1` = 0.1, `2` = 0.5, `3` = 2))
  m4 <- case_when(d4 == 1 ~ 1.5, d4 == 2 ~ 1, d4 == 3 ~ 0.5,
                  d4 == 4 ~ 1.2, d4 == 5 ~ 0, TRUE ~ NA_real_)
  m5 <- case_when(d5 == 0 ~ 0.5, d5 == 1 ~ 0, TRUE ~ NA_real_)
  m3 + m4 + m5
}

npv_equal_monthly_payments <- function(monthly_payment, n_months, discount_rate_per_period){
  sum(monthly_payment / (1 + discount_rate_per_period)^(1:n_months))
}

get_total_cost <- function(d1_levels, d2_levels, d3_levels, d4, d5, d6, d7, d8){
  programming <- get_programming_cost(d5, d6, d7, d8)
  computing <- get_computing_cost(d1_levels, d2_levels, d6) * RECURRING_KUSD_TO_USD_PER_MONTH
  third_party <- get_third_party_cost(d3_levels, d4, d5) * RECURRING_KUSD_TO_USD_PER_MONTH
  recurring_npv <- npv_equal_monthly_payments(computing + third_party, n_months_horizon, discount_rate_annual / 12)
  programming + recurring_npv
}

get_on_schedule_delivery_risk <- function(d5, d6, d7){
  r5 <- case_when(d5 == 0 ~ 0.01 * total_profit, d5 == 1 ~ 0.03 * total_profit, TRUE ~ NA_real_)
  r6 <- case_when(d6 == 0 ~ 0.01 * 0.2 * total_profit, d6 == 1 ~ 0.011 * 0.2 * total_profit, TRUE ~ NA_real_)
  r7 <- case_when(d7 == 1 ~ 0.03 * 0.8 * total_profit,
                  d7 == 2 ~ 0.07 * 0.8 * total_profit,
                  d7 == 3 ~ 0.06 * 0.8 * total_profit,
                  TRUE ~ NA_real_)
  as.numeric(theta_on_schedule["d5"]*r5 + theta_on_schedule["d6"]*r6 + theta_on_schedule["d7"]*r7)
}

get_reliability <- function(d3_count, d5){
  rel_d3 <- case_when(d3_count == 1 ~ 0.95, d3_count == 2 ~ 0.98, d3_count == 3 ~ 0.99, TRUE ~ NA_real_)
  rel_d5 <- case_when(d5 == 0 ~ 0.995, d5 == 1 ~ 0.98, TRUE ~ NA_real_)
  as.numeric(100 * (theta_reliability["d3"]*rel_d3 + theta_reliability["d5"]*rel_d5))
}


# EVALUATION #

evaluate <- function(xhat){
  
  d1_bits <- xhat$d1
  d2_values <- xhat$d2
  d3_bits <- xhat$d3
  d4 <- xhat$d4
  d5 <- xhat$d5
  d6 <- xhat$d6
  d7 <- xhat$d7
  d8_bits <- xhat$d8
  
  d2_level <- round(mean(d2_values - 1)) + 1
  d2_levels <- sort(unique(d2_values))
  
  d1_levels <- which(d1_bits == 1)
  d3_levels <- which(d3_bits == 1)
  d3_count <- length(d3_levels)
  
  travel_time_reduction <- get_travel_time_reduction(d3_count, d5, d6)
  prediction_accuracy <- get_prediction_accuracy(d2_level, d3_count, d6)
  response_time <- get_response_time(d2_level, d5, d6)
  data_coverage <- get_data_coverage(d2_level, d3_count)
  total_cost <- get_total_cost(d1_levels, d2_levels, d3_levels, d4, d5, d6, d7, d8_bits)
  on_schedule_delivery_risk <- get_on_schedule_delivery_risk(d5, d6, d7)
  reliability <- get_reliability(d3_count, d5)
  
  matrix(c(
    -travel_time_reduction,
    -prediction_accuracy,
    response_time,
    -data_coverage,
    total_cost,
    on_schedule_delivery_risk,
    -reliability
  ), ncol = 7)
}


# FITNESS FUNCTION #

f1 <- function(x, nobj = 7, ...){
  x <- constrain(x)
  xhat <- bit2int(x)
  evaluate(xhat)
}


# GENETIC ALGORITHM #

o <- rmoo(
  fitness = f1,
  type = "binary", algorithm = "NSGA-III",
  #Update lower/upper constraint on bitstring search
  lower = rep(0, total_bits), upper = rep(1, total_bits),
  #Settings
  monitor = TRUE, summary = TRUE,
  nObj = 7, nBits = total_bits, popSize = 100, maxiter = 200
)


# RESULTS #
o
summary(o)
o@solution