# HW4B 1.3 — architecture cost & on-schedule delivery risk (manual architectures)
# ------------------------------------------------------------------------------
# Option coding: integer levels starting at 1 (see comments per decision).
#
# Fill in decisions at the bottom of this file (arch_d1 … arch_d8), then run the
# entire script once (RStudio: Source, or select all lines and run).

library(dplyr)

# --- Global inputs ------------------------------------

# Basic COCOMO  (E = a_b * KLOC^b_b, effort in person-months)
ab_semi <- 3.0 # semi-detached
bb_semi <- 1.12 # semi-detached

# Convert person-months → labor hours → $  (REPLACE hours_per_person_month if needed)
hours_per_person_month <- 120
hourly_rate <- 150 # $/programming hour

# Optional complexity and other subjective cost driver markup on *programming* cost: fraction added to base (0 = none, 0.03 = +3%).
k_complex <- 0.03

# Nominal annual discount rate for NPV of recurring costs (decimal, e.g. 0.07 = 7%).
# Set to NA_real_ to leave npv_costs / discounted PVs as NA unless you override per call.
discount_rate_annual <- 0.07

# `get_computing_cost` / `get_third_party_cost` return thousands of USD per month (k$/month).
# Multiply by this before NPV and `total_architecture_cost` so recurring amounts match programming ($).
RECURRING_KUSD_TO_USD_PER_MONTH <- 1000

# On-schedule delivery: weights on D5, D6, D7 contributions — must sum to 1
theta_on_schedule <- c(d5 = 0.4, d6 = 0.2, d7 = 0.4) # relative importance

stopifnot(abs(sum(theta_on_schedule) - 1) < 1e-10)

# Total profit ($): one scenario-level value; loss magnitude if the event occurs is
# (fraction in `get_on_schedule_delivery_risk` tables) × this amount, for every option.
total_profit <- 1000000 # 1 million dollars assuming 250,000 households of attendees

# =============================================================================
# Cost Metrics — 
# =============================================================================

# --- Programming (one-time): Basic COCOMO on total KLOC — D5, D6, D7, D8 ------------
# Sum KLOC contributions from each decision, then E_pm = ab_semi * KLOC^bb_semi (semi-detached).

get_kloc_total <- function(d5, d6, d7, d8) {
  # D5 Decision Engine: 2 options — KLOC contribution (thousands of lines of code)
  # option 1 Use an existing routing framework (such as OpenTripPlanner, Navitia, GraphHopper)
  # option 2 Develop custom algorithm
  k5 <- case_when(
    d5 == 1 ~ 5, d5 == 2 ~ 15, TRUE ~ NA_real_)
  # D6 AI / ML: 2 options
  # option 1 No AI: rule-based and deterministic optimization
  # option 2 Use AI: Machine learning prediction model
  k6 <- case_when(
    d6 == 1 ~ 0, d6 == 2 ~ 10, TRUE ~ NA_real_)
  
  # D7 Frontend Web Layer: 3 options
  # option 1: React
  # option 2 Angular
  # option 3 Vue
  k7 <- case_when(
    d7 == 1 ~ 5, d7 == 2 ~ 7, d7 == 3 ~ 4, TRUE ~ NA_real_)
  # D8 Business Profit Model
  # option 1 LA28 funding
  # option 2 Data-as-a-service (sell data to stakeholder such as rideshare or local business)
  # option 3 Ads
  # option 4 Freemium (user pay for premium features)
  k8 <- case_when(
    d8 == 1 ~ 0, d8 == 2 ~ 0, d8 == 3 ~ 0, d8 == 4 ~ 2, TRUE ~ NA_real_)
  k5 + k6 + k7 + k8
}

#' COCOMO effort in person-months.
get_cocomo_effort_pm <- function(kloc) {
  ab_semi * kloc^bb_semi
}

get_programming_cost <- function(d5, d6, d7, d8) {
  kloc <- get_kloc_total(d5, d6, d7, d8)
  effort_pm <- get_cocomo_effort_pm(kloc)
  labor_hours <- effort_pm * hours_per_person_month
  cost_base <- labor_hours * hourly_rate
  cost_base * (1 + k_complex)
}

# --- Computing cost (recurring): thousands of dollars per month — D1, D2, D6 --------------------------------

get_computing_cost <- function(d1, d2, d6) {
  # D1 Planning Trigger: 4 options
  # option 1 Pre-event planning.
  # option 2 User-requested re-planning.
  # option 3 Event-triggered re-planning, such as major update of shuttle and metro schedule.
  # option 4 Continuous real-time re-planning.
  m1 <- case_when(
    d1 == 1 ~ 1, d1 == 2 ~ 1.5, d1 == 3 ~ 1.2, d1 == 4 ~ 2, TRUE ~ NA_real_)
  # D2 Stakholder Data Sharing: 3 options
  # option 1 Direct Point-to-Point Integrations.
  # option 2 Centralized API Gateway / Integration Layer.
  # option 3 Data Hub / Data Exchange Platform.
  m2 <- case_when(
    d2 == 1 ~ 1, d2 == 2 ~ 1.1, d2 == 3 ~ 1.2, TRUE ~ NA_real_)
  # D6 AI / ML: 2 options
  # option 1 No AI: rule-based and deterministic optimization
  # option 2 Use AI: Machine learning prediction model
  m6 <- case_when(
    d6 == 1 ~ 0, d6 == 2 ~ 1, TRUE ~ NA_real_)
  m1 + m2 + m6
}

# --- Third-party services cost (recurring): thousands of dollars per month — D3, D4, D5, D7

get_third_party_cost <- function(d3, d4, d5) {
  # D3 Traffic Data Source
  # option 1 Public government APIs.
  # option 2 Commercial navigation apps.
  # option 3 Sensor infrastructure.
  m3 <- case_when(
    d3 == 1 ~ 0.1, d3 == 2 ~ 0.5, d3 == 3 ~ 2, TRUE ~ NA_real_)
  # D4 Data Warehouse Solution
  # option 1: Snowflake;
  # option 2: Amazon Redshfit;
  # option 3: Google BigQuery;
  # option 4: Microsoft Azure Synapse;
  # option 5: or no data warehouse.
  m4 <- case_when(
    d4 == 1 ~ 1.5, d4 == 2 ~ 1, d4 == 3 ~ 0.5, d4 == 4 ~ 1.2, d4 == 5 ~ 0, TRUE ~ NA_real_)
  # D5 Decision Engine: 2 options
  # option 1 Use an existing routing framework (such as OpenTripPlanner, Navitia, GraphHopper)
  # option 2 Develop custom algorithm
  m5 <- case_when(
    d5 == 1 ~ 0.5, d5 == 2 ~ 0, TRUE ~ NA_real_)
  m3 + m4 + m5
}

# --- NPV & total cost (per-period discounting) -

#' Convert nominal annual discount rate to monthly rate (APR / 12).
#'
#' Use this when the annual figure is a \emph{nominal} rate with monthly
#' compounding of cash flows. For an \emph{effective} annual rate, use
#' \code{(1 + r_annual)^(1/12) - 1} instead.
#' @param r_annual Nominal annual rate as a decimal (e.g. 0.07 for 7\%).
#' @return Monthly rate for \code{\link{npv_equal_monthly_payments}}.
annual_nominal_to_monthly <- function(r_annual) r_annual / 12

#' Present value of a constant monthly cashflow for n months (end-of-month).
#' @param discount_rate_per_period Monthly rate r (e.g. from annual_nominal_to_monthly(0.07)).
npv_equal_monthly_payments <- function(monthly_payment, n_months, discount_rate_per_period) {
  if (is.na(discount_rate_per_period)) {
    return(NA_real_)
  }
  tibble(
    time = seq_len(n_months),
    netrev = monthly_payment,
    discount = discount_rate_per_period
  ) %>%
    mutate(npv = .data$netrev / (1 + .data$discount)^.data$time) %>%
    summarise(total = sum(.data$npv), .groups = "drop") %>%
    pull("total")
}

#' NPV of recurring costs only: add computing + third-party (USD/month), then discount the combined stream.
#' Programming is not included. \code{npv_costs} is NA when \code{discount_rate_annual} is NA.
npv_recurring_costs <- function(computing_monthly, third_party_monthly, n_months, discount_rate_annual = NA_real_) {
  r_month <- if (is.na(discount_rate_annual)) NA_real_ else annual_nominal_to_monthly(discount_rate_annual)
  recurring_total_monthly <- computing_monthly + third_party_monthly
  npv_costs <- npv_equal_monthly_payments(recurring_total_monthly, n_months, r_month)
  list(
    recurring_total_monthly = recurring_total_monthly,
    npv_costs = npv_costs,
    computing_pv_discounted = npv_equal_monthly_payments(computing_monthly, n_months, r_month),
    third_party_pv_discounted = npv_equal_monthly_payments(third_party_monthly, n_months, r_month),
    computing_horizon_undiscounted = computing_monthly * n_months,
    third_party_horizon_undiscounted = third_party_monthly * n_months,
    discount_rate_annual = discount_rate_annual,
    discount_rate_monthly = r_month
  )
}

#' Total architecture cost = programming (nominal) + NPV of combined recurring stream.
total_architecture_cost <- function(programming, npv_recurring) {
  if (is.na(npv_recurring)) NA_real_ else programming + npv_recurring
}

#' Print human-readable cost breakdown (uses values already computed by NPV helpers).
print_architecture_cost_summary <- function(
    programming,
    computing_monthly,
    third_party_monthly,
    n_months,
    npv_out,
    total_cost) {
  pv_c <- npv_out$computing_pv_discounted
  pv_t <- npv_out$third_party_pv_discounted
  npv_c <- npv_out$npv_costs
  h_c <- npv_out$computing_horizon_undiscounted
  h_t <- npv_out$third_party_horizon_undiscounted

  cat("--- Cost Metrics Summary ---\n")
  cat(sprintf("Programming cost:                    $%.2f\n", programming))
  cat(sprintf(
    "Computing (undiscounted):            $%.2f / month; $%.2f over %d months\n",
    computing_monthly, h_c, n_months
  ))
  cat(sprintf(
    "Computing (discounted PV):           $%s\n",
    if (is.na(pv_c)) "NA" else sprintf("%.2f", pv_c)
  ))
  cat(sprintf(
    "Third-party (undiscounted):          $%.2f / month; $%.2f over %d months\n",
    third_party_monthly, h_t, n_months
  ))
  cat(sprintf(
    "Third-party (discounted PV):         $%s\n",
    if (is.na(pv_t)) "NA" else sprintf("%.2f", pv_t)
  ))
  cat(sprintf(
    "NPV recurring only (computing + third-party): $%s\n",
    if (is.na(npv_c)) "NA" else sprintf("%.2f", npv_c)
  ))
  cat(sprintf(
    "Total cost (programming + NPV recurring): $%s\n",
    if (is.na(total_cost)) "NA" else sprintf("%.2f", total_cost)
  ))
  cat("--------------------\n")
  invisible(NULL)
}

# =============================================================================
# Risk Metrics
# =============================================================================

# --- On-schedule delivery risk: D5, D6, D7; p in [0,1], loss magnitude = (fraction of profit) × total_profit --

#' Branch r = expected loss ($); weighted risk = θ₅·r₅ + θ₆·r₆ + θ₇·r₇.
get_on_schedule_delivery_risk <- function(d5, d6, d7, total_profit) {
  # Branch score r (expected loss) = p × L, where
  # p is the probability of occurrence (likelihood)
  # L is the loss magnitude (severity) =(loss_fraction × total_profit), profit share lost if the adverse event occurs.
  na <- list(
    r_d5 = NA_real_, r_d6 = NA_real_, r_d7 = NA_real_,
    r_sum = NA_real_,
    contribution_d5 = NA_real_, contribution_d6 = NA_real_, contribution_d7 = NA_real_,
    risk_weighted = NA_real_
  )
  if (length(total_profit) != 1L || is.na(total_profit)) {
    return(na)
  }
  r5 <- case_when(
    d5 == 1 ~ 0.01 * 1 * total_profit,
    d5 == 2 ~ 0.03 * 1 * total_profit,
    TRUE ~ NA_real_
  )
  r6 <- case_when(
    d6 == 1 ~ 0.01 * 0.2 * total_profit,
    d6 == 2 ~ 0.011 * 0.2 * total_profit,
    TRUE ~ NA_real_
  )
  r7 <- case_when(
    d7 == 1 ~ 0.03 * 0.8 * total_profit,
    d7 == 2 ~ 0.07 * 0.8 * total_profit,
    d7 == 3 ~ 0.06 * 0.8 * total_profit,
    TRUE ~ NA_real_
  )
  t5 <- theta_on_schedule[["d5"]]
  t6 <- theta_on_schedule[["d6"]]
  t7 <- theta_on_schedule[["d7"]]
  c5 <- t5 * r5
  c6 <- t6 * r6
  c7 <- t7 * r7
  list(
    r_d5 = r5,
    r_d6 = r6,
    r_d7 = r7,
    r_sum = r5 + r6 + r7,
    contribution_d5 = c5,
    contribution_d6 = c6,
    contribution_d7 = c7,
    risk_weighted = c5 + c6 + c7
  )
}

print_on_schedule_delivery_risk_summary <- function(risk_out, total_profit) {
  cat("--- On-Schedule Delivery Risk Summary ---\n")
  fmt_money <- function(x) {
    if (length(x) != 1L || is.na(x)) "NA" else sprintf("$%.2f", x)
  }
  cat(sprintf("Total profit (loss basis):           %s\n", fmt_money(total_profit)))
  cat(sprintf("r (D5 expected loss):                %s\n", fmt_money(risk_out$r_d5)))
  cat(sprintf("r (D6 expected loss):                %s\n", fmt_money(risk_out$r_d6)))
  cat(sprintf("r (D7 expected loss):                %s\n", fmt_money(risk_out$r_d7)))
  cat(sprintf("Sum of r (D5 + D6 + D7, unweighted): %s\n", fmt_money(risk_out$r_sum)))
  cat(sprintf(
    "Weights θ: d5 = %.2f, d6 = %.2f, d7 = %.2f\n",
    theta_on_schedule[["d5"]], theta_on_schedule[["d6"]], theta_on_schedule[["d7"]]
  ))
  cat(sprintf("θ·r contributions: d5 %s; d6 %s; d7 %s\n",
    fmt_money(risk_out$contribution_d5),
    fmt_money(risk_out$contribution_d6),
    fmt_money(risk_out$contribution_d7)))
  cat(sprintf("Weighted on-schedule delivery risk (θ · r):       %s\n", fmt_money(risk_out$risk_weighted)))
  cat("-----------------------------\n")
  invisible(NULL)
}

# --- Facade: one architecture --------------------------------------------------

estimate_architecture <- function(
    d1, d2, d3, d4, d5, d6, d7, d8,
    n_months_horizon = 24L,
    discount_rate_annual,
    print_cost_summary = TRUE,
    print_risk_summary = TRUE,
    total_profit) {
  if (missing(discount_rate_annual)) {
    discount_rate_annual <- get0("discount_rate_annual", envir = .GlobalEnv, ifnotfound = NA_real_)
  }
  if (missing(total_profit)) {
    total_profit <- get0("total_profit", envir = .GlobalEnv, ifnotfound = NA_real_)
  }
  kloc_total <- get_kloc_total(d5, d6, d7, d8)
  cocomo_effort_pm <- get_cocomo_effort_pm(kloc_total)
  programming <- get_programming_cost(d5, d6, d7, d8)
  computing_k <- get_computing_cost(d1, d2, d6)
  third_party_k <- get_third_party_cost(d3, d4, d5)
  computing <- computing_k * RECURRING_KUSD_TO_USD_PER_MONTH
  third_party <- third_party_k * RECURRING_KUSD_TO_USD_PER_MONTH
  recurring_total <- computing + third_party
  recurring_horizon <- recurring_total * n_months_horizon

  npv_out <- npv_recurring_costs(computing, third_party, n_months_horizon, discount_rate_annual)
  total_cost <- total_architecture_cost(programming, npv_out$npv_costs)

  if (isTRUE(print_cost_summary)) {
    print_architecture_cost_summary(
      programming, computing, third_party, n_months_horizon, npv_out, total_cost
    )
  }

  on_schedule_risk <- get_on_schedule_delivery_risk(d5, d6, d7, total_profit)
  if (isTRUE(print_risk_summary)) {
    print_on_schedule_delivery_risk_summary(on_schedule_risk, total_profit)
  }

  cost_totals <- list(
    programming = programming,
    computing_monthly_undiscounted = computing,
    third_party_monthly_undiscounted = third_party,
    recurring_total_monthly = npv_out$recurring_total_monthly,
    computing_horizon_undiscounted = npv_out$computing_horizon_undiscounted,
    third_party_horizon_undiscounted = npv_out$third_party_horizon_undiscounted,
    computing_pv_discounted = npv_out$computing_pv_discounted,
    third_party_pv_discounted = npv_out$third_party_pv_discounted,
    npv_costs = npv_out$npv_costs,
    discount_rate_annual = npv_out$discount_rate_annual,
    discount_rate_monthly = npv_out$discount_rate_monthly,
    total_cost = total_cost
  )

  list(
    kloc_total = kloc_total,
    cocomo_effort_pm = cocomo_effort_pm,
    programming = programming,
    computing = computing,
    third_party = third_party,
    recurring_total_monthly = recurring_total,
    recurring_horizon = recurring_horizon,
    npv_costs = npv_out$npv_costs,
    total_cost = total_cost,
    computing_pv_discounted = npv_out$computing_pv_discounted,
    third_party_pv_discounted = npv_out$third_party_pv_discounted,
    discount_rate_annual = npv_out$discount_rate_annual,
    discount_rate_monthly = npv_out$discount_rate_monthly,
    cost_totals = cost_totals,
    total_profit = total_profit,
    on_schedule_risk = on_schedule_risk,
    on_schedule_delivery_risk = on_schedule_risk$risk_weighted,
    r_sum_on_schedule = on_schedule_risk$r_sum
  )
}

# =============================================================================
# YOUR ARCHITECTURE — edit only the integers below, then run the whole script
# =============================================================================

arch_d1 <- 1
arch_d2 <- 1
arch_d3 <- 1
arch_d4 <- 1
arch_d5 <- 1
arch_d6 <- 1
arch_d7 <- 1
arch_d8 <- 1

metrics <- estimate_architecture(
  arch_d1, arch_d2, arch_d3, arch_d4, arch_d5, arch_d6, arch_d7, arch_d8
)
# metrics: recurring & NPV in USD; `on_schedule_delivery_risk` = θ·r; `r_sum_on_schedule` = r_D5+r_D6+r_D7; detail in `on_schedule_risk`
