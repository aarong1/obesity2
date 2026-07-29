library(data.table)
library(readxl)

lifetables <- read_excel("data/lifetables.xlsx",
                         sheet = "2020-2022", skip = 5)

ltnames <- c('age', 'mx', 'qx', 'lx', 'dx', 'ex', 'sex')

lifetables <- rbind(
  lifetables[1:6] |> transform(sex = 'Males') |> setNames(ltnames),
  lifetables[8:13] |> transform(sex = 'Females') |> setNames(ltnames)
)

setDT(lifetables)

# ─── Healthy Life Expectancy (HLE) ────────────────────────────────────────────
# Methodology: Sullivan method (ONS England approach).
#
# The Sullivan method estimates HLE by combining:
#   (1) age-specific proportions in good health (π_x) from the observed/simulated
#       population, and
#   (2) person-years lived at each age (L_x) from a period life table.
#
# For each age x:
#   HL_x = π_x × L_x
#
# where L_x ≈ (l_x + l_{x+1}) / 2  (person-years lived age x to x+1)
#
# HLE from birth  = Σ_x HL_x / l_0   (l_0 = 100 000, life table radix)
# HLE from age a  = Σ_{x≥a} HL_x / l_a
#
# 'good_health' person-years are counted; 'not_good_health' and NA contribute 0
# (conservative, consistent with ONS self-reported general health approach).
#
# Reference: Sullivan DF (1971). A single index of mortality and morbidity.
#            HSMHA Health Reports 86(4):347-354.
#            ONS (2023). Health state life expectancies methodology guide.
calculate_hle <- function(past_populations, group_vars = as.character(), year_cut_off = NULL) {

  if (!is.null(year_cut_off)) {
    past_populations <- past_populations[year == year_cut_off, ]
  }

  pp <- setDT(copy(past_populations))

  # Living population only (HLE is not conditioned on death event)
  pp <- pp[death == 0 & !is.na(age) & !is.na(sex), ]

  # Binary good-health indicator; NA treated as not good health
  pp[, good_health := as.integer(!is.na(health) & health == "good_health")]

  # Age- and sex-specific proportion in good health (π_x), by group_vars + run
  health_props <- pp[,
    .(prop_good = mean(good_health, na.rm = TRUE), N = .N),
    by = c("age", "sex", "run", "intervention", "year", group_vars)
  ]

  # Build lifetable with L_x (person-years lived age x to x+1)
  lt <- copy(lifetables)
  setDT(lt)
  lt[, sex := fifelse(tolower(sex) == "males", "Males", "Females")]
  setorder(lt, sex, age)
  lt[, Lx := (lx + shift(lx, type = "lead", fill = 0)) / 2, by = sex]
  lt[age == max(age), Lx := lx / mx, by = sex]          # open-ended age group
  lt[, l0 := lx[age == min(age)], by = sex]              # radix per sex

  # Join proportions onto lifetable
  health_props <- lt[, .(age, sex, lx, Lx, l0)][
    health_props, on = .(age, sex)
  ]

  # Sullivan contribution per age: π_x × L_x
  health_props[, hl_x := prop_good * Lx]

  # Sum over all ages → HLE from birth = Σ HL_x / l_0
  hle_runs <- health_props[,
    .(
      hle = sum(hl_x, na.rm = TRUE) / first(l0),
      le  = sum(Lx,   na.rm = TRUE) / first(l0)   # total LE from same lifetable
    ),
    by = c("run", "intervention", "sex", "year", group_vars)
  ]

  # Average over runs
  hle <- hle_runs[,
    .(hle = mean(hle, na.rm = TRUE),
      le  = mean(le,  na.rm = TRUE)),
    by = c("intervention", "sex", "year", group_vars)
  ]

  hle[, years_not_good := le - hle]

  hle
}
