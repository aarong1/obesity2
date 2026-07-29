library(data.table)

# ─── Years of Working Life Lost (YWLL, ages 18–65) ───────────────────────────
# For each death occurring between ages 18 and 64, YWLL = 65 - age.
# Deaths outside working age do not contribute.
calculate_ywll <- function(past_populations, group_vars = as.character(), year_cut_off = NULL) {

  if (!is.null(year_cut_off)) {
    past_populations <- past_populations[year == year_cut_off, ]
  }

  pp <- setDT(copy(past_populations))

  pp <- pp[death != 0 & age >= 18 & age < 65, ]

  pp[, ywll := 65 - age]

  deaths <- pp[, .(N = .N, ywll = sum(ywll, na.rm = TRUE)),
               by = c('death_reason', 'run', group_vars, 'intervention', 'year')]

  full_death_reasons <- do.call(CJ,
    lapply(X = c('year', 'run', 'death_reason', group_vars),
           function(x) c(unique(deaths[[x]])))
  )

  names(full_death_reasons) <- c('year', 'run', 'death_reason', group_vars)

  full_death_reasons[, intervention := ifelse(run > max(run) / 2, 'intervention', 'non-intervention')]

  full_death_reasons[deaths,
    on = c('year', 'run', group_vars, 'intervention', 'death_reason'),
    `:=` (ywll = i.ywll, N = i.N)
  ]

  full_death_reasons[is.na(ywll), ywll := 0]

  ywll <- full_death_reasons[,
    .(ywll = sum(ywll, na.rm = TRUE), N = sum(N, na.rm = TRUE)),
    by = c('run', 'intervention', group_vars, 'death_reason', 'year')
  ][,
    .(ywll = mean(ywll, na.rm = TRUE)),
    by = c('intervention', group_vars, 'death_reason', 'year')
  ]

  ywll
}
