library(data.table)

# Potential Years of Life Lost (PYLL, deaths before age 75)
# For each death at age < 75, PYLL = 75 - age.
calculate_pyll <- function(past_populations, group_vars = as.character(), year_cut_off = NULL) {

	if (!is.null(year_cut_off)) {
		past_populations <- past_populations[year == year_cut_off, ]
	}

	pp <- setDT(copy(past_populations))

	# Keep only premature deaths.
	pp <- pp[death != 0 & !is.na(age) & age < 75, ]

	if (nrow(pp) == 0) {
		return(data.table())
	}

	pp[, pyll := pmax(0, 75 - age)]

	deaths <- pp[, .(N = .N, pyll = sum(pyll, na.rm = TRUE)),
							 by = c('death_reason', 'run', group_vars, 'intervention', 'year')]

	full_death_reasons <- do.call(
		CJ,
		lapply(X = c('year', 'run', 'death_reason', group_vars),
					 function(x) c(unique(deaths[[x]])))
	)

	names(full_death_reasons) <- c('year', 'run', 'death_reason', group_vars)

	full_death_reasons[, intervention := ifelse(run > max(run) / 2, 'intervention', 'non-intervention')]

	full_death_reasons[deaths,
										 on = c('year', 'run', group_vars, 'intervention', 'death_reason'),
										 `:=`(pyll = i.pyll, N = i.N)]

	full_death_reasons[is.na(pyll), pyll := 0]
	full_death_reasons[is.na(N), N := 0]

	pyll <- full_death_reasons[,
														 .(pyll = sum(pyll, na.rm = TRUE), N = sum(N, na.rm = TRUE)),
														 by = c('run', 'intervention', group_vars, 'death_reason', 'year')
	][,
		.(pyll = mean(pyll, na.rm = TRUE)),
		by = c('intervention', group_vars, 'death_reason', 'year')
	]

	pyll
}

# Example:
# calculate_pyll(past_populations)
# calculate_pyll(past_populations, group_vars = c('sex', 'mdm_quintile_soa_name'))
# calculate_pyll(past_populations, year_cut_off = 2030)