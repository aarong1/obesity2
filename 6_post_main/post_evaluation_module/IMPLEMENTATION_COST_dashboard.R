
library(data.table)
library(dplyr)
library(echarts4r)
library(reactable)
library(htmltools)
library(bslib)

# --- Logic from IMPLEMENTATION_COST.R ---

fixed_price <- 5e6
price_per_person <- 500
price_per_person_per_year <- 1000
price_per_year <- 1e6

# Note: Assuming 'past_populations', 'qalys', and 'calculate_costs_fn' exist in the environment
# just as IMPLEMENTATION_COST.R assumes.

total_time <- 3

total_people <- past_populations[
  intervention=='intervention',.(N=sum(target)) , by = .(run, intervention, year)
][, .(total_people = sum(N)), by = .(intervention, year)
]

total_people[year == min(year)+1,'fixed_cost':=fixed_price] 
total_people[year != min(year),'annual_cost':=price_per_year]
total_people[year != min(year),'ongoing_patient_cost':=total_people*price_per_person_per_year]
total_people[year == min(year)+1,'patient_cost':=total_people*price_per_person]

total_people[is.na(fixed_cost), fixed_cost:=0]
total_people[is.na(annual_cost), annual_cost:=0]
total_people[is.na(ongoing_patient_cost), ongoing_patient_cost:=0]
total_people[is.na(patient_cost), patient_cost:=0]

total_people[,total_cost := fixed_cost + annual_cost + ongoing_patient_cost + patient_cost]
total_people[,cumulative_total_cost := cumsum(total_cost)]
total_people[,year := as.character(year)]

costs <- calculate_costs_fn(past_populations, year_cut_off = NULL)

cost_per_qaly <- qalys[disease == 'combined_uw', ] %>%
  dcast(formula = year  ~intervention, value.var = 'total_uw', fill = 0L) %>%
  mutate(year = as.character(year)) %>% 
  mutate(averted = `non-intervention` - intervention) %>%
  mutate(cumulative_averted = cumsum(averted)) %>% 
  left_join(
    costs[,.(total_cost = sum(total_cost)), by = .(intervention,year)
    ][,year := as.character(year)
    ] %>%
      dcast(formula = year  ~intervention, value.var = 'total_cost', fill = 0L) %>%
      mutate(savings = `non-intervention` - intervention) %>% 
      mutate(cumulative_savings = cumsum(savings)),
    by='year'
  ) %>% 
  mutate(cost_per_qaly_gained = cumulative_savings / cumulative_averted)

icer <- cost_per_qaly[total_people,on='year'] %>%
  mutate(cost_per_qaly_gained = (cumulative_savings-cumulative_total_cost) / cumulative_averted) %>% 
  mutate(cumulative_monetised_qalys = cumulative_averted * 20000) %>%
  mutate(cost_per_qaly_gained_monetised = (cumulative_savings+cumulative_monetised_qalys-cumulative_total_cost) / cumulative_averted) %>% 
  mutate(cumulative_net_money = cumulative_savings-cumulative_total_cost) %>% 
  mutate(roi = cumulative_net_money/cumulative_total_cost)

# --- KPI Preparation ---

# Get the last row for the summary KPIs
summary_row <- tail(icer, 1)

format_currency_kpi <- function(x) {
  ifelse(abs(x) >= 1e6, 
         paste0("£", round(x / 1e6, 2), "M"), 
         paste0("£", format(round(x, 0), big.mark = ",")))
}

kpi_cards <- layout_column_wrap(
  width = 1/4,
  value_box(
    title = "Fixed Price",
    value = format_currency_kpi(fixed_price),
    showcase = bsicons::bs_icon("tag"),
    theme = "primary"
  ),
  value_box(
    title = "Price per Person",
    value = format_currency_kpi(price_per_person),
    showcase = bsicons::bs_icon("person"),
    theme = "secondary"
  ),
  value_box(
    title = "Annual Price/Person",
    value = format_currency_kpi(price_per_person_per_year),
    showcase = bsicons::bs_icon("arrow-repeat"),
    theme = "secondary"
  ),
  value_box(
    title = "Price per Year",
    value = format_currency_kpi(price_per_year),
    showcase = bsicons::bs_icon("calendar"),
    theme = "primary"
  ),
  # Derived KPIs
  value_box(
    title = "Total ROI",
    value = round(summary_row$roi, 2),
    showcase = bsicons::bs_icon("graph-up"),
    theme = ifelse(summary_row$roi > 1, "success", "danger")
  ),
  value_box(
    title = "Cumulative Net Money",
    value = format_currency_kpi(summary_row$cumulative_net_money),
    showcase = bsicons::bs_icon("piggy-bank")
  ),
  value_box(
    title = "Net Cost/QALY (Monetised)",
    value = format_currency_kpi(summary_row$cost_per_qaly_gained_monetised),
    showcase = bsicons::bs_icon("heart-pulse")
  ),
  value_box(
    title = "Cumulative Savings",
    value = format_currency_kpi(summary_row$cumulative_savings),
    showcase = bsicons::bs_icon("wallet2")
  )
)


# --- Charts ---

chart1 <- icer %>% 
  e_charts(year) %>% 
  e_line(cumulative_total_cost, name = "Cumulative Total Cost") %>% 
  e_line(cumulative_savings, name = "Cumulative Savings") %>% 
  e_line(cumulative_net_money, name = "Cumulative Net Money") %>% 
  e_line(cumulative_monetised_qalys, name = "Cumulative Monetised QALYs") %>% 
  e_tooltip(trigger = "axis") %>%
  e_title("Financial Overview") %>%
  e_legend(bottom = 0)

chart2 <- icer %>% 
  e_charts(year) %>% 
  e_line(roi, name = "ROI") %>% 
  e_format_y_axis(suffix='x') %>% 
  e_tooltip(trigger = "axis") %>%
  e_title("Return on Investment") %>%
  e_legend(bottom = 0)

chart3 <- icer %>% 
  e_charts(year) %>% 
  e_line(cost_per_qaly_gained, name = 'Cost per QALY gained') %>% 
  e_line(cumulative_averted, name = 'Cumulative QALYs', y_index = 1) %>%
  e_line(cost_per_qaly_gained_monetised, name = 'ICER (Monetised)') %>% 
  e_format_y_axis(suffix='£/QALY') %>% 
  e_tooltip(trigger = "axis") %>%
  e_title("Cost Effectiveness") %>%
  e_legend(bottom = 0)

# --- Table ---

table_data <- icer %>% 
  select(year,
         cumulative_total_cost,
         cumulative_savings,
         cumulative_net_money,
         cumulative_monetised_qalys,
         roi,
         cost_per_qaly_gained,
         cumulative_averted,
         cost_per_qaly_gained_monetised)

table_view <- reactable(
  table_data,
  columns = list(
    year = colDef(name = "Year"),
    cumulative_total_cost = colDef(name = "Cum. Total Cost", format = colFormat(currency = "GBP", separators = TRUE, digits = 0)),
    cumulative_savings = colDef(name = "Cum. Savings", format = colFormat(currency = "GBP", separators = TRUE, digits = 0)),
    cumulative_net_money = colDef(name = "Cum. Net Money", format = colFormat(currency = "GBP", separators = TRUE, digits = 0)),
    cumulative_monetised_qalys = colDef(name = "Cum. Monetised QALYs", format = colFormat(currency = "GBP", separators = TRUE, digits = 0)),
    roi = colDef(name = "ROI", format = colFormat(digits = 2)),
    cost_per_qaly_gained = colDef(name = "Cost/QALY Gained", format = colFormat(currency = "GBP", separators = TRUE, digits = 0)),
    cumulative_averted = colDef(name = "Cum. Averted", format = colFormat(digits = 0)),
    cost_per_qaly_gained_monetised = colDef(name = "ICER (Monetised)", format = colFormat(currency = "GBP", separators = TRUE, digits = 0))
  ),
  striped = TRUE,
  highlight = TRUE,
  bordered = TRUE
)


# --- Page Layout ---

page <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  h2("Implementation Cost Analysis"),
  
  card(
    card_header("Key Performance Indicators"),
    kpi_cards
  ),
  
  layout_columns(
    col_widths = c(12, 6, 6),
    card(card_header("Financial Overview"), chart1),
    card(card_header("ROI"), chart2),
    card(card_header("Cost Effectiveness"), chart3)
  ),
  
  card(
    card_header("Detailed Data"),
    table_view
  )
)

htmltools::browsable(page)
