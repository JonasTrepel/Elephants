library(data.table)
library(tidyverse)

dt_1 <- fread("data/processed_data/data_fragments/steps_1hr_incl_random.csv") %>% 
  filter(case_ == TRUE) %>% 
  group_by(individual_id) %>% filter(sum(case_) > 365) %>% ungroup()
dt_3 <- fread("data/processed_data/data_fragments/steps_3hrs_incl_random.csv") %>% 
  filter(case_ == TRUE) %>% group_by(individual_id) %>% filter(sum(case_) > 365) %>% ungroup()
dt_12 <- fread("data/processed_data/data_fragments/steps_12hrs_incl_random.csv") %>% 
  filter(case_ == TRUE) %>% group_by(individual_id) %>% filter(sum(case_) > 365) %>% ungroup()

unique(dt_12$individual_id)

dt_loc <- fread("data/processed_data/clean_data/all_location_data.csv") %>% 
  mutate(month = month(date_time)) %>% 
  select(-c(lon, lat, date_time, month, obs_id)) %>% 
  unique()

# Step length 
dt_1 %>% 
  summarize(median_sl = median(sl_km), 
            mean_sl = mean(sl_km), 
            q25_sl = quantile(sl_km, .25), 
            q75_sl = quantile(sl_km, .75), 
            n = n_distinct(individual_id))

hist(dt_1$sl_km)

dt_1 %>% group_by(sex) %>% 
  summarize(median_sl = median(sl_km), 
            mean_sl = mean(sl_km), 
            q25_sl = quantile(sl_km, .25), 
            q75_sl = quantile(sl_km, .75), 
            n = n_distinct(individual_id))


dt_3 %>% 
  summarize(median_sl = median(sl_km), 
            mean_sl = mean(sl_km), 
            q25_sl = quantile(sl_km, .25), 
            q75_sl = quantile(sl_km, .75), 
            n = n_distinct(individual_id))

hist(dt_3$sl_km)

dt_3 %>% group_by(sex) %>% 
  summarize(median_sl = median(sl_km), 
            mean_sl = mean(sl_km), 
            q25_sl = quantile(sl_km, .25), 
            q75_sl = quantile(sl_km, .75), 
            n = n_distinct(individual_id))


dt_12 %>% 
  summarize(median_sl = median(sl_km), 
            mean_sl = mean(sl_km), 
            q25_sl = quantile(sl_km, .25), 
            q75_sl = quantile(sl_km, .75), 
            n = n_distinct(individual_id))

hist(dt_12$sl_km)

dt_12 %>% group_by(sex) %>% 
  summarize(median_sl = median(sl_km), 
            mean_sl = mean(sl_km), 
            q25_sl = quantile(sl_km, .25), 
            q75_sl = quantile(sl_km, .75), 
            n = n_distinct(individual_id))

# Function to create summary tables
make_summary <- function(dt, group_vars = NULL) {
  if (!is.null(group_vars)) {
    dt_grouped <- dt %>%
      group_by(across(all_of(group_vars)))
  } else {
    dt_grouped <- dt
  }
  
  summary <- dt_grouped %>%
    summarize(
      N = n_distinct(individual_id),
      step_length = sprintf("%.2f (%.2f-%.2f)", 
                            median(sl_km, na.rm=TRUE), 
                            quantile(sl_km, .25, na.rm=TRUE), 
                            quantile(sl_km, .75, na.rm=TRUE)),
      turning_angle = if("ta_" %in% names(dt)) sprintf("%.2f (%.2f-%.2f)", 
                                                       median(ta_, na.rm=TRUE), 
                                                       quantile(ta_, .25, na.rm=TRUE), 
                                                       quantile(ta_, .75, na.rm=TRUE)) else NA,
      hr_locoh = if("hr_locoh_area_km2" %in% names(dt)) sprintf("%.2f (%.2f-%.2f)", 
                                                                median(hr_locoh_area_km2, na.rm=TRUE), 
                                                                quantile(hr_locoh_area_km2, .25, na.rm=TRUE), 
                                                                quantile(hr_locoh_area_km2, .75, na.rm=TRUE)) else NA,
      hr_mcp = if("hr_mcp_area_km2" %in% names(dt)) sprintf("%.2f (%.2f-%.2f)", 
                                                            median(hr_mcp_area_km2, na.rm=TRUE), 
                                                            quantile(hr_mcp_area_km2, .25, na.rm=TRUE), 
                                                            quantile(hr_mcp_area_km2, .75, na.rm=TRUE)) else NA,
      .groups = "drop"
    )
  return(summary)
}

# Overall summaries
overall_summary <- bind_rows(
  cbind(Time = "1hr", make_summary(dt_1)),
  cbind(Time = "3hrs", make_summary(dt_3)),
  cbind(Time = "12hrs", make_summary(dt_12))
)

# Summaries by sex
by_sex_summary <- bind_rows(
  cbind(Time = "1hr", make_summary(dt_1, group_vars = "sex") %>% rename(Sex = sex)),
  cbind(Time = "3hrs", make_summary(dt_3, group_vars = "sex") %>% rename(Sex = sex)),
  cbind(Time = "12hrs", make_summary(dt_12, group_vars = "sex") %>% rename(Sex = sex))
)

# Summaries by season
by_season_summary <- bind_rows(
  cbind(Time = "1hr", make_summary(dt_1, group_vars = "season")),
  cbind(Time = "3hrs", make_summary(dt_3, group_vars = "season")),
  cbind(Time = "12hrs", make_summary(dt_12, group_vars = "season"))
)

# Summaries by season and sex
by_season_sex_summary <- bind_rows(
  cbind(Time = "1hr", make_summary(dt_1, group_vars = c("season", "sex")) %>% rename(Sex = sex)),
  cbind(Time = "3hrs", make_summary(dt_3, group_vars = c("season", "sex")) %>% rename(Sex = sex)),
  cbind(Time = "12hrs", make_summary(dt_12, group_vars = c("season", "sex")) %>% rename(Sex = sex))
)

# Save as CSV
dir.create("output", showWarnings = FALSE)
write.csv(overall_summary, "builds/model_outputs/steps_overall_summary.csv", row.names = FALSE)
write.csv(by_sex_summary, "builds/model_outputs/steps_sex_based_summary.csv", row.names = FALSE)
write.csv(by_season_summary, "builds/model_outputs/steps_seasonal_summary.csv", row.names = FALSE)
write.csv(by_season_sex_summary, "builds/model_outputs/steps_season_sex_summary.csv", row.names = FALSE)
