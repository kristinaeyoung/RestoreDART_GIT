# Kristina Young April 23 2025
# Purpose: 
# TODO: 

# R version 4.2.2

# File Names
in_file <- '../RestoreDART_DATA/MIXED_MODELS/1_combined_filter_input_data.csv'

# OBJECTIVE 1: DECREASE IN TREE COVER
# Use an example of decrease tree cover as the objective and tree cover as 
# the functional group of interest
decrease_TRE <- dart_filtered_with_climate_and_performance_filtered %>%
  filter(OBJECTIVE == "decrease_TRE", fun_group == "tree_cover")%>%
  mutate(YearSinceTrt = year.index - trtYear) # Make a new column to look at year since treatment

positive_decrease_TRE <- decrease_TRE %>%
  filter(YearSinceTrt > 0 & YearSinceTrt <= 10)

# Ensure YearSinceTrt is numeric
positive_decrease_TRE <- positive_decrease_TRE %>%
  mutate(YearSinceTrt = as.numeric(YearSinceTrt))

# Make a graph for tree objective
tre_test_plot = ggplot(data = positive_decrease_TRE, 
                       aes(x = YearSinceTrt, y = point.effect, color = us_l4name)) +
  geom_hline(yintercept = 0, color = 'darkgrey') +  # Reference line at zero
  geom_jitter(width = 0.1, height = 0, alpha = 0.7) +  # Jitter x slightly, keep y fixed
  stat_summary(fun = mean, geom = "line", aes(group = us_l4name), color = "black", size = 1) + # Mean line
  facet_wrap(~us_l4name) +
  labs(x = "Years Since Treatment", 
       y = "TRE Point Effect",
       title = "Predicted Effect of Time Since Treatment") +
  theme_classic() +
  theme(legend.position = 'none')
tre_test_plot