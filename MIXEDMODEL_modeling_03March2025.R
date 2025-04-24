library(dplyr)
library(lme4)
library(nlme)
library(ggplot2)
library(ggeffects)
library(tidyr)
library(broom.mixed)
library(tidyverse)
library(broom)  
# remove.packages("lme4")
# db <- available.packages(repos = "https://cran.r-project.org/")
# tools::package_dependencies("Matrix", db = db, which = "LinkingTo", reverse = TRUE) 

# oo <- options(repos = "https://cran.r-project.org/")
# install.packages("Matrix")
# install.packages("lme4")
# options(oo)

dart <- read.csv('/project/aeroldc/RestoreDART/DART_running/trt_effects_post_trt_all_RestoreDART.csv', header = T)
objectives <- read.csv('/project/aeroldc/RestoreDART/DART_running/reduced_objectives_assigned.csv', header = T)
aridity <- read.csv('/project/aeroldc/RestoreDART/climate_data/Annual_Mean_AI_For_DART_1km.csv', header = T)
spei <- read.csv('/project/aeroldc/RestoreDART/climate_data/Annual_Mean_SPEI_For_DART_1km.csv', header = T)
preformance = read.csv('/project/aeroldc/RestoreDART/DART_running/model_performance_all_RestoreDART.csv', header = T)


## read in the ecoregion info
## this has continental US ecoregions levels 1-4 for each polygon. For each polygon, only the dominant (largest area) level 4 ecoregion was selected. 
ecoregion = read.csv("/project/aeroldc/RestoreDART/combined_spatial_data/RestoreDART_projects_with_ecoregion_info.csv", header = T)

## add in the ecoregion data - below the dart but we could join these dataframes in a bunch of locations
dart = left_join(dart,
                                ecoregion |> 
                                  select(polyID, us_l4code, us_l4name, trtYear)|>
                                  rename(PolyID = polyID), 
                                by = c('PolyID', 'trtYear'))


# Remove all NAs in OBJECTIVES
Objectives_filtered <- objectives %>%
  filter(!is.na(OBJECTIVE))

unique(Objectives_filtered$combined_TREATMENT_ASSIGNMENT)

# Rename polyID
Objectives_filtered <- Objectives_filtered %>%
  rename(PolyID = polyID_unique)


######## Climate data wrangling
# first, we need to join the spei and aridity data 
# reformat both of them from wide to long 
spei_long <- spei %>%
  pivot_longer(
    cols = starts_with("X"),  # Select all columns that start with "X"
    names_to = "Year",         # New column name for years
    values_to = "SPEI"         # New column name for SPEI values
  ) %>%
  mutate(Year = as.integer(sub("X", "", Year)))  %>% # Remove "X" and convert to integer
  select(-system.index, -plyID_g) %>%
  rename(PolyID = plyID_n)

aridity_long <- aridity %>%
  pivot_longer(
    cols = starts_with("X"),  # Select all columns that start with "X"
    names_to = "Year",         # New column name for years
    values_to = "Aridity"         # New column name for SPEI values
  ) %>%
  mutate(Year = as.integer(sub("X", "", Year)))  %>% # Remove "X" and convert to integer
  select(-system.index, -plyID_g) %>%
  rename(PolyID = plyID_n)


# join aridity and spei together to form a giant climate dataframe
climate_long <- left_join(aridity_long, spei_long, by = c('ActnDsc', 'trtYear', 'post_fr', 
                                                          'PolyID', "dat_src", 'trtID', 'Year'))

### filtering other data sets
# Filter to only the assigned Objectives
dart_filtered <- dart %>%
  filter(PolyID %in% Objectives_filtered$PolyID)
unique(dart_filtered$PolyID)

# Join the datasets
dart_filtered <- dart_filtered %>%
  left_join(Objectives_filtered %>% select(PolyID, OBJECTIVE, combined_TREATMENT_ASSIGNMENT), by = "PolyID")

unique(dart_filtered$OBJECTIVE)

# Look at the number of objectives
dart_filtered %>%
  count(OBJECTIVE) %>%
  arrange(desc(n))


## join the climate and model performance data to dart filtered
dart_filtered_with_climate = left_join(dart_filtered, 
                                       climate_long|> 
                                         select(-trtID, -trtYear, -ActnDsc, -post_fr, -dat_src) |>
                                         rename(year.index = Year),
                                       by = c('PolyID', 'year.index'))

# now add in the model performance data 
dart_filtered_with_climate_and_performance = left_join(dart_filtered_with_climate, 
                                                       preformance |> select(-nyrs_pretrt, -RMSE, -RelRMSE, -point.effect), 
                                                       by = c('PolyID', 'target_id', 'fun_group'))
# now we have r - which is our measure of model fit
# this is the pearsons correlation coefficient between observed and predicted cover BEFORE treatment in the TARGET pixel
# In Joe Smith's paper, they filtered to remove pixels which had poor fit with r<0.5
dart_filtered_with_climate_and_performance_filtered= dart_filtered_with_climate_and_performance|>
  filter(r>0.5)

# how many pixels does that remove? 
nrow(dart_filtered_with_climate_and_performance)
nrow(dart_filtered_with_climate_and_performance_filtered)
1885313 - 1841245 
# 44068 pixels were removed 
(44068/1885313)*100 
# 2.3% of pixels removed because of poor bsts model fit 


##### OBJECTIVE 1: DECREASE IN TREE COVER
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

# model with a random slope for YearSinceTrt
# model_lmer3 <- lmer(point.effect ~ YearSinceTrt + (1 | PolyID) + (1 + YearSinceTrt | PolyID:target_id), 
# data = positive_decrease_TRE, REML = FALSE)

model_1 <- lmer(point.effect ~ YearSinceTrt + (1 | PolyID), 
                    data = positive_decrease_TRE, REML = FALSE)

model_2 <- lmer(point.effect ~ YearSinceTrt + (1 | PolyID) + (1 | PolyID:target_id), 
                data = positive_decrease_TRE, REML = FALSE)

model_3 <- lmer(point.effect ~ YearSinceTrt + combined_TREATMENT_ASSIGNMENT + (1 | PolyID) + (1 | PolyID:target_id),  # Explicit nested random effects
                data = positive_decrease_TRE)

model_4 <- lmer(point.effect ~ YearSinceTrt + combined_TREATMENT_ASSIGNMENT + Aridity + (1 | PolyID) + (1 | PolyID:target_id),  # Explicit nested random effects
                 data = positive_decrease_TRE)

model_5 <- lmer(point.effect ~ YearSinceTrt + combined_TREATMENT_ASSIGNMENT + Aridity + SPEI + (1 | PolyID) + (1 | PolyID:target_id),  # Explicit nested random effects
                 data = positive_decrease_TRE)

model_6 <- lmer(point.effect ~ YearSinceTrt + combined_TREATMENT_ASSIGNMENT + Aridity + SPEI + us_l4name + (1 | PolyID) + (1 | PolyID:target_id),  # Explicit nested random effects
                 data = positive_decrease_TRE)

AIC(model_1, model_2, model_3, model_4, model_5, model_6)

summary(model_6)
plot(model_6)
qqnorm(residuals(model_6))


model_results <- data.frame( Variable = c("YearSinceTrt", "Aridity", "SPEI", "Vegetation Removal_manual;Prescribed Burn", "Vegetation Removal_mechanical;Seeding"), 
                             Estimate = c(0.022377, -0.487697, 0.706605, -27.275617, -27.402643), 
                             Std_Error = c(0.006575, 0.104140, 0.030663, 9.715203, 8.684298), 
                             t_value = c(3.403, -4.683, 23.044, -2.808, -3.155) )


ggplot(model_results, aes(x = reorder(Variable, Estimate), y = Estimate)) +
  geom_point(color = "blue", size = 4) +  # Increase point size
  geom_errorbar(aes(ymin = Estimate - 1.96 * Std_Error, ymax = Estimate + 1.96 * Std_Error),
                width = 0.3, color = "red", linewidth = 1) +  # Thicker error bars
  coord_flip() +
  labs(title = "Effect Sizes of Key Variables in LMM",
       x = "Variable",
       y = "Estimated Effect") +
  theme_minimal(base_size = 16) +  # Increase overall text size
  theme(
    axis.text = element_text(size = 14),   # Larger axis labels
    axis.title = element_text(size = 18),  # Larger axis titles
    plot.title = element_text(size = 20, face = "bold"),  # Emphasized title
    panel.grid.major = element_line(size = 0.5),  # Thicker grid lines
  )



# Generate predictions for fixed effects (YearSinceTrt)
predicted_values <- ggpredict(model_6, terms = "YearSinceTrt")

# Plot the model predictions
ggplot(predicted_values, aes(x = x, y = predicted)) +
  geom_line(color = "blue", size = 1) +  # Line for fixed effect prediction
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +  # Confidence interval
  geom_point(data = positive_decrease_TRE, aes(x = YearSinceTrt, y = point.effect), 
             alpha = 0.3, color = "black") +  # Raw data points
  labs(x = "Years Since Treatment", 
       y = "Point Effect",
       title = "Predicted Effect of Time Since Treatment") +
  theme_classic()


# looking at responses across treatments
tre_test_plot = ggplot(data = positive_decrease_TRE, 
                       aes(x = YearSinceTrt, y = point.effect, color = combined_TREATMENT_ASSIGNMENT)) +
  geom_hline(yintercept = 0, color = 'darkgrey') +  # Reference line at zero
  geom_jitter(width = 0.1, height = 0, alpha = 0.7) +  # Jitter x slightly, keep y fixed
  stat_summary(fun = mean, geom = "line", aes(group = combined_TREATMENT_ASSIGNMENT), color = "black", size = 1) + # Mean line
  facet_wrap(~combined_TREATMENT_ASSIGNMENT) +
  labs(x = "Years Since Treatment", 
       y = "TRE Point Effect",
       title = "Predicted Effect of Time Since Treatment") +
  theme_classic() +
  theme(legend.position = 'none')
tre_test_plot


##### OBJECTIVE 2: INCREASE IN PERENNAIL GRASS AND FORBS
# Use an example of increase in PFG as the objective and PFG cover as 
# the functional group of interest
# georgia replaced dart_filtered with dart_filtered_with_climate_and_performance_filtered
increase_PFG <- dart_filtered_with_climate_and_performance_filtered %>%
  filter(OBJECTIVE == "increase_PFG", fun_group == "annual_forb_and_grass_cover")

# Make a new column to look at year since treatment
increase_PFG <- increase_PFG %>%
  mutate(YearSinceTrt = year.index - trtYear)

positive_increase_PFG <- increase_PFG %>%
  filter(YearSinceTrt > 0 & YearSinceTrt <= 10)

# Ensure YearSinceTrt is numeric
positive_increase_PFG <- positive_increase_PFG %>%
  mutate(YearSinceTrt = as.numeric(YearSinceTrt))

# Fit a mixed-effects model with nested random effects (target_id within PolyID)
model_lmer <- lmer(
  point.effect ~ YearSinceTrt + (1 | PolyID) + (1 | PolyID:target_id),  # Explicit nested random effects
  data = positive_increase_PFG
)

summary(model_lmer)
# Check model output

plot(model_lmer)
qqnorm(residuals(model_lmer))

model_lmer1 <- lmer(point.effect ~ YearSinceTrt + combined_TREATMENT_ASSIGNMENT + Aridity + SPEI + (1 | PolyID) + (1 | PolyID:target_id), 
                    data = positive_increase_PFG, REML = FALSE)

model_lmer2 <- lmer(point.effect ~ YearSinceTrt + combined_TREATMENT_ASSIGNMENT + (1 | PolyID), 
                    data = positive_increase_PFG, REML = FALSE)

# model with a random slope for YearSinceTrt
# model_lmer3 <- lmer(point.effect ~ YearSinceTrt + (1 | PolyID) + (1 + YearSinceTrt | PolyID:target_id), 
# data = positive_increase_PFG, REML = FALSE)
AIC(model_lmer1, model_lmer2)
summary(model_lmer1)

# Generate predictions for fixed effects (YearSinceTrt)
predicted_values <- ggpredict(model_lmer1, terms = "YearSinceTrt")

# Plot the model predictions
ggplot(predicted_values, aes(x = x, y = predicted)) +
  geom_line(color = "blue", size = 1) +  # Line for fixed effect prediction
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +  # Confidence interval
  geom_point(data = positive_increase_PFG, aes(x = YearSinceTrt, y = point.effect), 
             alpha = 0.3, color = "black") +  # Raw data points
  labs(x = "Years Since Treatment", 
       y = "Point Effect",
       title = "Predicted Effect of Time Since Treatment") +
  theme_minimal()

head(positive_increase_PFG)

# pfg_test_plot = ggplot(data = positive_increase_PFG, 
#                        aes(x = YearSinceTrt, y = point.effect, color = combined_TREATMENT_ASSIGNMENT)) +
#   geom_jitter(width = 0.2, height = 0, alpha = 0.7) +  # Jitter x slightly, keep y fixed
#   labs(x = "Years Since Treatment", 
#        y = "Point Effect",
#        title = "Predicted Effect of Time Since Treatment") +
#   theme_minimal()
# pfg_test_plot

# Georgia exploring cover by treatment type
pfg_test_plot = ggplot(data = positive_increase_PFG, 
                       aes(x = YearSinceTrt, y = point.effect, color = combined_TREATMENT_ASSIGNMENT)) +
  geom_hline(yintercept = 0, color = 'darkgrey') +  # Reference line at zero
  geom_jitter(width = 0.1, height = 0, alpha = 0.7) +  # Jitter x slightly, keep y fixed
  stat_summary(fun = mean, geom = "line", aes(group = combined_TREATMENT_ASSIGNMENT), color = "black", size = 1) + # Mean line
  facet_wrap(~combined_TREATMENT_ASSIGNMENT) +
  labs(x = "Years Since Treatment", 
       y = "PFG Point Effect",
       title = "Predicted Effect of Time Since Treatment") +
  theme_classic() +
  theme(legend.position = 'none')
pfg_test_plot


