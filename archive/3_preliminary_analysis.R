# Kristina Young April 23 2025
# Purpose: Visualize the data prior to model fitting. Examining spatial and temporal autocorrelation
# TODO: 

library(lme4) # lme4 version 1.1-31
# R version 4.2.2

# File Names
in_file <- ('../RestoreDART_DATA/MIXED_MODELS/2_combined_filter_input_data.csv')

modeling_data <- read.csv(in_file)

# OBJECTIVE 1: DECREASE IN TREE COVER
# Use an example of decrease tree cover as the objective and tree cover as 
# the functional group of interest
decrease_TRE <- modeling_data %>%
  filter(OBJECTIVE == "decrease_TRE", fun_group == "tree_cover")

positive_decrease_TRE <- decrease_TRE %>%
  filter(YearSinceTrt > 0 & YearSinceTrt <= 10)

### Data Visualizations

## Boxplot: Treatment types
ggplot(positive_decrease_TRE, aes(x = combined_TREATMENT_ASSIGNMENT, y = point.effect)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Point Effect by Treatment Assignment",
       x = "Treatment Assignment", y = "Point Effect") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatterplot: pre-treatment cover vs. effect
ggplot(positive_decrease_TRE, aes(x = mean_cover_5YBT, y = point.effect)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Effect vs. Pre-Treatment Mean Cover")

# Boxpot of ecoregions
ggplot(positive_decrease_TRE, aes(x = us_l4name, y = point.effect)) +
  geom_boxplot() +
  theme_minimal() +
  coord_flip() +
  labs(title = "Point Effect by US Level IV Ecoregion")

positive_decrease_TRE <- positive_decrease_TRE |>
  mutate(aridity_bin = cut(Aridity, breaks = 4))

# Boxplot treatment by binned aridity
ggplot(positive_decrease_TRE, aes(x = aridity_bin, y = point.effect, fill = combined_TREATMENT_ASSIGNMENT)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Point Effect by Aridity Bin and Treatment",
       x = "Aridity Bin", y = "Point Effect") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##### Modeling decreases in tree cover

# model with a random slope for YearSinceTrt
# model_lmer3 <- lmer(point.effect ~ YearSinceTrt + (1 | PolyID) + (1 + YearSinceTrt | PolyID:target_id), 
# data = positive_decrease_TRE, REML = FALSE)


form0 <- point.effect ~ (1 | target_id/YearsSinceTrt)

model_0 <- lmer(formula = form0, data = positive_decrease_TRE, REML = FALSE)

#model_2 <- lmer(point.effect ~ YearSinceTrt + (1 | PolyID) + (1 | PolyID:target_id), 
#                data = positive_decrease_TRE, REML = FALSE)

#model_3 <- lmer(point.effect ~ YearSinceTrt + combined_TREATMENT_ASSIGNMENT + (1 | PolyID) + (1 | PolyID:target_id),  # Explicit nested random effects
#                data = positive_decrease_TRE)

#model_4 <- lmer(point.effect ~ YearSinceTrt + combined_TREATMENT_ASSIGNMENT + Aridity + (1 | PolyID) + (1 | PolyID:target_id),  # Explicit nested random effects
#                data = positive_decrease_TRE)

#model_5 <- lmer(point.effect ~ YearSinceTrt + combined_TREATMENT_ASSIGNMENT + Aridity + SPEI + (1 | PolyID) + (1 | PolyID:target_id),  # Explicit nested random effects
#                data = positive_decrease_TRE)

#model_6 <- lmer(point.effect ~ YearSinceTrt + combined_TREATMENT_ASSIGNMENT + Aridity + SPEI + us_l4name + (1 | PolyID) + (1 | PolyID:target_id),  # Explicit nested random effects
#                data = positive_decrease_TRE)

#model_7 <- lmer(point.effect ~ YearSinceTrt + combined_TREATMENT_ASSIGNMENT + Aridity + SPEI + us_l4name + mean_cover_5YBT + (1 | PolyID) + (1 | PolyID:target_id),  # Explicit nested random effects
#                data = positive_decrease_TRE)

#AIC(model_1, model_2, model_3, model_4, model_5, model_6, model_7)

summary(model_7)
plot(model_7)
qqnorm(residuals(model_7))

#ape::Moran.I
#stats::acf

library(broom.mixed)
library(ggplot2)
library(dplyr)
library(forcats)

# Tidy the model to extract fixed effects
model_fixed <- tidy(model_7, effects = "fixed")

# Reorder terms for better readability
model_fixed <- model_fixed %>%
  mutate(term = fct_reorder(term, estimate))

# Plot the fixed effects with confidence intervals
ggplot(model_fixed, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - std.error * 1.96,
                     xmax = estimate + std.error * 1.96), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Fixed Effects from Mixed Model",
    x = "Estimate (± 95% CI)", y = "Predictor"
  )


# Extract fixed effects and standard errors
coefs <- summary(model_7)$coefficients
fixed_df <- data.frame(
  term = rownames(coefs),
  estimate = coefs[, "Estimate"],
  std_error = coefs[, "Std. Error"]
)

# Compute 95% CI
fixed_df$lower <- fixed_df$estimate - 1.96 * fixed_df$std_error
fixed_df$upper <- fixed_df$estimate + 1.96 * fixed_df$std_error

# Reorder for plotting
fixed_df$term <- factor(fixed_df$term, levels = fixed_df$term[order(fixed_df$estimate)])

# Plot
ggplot(fixed_df, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Fixed Effects from Mixed Model",
       x = "Estimate (± 95% CI)", y = "Predictor")


# Look at responses across treatments
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

positive_increase_PFG <- increase_PFG %>%
  filter(YearSinceTrt > 0 & YearSinceTrt <= 10)

##### OBJECTIVE 2: INCREASE IN PERENNAIL GRASS AND FORBS

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