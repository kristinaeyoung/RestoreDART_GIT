# Kristina Young April 23 2025
# Purpose: Visualize the data prior to model fitting. Examining spatial and temporal autocorrelation
# TODO: 

# R version 4.2.2

##### OBJECTIVE 1: DECREASE IN TREE COVER

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