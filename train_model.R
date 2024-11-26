# Load necessary libraries
library(dplyr)
library(lme4)

# Load and preprocess the data
data <- Catcher_data

# Define the strike zone and calculate framing impacts
data <- data %>%
  mutate(
    in_strike_zone = PLATELOCHEIGHT >= 1.524 & PLATELOCHEIGHT <= 3.673 &
      PLATELOCSIDE >= -0.831 & PLATELOCSIDE <= 0.831,
    positive_framing = !in_strike_zone & PITCHCALL == "StrikeCalled",
    negative_framing = in_strike_zone & PITCHCALL == "BallCalled"
  )

# Rescale predictors to improve model stability
data <- data %>%
  mutate(
    PLATELOCHEIGHT = scale(PLATELOCHEIGHT),
    PLATELOCSIDE = scale(PLATELOCSIDE),
    RELHEIGHT = scale(RELHEIGHT),
    RELSPEED = scale(RELSPEED),
    INDUCEDVERTBREAK = scale(INDUCEDVERTBREAK),
    HORZBREAK = scale(HORZBREAK)
  )

# Set control parameters to assist with convergence
control_params <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))

# Fit the mixed-effects model
initial_model <- glmer(PITCHCALL == "StrikeCalled" ~ PLATELOCHEIGHT + PLATELOCSIDE + 
                         RELHEIGHT + RELSPEED + INDUCEDVERTBREAK + HORZBREAK + 
                         (1 | CATCHER_ID),
                       data = data, family = binomial, control = control_params)

 # Save the trained model to a file
saveRDS(initial_model, "initial_model.rds")

# Predict expected strike probabilities
data <- data %>%
  mutate(predicted_strike_prob = predict(initial_model, data, type = "response"))

# Aggregate by CATCHER_ID and GAME_YEAR
yearly_data <- data %>%
  group_by(CATCHER_ID, GAME_YEAR) %>%
  summarise(
    Opportunities = n(),
    Actual_Called_Strikes = sum(PITCHCALL == "StrikeCalled", na.rm = TRUE),
    Expected_Called_Strikes = sum(predicted_strike_prob, na.rm = TRUE),
    Called_Strikes_Added = Actual_Called_Strikes - Expected_Called_Strikes
  )

# Final formatting with rounding
output <- yearly_data %>%
  mutate(
    Called_Strikes_Added_Per_100 = round((Called_Strikes_Added / Opportunities) * 100, 3),
    Opportunities = round(Opportunities),
    Actual_Called_Strikes = round(Actual_Called_Strikes),
    Called_Strikes_Added = round(Called_Strikes_Added)
  ) %>%
  select(CATCHER_ID, GAME_YEAR, Opportunities, Actual_Called_Strikes, 
         Called_Strikes_Added, Called_Strikes_Added_Per_100)

# Save the output to Excel for reference
write_xlsx(output, "output_data.xlsx")