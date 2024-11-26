# Load necessary libraries
library(dplyr)
library(lme4)

# Load the pre-trained model
initial_model <- readRDS("initial_model.rds")

# Read in the new data
new_data <- read.csv("new_data.csv")

# Define the strike zone and calculate framing impacts in new data
new_data <- new_data %>%
  mutate(
    in_strike_zone = PLATELOCHEIGHT >= 1.524 & PLATELOCHEIGHT <= 3.673 &
      PLATELOCSIDE >= -0.831 & PLATELOCSIDE <= 0.831
  )

# Predict expected strike probabilities
new_data <- new_data %>%
  mutate(predicted_strike_prob = predict(initial_model, newdata = new_data, type = "response"))

# Aggregate results by CATCHER_ID and GAME_YEAR
output <- new_data %>%
  group_by(CATCHER_ID, GAME_YEAR) %>%
  summarise(
    Opportunities = n(),
    Actual_Called_Strikes = sum(PITCHCALL == "StrikeCalled", na.rm = TRUE),
    Expected_Called_Strikes = sum(predicted_strike_prob, na.rm = TRUE),
    Called_Strikes_Added = Actual_Called_Strikes - Expected_Called_Strikes
  ) %>%
  mutate(
    Called_Strikes_Added_Per_100 = round((Called_Strikes_Added / Opportunities) * 100, 3)
  ) %>%
  select(CATCHER_ID, GAME_YEAR, Opportunities, Actual_Called_Strikes, 
         Called_Strikes_Added, Called_Strikes_Added_Per_100)

# Save to CSV
write.csv(output, "new_output.csv", row.names = FALSE)