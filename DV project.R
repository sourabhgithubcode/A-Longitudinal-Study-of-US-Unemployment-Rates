# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)

# Load the dataset

unemployment_data <- read.csv("/Users/HP/Downloads/ECO_520 BA Tools 2 Peter B/Data/Unemployment in America Per US State 3.csv")



# Check the column names
colnames(unemployment_data)


# Convert necessary columns from string to numeric using correct column names
unemployment_data <- unemployment_data %>%
  mutate(across(c(`Total.Civilian.Non.Institutional.Population.in.State.Area`,
                  `Total.Civilian.Labor.Force.in.State.Area`,
                  `Total.Employment.in.State.Area`,
                  `Total.Unemployment.in.State.Area`), 
                ~ as.numeric(gsub(",", "", .))))

# Handle missing values (for simplicity, let's fill with median of respective columns)
unemployment_data <- unemployment_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Aggregate the data to calculate the average unemployment rate per year for each state
unemployment_trends <- unemployment_data %>%
  group_by(State.Area, Year) %>%
  summarise(avg_unemployment_rate = mean(`Percent.....of.Labor.Force.Unemployed.in.State.Area`, na.rm = TRUE))

# Visualize the trends over time for a few selected states
ggplot(unemployment_trends %>% filter(State.Area %in% c("California", "Texas", "New York", "Florida")), 
       aes(x = Year, y = avg_unemployment_rate, color = State.Area)) +
  geom_line() +
  labs(title = "Unemployment Rate Trends Over Time", x = "Year", y = "Average Unemployment Rate (%)") +
  theme_minimal()

# Analysis of trends
model <- lm(avg_unemployment_rate ~ Year + State.Area, data = unemployment_trends)
summary(model)

# Save the processed data to a CSV file
write_csv(unemployment_trends, "/Users/HP/Downloads/ECO_520 BA Tools 2 Peter B/Data/unemployment_trends.csv")




# Convert necessary columns from string to numeric using correct column names
unemployment_data <- unemployment_data %>%
  mutate(across(c(`Total.Civilian.Non.Institutional.Population.in.State.Area`,
                  `Total.Civilian.Labor.Force.in.State.Area`,
                  `Total.Employment.in.State.Area`,
                  `Total.Unemployment.in.State.Area`), 
                ~ as.numeric(gsub(",", "", .))))

# Handle missing values (for simplicity, let's fill with median of respective columns)
unemployment_data <- unemployment_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# View the first few rows of the processed data
head(unemployment_data)

# Save the processed data to a CSV file
write_csv(unemployment_data, "/Users/HP/Downloads/ECO_520 BA Tools 2 Peter B/Data/unemployment_trends1.csv")




# Check the structure of the data
str(unemployment_data)

# Extract unique state names from the dataset
unique_states <- unique(unemployment_data$State.Area)
print(unique_states)


# List of recognized state names
recognized_states <- c(
  'Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'Florida', 'Georgia',
  'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts',
  'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey',
  'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island',
  'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming'
)

# Find missing and additional states
missing_states <- setdiff(unique_states, recognized_states)
additional_states <- setdiff(recognized_states, unique_states)

print("Missing States in Dataset:")
print(missing_states)

print("Additional States Not in Dataset:")
print(additional_states)

# Example mapping dictionary for common discrepancies
state_mapping <- c(
  'CA' = 'California',
  'TX' = 'Texas',
  'NY' = 'New York'
  # Add other mappings as needed
)

# Update state names in the dataset
unemployment_data <- unemployment_data %>%
  mutate(State.Area = ifelse(State.Area %in% names(state_mapping), state_mapping[State.Area], State.Area))

# Verify the changes
unique_states_corrected <- unique(unemployment_data$State.Area)
print(unique_states_corrected)



# Correct the state names
unemployment_data <- unemployment_data %>%
  mutate(State.Area = case_when(
    State.Area == "Los Angeles County" ~ "California",
    State.Area == "New York city" ~ "New York",
    TRUE ~ State.Area
  ))

# Verify the changes
unique_states_corrected <- unique(unemployment_data$State.Area)
print(unique_states_corrected)

# Save the corrected dataset
write.csv(unemployment_data, '/Users/HP/Downloads/ECO_520 BA Tools 2 Peter B/Data/corrected_unemployment_trends.csv', row.names = FALSE)

