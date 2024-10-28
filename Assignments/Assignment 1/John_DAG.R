library(mgcv)

#---------------------------------------------------------------------------------------------------------------------
# load data
library(readr)
data <- read_csv("Assignments/Assignment 1/Crime_Data_from_2020_to_Present.csv",
                       locale = locale(tz = "America/Los_Angeles"))

head(data)

# Ensure Correct Data Types
data$"Vict.Age" <- as.character(data$"Vict Age")
data$"TIME.OCC" <- as.character(data$"TIME OCC")

# Convert to Numeric and Handle Conversion Issues
data$"Vict.Age" <- as.numeric(data$"Vict Age")
data$"TIME.OCC" <- as.numeric(data$"TIME OCC")

# Remove Rows Where Conversion Introduced NAs
data_clean <- data[!is.na(data$"Vict.Age") & !is.na(data$"TIME.OCC"), ]

# Remove Infinite Values
data_clean <- data_clean[is.finite(data_clean$"Vict.Age") & is.finite(data_clean$"TIME.OCC"), ]

# Verify Data is Not Empty
if (nrow(data_clean) == 0) {
    stop("No data available after cleaning. Please check your data for issues.")
}

# Plot
plot(
    data_clean$"Vict.Age",
    data_clean$"TIME.OCC",
    main = "Scatter Plot",
    xlab = "Vict.Age",
    ylab = "TIME.OCC"
)

#-------------------------------------------------------------------------------

# Exploratory Data Analysis
summary(data)
plot(data$"Vict.Age", data$"TIME.OCC")

# Simple Regression
model <- lm(TIME.OCC ~ Vict.Age, data=data)
summary(model)

# Adjusted Regression
adjusted_model <- lm(TIME.OCC ~ Vict.Age + Crm.Cd, data=data)
summary(adjusted_model)

#--------------------------------------------------------------
# Install and load the hexbin package if not already installed
library(hexbin)

# Create a hexbin object
hb <- hexbin(crime_data$Vict.Age, data_clean$TIME.OCC, xbins = 50)

# Plot the hexbin object
plot(hb,
     main = "Hexbin Plot of Victim Age vs Time of Occurrence",
     xlab = "Victim Age",
     ylab = "Time of Occurrence")

#-----------------------------------------------------------------------------
correlation <- cor.test(data$"Vict.Age", data$"TIME.OCC")
print(correlation)

#-----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)

# filter out rows where Vict Age is <= 0
crime_data <- crime_data %>% filter(Vict.Age > 0)

# convert TIME OCC to numeric, check for issues with parsing, and then remove rows with issues
crime_data$TIME.OCC <- as.numeric(crime_data$TIME.OCC)
#problems(crime_data)
crime_data <- crime_data[!is.na(crime_data$TIME.OCC), ]

# convert to minutes since midnight
crime_data$time_in_minutes <- (crime_data$TIME.OCC %/% 100) * 60 + (crime_data$TIME.OCC %% 100)
# explicit conversion of time_in_minutes to numeric
crime_data$time_in_minutes <- as.numeric(crime_data$time_in_minutes)

# str(crime_data$time_in_minutes)

# TIME_OCC is a numeric column in HHMM format
ggplot(crime_data, aes(x = Vict.Age, y = time_in_minutes)) +
    geom_point() +
    #geom_density2d(color = "blue") + 
    labs(x = "Victim Age", y = "Time Occured", title = "Victim Age to Time Occured for Crime") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(0, 100, by = 5))
# %/% 100 extracts the hour from HHMM.
# %% 100 extracts the minutes from HHMM.
# scale_y_continuous() helps format the y-axis ticks into a readable HH:MM format.
scale_y_continuous(
    breaks = seq(0, 1440, by = 30),
    labels = function(x) sprintf("%02d:%02d", x %/% 60, x %% 60)
)

#---------------------------------------------------------------------------------
library(dagitty)
library(ggdag)
library(ggplot2)

# Convert the dagitty DAG to a ggdag object
gg_dag <- dagitty::dagitty('
  dag {
    Date_Occ -> Date_Rep
    Vict_Age -> Area
    Vict_Descent -> Area
    Date_Occ -> Time_Occ
    Vict_Age -> Crm_Cd
    Vict_Descent -> Crm_Cd
    Vict_Sex -> Crm_Cd
    Weapon_Used -> Crm_Cd
    Time_Occ -> Crm_Cd
    Vict_Age -> Status
    Vict_Sex -> Status
    Weapon_Used -> Status
    Area -> Status
  }
')

# Plot with ggdag and customize arrow size
ggdag(gg_dag) +
    geom_dag_edges_link(arrow = grid::arrow(length = grid::unit(0.3, "cm"), 
                                            type = "closed"), linewidth = 1.5) +
    geom_dag_point() +
    geom_dag_text() +
    theme_dag()

plot(gg_dag)
