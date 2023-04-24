library(ggplot2)
library(dplyr)
#Import Data
df <- read.csv("STT-3850-101 Project Data - Form Responses 1.csv")
# Rename columns
colnames(df) <- c("time","major","year","water","nonwater")
# Delete timestamp
df <- subset(df, select = -c(time))
# df will be our working data frame
# We want to find the correlations between major and year on the consumption of water and beverages

# First let's see the plot of water vs beverages

ggplot(data = df, aes(x = water, y = nonwater)) +
  geom_point() +
  # Remove the 100 cups of water/day outlier
  xlim(0, 25) +
  geom_smooth(method = "lm")

water_nonwater_model_year = lm(water ~ nonwater + year, data = df)
summary(water_nonwater_model_year)
get_regression_table(water_nonwater_model_year)


# Based on initial appearance, it would seem that there is, logically, a negative correlation between
# water and nonwater consumption.

# Factors major by median of water by major
water_orderedF <- with(df, reorder(major,
                                  water,
                                  median))

# Factors major by median of nonwater by major
nonwater_orderedF <- with(df, reorder(major,
                                     nonwater,
                                     median))

# Applies the factored water
water_ordered <- df
water_ordered$major <- factor(water_ordered$major,
                              levels = levels(water_orderedF))

# Applies the factored nonwater
nonwater_ordered <- df
nonwater_ordered$major <- factor(nonwater_ordered$major,
                                 levels = levels(nonwater_orderedF))

# Boxplot for water by major
ggplot(data = water_ordered, aes(x = major, y = water)) +
  geom_boxplot() +
  ylim(0, 25)

# Boxplot for nonwater by major
ggplot(data = nonwater_ordered, aes(x = major, y = nonwater)) +
  geom_boxplot()

new_data <- df %>%
  mutate(upper = ifelse(year == "Freshman" | year == "Sophomore", 0, 1)) %>%
  mutate(major = ifelse(major == "College of Fine and Applied Arts" | major == "Hayes School of Music", "College of Fine and Applied Art/Hayes School of Music", major))

new_data <- new_data %>%
  select(major, upper, water, nonwater)

ggplot(data = new_data, aes(x = upper == 1, y = water)) +
  ylim(0, 25) +
  geom_boxplot()

ggplot(data = new_data, aes(x = major, y = water)) +
  ylim(0, 25) +
  geom_boxplot()