# ========================================
# DAY 3: Time Series Visualization
# Sales Trend Analysis Over Time
# ========================================

# packages
library(tidyverse)
library(lubridate)  # For working with dates
library(scales)


# PART 1: CREATE TIME SERIES DATA



set.seed(456)

# We need to enerate dates for entire year 2023
dates <- seq(from = as.Date("2023-01-01"), 
             to = as.Date("2023-12-31"), 
             by = "day")


#Set up daily sales data with seasonal pattern
daily_sales <- tibble(
  date = dates,
   daily_revenue = 50000 + 
    10000 * sin(2 * pi * (as.numeric(date) - as.numeric(min(date))) / 365) +  # Seasonal wave
    rnorm(365, 0, 5000)  # Random daily variation
) %>%
  mutate(
    daily_revenue = pmax(daily_revenue, 20000),  # Ensure no negative sales
    
    year = year(date),
    month = month(date, label = TRUE),  # Jan, Feb, Mar...
    month_num = month(date),
    quarter = quarter(date),
    day_of_week = wday(date, label = TRUE),  # Mon, Tue, Wed...
    week = week(date),
    day_of_month = day(date)
  )

#The data structure
glimpse(daily_sales)
head(daily_sales, 10)


# PART 2: AGGREGATE TO MONTHLY LEVEL

# To show trend of the data
monthly_sales <- daily_sales %>%
  group_by(year, month, month_num) %>%  # Group by month
  summarize(
    monthly_revenue = sum(daily_revenue),  # Total revenue per month
    avg_daily_revenue = mean(daily_revenue),  # Average per day
    days_in_month = n(),  # Count days
    .groups = "drop"
  ) %>%
  arrange(month_num)  # Sort chronologically

print("Monthly Sales Summary:")
print(monthly_sales)


# PART 3: VISUALIZATIONS

plot_daily <- ggplot(daily_sales, aes(x = date, y = daily_revenue)) +
  geom_line(color = "steelblue", alpha = 0.6) +  # Line for trend
  # WHY alpha=0.6: Semi-transparent to see overlaps
  geom_smooth(method = "loess", color = "red", se = FALSE) +  # Smooth trend line
  # WHY geom_smooth: Shows underlying trend without noise
  # WHAT loess: Local regression, fits curve to data
  scale_y_continuous(labels = dollar_format()) +  # Format as dollars
  labs(
    title = "Daily Sales Revenue - 2023",
    subtitle = "Blue line = actual sales, Red line = trend",
    x = "Date",
    y = "Daily Revenue"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(plot_daily)

# VISUALIZATION 2: Monthly Revenue Bar Chart
# WHY: Compare months easily
# WHAT: Bar chart showing total revenue per month

plot_monthly <- ggplot(monthly_sales, aes(x = month, y = monthly_revenue)) +
  geom_col(fill = "steelblue") +  # Bar chart
  # WHY geom_col: Use when you have exact values
  geom_text(aes(label = dollar(monthly_revenue, scale = 1e-3, suffix = "K")), 
            vjust = -0.5, size = 3) +  # Add labels on top
  # WHY geom_text: Shows exact values for clarity
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Monthly Revenue - 2023",
    x = "Month",
    y = "Total Revenue"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(plot_monthly)

# VISUALIZATION 3: Sales by Day of Week
# WHY: Identify weekly patterns (weekends vs weekdays)
# WHAT: Box plot showing distribution by day

plot_weekday <- daily_sales %>%
  ggplot(aes(x = day_of_week, y = daily_revenue, fill = day_of_week)) +
  geom_boxplot() +  # Box plot shows distribution
  # WHY boxplot: Shows median, quartiles, outliers
  # WHAT: Box = middle 50% of data, line = median, dots = outliers
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Revenue Distribution by Day of Week",
    subtitle = "Box shows middle 50% of days, line shows median",
    x = "Day of Week",
    y = "Daily Revenue"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

print(plot_weekday)

# VISUALIZATION 4: Quarter Comparison
# WHY: Compare business quarters (Q1, Q2, Q3, Q4)
# WHAT: Bar chart by quarter

quarterly_sales <- daily_sales %>%
  group_by(quarter) %>%
  summarize(
    total_revenue = sum(daily_revenue),
    avg_daily = mean(daily_revenue)
  )

plot_quarterly <- ggplot(quarterly_sales, aes(x = factor(quarter), y = total_revenue)) +
  geom_col(aes(fill = factor(quarter))) +
  geom_text(aes(label = dollar(total_revenue, scale = 1e-6, suffix = "M")), 
            vjust = -0.5) +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Quarterly Revenue Comparison - 2023",
    x = "Quarter",
    y = "Total Revenue",
    fill = "Quarter"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(plot_quarterly)


# PART 4: TREND ANALYSIS

monthly_growth <- monthly_sales %>%
  mutate(
    prev_month_revenue = lag(monthly_revenue),  # Previous month's value
    # WHY lag(): Shifts data down one row (gets previous value)
    mom_growth = monthly_revenue - prev_month_revenue,  # Absolute change
    mom_growth_pct = (mom_growth / prev_month_revenue) * 100  # Percentage change
  )

print("Month-over-Month Growth:")
print(monthly_growth %>% select(month, monthly_revenue, mom_growth_pct))

# VISUALIZATION 5: Growth Rate Chart
# WHY: Visualize which months grew vs declined
# WHAT: Bar chart with positive/negative colors

plot_growth <- monthly_growth %>%
  filter(!is.na(mom_growth_pct)) %>%  # Remove first month (no previous data)
  ggplot(aes(x = month, y = mom_growth_pct, fill = mom_growth_pct > 0)) +
  geom_col() +
  # WHY fill by positive/negative: Visual indicator of growth vs decline
  scale_fill_manual(values = c("TRUE" = "#27ae60", "FALSE" = "#e74c3c"),
                    labels = c("Decline", "Growth")) +
  geom_hline(yintercept = 0, linetype = "dashed") +  # Zero line
  labs(
    title = "Month-over-Month Growth Rate",
    subtitle = "Green = growth, Red = decline",
    x = "Month",
    y = "Growth Rate (%)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(plot_growth)


# PART 5: KEY INSIGHTS

cat("\n========== TIME SERIES INSIGHTS ==========\n\n")

# Total annual metrics
total_revenue <- sum(daily_sales$daily_revenue)
avg_daily <- mean(daily_sales$daily_revenue)
best_month <- monthly_sales %>% slice_max(monthly_revenue, n = 1)
worst_month <- monthly_sales %>% slice_min(monthly_revenue, n = 1)

cat("ANNUAL SUMMARY:\n")
cat("Total Revenue 2023:", dollar(total_revenue), "\n")
cat("Average Daily Revenue:", dollar(avg_daily), "\n\n")

cat("BEST PERFORMING MONTH:\n")
cat(as.character(best_month$month), "-", dollar(best_month$monthly_revenue), "\n\n")

cat("LOWEST PERFORMING MONTH:\n")
cat(as.character(worst_month$month), "-", dollar(worst_month$monthly_revenue), "\n\n")

# Best day of week
best_weekday <- daily_sales %>%
  group_by(day_of_week) %>%
  summarize(avg_revenue = mean(daily_revenue)) %>%
  slice_max(avg_revenue, n = 1)

cat("BEST DAY OF WEEK:\n")
cat(as.character(best_weekday$day_of_week), "- Avg:", dollar(best_weekday$avg_revenue), "\n\n")

# Calculate trend direction
first_half <- sum(monthly_sales$monthly_revenue[1:6])
second_half <- sum(monthly_sales$monthly_revenue[7:12])
trend <- ifelse(second_half > first_half, "GROWING", "DECLINING")

cat("OVERALL TREND:\n")
cat("First Half:", dollar(first_half), "\n")
cat("Second Half:", dollar(second_half), "\n")
cat("Business is", trend, "\n\n")

cat("==========================================\n")


# Save data
write_csv(daily_sales, "day03_daily_sales.csv")
write_csv(monthly_sales, "day03_monthly_sales.csv")


 ggsave("day03_daily_trend.png", plot_daily, width = 12, height = 6)
 ggsave("day03_monthly_bars.png", plot_monthly, width = 10, height = 6)

cat("Data and analysis saved!\n")






##📋 DAY 3 TASKS:
# Step 1: Run the code above section by section
# Step 2: Understand each WHAT and WHY comment
# Step 3: Answer these questions:
  
# What does lag() do and why is it useful? Shifts data down one row (gets previous value)
# Which month had highest revenue? March 
# Which day of week performs best? Friday
# Is the business growing or declining in 2nd half vs 1st half?growing in 1st half declining in 2nd half
