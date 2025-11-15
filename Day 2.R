library(tidyverse)
library(scales)

set.seed(123)  # Makes random numbers repeatable

supply_chain_data <- tibble(
  product_id = paste0("SKU_", str_pad(1:50, 3, pad = "0")),
  product_name = paste("Product", LETTERS[1:50]),
  monthly_demand = round(rnorm(50, mean = 200, sd = 100)),
  selling_price = round(runif(50, 15, 750), 2),
  current_stock = round(runif(50, 50, 500))
)

View(supply_chain_data)  # Opens data in a tab
glimpse(supply_chain_data)  # Shows structure
head(supply_chain_data, 10)  # Shows first 10 rows

supply_chain_data <- supply_chain_data %>%
  mutate(
    monthly_revenue = monthly_demand * selling_price,
    inventory_value = current_stock * selling_price
  )

head(supply_chain_data)

abc_analysis <- supply_chain_data %>%
  arrange(desc(monthly_revenue)) %>%  # Sort highest to lowest
  mutate(
    cumulative_revenue = cumsum(monthly_revenue),
    total_revenue = sum(monthly_revenue),
    cumulative_pct = (cumulative_revenue / total_revenue) * 100
  )

head(abc_analysis)
tail(abc_analysis, 1)  # Should show ~100%

abc_analysis <- abc_analysis %>%
  mutate(
    abc_class = case_when(
      cumulative_pct <= 80 ~ "A",
      cumulative_pct <= 95 ~ "B",
      TRUE ~ "C"
    )
  )

table(abc_analysis$abc_class)  # Count how many A, B, C

ggplot(abc_analysis, aes(x = 1:nrow(abc_analysis), y = cumulative_pct))

ggplot(abc_analysis, aes(x = 1:nrow(abc_analysis), y = cumulative_pct)) +
  geom_line() + geom_point(aes(color = abc_class))

ggplot(abc_analysis, aes(x = 1:nrow(abc_analysis), y = cumulative_pct)) +
  geom_line() + geom_point(aes(color = abc_class)) + 
  labs(
    title = "ABC Analysis - Pareto Chart",
    x = "Product Rank",
    y = "Cumulative Revenue (%)"
  )

ggplot(supply_chain_data, aes(x = product_name, y = monthly_revenue)) + geom_col()


top_10 <- supply_chain_data %>%
  arrange(desc(monthly_revenue)) %>%
  head(10)

ggplot(top_10, aes(x = product_name, y = monthly_revenue)) +
  geom_col()

ggplot(top_10, aes(x = product_name, y = monthly_revenue)) +
  geom_col() +
  coord_flip()

ggplot(top_10, aes(x = reorder(product_name, monthly_revenue), y = monthly_revenue)) +
  geom_col() +
  coord_flip()

# Check the classification
abc_check <- abc_analysis %>%
  group_by(abc_class) %>%
  summarize(
    product_count = n(),
    total_revenue = sum(monthly_revenue),
    pct_of_revenue = (sum(monthly_revenue) / sum(abc_analysis$monthly_revenue)) * 100
  ) %>%
  arrange(abc_class)

print(abc_check)
```

---
  
  ## 🔧 DEBUGGING FRAMEWORK
  
  **When you get an error:**
  
  **1. READ the error message**
  - "object not found" → Did you create it? Check spelling
- "could not find function" → Did you load the library?
  - "unexpected symbol" → Check for typos, missing commas

**2. ISOLATE the problem**
  - Run code line by line
- Which line causes the error?
  
  **3. CHECK the basics**
  - Is the object created? Type its name and press Enter
- Are column names spelled correctly? Use `names(your_data)`
- Is the data structured correctly? Use `str(your_data)`

**4. GOOGLE the error**
  - Copy exact error message
- Add "R ggplot2" or "R dplyr" to search

**5. EXPERIMENT**
  - Try a simpler version
- Comment out (`#`) parts of code to find the problem

---
  
  ## 📝 YOUR DAY 2 REPORT:
  ```
DAY 2 COMPLETE ✅

Time: [X minutes]

What I built:
  - ABC analysis: [X] A products, [X] B products, [X] C products
- Charts created: [Number]

Key learning: [What clicked for you?]

Debugging wins: [What error did you solve yourself?]

Question: [Only if truly stuck after trying to debug]

DAY 2 ✅

Total time: [cumulatively 40mins]
ABC: [50] A, [24] B, [26] C products
Hardest part: none was difficult 
Ready for Day 3: Yes