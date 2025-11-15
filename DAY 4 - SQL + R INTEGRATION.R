# ========================================
# DAY 4: SQL + R Integration
# Querying Databases from R
# ========================================

# Load packages
library(tidyverse)
library(DBI)        # Database interface
library(RSQLite)    # SQLite database (easiest to start)
library(scales)

# WHY RSQLite: Lightweight database, no server needed
# WHAT: We'll create a database, query it with SQL, analyze with R

# ========================================
# PART 1: CREATE A SQLITE DATABASE
# ========================================

# WHY: Simulate real company database
# WHAT: Create 3 related tables (orders, products, customers)

# Connect to database (creates if doesn't exist)
con <- dbConnect(SQLite(), "supply_chain.db")
# WHY dbConnect: Opens connection to database
# WHAT SQLite(): Type of database (file-based, simple)

# Create Products table
products <- tibble(
  product_id = 1:20,
  product_name = paste0("Product_", LETTERS[1:20]),
  category = sample(c("Electronics", "Furniture", "Clothing"), 20, replace = TRUE),
  unit_cost = round(runif(20, 20, 200), 2),
  unit_price = round(runif(20, 30, 300), 2)
)

# Create Customers table
customers <- tibble(
  customer_id = 1:50,
  customer_name = paste0("Customer_", 1:50),
  region = sample(c("North", "South", "East", "West"), 50, replace = TRUE),
  customer_type = sample(c("Retail", "Wholesale", "Online"), 50, replace = TRUE)
)

# Create Orders table
set.seed(789)
orders <- tibble(
  order_id = 1:500,
  order_date = sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"), 
                      500, replace = TRUE),
  customer_id = sample(1:50, 500, replace = TRUE),
  product_id = sample(1:20, 500, replace = TRUE),
  quantity = sample(1:50, 500, replace = TRUE),
  # WHY sample with replace=TRUE: Same customer can order multiple times
  status = sample(c("Delivered", "Shipped", "Pending"), 500, replace = TRUE, 
                  prob = c(0.7, 0.2, 0.1))  # 70% delivered, 20% shipped, 10% pending
)

# Write tables to database
# WHY dbWriteTable: Saves R data frame as database table
dbWriteTable(con, "products", products, overwrite = TRUE)
dbWriteTable(con, "customers", customers, overwrite = TRUE)
dbWriteTable(con, "orders", orders, overwrite = TRUE)

cat("Database created with 3 tables: products, customers, orders\n")

# List tables in database
dbListTables(con)

# ========================================
# PART 2: SQL QUERIES - BASICS
# ========================================

# QUERY 1: Select all products
# WHY: Most basic SQL operation
# WHAT: Get all rows and columns from products table

query1 <- "SELECT * FROM products"
result1 <- dbGetQuery(con, query1)
# WHY dbGetQuery: Sends SQL to database, returns results as data frame

cat("\n--- All Products ---\n")
print(head(result1))

# QUERY 2: Filter products by category
# WHY WHERE clause: Filter rows based on condition
# WHAT: Only get Electronics

query2 <- "
SELECT product_name, category, unit_price
FROM products
WHERE category = 'Electronics'
"
result2 <- dbGetQuery(con, query2)

cat("\n--- Electronics Products ---\n")
print(result2)

# QUERY 3: Count orders by status
# WHY GROUP BY: Aggregate data by category
# WHAT: Count how many orders in each status

query3 <- "
SELECT status, COUNT(*) as order_count
FROM orders
GROUP BY status
ORDER BY order_count DESC
"
result3 <- dbGetQuery(con, query3)

cat("\n--- Orders by Status ---\n")
print(result3)

# QUERY 4: Calculate total quantity per product
# WHY: Business intelligence - which products sell most
# WHAT: Sum quantities, group by product

query4 <- "
SELECT 
  product_id,
  SUM(quantity) as total_quantity,
  COUNT(*) as num_orders,
  AVG(quantity) as avg_quantity_per_order
FROM orders
GROUP BY product_id
ORDER BY total_quantity DESC
LIMIT 10
"
# WHY LIMIT: Only show top 10 to keep output manageable

result4 <- dbGetQuery(con, query4)

cat("\n--- Top 10 Products by Quantity Sold ---\n")
print(result4)

# ========================================
# PART 3: SQL QUERIES - JOINS
# ========================================

# QUERY 5: Join orders with products
# WHY JOIN: Connect related tables to get complete picture
# WHAT: Get order details with product names and prices

query5 <- "
SELECT 
  o.order_id,
  o.order_date,
  o.quantity,
  p.product_name,
  p.unit_price,
  (o.quantity * p.unit_price) as order_value
FROM orders o
INNER JOIN products p ON o.product_id = p.product_id
LIMIT 20
"
# WHY INNER JOIN: Only include orders that match products
# WHAT ON clause: Specifies how tables connect (product_id matches)

result5 <- dbGetQuery(con, query5)

cat("\n--- Orders with Product Details ---\n")
print(head(result5, 10))

# QUERY 6: Multi-table join with aggregation
# WHY: Real-world analysis needs data from multiple tables
# WHAT: Revenue by customer region

query6 <- "
SELECT 
  c.region,
  COUNT(DISTINCT o.order_id) as total_orders,
  SUM(o.quantity * p.unit_price) as total_revenue,
  AVG(o.quantity * p.unit_price) as avg_order_value
FROM orders o
INNER JOIN customers c ON o.customer_id = c.customer_id
INNER JOIN products p ON o.product_id = p.product_id
WHERE o.status = 'Delivered'
GROUP BY c.region
ORDER BY total_revenue DESC
"
# WHY multiple JOINs: Need info from all 3 tables
# WHY WHERE: Only count completed (delivered) orders

result6 <- dbGetQuery(con, query6)

cat("\n--- Revenue by Region (Delivered Orders Only) ---\n")
print(result6)

# ========================================
# PART 4: COMBINING SQL + DPLYR
# ========================================

# WHY: Extract with SQL, analyze with dplyr
# WHAT: Get raw data with SQL, then use R's power for complex analysis

# Extract all order details with SQL
full_data_query <- "
SELECT 
  o.order_id,
  o.order_date,
  o.quantity,
  o.status,
  c.customer_name,
  c.region,
  c.customer_type,
  p.product_name,
  p.category,
  p.unit_cost,
  p.unit_price,
  (o.quantity * p.unit_price) as revenue,
  (o.quantity * (p.unit_price - p.unit_cost)) as profit
FROM orders o
INNER JOIN customers c ON o.customer_id = c.customer_id
INNER JOIN products p ON o.product_id = p.product_id
"

# Get all data
full_data <- dbGetQuery(con, full_data_query)

cat("\n--- Full Dataset Retrieved from Database ---\n")
cat("Total rows:", nrow(full_data), "\n")
glimpse(full_data)

# Now use dplyr for analysis
# ANALYSIS 1: Revenue by category and region
revenue_analysis <- full_data %>%
  filter(status == "Delivered") %>%
  group_by(category, region) %>%
  summarize(
    total_revenue = sum(revenue),
    total_profit = sum(profit),
    avg_order_size = mean(quantity),
    .groups = "drop"
  ) %>%
  arrange(desc(total_revenue))

cat("\n--- Revenue by Category and Region ---\n")
print(revenue_analysis)

# ANALYSIS 2: Customer type performance
customer_analysis <- full_data %>%
  filter(status == "Delivered") %>%
  group_by(customer_type) %>%
  summarize(
    total_orders = n(),
    total_revenue = sum(revenue),
    total_profit = sum(profit),
    profit_margin = (total_profit / total_revenue) * 100,
    avg_order_value = mean(revenue)
  ) %>%
  arrange(desc(total_revenue))

cat("\n--- Performance by Customer Type ---\n")
print(customer_analysis)

# ANALYSIS 3: Monthly trend
monthly_trend <- full_data %>%
  mutate(
    order_date = as.Date(order_date),
    month = floor_date(order_date, "month")  # Round to first of month
  ) %>%
  filter(status == "Delivered") %>%
  group_by(month) %>%
  summarize(
    total_orders = n(),
    total_revenue = sum(revenue),
    total_profit = sum(profit)
  ) %>%
  arrange(month)

cat("\n--- Monthly Performance Trend ---\n")
print(monthly_trend)

# ========================================
# PART 5: VISUALIZATIONS
# ========================================

# VIZ 1: Revenue by Region
viz_region <- ggplot(result6, aes(x = reorder(region, -total_revenue), 
                                  y = total_revenue, 
                                  fill = region)) +
  geom_col() +
  geom_text(aes(label = dollar(total_revenue, scale = 1e-3, suffix = "K")), 
            vjust = -0.5) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Total Revenue by Region",
    subtitle = "Delivered orders only",
    x = "Region",
    y = "Total Revenue"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

print(viz_region)

# VIZ 2: Category Performance Heatmap
viz_heatmap <- revenue_analysis %>%
  ggplot(aes(x = region, y = category, fill = total_revenue)) +
  geom_tile(color = "white") +  # Heatmap tiles
  # WHY geom_tile: Shows 2D data with color intensity
  geom_text(aes(label = dollar(total_revenue, scale = 1e-3, suffix = "K")), 
            color = "white", fontface = "bold") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", 
                      labels = dollar_format()) +
  labs(
    title = "Revenue Heatmap: Category × Region",
    subtitle = "Darker = higher revenue",
    x = "Region",
    y = "Category",
    fill = "Revenue"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(viz_heatmap)

# VIZ 3: Monthly Revenue Trend
viz_monthly <- ggplot(monthly_trend, aes(x = month, y = total_revenue)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +
  # WHY method="lm": Linear model - shows if trend is up or down
  scale_y_continuous(labels = dollar_format()) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(
    title = "Monthly Revenue Trend - 2023",
    subtitle = "Red dashed line = overall trend",
    x = "Month",
    y = "Total Revenue"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(viz_monthly)

# VIZ 4: Customer Type Comparison
viz_customer <- ggplot(customer_analysis, 
                       aes(x = reorder(customer_type, -total_revenue), 
                           y = total_revenue, 
                           fill = customer_type)) +
  geom_col() +
  geom_text(aes(label = paste0(dollar(total_revenue, scale = 1e-3, suffix = "K"), 
                               "\n", round(profit_margin, 1), "% margin")), 
            vjust = 1.5, color = "white", fontface = "bold") +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Revenue & Profit Margin by Customer Type",
    x = "Customer Type",
    y = "Total Revenue"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

print(viz_customer)

# ========================================
# PART 6: KEY SQL CONCEPTS REVIEW
# ========================================

cat("\n========== SQL CONCEPTS REVIEW ==========\n\n")

cat("1. SELECT: Choose which columns to retrieve\n")
cat("   Example: SELECT product_name, price FROM products\n\n")

cat("2. WHERE: Filter rows based on condition\n")
cat("   Example: WHERE category = 'Electronics' AND price > 100\n\n")

cat("3. GROUP BY: Aggregate data by category\n")
cat("   Example: GROUP BY region (then use SUM, COUNT, AVG)\n\n")

cat("4. JOIN: Combine data from multiple tables\n")
cat("   - INNER JOIN: Only matching rows\n")
cat("   - LEFT JOIN: All from left table + matches from right\n")
cat("   Example: FROM orders o INNER JOIN products p ON o.product_id = p.product_id\n\n")

cat("5. ORDER BY: Sort results\n")
cat("   Example: ORDER BY revenue DESC (highest first)\n\n")

cat("6. LIMIT: Restrict number of results\n")
cat("   Example: LIMIT 10 (top 10 only)\n\n")

cat("==========================================\n")

# ========================================
# PART 7: BUSINESS INSIGHTS
# ========================================

cat("\n========== BUSINESS INSIGHTS ==========\n\n")

# Find best performing region
best_region <- result6 %>% slice_max(total_revenue, n = 1)
cat("1. TOP REGION:", best_region$region, "-", 
    dollar(best_region$total_revenue), "revenue\n\n")

# Find most profitable customer type
best_customer_type <- customer_analysis %>% slice_max(total_profit, n = 1)
cat("2. MOST PROFITABLE CUSTOMER TYPE:", best_customer_type$customer_type, "\n")
cat("   Revenue:", dollar(best_customer_type$total_revenue), "\n")
cat("   Profit:", dollar(best_customer_type$total_profit), "\n")
cat("   Margin:", round(best_customer_type$profit_margin, 1), "%\n\n")

# Calculate overall metrics
total_orders_delivered <- sum(result6$total_orders)
total_revenue_all <- sum(result6$total_revenue)

cat("3. OVERALL PERFORMANCE:\n")
cat("   Total Delivered Orders:", total_orders_delivered, "\n")
cat("   Total Revenue:", dollar(total_revenue_all), "\n\n")

cat("==========================================\n")

# ========================================
# PART 8: CLEANUP
# ========================================

# Close database connection
# WHY: Good practice to close connections when done
dbDisconnect(con)

cat("\nDatabase connection closed.\n")
cat("Day 4 complete! You've learned SQL + R integration.\n")


#What's the difference between INNER JOIN and LEFT JOIN? INNER JOIN  include only orders that match products and LEFT JOIN bring all from left table that matches from right
#What does GROUP BY do? Aggregate data by category
#Which region generated most revenue? East
#Which customer type has highest profit margin?Wholesale
#How many total delivered orders were there?338



