# ========================================
# DAY 1 PROJECT: Basic Inventory Analysis
# Olatunbosun Oyindasola
# Date: r Sys.Date()
# ========================================

# STEP 1: I created a synthetic inventory data
# ------------------------------------
product_names <- c("Laptop", "Mouse", "Keyboard", "Monitor", "Headset", 
                   "Webcam", "USB Cable", "HDMI Cable", "Charger", "Docking Station")

stock_quantity <- c(45, 150, 80, 30, 65, 40, 200, 120, 95, 25)

unit_price <- c(899, 25, 75, 350, 125, 85, 15, 20, 45, 275)

reorder_point <- c(20, 50, 30, 15, 25, 20, 100, 50, 40, 10)

# STEP 2: Then I calculated inventory value
# ----------------------------------
inventory_value <- stock_quantity * unit_price
print("Inventory Value by Product:")
print(inventory_value)

# STEP 3: Calculated total metrics
# --------------------------------
total_inventory_value <- sum(inventory_value)
print(paste("Total Inventory Value: $", total_inventory_value))

average_stock <- mean(stock_quantity)
print(paste("Average Stock Level:", round(average_stock, 2), "units"))

total_products <- length(product_names)
print(paste("Total Product Types:", total_products))

# STEP 4: This was to identify products below reorder point
# ----------------------------------------------
below_reorder <- stock_quantity < reorder_point
products_to_reorder <- product_names[below_reorder]

print("Products Below Reorder Point:")
print(products_to_reorder)

number_to_reorder <- sum(below_reorder)
print(paste("Number of Products to Reorder:", number_to_reorder))

# STEP 5: Find most/least valuable inventory
# -------------------------------------------
max_value_index <- which.max(inventory_value)
most_valuable <- product_names[max_value_index]
max_value <- inventory_value[max_value_index]

print(paste("Most Valuable Product:", most_valuable, "($", max_value, ")"))

min_value_index <- which.min(inventory_value)
least_valuable <- product_names[min_value_index]
min_value <- inventory_value[min_value_index]

print(paste("Least Valuable Product:", least_valuable, "($", min_value, ")"))

# STEP 6: Summary statistics
# ---------------------------
print("========== INVENTORY SUMMARY ==========")
print(paste("Total Products:", total_products))
print(paste("Total Inventory Value: $", format(total_inventory_value, big.mark=",")))
print(paste("Average Stock Level:", round(average_stock, 2), "units"))
print(paste("Products Below Reorder Point:", number_to_reorder))
print(paste("Most Valuable Product:", most_valuable))
print(paste("Reorder Alert:", ifelse(number_to_reorder > 0, "ACTION REQUIRED", "All Good")))
print("======================================")


