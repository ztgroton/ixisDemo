
# 0) Reset Global Environment ----
rm(list = ls())
data("sessionCounts")
data("addsToCart")

# ________________________________ ----
# 1) Prepare Reference Tables ----

# * Metric Aggregation by Month/Device ----

# ** Initialize Data
month_device <- sessionCounts

# ** Format Date Fields
month_device$dim_date <- as.Date(month_device$dim_date, "%m/%d/%y")
month_device <- month_device %>% 
  dplyr::mutate(
    dim_month = lubridate::month(dim_date), 
    dim_year = lubridate::year(dim_date)
  )

# ** Calculate New Derived Measures / ECR
month_device <- month_device %>% dplyr::mutate(ECR = transactions/sessions)

# ** Aggregate Data
month_device <- month_device %>% 
  dplyr::group_by(dim_year, dim_month, dim_deviceCategory) %>% 
  dplyr::summarise(dplyr::across(.cols = c('sessions', 'transactions', 'QTY'), .fns = sum)) %>% 
  dplyr::mutate(ECR = transactions/sessions) %>% 
  dplyr::arrange(dim_year, dim_month, dim_deviceCategory)

# * Month-over-Month Comparison ----

# ** Initialize Data
month_over_month <- sessionCounts

# ** Format Date Fields
month_over_month$dim_date <- as.Date(month_over_month$dim_date, "%m/%d/%y")
month_over_month <- month_over_month %>% 
  dplyr::mutate(
    dim_month = lubridate::month(dim_date), 
    dim_year = lubridate::year(dim_date)
  )

# ** Aggregate Data 
month_over_month <- month_over_month %>% 
  dplyr::group_by(dim_year, dim_month) %>% 
  dplyr::summarise(dplyr::across(.cols = c('sessions', 'transactions', 'QTY'), .fns = sum)) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(ECR = transactions/sessions) %>% 
  dplyr::left_join(addsToCart)

# ** Filter Data to Previous Two Months
month_over_month <- month_over_month %>% 
  dplyr::filter(dim_year == 2013, dim_month %in% c(5,6))

# ** Label Months as 'Previous' and 'Most Recent'
month_over_month <- month_over_month %>% 
  dplyr::mutate(label = dplyr::if_else(dim_month == 5, 'prev_month', 'most_recent_month')) %>% 
  dplyr::select(-dim_year, -dim_month)

# ** Transpose Columns
month_over_month <- month_over_month %>% 
  tidyr::pivot_longer(cols = -label, names_to = 'measure') %>% 
  tidyr::pivot_wider(names_from = 'label', values_from = 'value')

# ** Calculate Absolute and Relative Differences
month_over_month <- month_over_month %>% 
  dplyr::mutate(
    absolute_diff = most_recent_month - prev_month, 
    relative_diff = (most_recent_month - prev_month)/prev_month
  )

# * Export Data to XLSX File ----

# ** Initialize Workbook & Styles
wb <- openxlsx::createWorkbook()
styleInt <- openxlsx::createStyle(numFmt = "#,##", border = "TopBottomLeftRight")
stylePct <- openxlsx::createStyle(numFmt = "0.0%", border = "TopBottomLeftRight")
styleHeader <- openxlsx::createStyle(textDecoration = "Bold", border = "TopBottomLeftRight")

# ** Rename Columns
month_device_names <- c(
  "Year" = "dim_year", "Month" = "dim_month", "DeviceCat" = "dim_deviceCategory", 
  "Sessions" = "sessions", "Transactions" = "transactions"
)
month_device <- month_device %>% dplyr::rename(!!!month_device_names)

month_over_month_names <- c(
  "Measure" = "measure", "Prev Month" = "prev_month", "Most Recent Month" = "most_recent_month", 
  "Absolute Diff" = "absolute_diff", "Relative Diff" = "relative_diff"
)
month_over_month <- month_over_month %>% dplyr::rename(!!!month_over_month_names)

# ** Write First Sheet
openxlsx::addWorksheet(wb, "AggByMonthDevice", gridLines = FALSE)
openxlsx::writeData(wb, sheet = 1, month_device, headerStyle = styleHeader)
openxlsx::addStyle(wb, sheet = 1, style = styleInt, rows = 2:37, cols = 4:6, gridExpand = TRUE)
openxlsx::addStyle(wb, sheet = 1, style = stylePct, rows = 2:37, cols = 7, gridExpand = TRUE)
width_vec <- apply(month_device, 2, function(x) max(nchar(as.character(x)) + 6, na.rm = TRUE))
openxlsx::setColWidths(wb, sheet = 1, cols = 1:ncol(month_device), widths = width_vec)

# ** Write Second Sheet
openxlsx::addWorksheet(wb, "MonthOverMonth", gridLines = FALSE)
openxlsx::writeData(wb, sheet = 2, month_over_month, headerStyle = styleHeader)
openxlsx::addStyle(wb, sheet = 2, style = styleInt, rows = 2:6, cols = 2:4, gridExpand = TRUE)
openxlsx::addStyle(wb, sheet = 2, style = stylePct, rows = 2:6, cols = 5, gridExpand = TRUE)
openxlsx::addStyle(wb, sheet = 2, style = stylePct, rows = 5, cols = 2:4, gridExpand = TRUE)
width_vec <- apply(month_over_month, 2, function(x) max(nchar(as.character(x)) + 6, na.rm = TRUE))
openxlsx::setColWidths(wb, sheet = 2, cols = 1:ncol(month_over_month), widths = width_vec)

# ** Save Workbook
openxlsx::saveWorkbook(wb, system.file("outputs/reference_tables.xlsx", package = 'ixisDemo'), overwrite = TRUE)

# ________________________________ ----
# 2) Drill Down to Main Drivers ------

# * Use 'Pareto Table' to look for dimension combinations driving bulk of count measures

# ** Pareto Table - Browser & Device
toParetoTable(
  data = sessionCounts, 
  dims = c('dim_deviceCategory', 'dim_browser'), 
  vals = c('sessions', 'transactions', 'QTY'), 
  desc_by = 'sessions'
) -> pareto_all

# * Insight! - 80% of Sessions, 87% of Transactions & 90% of QTY driven by TOP 5 Browser/Device Combinations

# ________________________________ ----
# 3) Process Data for Main Drivers ------

# Filter to Top5 Device/Browser Combos
top_device_browsers <- pareto_all %>% dplyr::slice(1:5) %>% dplyr::distinct(dim_deviceCategory, dim_browser)
topSummary <- pareto_all %>% dplyr::semi_join(top_device_browsers)
topData <- sessionCounts %>% dplyr::semi_join(top_device_browsers)
write.csv(topSummary, system.file("outputs/topSummary.csv", package = 'ixisDemo'), row.names = FALSE, na = '')

# Convert 'dim_date' to 'Date' type
topData$dim_date <- as.Date(topData$dim_date, "%m/%d/%y")

# Rename Columns
topData <- topData %>% 
  dplyr::rename(
    Browser = dim_browser, 
    DeviceType = dim_deviceCategory
  )

# Add Month-Year
topData <- topData %>% 
  dplyr::mutate(
    dim_month = lubridate::month(dim_date), 
    dim_year = lubridate::year(dim_date)
  ) %>% 
  dplyr::mutate(MonthYear = as.Date(paste(dim_month, "1", dim_year, sep = '/'), "%m/%d/%Y"))

# Aggregate by Browser, Device Type, Month & Year
topDataAgg <- topData %>% 
  dplyr::group_by(Browser, DeviceType, MonthYear) %>% 
  dplyr::summarise(dplyr::across(c('sessions', 'transactions', 'QTY'), sum)) %>% 
  dplyr::ungroup()

# Left-Join 'Adds to Cart' Dataset
addsToCart <- addsToCart %>% 
  dplyr::mutate(MonthYear = as.Date(paste(dim_month, "1", dim_year, sep = '/'), "%m/%d/%Y")) %>% 
  dplyr::select(MonthYear, addsToCart)

topDataAgg <- topDataAgg %>% dplyr::left_join(addsToCart)

# Calculate New Derived Measures / ECR and AOV
topDataAgg <- topDataAgg %>% 
  dplyr::mutate(
    ECR = transactions/sessions, 
    AOV = QTY/transactions
  )

# Collect Measure Values into Standard Columns
topDataAgg <- topDataAgg %>% 
  tidyr::pivot_longer(
    cols = c('sessions', 'transactions', 'QTY', 'ECR', 'AOV'), 
    names_to = 'measure', values_to = 'value'
  )

# Convert Character Columns to Factors with Custom Orderings
topDataAgg$Browser <- factor(topDataAgg$Browser, levels = c('Safari', 'Chrome', 'Firefox'))
topDataAgg$DeviceType <- factor(topDataAgg$DeviceType, levels = c('desktop', 'mobile', 'tablet'))
topDataAgg$measure <- factor(topDataAgg$measure, levels = c('sessions', 'transactions', 'QTY', 'ECR', 'AOV'))

# ________________________________ ----
# 4) Generate Charts for PPT ----

# Facet Grid of Stacked Bar Charts 
ggplot2::ggplot(
  data = dplyr::filter(topDataAgg, !(measure %in% c('ECR', 'AOV'))), 
  mapping = ggplot2::aes(x = MonthYear, y = value, fill = DeviceType)
) + 
  ggplot2::geom_bar(stat = 'identity') + 
  ggplot2::facet_grid(measure ~ Browser, scales = 'free_y') + 
  ggplot2::theme(
    legend.position="bottom", 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )

ggplot2::ggsave(system.file("outputs/trending_drivers.png", package = 'ixisDemo'), scale = 3, width = 1000, height = 600, units = 'px')

# Facet Grid of Time Series
ggplot2::ggplot(
  data = dplyr::filter(topDataAgg, measure %in% c('ECR', 'AOV')), 
  mapping = ggplot2::aes(group = DeviceType, y = value)
) + 
  ggplot2::geom_line(mapping = ggplot2::aes(x = MonthYear, color = DeviceType)) + 
  ggplot2::facet_grid(measure ~ Browser, scales = 'free_y') + 
  ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot2::ggsave(system.file("outputs/measure_series.png", package = 'ixisDemo'), scale = 3, width = 750, height = 600, units = 'px')

# Facet Grid of Box Plots
ggplot2::ggplot(
  data = dplyr::filter(topDataAgg, Browser == 'Safari', measure %in% c('ECR', 'AOV')), # Browser == 'Safari'
  mapping = ggplot2::aes(group = DeviceType, y = value)
) + 
  geom_boxplot(mapping = ggplot2::aes(color = DeviceType)) + 
  ggplot2::facet_grid(measure ~ Browser, scales = 'free_y') + 
  ggplot2::theme(
    legend.position="bottom", 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )

ggplot2::ggsave(system.file("outputs/measure_boxplots.png", package = 'ixisDemo'), scale = 3, width = 350, height = 600, units = 'px')
