
#' 'Session Counts' 
#'
#' @name sessionCounts
#' @format
#' A data frame with 7,734 rows and 6 columns:
#' \describe{
#'   \item{dim_browser}{Name of Web Browser}
#'   \item{dim_deviceCategory}{Category of Device}
#'   \item{dim_date}{Calendar Date for Browsing Activity}
#'   \item{sessions}{Total # of Web Browsing Sessions - aggregated by Calendar Date, Browser and Device Category}
#'   \item{transactions}{Total # of Transactions - aggregated by Calendar Date, Browser and Device Category }
#'   \item{QTY}{Total Quantity of Purchased Items - aggregated by Calendar Date, Browser and Device Category}
#' }
NULL

#' 'Adds-To-Cart'
#'
#' @name addsToCart
#' @format
#' A data frame with 12 rows and 3 columns:
#' \describe{
#'   \item{dim_year}{Calendar Year}
#'   \item{dim_month}{Calendar Month}
#'   \item{addsToCart}{Total Quantity of Items Added to Cart - aggregated by Calendar Year & Month}
#' }
NULL
