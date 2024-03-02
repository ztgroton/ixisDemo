
#' Rank of Descending Values (Largest to Smallest)
#'
#' @param data data.frame
#' @param x character
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'   calcDescRank(
#'     data = sessionCounts, 
#'     x = c('sessions', 'transactions', 'QTY')
#'   )
#' }
calcDescRank <- function(data, x) {
  
  # validate inputs
  stopifnot(is.data.frame(data))
  stopifnot(is.colname(data, x))
  
  # for each distinct column referenced by x, compute rank across entire dataset
  data <- data %>% dplyr::mutate(
    dplyr::across(
      .cols = dplyr::all_of(unique(x)), 
      .fns = ~dplyr::row_number(dplyr::desc(.)), 
      .names = "rank_{.col}"
    )
  )
  
  # return data
  return(data)
  
}

#' Percentage of Total Sum (Ordered by Rank of Descending Values)
#'
#' @param data data.frame
#' @param x character - must be length 1
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'   pctSumDesc(data = sessionCounts, x = 'sessions')
#'   pctSumDesc(data = sessionCounts, x = 'transactions')
#'   pctSumDesc(data = sessionCounts, x = 'QTY')
#' }
pctSumDesc <- function(data, x) {
  
  # validate inputs 
  stopifnot(is.data.frame(data))
  stopifnot(is.colname(data, x, scalar = TRUE))
  
  # order 'data' by 'desc(x)'
  data <- data[order(data[[x]], decreasing = TRUE), ]
  
  # calculate cumulative percent of total sum
  data[[paste0('pct_', x)]] <- data[[x]]/sum(data[[x]])
  
  # return data
  return(data)
  
}

#' Cumulative Percentage of Total Sum (Ordered by Rank of Descending Values)
#'
#' @param data data.frame
#' @param x character - must be length 1
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'   cumPctSumDesc(data = sessionCounts, x = 'sessions')
#'   cumPctSumDesc(data = sessionCounts, x = 'transactions')
#'   cumPctSumDesc(data = sessionCounts, x = 'QTY')
#' }
cumPctSumDesc <- function(data, x) {
  
  # validate inputs 
  stopifnot(is.data.frame(data))
  stopifnot(is.colname(data, x, scalar = TRUE))
  
  # order 'data' by 'desc(x)'
  data <- data[order(data[[x]], decreasing = TRUE), ]
  
  # calculate cumulative percent of total sum
  data[[paste0('cum_pct_', x)]] <- cumsum(data[[x]])/sum(data[[x]])
  
  # return data
  return(data)
  
}

#' Summarize Data in Pareto Table
#'
#' @param data data.frame
#' @param dims character
#' @param vals character
#' @param desc_by character - must be length 1
#' @param agg function
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'   toParetoTable(
#'     data = sessionCounts, 
#'     dims = 'dim_browser', 
#'     vals = c('sessions', 'transactions', 'QTY'), 
#'     desc_by = 'sessions'
#'   )
#' }
toParetoTable <- function(data = NULL, 
                          dims = NULL, 
                          vals = NULL, 
                          desc_by = NULL, 
                          agg = sum) {
  
  # validate inputs
  stopifnot(is.data.frame(data))
  stopifnot(is.colname(data, dims))
  stopifnot(is.colname(data, vals))
  stopifnot(is.function(agg))
  stopifnot(isTRUE(desc_by %in% vals))
  
  # summarise data / grouping by 'dims', apply 'agg' func across 'vals'
  data <- data %>% 
    dplyr::group_by(dplyr::pick(dplyr::all_of(dims))) %>% 
    dplyr::summarise(dplyr::across(.cols = dplyr::all_of(vals), .fns = agg)) %>% 
    dplyr::ungroup()
  
  # for each summarised measure, compute rank (desc values)
  data <- calcDescRank(data, x = vals)
  
  # for each summarised measure, compute percentages of total sum
  for (x in vals) {data <- cumPctSumDesc(pctSumDesc(data, x), x)}
  
  # order data using 'desc_by'
  data <- data[order(data[[desc_by]], decreasing = TRUE), ]
  
  # return data
  return(data)
  
}
