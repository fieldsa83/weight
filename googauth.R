library(gargle)
library(httr)
library(jsonlite)
library(base64enc)

token_b64 <- Sys.getenv("GOOGLE_FITNESS_TOKEN")
token_rds <- base64enc::base64decode(token_b64)
writeBin(token_rds, token_path)

google_token <- httr::config(token = readRDS(token_path))


get_weight <- function(days_back = 3000, token) {
  
  # Time range in nanoseconds
  end_ns <- as.numeric(Sys.time()) * 1e9
  start_ns <- as.numeric(Sys.time() - days_back * 24 * 3600) * 1e9
  
  # API call
  url <- paste0("https://www.googleapis.com/fitness/v1/users/me/dataSources/derived:com.google.weight:com.google.android.gms:merge_weight/datasets/", start_ns, "-", end_ns)
  
  # The httr::GET call uses the token object correctly here
  response <- GET(url, config = token)
  data <- fromJSON(content(response, "text"))
  
  # Convert to dataframe
  if (length(data$point) > 0) {
    df <- data.frame(
      Date = as.Date(as.POSIXct(as.numeric(data$point$startTimeNanos) / 1e9, origin = "1970-01-01")),
      weight_lb = sapply(data$point$value, function(x) x$fpVal)*2.20462
    )
    return(df)
  } else {
    return(data.frame(Date = as.Date(character(0)), weight_lb = numeric(0)))
  }
}


# --- 4. Get and Print Your Data ---
# This part of your code works perfectly with the 'google_token' created in step 2.
weight_data_google <- get_weight(6000, google_token)




# --- 5. Save the Data Frame as an Interactive HTML file ---
# Load the required libraries
library(DT)
library(htmlwidgets)

# Check if there is data to save
if (nrow(weight_data_google) > 0) {
  
  # Create an interactive datatable widget
  interactive_table <- datatable(
    weight_data_google,
    options = list(pageLength = 25, order = list(list(0, 'desc'))), # Show 25 rows, sort by date descending
    rownames = FALSE,
    caption = paste("Latest Google Fit Weight Data as of:", Sys.Date())
  )
  
  # Save the widget as a self-contained HTML file
  saveWidget(interactive_table, file = "index2.html", selfcontained = TRUE)
  
  print("Successfully saved data to index.html")
  
} else {
  print("No new weight data to save.")
}
