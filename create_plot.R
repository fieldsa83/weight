# 1. SETUP: LOAD LIBRARIES
#-------------------------------------------------------------------------------
# Data manipulation and I/O
library(dplyr)
library(zoo)        # <-- ADDED: Required for na.approx() for interpolation
library(lubridate)  # <-- ADDED: Required for year(), month(), and floor_date()

# Google API and web
library(gargle)
library(httr)
library(jsonlite)
library(base64enc)

# Plotting and output
library(plotly)
library(htmlwidgets)


# 2. CONFIGURATION: SETTINGS & PARAMETERS
#-------------------------------------------------------------------------------
TARGET_WEIGHT <- 135  # Target weight in lbs
DAYS_TO_FETCH <- 6000 # How many days of data to pull from the API


# 3. AUTHENTICATION: GOOGLE API
#-------------------------------------------------------------------------------
token_path <- "google_fitness_token.rds"

if (Sys.getenv("GOOGLE_FITNESS_TOKEN") != "") {
  cat("Authentication: Using GOOGLE_FITNESS_TOKEN secret.\n")
  token_b64 <- Sys.getenv("GOOGLE_FITNESS_TOKEN")
  token_rds <- base64enc::base64decode(token_b64)
  writeBin(token_rds, token_path)
} else {
  cat("Authentication: Using local .rds file.\n")
}

stopifnot(file.exists(token_path))
google_token <- httr::config(token = readRDS(token_path))


# 4. DATA FETCHING: GOOGLE FIT API FUNCTION
#-------------------------------------------------------------------------------
get_weight <- function(days_back, token) {
  end_ns <- as.numeric(Sys.time()) * 1e9
  start_ns <- as.numeric(Sys.time() - days_back * 24 * 3600) * 1e9
  
  url <- paste0("https://www.googleapis.com/fitness/v1/users/me/dataSources/derived:com.google.weight:com.google.android.gms:merge_weight/datasets/", start_ns, "-", end_ns)
  
  response <- GET(url, config = token)
  
  if (status_code(response) != 200) {
    stop("Google Fit API request failed. Status: ", status_code(response))
  }
  
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  
  if (length(data$point) > 0) {
    df <- data.frame(
      Date = as.Date(as.POSIXct(as.numeric(data$point$startTimeNanos) / 1e9, origin = "1970-01-01")),
      weight_lb = sapply(data$point$value, function(x) x$fpVal) * 2.20462
    )
    return(df)
  } else {
    return(data.frame(Date = as.Date(character(0)), weight_lb = numeric(0)))
  }
}


# 5. DATA PREPARATION: INTERPOLATE & AGGREGATE
#-------------------------------------------------------------------------------
# Fetch raw data from the API
weight_data_raw <- get_weight(DAYS_TO_FETCH, google_token)

# Create a complete daily timeline and interpolate missing values
all_dates <- data.frame(Date = seq(min(weight_data_raw$Date), max(weight_data_raw$Date), by = "day"))

df_daily <- all_dates %>%
  left_join(weight_data_raw, by = "Date") %>%
  mutate(Weight = round(na.approx(weight_lb, na.rm = FALSE), 1))

# Create weekly and monthly aggregated data frames
df_weekly <- df_daily %>%
  group_by(Date = floor_date(Date, unit = "week")) %>%
  summarise(Avg_weight = mean(Weight, na.rm = TRUE))

df_monthly <- df_daily %>%
  group_by(Date = floor_date(Date, unit = "month")) %>%
  summarise(Avg_weight = mean(Weight, na.rm = TRUE))


# 6. PLOTTING: CREATE INTERACTIVE CHART
#-------------------------------------------------------------------------------
p <- plot_ly(height = 500, width = 750) %>%
  add_trace(data = df_monthly, x = ~Date, y = ~Avg_weight, type = "scatter", mode = "lines", name = "Monthly") %>%
  add_trace(data = df_weekly, x = ~Date, y = ~Avg_weight, type = "scatter", mode = "lines", name = "Weekly", visible = FALSE) %>%
  add_trace(data = df_daily, x = ~Date, y = ~Weight, type = "scatter", mode = "lines", name = "Daily", visible = FALSE) %>%
  add_trace(x = ~df_daily$Date, y = ~TARGET_WEIGHT, type = "scatter", mode = "lines", name = "Target", line = list(color = 'darkred', dash = 'dot')) %>%
  
  layout(
    title = list(text = "<b>Weight Timeseries</b>", y = 0.94, x = 0.06),
    xaxis = list(
      title = list(text = "Date", standoff = 15),
      rangeslider = list(visible = TRUE, thickness = 0.08)
    ),
    yaxis = list(
      title = list(text = "Weight (lbs)", standoff = 10),
      range = c(min(df_daily$Weight, na.rm = TRUE) - 5, max(df_daily$Weight, na.rm = TRUE) + 5)
    ),
    updatemenus = list(
      list(
        type = "buttons",
        direction = "right",
        y = 1.15, x = 0.9,
        active = 0,
        buttons = list(
          list(method = "restyle", label = "Monthly",
               args = list(list(visible = c(TRUE, FALSE, FALSE, TRUE)))),
          list(method = "restyle", label = "Weekly",
               args = list(list(visible = c(FALSE, TRUE, FALSE, TRUE)))),
          list(method = "restyle", label = "Daily",
               args = list(list(visible = c(FALSE, FALSE, TRUE, TRUE))))
        )
      )
    )
  ) %>%
  config(displayModeBar = FALSE)


# 7. OUTPUT: SAVE PLOT TO HTML
#-------------------------------------------------------------------------------
if (!dir.exists("docs")) dir.create("docs")
saveWidget(p, file = "docs/index.html", selfcontained = TRUE)

cat("Script finished. Interactive plot saved to docs/index.html\n")
