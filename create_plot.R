library(dplyr)
library(readr)
library(readxl)
library(htmlwidgets)
library(plotly)
library(gargle)
library(httr)
library(jsonlite)
library(base64enc)


### Google auth
token_path <- "google_fitness_token.rds" # The name of your local token file

# Check if the script is running in GitHub Actions and the secret is available
if (Sys.getenv("GOOGLE_FITNESS_TOKEN") != "") {
  
  cat("Authentication: Using GOOGLE_FITNESS_TOKEN secret.\n")
  # If it is, decode the Base64 secret and write it to a temporary file
  token_b64 <- Sys.getenv("GOOGLE_FITNESS_TOKEN")
  token_rds <- base64enc::base64decode(token_b64)
  writeBin(token_rds, token_path)
  
} else {
  cat("Authentication: Using local .rds file.\n")
}

# Ensure the token file exists (either the local one or the one from the secret)
stopifnot(file.exists(token_path))

# Create the final token object for the httr request.
# Your function expects this object.
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
### End google auth

# ---  Get and Print Your Data ---
weight_data_google <- get_weight(6000, google_token)


weight_data <- weight_data_google

      
# 2. Prepare the data
# Convert the 'Date' column to a Date object
# Create a full sequence of dates from the start to the end date
all_dates <- data.frame(Date = seq(as.Date(min(weight_data$Date)), as.Date(max(weight_data$Date)), by = "day"))

# 3. Merge and Interpolate
# Join the original data with the complete list of dates
df_full <- all_dates %>%
  left_join(weight_data, by = "Date") %>%
  # Use na.approx() from the zoo library for linear interpolation
  mutate(Weight = na.approx(weight_lb)) %>%
  # Round the interpolated weights to one decimal place
  mutate(Weight = round(Weight, 1))

df_full_avg_month <- df_full %>%
  mutate(
    Year=year(as.Date(Date)),
    Month=month(as.Date(Date))
  ) %>% 
  group_by(Year,Month) %>% 
  summarise(Avg_weight=mean(Weight)) %>% 
  mutate(Date=as.Date(sprintf("%d-%02d-01", Year, Month))) %>% 
  ungroup() %>%
  arrange(Date)


df_full_avg_week <- df_full %>%
  group_by(Date = floor_date(as.Date(Date), unit = "week")) %>%
  summarise(Avg_weight = mean(Weight)) %>%
  arrange(Date)



P <- plot_ly(height=500,width=750) %>%
  # 1. Add the first trace for Average monthly Weight (visible by default)
  add_trace(data = df_full_avg_month, x = ~Date, y = ~Avg_weight,
            type = "scatter", mode = "lines", name = "Monthly average") %>%
  
  # 2a. Add the second trace for weekly Weight (initially invisible)
  add_trace(data = df_full_avg_week, x = ~Date, y = ~Avg_weight,
            type = "scatter", mode = "lines", name = "Weekly", 
            visible = FALSE,
            line = list(color = '#1f77b4')
  ) %>% # <-- This trace is hidden at the start
  
  # 2b. Add the second trace for Daily Weight (initially invisible)
  add_trace(data = df_full, x = ~Date, y = ~Weight,
            type = "scatter", mode = "lines", name = "Daily", 
            visible = FALSE,
            line = list(color = '#1f77b4')
            ) %>% # <-- This trace is hidden at the start
  

  # 3. Add the target line with its OWN x and y data
  add_trace(data = df_full, x = ~Date, y = ~135, 
            type = "scatter", mode = "lines",
            name = "Target Weight", mode = "lines",
            line = list(color = 'darkred', dash = 'dot'))   %>% 
  
  # 4. Define the layout and the dropdown menu
  layout(
    title = list(
      text="<b>Weight timeseries</b>", 
      y=0.94,x=0.06
    ),
    xaxis = list(
      showgrid = F, 
      title = list(text = "Month", standoff = 15),
      rangeslider = list(visible = TRUE, thickness = 0.08),
      # Use the larger dataset for the max range to ensure it covers both
      range = c("2019-01-01", as.character(max(df_full_avg_week$Date)+7))
    ),
    yaxis = list(
      title = list(text = "lbs", standoff = 10), 
      range = c(124.8, 150.2),
      standoff = 25
    ),
    margin = list(t = 80, r = 80, b = 100, l = 100, pad = 30),
    
    # This section creates the dropdown menu
    updatemenus = list(
      list(
        type="buttons",
        direction="right",
        y=1.25, x=1,
        active = 0, # The first button is selected by default
        buttons = list(
          
          list(method = "restyle",
               # This makes the 1st trace TRUE (visible) and 2nd FALSE (hidden)
               args = list(list(visible = c(TRUE, FALSE, FALSE)), c(0, 1, 2)),
               label = "Monthly View"),
          
          
          list(method = "restyle",
               # This makes the 1st trace FALSE (hidden) and 2nd TRUE (visible)
               args = list(list(visible = c(FALSE, TRUE, FALSE)), c(0, 1, 2)),
               label = "Weekly View"),
          
          list(method = "restyle",
               # This makes the 1st trace FALSE (hidden) and 2nd TRUE (visible)
               args = list(list(visible = c(FALSE, FALSE, TRUE)), c(0, 1, 2)),
               label = "Daily View")


        )
      )
    )
  ) %>% 
  config(displayModeBar = F)



# 1. Create the 'docs' directory if it doesn't exist.
# This is the folder the GitHub Action looks for.
if (!dir.exists("docs")) dir.create("docs")

# 2. Save the plot object as a single, self-contained HTML file.
saveWidget(p, file = "docs/index.html", selfcontained = TRUE)
