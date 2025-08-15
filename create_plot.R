# List of packages to install and load
required_packages <- c("httr", "jsonlite", "tidyverse", "zoo", "plotly")

# Install missing packages and load all of them
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# Historic csv
weight_data_historic <- readxl::read_excel("weighttest.xlsx") %>% 
  rename(Date = DATE,
         weight_lb=Weight) %>% 
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"))

# 1. Set up OAuth (run this once)
google_app <- oauth_app("google",
                        key = Sys.getenv("API_KEY"),  
                        secret = Sys.getenv("SECRET_KEY")
                        )

google_token <- oauth2.0_token(oauth_endpoints("google"), google_app,
                               scope = "https://www.googleapis.com/auth/fitness.body.read")

# 2. Get weight data
get_weight <- function(days_back = 3000, token) {
  
  # Time range in nanoseconds
  end_ns <- as.numeric(Sys.time()) * 1e9
  start_ns <- as.numeric(Sys.time() - days_back * 24 * 3600) * 1e9
  
  # API call
  url <- paste0("https://www.googleapis.com/fitness/v1/users/me/dataSources/derived:com.google.weight:com.google.android.gms:merge_weight/datasets/", start_ns, "-", end_ns)
  
  # The fix is here: pass the token object directly to the config argument
  response <- GET(url, config = token)
  data <- fromJSON(content(response, "text"))
  
  # Convert to dataframe
  if (length(data$point) > 0) {
    df <- data.frame(
      Date = as.Date(as.POSIXct(as.numeric(data$point$startTimeNanos) / 1e9, origin = "1970-01-01")),
      #weight_kg = sapply(data$point$value, function(x) x$fpVal),
      weight_lb = sapply(data$point$value, function(x) x$fpVal)*2.20462
    )
    return(df)
  } else {
    return(data.frame(Date = as.Date(character(0)), weight_lb = numeric(0)))
  }
}

# 3. Get your data
weight_data_google <- get_weight(6000, google_token)
print(weight_data_google)



weight_data <- rbind(weight_data_historic, weight_data_google) %>%
  # Sort by date to make sure we keep the correct entry if there are duplicates
  arrange(Date) %>%
  # Keep only the unique rows based on the Date column
  distinct(Date, .keep_all = TRUE)







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



plot_ly(height=500,width=750) %>%
  # 1. Add the first trace for Average Weight (visible by default)
  add_trace(data = df_full_avg, x = ~Date, y = ~Avg_weight,
            type = "scatter", mode = "lines", name = "Monthly average") %>%
  
  # 2. Add the second trace for Daily Weight (initially invisible)
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
      range = c("2019-01-01", as.character(max(df_full$Date)))
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
               args = list(list(visible = c(TRUE, FALSE)), c(0, 1)),
               label = "Monthly View"),
          
          list(method = "restyle",
               # This makes the 1st trace FALSE (hidden) and 2nd TRUE (visible)
               args = list(list(visible = c(FALSE, TRUE)), c(0, 1)),
               label = "Daily View")
        )
      )
    )
  ) %>% 
  config(displayModeBar = F)
