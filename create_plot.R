# List of packages to install and load
required_packages <- c("dplyr", "readr", "htmlwidgets", "plotly")

# Install missing packages and load all of them
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}



df_full <- read_csv("df_full.csv")
df_full_avg <- read_csv("df_full_avg.csv")

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
