library(dplyr)
library(plotly)
library(htmlwidgets)
library(readxl)

# Define the path to your Excel file
file_path <- "weight_data.xlsx" # Or whatever you named it

# Read each sheet by its name into a separate data frame
df_full <- read_excel(file_path, sheet = "Full_Data")
df_full_avg_month <- read_excel(file_path, sheet = "Monthly_Avg")
df_full_avg_week <- read_excel(file_path, sheet = "Weekly_Avg")


p <- plot_ly(height=500,width=750) %>%
  # Trace 0: Marker trace (background) - will always be visible
  add_trace(data = df_full, x = ~Date, y = ~weight_lb, 
            type = "scatter", 
            mode = 'markers', 
            name = "Daily Reading",
            showlegend=F,
            # Updated marker list to include a border (line)
            marker = list(size = 5, 
                          color = '#9ecae1',
                          line = list(
                            color = '#1f77b4', # Border color
                            width = .8         # Border width
                          )), 
            opacity=0.6) %>% 
  
  # 1. Add the first trace for Average monthly Weight (visible by default)
  add_trace(data = df_full_avg_month, x = ~Date, y = ~Avg_weight,
            type = "scatter", mode = "lines", name = "Monthly average", line = list(color = '#1f77b4')) %>%
  
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
      # This sets the initial ZOOMED IN view to the last year
      range = c("2019-01-01", as.character(max(df_full$Date)+14)),
      
      # UPDATED: This explicitly sets the range of the SLIDER itself
      rangeslider = list(
        visible = TRUE, 
        thickness = 0.08,
        # Force the slider's range to match your data exactly
        range = c(as.character(min(df_full$Date)), as.character(max(df_full$Date)+14)) 
      )
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
          
          # Button to show Monthly View (Trace 1)
          list(method = "restyle",
               # Sets visibility for traces 1, 2, and 3
               args = list(list(visible = c(TRUE, FALSE, FALSE)), c(1, 2, 3)),
               label = "Monthly View"),
          
          # Button to show Weekly View (Trace 2)
          list(method = "restyle",
               # Sets visibility for traces 1, 2, and 3
               args = list(list(visible = c(FALSE, TRUE, FALSE)), c(1, 2, 3)),
               label = "Weekly View"),
          
          # Button to show Daily View (Trace 3)
          list(method = "restyle",
               # Sets visibility for traces 1, 2, and 3
               args = list(list(visible = c(FALSE, FALSE, TRUE)), c(1, 2, 3)),
               label = "Daily View")
        )
      )
    )
  ) %>% 
  config(displayModeBar = F)


# 7. OUTPUT: SAVE PLOT TO HTML
#-------------------------------------------------------------------------------
if (!dir.exists("docs")) dir.create("docs")
saveWidget(p, file = "docs/index.html", selfcontained = TRUE, title = "Weights")

cat("Script finished. Interactive plot saved to docs/index.html\n")
