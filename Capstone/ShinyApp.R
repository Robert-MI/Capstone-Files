library(shiny)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(dplyr)
library(magrittr)
library(reshape2)

## change path to a non temp diretcory to keep that even after reboot
shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()),"myapp-cache")))

load("deaths_per_1000_groups.RData")
load("sorted_merged_data.RData")
load("economicFactorsCombined.RData")
load("deaths.RData")
load("deaths_total_econ.RData")
load("deaths_sex_econ_total.RData")
load("deaths_groups_econ_total.RData")

# Filter out rows with missing or empty values
filtered_inflation <- econ[!is.na(econ$Annual_Inflation_percent) & econ$Annual_Inflation_percent != "", ]
filtered_unemployment <- econ[!is.na(econ$Unemployment_percent) & econ$Unemployment_percent != "", ]
filtered_gdp <- econ[!is.na(econ$GDP_Per_capita) & econ$GDP_Per_capita != "", ]
filtered_ppp <- econ[!is.na(econ$PPP_Per_capita) & econ$PPP_Per_capita != "", ]
filtered_cehs <- econ[!is.na(econ$CEHS_Index) & econ$CEHS_Index != "", ]
filtered_haq <- econ[!is.na(econ$Healthcare_Access_Quality_Index) & econ$Healthcare_Access_Quality_Index != "", ]
filtered_edu <- econ[!is.na(econ$Average_Years_Of_Education) & econ$Average_Years_Of_Education != "", ]
filtered_co2 <- econ[!is.na(econ$CO2_mt_Per_Capita) & econ$CO2_mt_Per_Capita != "", ]

# Calculate the number of years of data per country
country_data_count_inflation <- aggregate(year ~ country, data = filtered_inflation, FUN = function(x) length(unique(x)))
country_data_count_unemployment <- aggregate(year ~ country, data = filtered_unemployment, FUN = function(x) length(unique(x)))
country_data_count_gdp <- aggregate(year ~ country, data = filtered_gdp, FUN = function(x) length(unique(x)))
country_data_count_ppp <- aggregate(year ~ country, data = filtered_ppp, FUN = function(x) length(unique(x)))
country_data_count_cehs <- aggregate(year ~ country, data = filtered_cehs, FUN = function(x) length(unique(x)))
country_data_count_haq <- aggregate(year ~ country, data = filtered_haq, FUN = function(x) length(unique(x)))
country_data_count_edu <- aggregate(year ~ country, data = filtered_edu, FUN = function(x) length(unique(x)))
country_data_count_co2 <- aggregate(year ~ country, data = filtered_co2, FUN = function(x) length(unique(x)))

# Filter countries with at least 10 years of data
valid_countries_inflation <- country_data_count_inflation$country[country_data_count_inflation$year >= 10]
valid_countries_unemployment <- country_data_count_unemployment$country[country_data_count_unemployment$year >= 10]
valid_countries_gdp <- country_data_count_gdp$country[country_data_count_gdp$year >= 10]
valid_countries_ppp <- country_data_count_ppp$country[country_data_count_ppp$year >= 10]
valid_countries_cehs <- country_data_count_cehs$country[country_data_count_cehs$year >= 10]
valid_countries_haq <- country_data_count_haq$country[country_data_count_haq$year >= 4]
valid_countries_edu <- country_data_count_edu$country[country_data_count_edu$year >= 4]
valid_countries_co2 <- country_data_count_co2$country[country_data_count_co2$year >= 4]

# Define function to generate tab panels
generateTab <- function(title, country_input, plot_output, valid_countries) {
  tabPanel(
    title,
    fluidPage(
      titlePanel(paste(title)),
      absolutePanel(
        top = 10, right = 10, width = "200px",
        selectInput(country_input, "Select Country", choices = valid_countries)
      ),
      tags$head(
        tags$style(HTML("
          .plot-container {
            height: calc(100vh - 150px) !important;
          }
          .plot-legend text {
            font-size: 14px;
          }
          .navbar {
            min-width: 500px; /* Adjust height as needed */
            max-width: 515px; /* Adjust height as needed */
          }
        "))
      ),
      mainPanel(
        div(
          plotlyOutput(plot_output, height = "90%", width = "155%"),
          class = "plot-container"
        )
      )
    )
  )
}

# Define UI
ui <- fluidPage(
  titlePanel("Country Data Analysis"),
  navbarPage("Country Data",
             navbarMenu("Economic Factors",
                        generateTab("Inflation Per Year", "inflation_country", "inflation_plot", valid_countries_inflation),
                        generateTab("Unemployment Per Year", "unemployment_country", "unemployment_plot", valid_countries_unemployment),
                        generateTab("GDP Per Capita Per Year", "gdp_country", "gdp_plot", valid_countries_gdp),
                        generateTab("PPP Per Capita Per Year", "ppp_country", "ppp_plot", valid_countries_ppp),
                        generateTab("CEHS Index Per Year", "cehs_country", "cehs_plot", valid_countries_cehs),
                        generateTab("HAQ Index Per Year", "haq_country", "haq_plot", valid_countries_haq),
                        generateTab("Years of Education Per Year", "edu_country", "edu_plot", valid_countries_edu),
                        generateTab("CO2 Emissions Per Year", "co2_country", "co2_plot", valid_countries_co2)
             ),
             navbarMenu("Deaths",
                        generateTab("Total Deaths per 1000", "deathTotal_country", "deathTotal_plot", unique(combined_data$country)),
                        generateTab("Deaths per 1000 by Age Groups", "deathGroups_country", "deathGroups_plot", unique(data$country)),
                        generateTab("Deaths per 1000 at Each Age", "deathEachAge_country", "deathEachAge_plot", unique(combined_data$country)),
                        generateTab("Deaths per 1000 Each Year", "deathEachYear_country", "deathEachYear_plot", unique(combined_data$country))
             ),
             navbarMenu("Correlations",
                        generateTab("Total Correlation", "deaths_total_econ_country", "deaths_total_econ_plot", unique(deaths_total_econ$country)),
                        generateTab("Correlation by Sex", "deaths_sex_total_econ_country", "deaths_sex_total_econ_plot", unique(deaths_sex_econ_total$country)),
                        generateTab("Correlation by Groups", "deaths_groups_econ_total_country", "deaths_groups_econ_total_plot", unique(deaths_groups_econ_total$country))
             )
  )
)

# Define server logic
server <- function(input, output) {
  output$deathGroups_plot <- renderPlotly({
    req(input$deathGroups_country)
    
    df_country <- subset(data, country == input$deathGroups_country & sex != "Total")
    
    # Define the order of groups and corresponding labels
    group_order <- c("children (0-7)", "youngadults (8-17)", "adults (18-59)", "geriatric (60+)")
    
    df_country$Group <- factor(df_country$group_with_age, levels = group_order)
    
    p <- ggplot(df_country, aes(x = Year, y = deaths, 
                                group = interaction(sex, Group), 
                                color = sex, 
                                linetype = Group)) +
      geom_line(linewidth = 1.2) +  # Increase line thickness
      scale_color_manual(values = c("Male" = "blue", "Female" = "red"), 
                         labels = c("Male", "Female")) +  # Specify labels for sex
      scale_linetype_manual(values = c("children (0-7)" = "dotted", "youngadults (8-17)" = "dashed", "adults (18-59)" = "dotdash", "geriatric (60+)" = "solid")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.position = "none",
            legend.title = element_blank(),
            legend.text = element_text(size = 8),
            axis.text = element_text(size = 16),
            axis.title = element_text(size = 16, face = "bold")) +
      labs(x = "Year", y = "Number of Deaths per 1000", title = paste("Deaths Trends in", input$deathGroups_country)) +
      scale_x_continuous(breaks = seq(min(df_country$Year), max(df_country$Year), by = 5)) + 
      scale_y_continuous(breaks = seq(0, max(df_country$deaths), by = round((max(df_country$deaths) - min(df_country$deaths)) / 8))) +
      guides(color = guide_legend(title = "Sex"),
             linetype = guide_legend(title = "Age Group"))
    
    p_plotly <- ggplotly(p, tooltip = c("Year", "deaths","Group")) %>%
      layout(hoverlabel = list(bgcolor = "lightblue"),showlegend = TRUE) 
    
    # Display the interactive plotly plot
    p_plotly
  })
  
  output$deathTotal_plot <- renderPlotly({
    req(input$deathTotal_country)
    
    df_country <- subset(combined_data, country == input$deathTotal_country & cat == 'total')
    
    p <- ggplot(df_country, aes(x = Year, y = Deaths1000, color = sex)) +
      geom_point(size = 2) +
      geom_smooth(method = "loess") +
      scale_color_manual(values = c("Male" = "blue", "Female" = "red"), 
                         labels = c("Male", "Female")) +  # Specify labels for sex
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.position = "none",
            legend.title = element_blank(),
            legend.text = element_text(size = 8),
            axis.text = element_text(size = 16),
            axis.title = element_text(size = 16, face = "bold")) +
      labs(x = "Year", y = "Number of Deaths per 1000", title = paste("Total Deaths Trends in", input$deathTotal_country)) +
      scale_x_continuous(breaks = seq(min(df_country$Year), max(df_country$Year), by = 5)) + 
      scale_y_continuous(breaks = seq(0, ceiling(max(df_country$Deaths1000)), by = round((max(df_country$Deaths1000) - min(df_country$Deaths1000)) /4,digits=2))) +
      
      guides(color = guide_legend(title = "Sex"))
    
    # Convert ggplot to plotly object with custom hover text
    p_plotly <- ggplotly(p, tooltip = c("Year", "Deaths1000")) %>%
      layout(hoverlabel = list(bgcolor = "lightblue"),showlegend = TRUE) 
    
    # Display the interactive plotly plot
    p_plotly
  })
  
  output$deathEachAge_plot <- renderPlotly({
    req(input$deathEachAge_country)
    
    df_country <- subset(combined_data, country == input$deathEachAge_country & cat == 'Age')
    
    p <- ggplot(df_country, aes(x = Year, y = Deaths1000, color = sex,frame=age)) +
      geom_point() +
      scale_color_manual(values = c("Male" = "blue", "Female" = "red"), 
                         labels = c("Male", "Female")) +  # Specify labels for sex
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.position = "none",
            legend.title = element_blank(),
            legend.text = element_text(size = 8),
            axis.text = element_text(size = 16),
            axis.title = element_text(size = 16, face = "bold")) +
      labs(x = "Year", y = "Number of Deaths per 1000", title = paste("Deaths Trends in", input$deathEachAge_country)) +
      scale_x_continuous(breaks = seq(min(df_country$Year), max(df_country$Year), by = 5)) + 
      scale_y_continuous(breaks = seq(0, ceiling(max(df_country$Deaths1000)), by = round((max(df_country$Deaths1000) - min(df_country$Deaths1000)) /4,digits=2))) +
      
      guides(color = guide_legend(title = "Sex"))
    
    # Convert ggplot to plotly object with custom hover text
    p_plotly <- ggplotly(p, tooltip = c("Year", "Deaths1000")) %>%
      layout(hoverlabel = list(bgcolor = "lightblue"),showlegend = TRUE)
    
    # Display the interactive plotly plot
    p_plotly
  })
  
  output$deathEachYear_plot <- renderPlotly({
    req(input$deathEachYear_country)
    
    df_country <- subset(sorted_merged, country == input$deathEachYear_country & cat == 'Age')
    
    p <- ggplot(df_country, aes(x = age, y = Deaths1000, color = sex,frame = Year)) +
      geom_point() +
      scale_color_manual(values = c("Male" = "blue", "Female" = "red"), 
                         labels = c("Male", "Female")) +  # Specify labels for sex
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.position = "none",
            legend.title = element_blank(),
            legend.text = element_text(size = 8),
            axis.text = element_text(size = 16),
            axis.title = element_text(size = 16, face = "bold")) +
      labs(x = "Age", y = "Number of Deaths per 1000", title = paste("Deaths Trends in", input$deathEachYear_country)) +
      scale_x_continuous(breaks = seq(min(df_country$age), max(df_country$age), by = 5)) + 
      scale_y_continuous(breaks = seq(0, ceiling(max(df_country$Deaths1000)), by = round((max(df_country$Deaths1000) - min(df_country$Deaths1000)) /4,digits=2))) +
      guides(color = guide_legend(title = "Sex"))
    
    # Convert ggplot to plotly object with custom hover text
    p_plotly <- ggplotly(p, tooltip = c("age", "Deaths1000"))
    
    # Add vertical lines for each unique Year
    for (yr in unique(df_country$Year)) {
      df_year <- subset(df_country, Year == yr)
      
      # Extract x values and add as vertical lines
      x_values <- unique(df_year$value)
      for (x in x_values) {
        p_plotly <- p_plotly %>% 
          add_trace(
            data = NULL, 
            x = list(x, x), 
            y = list(0, max(df_country$Deaths1000)/2), 
            type = "scatter", 
            mode = "lines", 
            line = list(color = "black", width = 1, dash = "dash"),
            frame = as.character(yr),
            showlegend = FALSE,
            inherit = FALSE
          ) %>%
          add_trace(
            data = NULL,
            x = c(x),
            y = c(max(df_country$Deaths1000)/2),
            type = "scatter",
            mode = "text",
            text = paste("Life Expectancy:",round(x, digits = 1)),
            textposition = "top",
            frame = as.character(yr),
            showlegend = FALSE,
            inherit = FALSE
          )
      }
      
      # Extract x values and add as vertical lines
      x_median <- unique(df_year$Median_age)
      for (x in x_median) {
        p_plotly <- p_plotly %>% 
          add_trace(
            data = NULL, 
            x = list(x, x), 
            y = list(0, max(df_country$Deaths1000)/3), 
            type = "scatter", 
            mode = "lines", 
            line = list(color = "black", width = 1, dash = "solid"),
            frame = as.character(yr),
            showlegend = FALSE,
            inherit = FALSE
          ) %>%
          add_trace(
            data = NULL,
            x = c(x),
            y = c(max(df_country$Deaths1000)/3),
            type = "scatter",
            mode = "text",
            text = paste("Median Age:",round(x, digits = 1)),
            textposition = "top",
            frame = as.character(yr),
            showlegend = FALSE,
            inherit = FALSE
          )
      }
    }
    
    # Customize hover label background color
    p_plotly <- p_plotly %>% layout(hoverlabel = list(bgcolor = "lightblue"),showlegend = TRUE)
    
    # Display the interactive plotly plot
    p_plotly
  })%>% bindCache(input$deathEachYear_country)
  
  output$inflation_plot <- renderPlotly({
    req(input$inflation_country)
    
    # Subset data for the selected country
    df_country <- subset(filtered_inflation, country == input$inflation_country)
    
    # Create the ggplot object
    p <- ggplot(df_country, aes(x = year, y = Annual_Inflation_percent, color = country, linetype = country)) +
      geom_point(size = 2) +
      geom_smooth(method = "loess", se = FALSE, color = "blue", linetype = "dashed") +
      geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "solid") +
      theme_minimal() +
      labs(x = "Year", y = "Annual Inflation in Percents", title = paste("Inflation Trends in", input$inflation_country)) +
      scale_x_continuous(breaks = seq(min(df_country$year), max(df_country$year), by = 5)) +
      scale_color_manual(values = brewer.pal(min(length(valid_countries_inflation), 9), "Set1")) + 
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold")
      )
    
    p_plotly <- ggplotly(p, tooltip = c("year", "Annual_Inflation_percent")) %>%
      layout(hoverlabel = list(bgcolor = "lightblue"))
    
    p_plotly
  })
  
  output$unemployment_plot <- renderPlotly({
    req(input$unemployment_country)
    
    df_country <- subset(filtered_unemployment, country == input$unemployment_country)
    
    p <- ggplot(df_country, aes(x = year, y = Unemployment_percent, color = country, linetype = country)) +
      geom_point(size = 2) +
      geom_smooth(method = "loess", se = FALSE, color = "blue", linetype = "dashed") +
      geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "solid") +
      theme_minimal() +
      labs(x = "Year", y = "Annual Unemployment in Percents", title = paste("Unemployment Trends in", input$unemployment_country)) +
      scale_x_continuous(breaks = seq(min(df_country$year), max(df_country$year), by = 5)) +
      scale_color_manual(values = brewer.pal(min(length(valid_countries_inflation), 9), "Set1")) +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold")
      )
    
    p_plotly <- ggplotly(p, tooltip = c("year", "Unemployment_percent")) %>%
      layout(hoverlabel = list(bgcolor = "lightblue"))
    
    p_plotly
  })
  
  output$gdp_plot <- renderPlotly({
    req(input$gdp_country)
    
    df_country <- subset(filtered_gdp, country == input$gdp_country)
    
    p <- ggplot(df_country, aes(x = year, y = GDP_Per_capita, color = country, linetype = country)) +
      geom_point(size = 2) +
      geom_smooth(method = "loess", se = FALSE, color = "blue", linetype = "dashed") +
      geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "solid") +
      theme_minimal() +
      labs(x = "Year", y = "GDP Per Capita ($)", title = paste("Gross Domestic Product Per Capita by Year in", input$gdp_country)) +
      scale_x_continuous(breaks = seq(min(df_country$year), max(df_country$year), by = 5)) +
      scale_color_manual(values = brewer.pal(min(length(valid_countries_gdp), 9), "Set1")) +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold")
      )
    
    p_plotly <- ggplotly(p, tooltip = c("year", "GDP_Per_capita")) %>%
      layout(hoverlabel = list(bgcolor = "lightblue"))
    
    p_plotly
  })
  
  output$ppp_plot <- renderPlotly({
    req(input$ppp_country)
    
    df_country <- subset(filtered_ppp, country == input$ppp_country)
    
    p <- ggplot(df_country, aes(x = year, y = PPP_Per_capita, color = country, linetype = country)) +
      geom_point(size = 2) +
      geom_smooth(method = "loess", se = FALSE, color = "blue", linetype = "dashed") +
      geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "solid") +
      theme_minimal() +
      labs(x = "Year", y = "PPP Per Capita ($)", title = paste("Purchasing Power Parity Per Capita by Year in", input$ppp_country)) +
      scale_x_continuous(breaks = seq(min(df_country$year), max(df_country$year), by = 5)) +
      scale_color_manual(values = brewer.pal(min(length(valid_countries_ppp), 9), "Set1")) +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold")
      )
    
    p_plotly <- ggplotly(p, tooltip = c("year", "PPP_Per_capita")) %>%
      layout(hoverlabel = list(bgcolor = "lightblue"))
    
    p_plotly
  })
  
  output$ppp_plot <- renderPlotly({
    req(input$ppp_country)
    
    df_country <- subset(filtered_ppp, country == input$ppp_country)
    
    p <- ggplot(df_country, aes(x = year, y = PPP_Per_capita, color = country, linetype = country)) +
      geom_point(size = 2) +
      geom_smooth(method = "loess", se = FALSE, color = "blue", linetype = "dashed") +
      geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "solid") +
      theme_minimal() +
      labs(x = "Year", y = "PPP Per Capita ($)", title = paste("Purchasing Power Parity Per Capita by Year in", input$ppp_country)) +
      scale_x_continuous(breaks = seq(min(df_country$year), max(df_country$year), by = 5)) +
      scale_color_manual(values = brewer.pal(min(length(valid_countries_ppp), 9), "Set1")) +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold")
      )
    
    p_plotly <- ggplotly(p, tooltip = c("year", "PPP_Per_capita")) %>%
      layout(hoverlabel = list(bgcolor = "lightblue"))
    
    p_plotly
  })
  
  output$cehs_plot <- renderPlotly({
    req(input$cehs_country)
    
    df_country <- subset(filtered_cehs, country == input$cehs_country)
    
    p <- ggplot(df_country, aes(x = year, y = CEHS_Index, color = country, linetype = country)) +
      geom_point(size = 2) +
      geom_smooth(method = "loess", se = FALSE, color = "blue", linetype = "dashed") +
      geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "solid") +
      theme_minimal() +
      labs(x = "Year", y = "CEHS Index", title = paste("Coverage of Essential Health Services Index by Year in", input$cehs_country)) +
      scale_x_continuous(breaks = seq(min(df_country$year), max(df_country$year), by = 5)) +
      scale_color_manual(values = brewer.pal(min(length(valid_countries_cehs), 9), "Set1")) +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold")
      )
    
    p_plotly <- ggplotly(p, tooltip = c("year", "CEHS_Index")) %>%
      layout(hoverlabel = list(bgcolor = "lightblue"))
    
    p_plotly
  })
  
  output$haq_plot <- renderPlotly({
    req(input$haq_country)
    
    df_country <- subset(filtered_haq, country == input$haq_country)
    
    p <- ggplot(df_country, aes(x = year, y = Healthcare_Access_Quality_Index, color = country, linetype = country)) +
      geom_point(size = 2) +
      geom_smooth(method = "loess", se = FALSE, color = "blue", linetype = "dashed") +
      geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "solid") +
      theme_minimal() +
      labs(x = "Year", y = "HAQ Index", title = paste("Healthcare Access Quality Index by Year in", input$haq_country)) +
      scale_x_continuous(breaks = seq(min(df_country$year), max(df_country$year), by = 5)) +
      scale_color_manual(values = brewer.pal(min(length(valid_countries_haq), 9), "Set1")) +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold")
      )
    
    p_plotly <- ggplotly(p, tooltip = c("year", "Healthcare_Access_Quality_Index")) %>%
      layout(hoverlabel = list(bgcolor = "lightblue"))
    
    p_plotly
  })
  
  output$edu_plot <- renderPlotly({
    req(input$edu_country)
    
    df_country <- subset(filtered_edu, country == input$edu_country)
    
    p <- ggplot(df_country, aes(x = year, y = Average_Years_Of_Education, color = country, linetype = country)) +
      geom_point(size = 2) +
      geom_smooth(method = "loess", se = FALSE, color = "blue", linetype = "dashed") +
      geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "solid") +
      theme_minimal() +
      labs(x = "Year", y = "Years of Education", title = paste("Average Years of Education Trends by Year in", input$edu_country)) +
      scale_x_continuous(breaks = seq(min(df_country$year), max(df_country$year), by = 5)) +
      scale_color_manual(values = brewer.pal(min(length(valid_countries_edu), 9), "Set1")) +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold")
      )
    
    p_plotly <- ggplotly(p, tooltip = c("year", "Average_Years_Of_Education")) %>%
      layout(hoverlabel = list(bgcolor = "lightblue"))
    
    p_plotly
  })
  
  output$co2_plot <- renderPlotly({
    req(input$co2_country)
    
    df_country <- subset(filtered_co2, country == input$co2_country)
    
    p <- ggplot(df_country, aes(x = year, y = CO2_mt_Per_Capita, color = country, linetype = country)) +
      geom_point(size = 2) +
      geom_smooth(method = "loess", se = FALSE, color = "blue", linetype = "dashed") +
      geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "solid") +
      theme_minimal() +
      labs(x = "Year", y = "CO2 Emissions (kt)", title = paste("CO2 Emissions per Capita Trends by Year in", input$co2_country)) +
      scale_x_continuous(breaks = seq(min(df_country$year), max(df_country$year), by = 5)) +
      scale_color_manual(values = brewer.pal(min(length(valid_countries_co2), 9), "Set1")) +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold")
      )
    
    p_plotly <- ggplotly(p, tooltip = c("year", "CO2_mt_Per_Capita")) %>%
      layout(hoverlabel = list(bgcolor = "lightblue"))
    
    p_plotly
  })
  
  output$deaths_total_econ_plot <- renderPlotly({
    req(input$deaths_total_econ_country)
    
    df_country <- subset(deaths_total_econ, country == input$deaths_total_econ_country )
    names(df_country)[names(df_country) == 'deaths_per_1000'] <- 'deaths1000'
    constant_cols <- sapply(df_country, function(col) length(unique(col)) <= 3)
    data <- subset(df_country[, -which(constant_cols)], select = -c(deaths))
    
    # Calculate correlations
    correlation_matrix <- round(cor(na.omit(data[,-c(1)])), 2)
    correlation_matrix[upper.tri(correlation_matrix)] <- NA
    cor_mat <- melt(correlation_matrix, na.rm = TRUE)
    
    g1 <- ggplot(data = cor_mat, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      theme(
        axis.title = element_blank(),
        axis.text.x = element_text(size = 11, angle = 45),
        axis.text.y = element_text(size = 11)
      ) +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), name = "Correlation") +
      coord_fixed() +
      geom_text(aes(label = value), color = "black", size = 4)
    
    # Convert ggplot to plotly object with custom hover text
    p_plotly <- ggplotly(g1, tooltip = c("Var1", "Var2")) %>%
      layout(hoverlabel = list(bgcolor = "lightblue"))
    
    # Display the interactive plotly plot
    p_plotly
  })
  
  output$deaths_sex_total_econ_plot <- renderPlotly({
    req(input$deaths_sex_total_econ_country)
    
    df_country_male <- subset(deaths_sex_econ_total, country == input$deaths_sex_total_econ_country & sex == "Male")
    constant_cols <- sapply(df_country_male, function(col) length(unique(col)) <= 3)
    datam <- subset(df_country_male[, -which(constant_cols)], select = -c(deaths))
    
    # Calculate correlations Male
    correlation_matrix_M <- round(cor(na.omit(datam[,-c(1)])), 2)
    correlation_matrix_M[upper.tri(correlation_matrix_M)] <- NA
    cor_mat_M <- melt(correlation_matrix_M, na.rm = T)
    
    
    df_country_female <- subset(deaths_sex_econ_total, country == input$deaths_sex_total_econ_country & sex == "Female")
    constant_cols <- sapply(df_country_female, function(col) length(unique(col)) <= 3)
    dataf <- subset(df_country_female[, -which(constant_cols)], select = -c(deaths))
    
    # Calculate correlations Female
    correlation_matrix_F <- round(cor(na.omit(dataf[,-c(1)])), 2)
    correlation_matrix_F[upper.tri(correlation_matrix_F)] <- NA
    cor_mat_F <- melt(correlation_matrix_F, na.rm = T)
    
    combined_cor_mat <- rbind(cor_mat_F, cor_mat_M)
    combined_cor_mat$Gender <- c(rep("Female", nrow(cor_mat_F)), rep("Male", nrow(cor_mat_M)))
    
    g1 <- ggplot(data = combined_cor_mat, aes(x=Var1, y=Var2, fill=value)) + geom_tile() +
      theme(
        axis.title = element_blank(),
        axis.text.x = element_text(size = 11, angle = 45),
        axis.text.y = element_text(size = 11)
      ) +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), name=paste("Correlation")) + coord_fixed() +
      geom_text(aes(label = value), color = "black", size = 4)+
      facet_wrap(~ Gender, ncol = 2)
    
    # Convert ggplot to plotly object with custom hover text
    p_plotly <- ggplotly(g1, tooltip = c("Var1", "Var2")) %>%
      layout(hoverlabel = list(bgcolor = "lightblue"))
    
    # Display the interactive plotly plot
    p_plotly
  })
  
  output$deaths_groups_econ_total_plot <- renderPlotly({
    req(input$deaths_groups_econ_total_country)
    
    df_country <- subset(deaths_groups_econ_total, country == input$deaths_groups_econ_total_country )
    constant_cols <- sapply(df_country, function(col) length(unique(col)) <= 3)
    data <- subset(df_country[, -which(constant_cols)]) #select = -c(deaths)
    
    # Calculate correlations
    correlation_matrix <- round(cor(na.omit(data[,-c(1)])), 2)
    correlation_matrix[upper.tri(correlation_matrix)] <- NA
    cor_mat <- melt(correlation_matrix, na.rm = TRUE)
    
    g1 <- ggplot(data = cor_mat, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      theme(
        axis.title = element_blank(),
        axis.text.x = element_text(size = 11, angle = 45),
        axis.text.y = element_text(size = 11)
      ) +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), name = "Correlation") +
      coord_fixed() +
      geom_text(aes(label = value), color = "black", size = 3.5)
    
    # Convert ggplot to plotly object with custom hover text
    p_plotly <- ggplotly(g1, tooltip = c("Var1", "Var2")) %>%
      layout(hoverlabel = list(bgcolor = "lightblue"))
    
    # Display the interactive plotly plot
    p_plotly
  })
}

# Run the application
shinyApp(ui = ui, server = server)