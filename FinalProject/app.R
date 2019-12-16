###### CURRENT CODE #####


library(maps)
library(shiny)
library(usmap)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
## Cleaning Data 
details = read_csv("stormevents.csv")
EVENT_TYPE_Key = read_csv("EVENT_TYPE_Key.csv")



details = details %>% select(STATE, MONTH_NAME, DAMAGE_PROPERTY, CZ_NAME)

details$MONTH_NAME = as.factor(details$MONTH_NAME)
details$MONTH_NAME = factor(details$MONTH_NAME, levels = c("January",
                                                           "February",
                                                           "March",
                                                           "April",
                                                           "May",
                                                           "June",
                                                           "July",
                                                           "August",
                                                           "September",
                                                           "October",
                                                           "November",
                                                           "December"))

month_name = c("All" , levels(details$MONTH_NAME))
names(month_name) = month_name

details$DAMAGE_PROPERTY = str_replace_all(details$DAMAGE_PROPERTY, fixed("."), "")
details$DAMAGE_PROPERTY = str_replace_all(details$DAMAGE_PROPERTY, fixed("K"), "000")
details$DAMAGE_PROPERTY = str_replace_all(details$DAMAGE_PROPERTY, fixed("M"), "000000")
details$DAMAGE_PROPERTY = str_replace_all(details$DAMAGE_PROPERTY, fixed("B"), "000000000")
details$DAMAGE_PROPERTY[is.na(details$DAMAGE_PROPERTY)] = 0
details = details %>% mutate(DAMAGE_PROPERTY = as.numeric(DAMAGE_PROPERTY))
details = details %>% rename(state = STATE)


#STATE WITHOUT MONTH#
state_data = details %>% select("state", "DAMAGE_PROPERTY")
state_data = state_data[tolower(state_data$state) %in% tolower(state.name),]
state_damage = state_data %>% group_by(state) %>% summarize(sumdamage = sum(DAMAGE_PROPERTY, na.rm = TRUE))
#END STATE WITHOUT MONTH#

#STATE WITH MONTH#
state_datawmonth =  details %>% select("state", "DAMAGE_PROPERTY", "MONTH_NAME")
state_datawmonth = state_datawmonth[tolower(state_datawmonth$state) %in% tolower(state.name),]
state_datawmonth = state_datawmonth %>% group_by(state, MONTH_NAME) %>%
  summarize(sumdamage = sum(DAMAGE_PROPERTY, na.rm = TRUE))
#END STATE WITH MONTH#

#COUNTY WITHOUT MONTH#
county_data = details %>% select("state", "DAMAGE_PROPERTY", "CZ_NAME")
county_data$county = county_data$CZ_NAME
county_data$CZ_NAME = NULL
county_data = county_data %>% filter(tolower(state) %in% tolower(state.name))
county_data$polyname = paste(tolower(county_data$state), tolower(county_data$county), sep = ",")
county_data = county_data %>% 
  filter(polyname %in% county.fips$polyname)
county_data = county.fips %>% left_join(county_data, by = "polyname")
county_data = county_data %>% 
  select(DAMAGE_PROPERTY, fips, state, county) %>%
  mutate(fips = str_pad(fips, 5, pad = "0"))
county_data = county_data %>% group_by(fips) %>% 
  summarize(sumdamage = sum(DAMAGE_PROPERTY, na.rm = FALSE))
county_data$sumdamage[is.na(county_data$sumdamage)] = 0
#END COUNTY WITHOUT MONTH#
#COUNTY WITH MONTH#
county_datawmonth = details %>% select("state", "DAMAGE_PROPERTY", "MONTH_NAME", "CZ_NAME")
county_datawmonth$county = county_datawmonth$CZ_NAME
county_datawmonth$CZ_NAME = NULL
county_datawmonth = county_datawmonth %>% filter(tolower(state) %in% tolower(state.name))
county_datawmonth$polyname = paste(tolower(county_datawmonth$state), tolower(county_datawmonth$county), sep = ",")
county_datawmonth = county_datawmonth %>% 
  filter(polyname %in% county.fips$polyname)
county_datawmonth = county_datawmonth %>% right_join(county.fips, by = "polyname")
county_datawmonth = county_datawmonth %>% mutate(fips = str_pad(fips, 5, pad = "0"))
county_datawmonth = county_datawmonth %>% group_by(fips, MONTH_NAME) %>%
  summarize(sumdamage = sum(DAMAGE_PROPERTY, na.rm = TRUE)) %>% complete(MONTH_NAME) %>% replace(is.na(.), 0)
#END COUNTY WITH MONTH#

## Shiny 
states_regions = data.frame(States = state.name, Abb = state.abb)
states_regions = states_regions %>% mutate(Region = case_when(state.abb %in% .northeast_region ~ "Northeast",
                                                              state.abb %in% .south_region ~ "South",
                                                              state.abb %in% .midwest_region ~ "Midwest",
                                                              state.abb %in% .west_region ~ "West",
                                                              state.abb %in% state.abb ~ "All"))
ui = fluidPage(
  
  
  #this script makes the plot scale with screen size
  tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),

  # Application title
  titlePanel("NOAA Property Damage Index"),
  fluidRow(
    
    
    # Sidebar with selections of filtering data by month, state, region,
    #view, and whether to include or exclude california
    sidebarLayout(
      sidebarPanel("Options", selectInput('inccal', 'Include or Exclude California', 
                                          c("Include California", "Exclude California")),
                   selectInput("Month", "Choose a Month",
                               choices = month_name),
                   selectInput("State", "Choose a State",
                               choices = list(All = "All",
                                              Northeast = states_regions$States[states_regions$Region == "Northeast"],
                                              Midwest = states_regions$States[states_regions$Region == "Midwest"],
                                              South = states_regions$States[states_regions$Region == "South"],
                                              West = states_regions$States[states_regions$Region == "West"])),
                   selectInput("View", "County Level Data?",
                               choices = c("States", "Counties")),
                   selectInput("Region", "Filter by Region",
                               choices = c("All",
                                           "Northeast",
                                           "Midwest",
                                           "West",
                                           "South")), width = "2"),
      # Conditional panel, output "map" if california included,
      # output "map2" if california excluded.
      mainPanel(
        conditionalPanel(
          "input.inccal == 'Include California'",
          plotOutput("Map", width = "auto")
        ),
        
        conditionalPanel(
          "input.inccal == 'Exclude California'",
          plotOutput("Map2", width = "auto")
        )
      )
    ),
    tags$footer(paste('Made by: Amogh Bhoopalam, Josh Janda, Anna Wysocka, Daniel Cherny, Sophia Ding. For STAT 385'))
  )
)

server = function(input, output, session) {
  
  current_selection = reactiveVal(NULL)
  
  observeEvent(input$Region, {
    current_selection(input$Region)
  })
  #saves region selection by user so when updating it selects correct region
  
  #due to conditionials including "all", extra code was needed.
  output$Map = renderPlot({
    if (input$View == "States") {
      if (input$Month == "All") {
        if (input$State == "All") {
          #case when view is states, all months, all states
          updateSelectInput(session, "Region",
                            "Filter by Region",
                            choices = c("All",
                                        "Northeast",
                                        "Midwest",
                                        "West",
                                        "South"),
                            selected = current_selection())
          region = states_regions %>% filter(Region == input$Region) %>% select(Abb)
          plot_usmap(regions = c("states"), include = as.vector(region$Abb), data = state_damage, values = "sumdamage") +
            scale_fill_continuous(name = "Property Damage",  label = scales::comma) +
            theme(legend.position = "right",
                  legend.key.size = unit(1, "cm"),
                  legend.key.width = unit(0.5,"cm"),
                  legend.text = element_text(size = unit(15, "cm")),
                  legend.title = element_text(size = unit(15, "cm")))
          
        } else{
          #case when view is states, all months, specific state
          mapdata = state_damage %>% filter(tolower(state) == tolower(input$State))
          updateSelectInput(session, "Region",
                            "Filter by Region\nNot Available When Selecting Individual States",
                            choices = c("All"))
          plot_usmap(regions = c("states"), include = c(input$State),
                     data = mapdata, values = "sumdamage") +
            scale_fill_continuous(name = "Property Damage", label = scales::comma) +
            theme(legend.position = "right",
                  legend.key.size = unit(1, "cm"),
                  legend.key.width = unit(0.5,"cm"),
                  legend.text = element_text(size = unit(15, "cm")),
                  legend.title = element_text(size = unit(15, "cm")))
        }
      } else{
        if (input$State == "All") {
          #case when view is states, all states, specific month
          updateSelectInput(session, "Region",
                            "Filter by Region",
                            choices = c("All",
                                        "Northeast",
                                        "Midwest",
                                        "West",
                                        "South"),
                            selected = current_selection())
          mapdata = state_datawmonth %>% filter(MONTH_NAME == input$Month)
          region = states_regions %>% filter(Region == input$Region) %>% select(Abb)
          plot_usmap(regions = c("states"), include = as.vector(region$Abb), data = mapdata, values = "sumdamage") +
            scale_fill_continuous(name = "Property Damage", label = scales::comma) +
            theme(legend.position = "right",
                  legend.key.size = unit(1, "cm"),
                  legend.key.width = unit(0.5,"cm"),
                  legend.text = element_text(size = unit(15, "cm")),
                  legend.title = element_text(size = unit(15, "cm")))
          
        } else{
          #case when view is states, specific state and month
          mapdata = state_datawmonth %>% filter(MONTH_NAME == input$Month, tolower(state) == tolower(input$State))
          updateSelectInput(session, "Region",
                            "Filter by Region\nNot Available When Selecting Individual States",
                            choices = c("All"))
          plot_usmap(regions = c("states"), include = c(input$State),
                     data = mapdata, values = "sumdamage") +
            scale_fill_continuous(name = "Property Damage", label = scales::comma) +
            theme(legend.position = "right",
                  legend.key.size = unit(1, "cm"),
                  legend.key.width = unit(0.5,"cm"),
                  legend.text = element_text(size = unit(15, "cm")),
                  legend.title = element_text(size = unit(15, "cm")))
          
        }
      }
    }
    else{
      if (input$Month == "All") {
        if (input$State == "All") {
          #case when view is counties, all states and all months
          updateSelectInput(session, "Region",
                            "Filter by Region",
                            choices = c("All",
                                        "Northeast",
                                        "Midwest",
                                        "West",
                                        "South"),
                            selected = current_selection())
          region = states_regions %>% filter(Region == input$Region) %>% select(Abb)
          plot_usmap(regions = c("counties"), include = as.vector(region$Abb), data = county_data, values = "sumdamage") +
            scale_fill_continuous(name = "Property Damage", label = scales::comma) +
            theme(legend.position = "right",
                  legend.key.size = unit(1, "cm"),
                  legend.key.width = unit(0.5,"cm"),
                  legend.text = element_text(size = unit(15, "cm")),
                  legend.title = element_text(size = unit(15, "cm")))
          
        } else{
          #case when view is counties, all months and specific state
          mapdata = county_data %>% filter(substr(fips, 1, 2) %in% fips(input$State))
          updateSelectInput(session, "Region",
                            "Filter by Region\nNot Available When Selecting Individual States",
                            choices = c("All"))
          plot_usmap(regions = c("counties"), include = c(input$State),
                     data = county_data, values = "sumdamage") +
            scale_fill_continuous(name = "Property Damage", label = scales::comma) +
            theme(legend.position = "right",
                  legend.key.size = unit(1, "cm"),
                  legend.key.width = unit(0.5,"cm"),
                  legend.text = element_text(size = unit(15, "cm")),
                  legend.title = element_text(size = unit(15, "cm")))
          
        }
      } else{
        if (input$State == "All") {
          #case when view is counties, all states and specific month
          updateSelectInput(session, "Region",
                            "Filter by Region",
                            choices = c("All",
                                        "Northeast",
                                        "Midwest",
                                        "West",
                                        "South"),
                            selected = current_selection())
          region = states_regions %>% filter(Region == input$Region) %>% select(Abb)
          mapdata = county_datawmonth %>% filter(MONTH_NAME == input$Month)
          plot_usmap(regions = c("counties"), include = as.vector(region$Abb), data = mapdata, values = "sumdamage") +
            scale_fill_continuous(name = "Property Damage", label = scales::comma) +
            theme(legend.position = "right",
                  legend.key.size = unit(1, "cm"),
                  legend.key.width = unit(0.5,"cm"),
                  legend.text = element_text(size = unit(15, "cm")),
                  legend.title = element_text(size = unit(15, "cm")))
          
        } else{
          #case when view is counties, specific state and specific month
          mapdata = county_datawmonth %>% filter(MONTH_NAME == input$Month, substr(fips, 1, 2) %in% fips(input$State))
          updateSelectInput(session, "Region",
                            "Filter by Region\nNot Available When Selecting Individual States",
                            choices = c("All"))
          plot_usmap(regions = c("counties"), include = c(input$State),
                     data = mapdata, values = "sumdamage") +
            scale_fill_continuous(name = "Property Damage", label = scales::comma) +
            theme(legend.position = "right",
                  legend.key.size = unit(1, "cm"),
                  legend.key.width = unit(0.5,"cm"),
                  legend.text = element_text(size = unit(15, "cm")),
                  legend.title = element_text(size = unit(15, "cm")))
          
        }
      }
    }
  })
  output$Map2 = renderPlot({
    if (input$View == "States") {
      if (input$Month == "All") {
        if (input$State == "All") {
          #case when view is states, all months, all states
          updateSelectInput(session, "Region",
                            "Filter by Region",
                            choices = c("All",
                                        "Northeast",
                                        "Midwest",
                                        "West",
                                        "South"),
                            selected = current_selection())
          region = states_regions %>% filter(Region == input$Region) %>% select(Abb)
          plot_usmap(regions = c("states"), include = as.vector(region$Abb), exclude = c("CA"),
                     data = state_damage, values = "sumdamage") +
            scale_fill_continuous(name = "Property Damage",  label = scales::comma) +
            theme(legend.position = "right",
                  legend.key.size = unit(1, "cm"),
                  legend.key.width = unit(0.5,"cm"),
                  legend.text = element_text(size = unit(15, "cm")),
                  legend.title = element_text(size = unit(15, "cm")))
          
        } else{
          #case when view is states, all months, specific state
          mapdata = state_damage %>% filter(tolower(state) == tolower(input$State))
          updateSelectInput(session, "Region",
                            "Filter by Region\nNot Available When Selecting Individual States",
                            choices = c("All"))
          plot_usmap(regions = c("states"), include = c(input$State), exclude = c("CA"),
                     data = mapdata, values = "sumdamage") +
            scale_fill_continuous(name = "Property Damage", label = scales::comma) +
            theme(legend.position = "right",
                  legend.key.size = unit(1, "cm"),
                  legend.key.width = unit(0.5,"cm"),
                  legend.text = element_text(size = unit(15, "cm")),
                  legend.title = element_text(size = unit(15, "cm")))
        }
      } else{
        if (input$State == "All") {
          #case when view is states, all states, specific month
          updateSelectInput(session, "Region",
                            "Filter by Region",
                            choices = c("All",
                                        "Northeast",
                                        "Midwest",
                                        "West",
                                        "South"),
                            selected = current_selection())
          mapdata = state_datawmonth %>% filter(MONTH_NAME == input$Month)
          region = states_regions %>% filter(Region == input$Region) %>% select(Abb)
          plot_usmap(regions = c("states"), include = as.vector(region$Abb), exclude = c("CA"),
                     data = mapdata, values = "sumdamage") +
            scale_fill_continuous(name = "Property Damage", label = scales::comma) +
            theme(legend.position = "right",
                  legend.key.size = unit(1, "cm"),
                  legend.key.width = unit(0.5,"cm"),
                  legend.text = element_text(size = unit(15, "cm")),
                  legend.title = element_text(size = unit(15, "cm")))
          
        } else{
          #case when view is states, specific state and month
          mapdata = state_datawmonth %>% filter(MONTH_NAME == input$Month, tolower(state) == tolower(input$State))
          updateSelectInput(session, "Region",
                            "Filter by Region\nNot Available When Selecting Individual States",
                            choices = c("All"))
          plot_usmap(regions = c("states"), include = c(input$State), exclude = c("CA"),
                     data = mapdata, values = "sumdamage") +
            scale_fill_continuous(name = "Property Damage", label = scales::comma) +
            theme(legend.position = "right",
                  legend.key.size = unit(1, "cm"),
                  legend.key.width = unit(0.5,"cm"),
                  legend.text = element_text(size = unit(15, "cm")),
                  legend.title = element_text(size = unit(15, "cm")))
          
        }
      }
    }
    else{
      if (input$Month == "All") {
        if (input$State == "All") {
          #case when view is counties, all states and all months
          updateSelectInput(session, "Region",
                            "Filter by Region",
                            choices = c("All",
                                        "Northeast",
                                        "Midwest",
                                        "West",
                                        "South"),
                            selected = current_selection())
          region = states_regions %>% filter(Region == input$Region) %>% select(Abb)
          plot_usmap(regions = c("counties"), include = as.vector(region$Abb), exclude = c("CA"),
                     data = county_data, values = "sumdamage") +
            scale_fill_continuous(name = "Property Damage", label = scales::comma) +
            theme(legend.position = "right",
                  legend.key.size = unit(1, "cm"),
                  legend.key.width = unit(0.5,"cm"),
                  legend.text = element_text(size = unit(15, "cm")),
                  legend.title = element_text(size = unit(15, "cm")))
          
        } else{
          #case when view is counties, all months and specific state
          updateSelectInput(session, "Region",
                            "Filter by Region",
                            choices = c("All",
                                        "Northeast",
                                        "Midwest",
                                        "West",
                                        "South"),
                            selected = current_selection())
          mapdata = county_data %>% filter(substr(fips, 1, 2) %in% fips(input$State))
          updateSelectInput(session, "Region",
                            "Filter by Region\nNot Available When Selecting Individual States",
                            choices = c("All"))
          plot_usmap(regions = c("counties"), include = c(input$State), exclude = c("CA"),
                     data = mapdata, values = "sumdamage") +
            scale_fill_continuous(name = "Property Damage", label = scales::comma) +
            theme(legend.position = "right",
                  legend.key.size = unit(1, "cm"),
                  legend.key.width = unit(0.5,"cm"),
                  legend.text = element_text(size = unit(15, "cm")),
                  legend.title = element_text(size = unit(15, "cm")))
          
        }
      } else{
        if (input$State == "All") {
          #case when view is counties, all states and specific month
          updateSelectInput(session, "Region",
                            "Filter by Region",
                            choices = c("All",
                                        "Northeast",
                                        "Midwest",
                                        "West",
                                        "South"),
                            selected = current_selection())
          region = states_regions %>% filter(Region == input$Region) %>% select(Abb)
          mapdata = county_datawmonth %>% filter(MONTH_NAME == input$Month)
          plot_usmap(regions = c("counties"), include = as.vector(region$Abb), exclude = c("CA"),
                     data = mapdata, values = "sumdamage") +
            scale_fill_continuous(name = "Property Damage", label = scales::comma) +
            theme(legend.position = "right",
                  legend.key.size = unit(1, "cm"),
                  legend.key.width = unit(0.5,"cm"),
                  legend.text = element_text(size = unit(15, "cm")),
                  legend.title = element_text(size = unit(15, "cm")))
          
        } else{
          #case when view is counties, specific state and specific month
          updateSelectInput(session, "Region",
                            "Filter by Region",
                            choices = c("All",
                                        "Northeast",
                                        "Midwest",
                                        "West",
                                        "South"),
                            selected = current_selection())
          mapdata = county_datawmonth %>% filter(MONTH_NAME == input$Month, substr(fips, 1, 2) %in% fips(input$State))
          updateSelectInput(session, "Region",
                            "Filter by Region\nNot Available When Selecting Individual States",
                            choices = c("All"))
          
          plot_usmap(regions = c("counties"), include = c(input$State), exclude = c("CA"),
                     data = mapdata, values = "sumdamage") +
            scale_fill_continuous(name = "Property Damage", label = scales::comma) +
            theme(legend.position = "right",
                  legend.key.size = unit(1, "cm"),
                  legend.key.width = unit(0.5,"cm"),
                  legend.text = element_text(size = unit(15, "cm")),
                  legend.title = element_text(size = unit(15, "cm")))
          
        }
      }
    }
  })
}
# Run the application 
shinyApp(ui = ui, server = server)