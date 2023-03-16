#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('tidyr')
#install.packages('shiny')
#install.packages('shinydashboard')
library('dplyr')
library('tidyr')
library('ggplot2')
library('shiny')
library('shinydashboard')
#library('hrbrthemes')
data_dir <- "D:/Data science/01_DS_Project/Data - Copy/file_csv/"

#Import the datasets
feb_data <- read.csv(file.path(data_dir, "modified_data.csv"))
aug_data <- read.csv(file.path(data_dir, "modified_data_2.csv"))
dec_data <- read.csv(file.path(data_dir, "modified_data_3.csv"))


# Function to calculate and display the visualization for ride time by membership
custom_func_1 <- function(df) {
   df[-1,] %>%  
    filter(member_casual != "" & ride_duration_1 !="" & !is.na(ride_duration_1)) %>% 
    group_by(member_casual) %>% 
    summarise(total_ride_time = sum(as.numeric(ride_duration_1))) %>% 
    ggplot(aes(x = member_casual, y = total_ride_time, fill = member_casual)) + 
    geom_bar(stat = "identity") +
    ggtitle("Difference between the ride time of member and casual riders") +
    xlab("Membership Type") +
    ylab("Total Ride Time") +
    theme_bw()
}

# Function to calculate and display visualization for ride time by weekday
custom_func_2 <- function(df) {
  df<-df[-1,] %>%
    filter(weekday != "" & ride_duration_1 !="" & !is.na(ride_duration_1)) %>% 
    group_by(weekday) %>% 
    summarise(total_ride_time = sum(as.numeric(ride_duration_1))) %>% 
    ggplot(aes(x = weekday, y = total_ride_time, group = 1)) +
    geom_line(color = "black") +
    geom_point(shape = 21, color = "black", fill = "#69b3a2", size = 6) +
    ggtitle("Ride duration of member and casual riders by weekday") +
    xlab("Weekday") +
    ylab("Ride Duration (by hours)") +
    scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
    scale_y_continuous(limits = c(0, 1200), breaks = seq(0, 1200, by = 200))+   
    geom_text(aes(label = round(total_ride_time, 2), y = total_ride_time), vjust = -1)+
    theme_bw()
}
# Apply custom_func_1 and custom_func_2 to datasets
plots1 <- lapply(list(feb_data, aug_data, dec_data), custom_func_1)
plots2 <- lapply(list(feb_data, aug_data, dec_data), custom_func_2)
# Use shinydashboard to create an interactive dashboard

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    selectInput("dataset", "Select a dataset", choices = c("feb_data", "aug_data","dec_data")),
    radioButtons("plot", "Select a plot", choices = c("Plot 1", "Plot 2"))
  ),
  dashboardBody(
    uiOutput("output_plot")
  )
)

server <- function(input, output) {
  output$output_plot <- renderUI({
    if (input$plot == "Plot 1") {
      if (input$dataset == "feb_data") {
        plots1[[1]]
      } else if (input$dataset == "aug_data") {
        plots1[[2]]
      } else {
        plots1[[3]]
      }
    } else {
      if (input$dataset == "feb_data") {
        plots2[[1]]
      } else if (input$dataset == "aug_data") {
        plots2[[2]]
      } else {
        plots2[[3]]
      }
    }
  })
}

shinyApp(ui=ui, server=server)


