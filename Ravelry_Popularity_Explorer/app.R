library(tidyverse)
library(shiny)
library(scales)
library(rsconnect)
library(readr)

all_patts_shiny <- read_csv("all_patts_shiny.csv")

theme_set(theme_bw())

ui <- fluidPage(
  selectInput("x_var", "Choose X-axis variable:", 
              choices = c("Average_Difficulty_Rating", 
                          "Average_Rating", 
                          "Craft", "Free", 
                          "Knitting_Needle_Size", 
                          "Number_of_Languages", 
                          "Pattern_Category", 
                          "Pattern_Parent_Category", 
                          "Pattern_Grandparent_Category", 
                          "Yarn_Weight")),
  selectInput("y_var", "Choose Y-axis variable:", 
              choices = c("Number_of_Favorites", 
                          "Number_of_Top_20_Appearances")),
  plotOutput("my_bar_plot")
)

server <- function(input, output) {
  output$my_bar_plot <- renderPlot({
    # Assuming 'data' is your dataset
    ggplot(all_patts_shiny, aes_string(x = input$x_var, y = input$y_var)) +
      geom_bar(stat = ifelse(input$y_var == "count", "count", "identity"),
               fill = "dodgerblue2") +
      scale_y_continuous(labels = label_comma()) +
      labs(title = paste("Bar Chart of", input$y_var, "by", input$x_var),
           x = input$x_var,
           y = input$y_var) +
      theme(axis.title.x = element_text(vjust = 0, 
                                        size = 15, 
                                        colour = "grey0", 
                                        face = "bold"),
            axis.title.y = element_text(vjust = 2, 
                                        size = 15, 
                                        colour = "grey0", 
                                        face = "bold"),
            plot.title = element_text(size = 18, 
                                      colour = "grey0", 
                                      face = "bold", 
                                      hjust = 0.5, 
                                      margin = margin(10, 0, 10, 0)),
            axis.text = element_text(colour = "grey0", 
                                     size = 12),
            axis.text.x = element_text(angle = 50,
                                       vjust = 0.5,
                                       hjust = 0.5))
  })
}

shinyApp(ui, server)