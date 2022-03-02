library(tidyverse)
library(shiny)  
library(purrr)


mlb2016 <- read_csv("data/mlb2016.csv", col_types = cols(Salary = col_number(), 
                                                         Total.Value = col_number(), 
                                                         Avg.Annual = col_number()))

mlbdf_years <- mlb2016 %>% 
  separate(col = Years, into = c("Years.Total", "Years.Range"), sep = " ") %>%
  separate(col = Years.Range, into = c("First.Year.Of.Contract", "Other"), sep = c("-")) %>%
  separate(col = First.Year.Of.Contract, into = c("Other", "First.Year.Of.Contract"), sep = c("20")) %>%
  mutate(First.Year.Of.Contract = 2000 + as.numeric(First.Year.Of.Contract)) %>%
  select(-c(Other)) %>%
  filter(is.na(First.Year.Of.Contract) == FALSE)

mlb_1years <- mlb2016 %>% 
  separate(col = Years, into = c("Years.Total", "Years.Range"), sep = " ") %>%
  filter(Years.Total == 1) %>%
  mutate(First.Year.Of.Contract = 2016) %>%
  select(-c(Years.Range))

mlbdf <- bind_rows(mlbdf_years, mlb_1years) %>%
  mutate(First.Year.Of.Contract = as.character(First.Year.Of.Contract))


varchoices = names(mlbdf)[c(2,3,5,6)]


ui <- fluidPage(
  sidebarLayout(sidebarPanel(
    selectizeInput("playerchoice",
                   label = "Choose a player",
                   choices = levels(factor(mlbdf$Name)),
                   selected = "Clayton Kershaw"),
    radioButtons("filterchoice",
                 label = "Choose filters",
                 choices = varchoices)),
    mainPanel(plotOutput("pointplot"))
    )
  
)


server <- function(input, output, session) {
  
  chosen_player <- reactive({
    mlbdf %>% filter(Name == input$playerchoice)
  })


  
  point_plot <- reactive({
    ggplot(mlbdf, aes(x = Salary, y = Total.Value)) +
      geom_point(aes_string(color = input$filterchoice)) +
      geom_label(data = chosen_player(), aes(label = Name))
  })
  
  output$pointplot <- renderPlot({
    point_plot()
  })
  
}

shinyApp(ui, server)

## The purpose of this app is to show where MLB players (both the amount they are making in 2016 and their contracts total value) rank amongst other MLB players in 2016. The app labels the selected player on the scatter plot and it also gives options of changing the way the graph is colored based on variables the user can choose. (My original plan was to filter and only show the data of that certains players variable (ie. only show players in that position), but I just couldn't figure out how to filter it using the 2 inputs)