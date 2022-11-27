


library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)

data <- read_excel("foreign_students_by_nationality_2021_2022.xlsx")


colnames(data) <- c("Universite_Adi", "Universite_Turu", "Il_Adi", "Uyruk", "E", "K", "T")


data <- drop_na(data,Il_Adi)

# adjusting the data types

data <- transform(data,  E = as.numeric(E), K = as.numeric(K), T = as.numeric(T))



# The distribution of top n foreign students by their nations

df1 <- data %>%
  group_by(Uyruk) %>% 
  dplyr::summarise(Sum_Of_Male_Student = sum(E), Sum_of_Female_Student = sum(K), Sum_Of_Students = sum(T)) %>%
  dplyr::mutate(Ratio_Of_Male_Students = sprintf("%.2f",Sum_Of_Male_Student/Sum_Of_Students), Ratio_Of_Female_Students = sprintf("%.2f",Sum_of_Female_Student/Sum_Of_Students)) %>%
  dplyr::ungroup()


df2 <- 
  data %>%
  dplyr::group_by(Il_Adi) %>%
  dplyr::summarise(Sum_Of_Male_Student = sum(E), Sum_of_Female_Student = sum(K), Sum_Of_Students = sum(T),Count_Of_Uni = n_distinct(Universite_Adi)) %>%
  mutate(Ratio_Of_Male_Students = sprintf("%.2f",Sum_Of_Male_Student/Sum_Of_Students), Ratio_Of_Female_Students = sprintf("%.2f",Sum_of_Female_Student/Sum_Of_Students), Students_Per_Uni_On_Provincial_Basis = Sum_Of_Students/Count_Of_Uni) %>%
  ungroup() %>%
  select(Il_Adi, Sum_Of_Male_Student, Ratio_Of_Male_Students, Sum_of_Female_Student, Ratio_Of_Female_Students, Sum_Of_Students, Count_Of_Uni, Students_Per_Uni_On_Provincial_Basis)



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  
  
  tabsetPanel(
    tabPanel(
      title = "The distribution of top N foreign students by their nations",
      selectInput("uyruk","Nationality", choices = c(df1$Uyruk,"All"), multiple = TRUE , selected = "All"),
      radioButtons("max_row","Selecting By The Most Students Population", choices = list(3,5,7,10), selected = 5, inline = TRUE),
     
      
  tabPanel(
    title = "Table",
    DT::dataTableOutput("table")
    ),
  tabPanel(
    title = "Male/Female Distribution",
    plotOutput("plot")
  ),
  tabPanel(
    title = "Sum Of Students",
    plotOutput("plot2")
  )
  
    ),
  
  tabPanel(
    title = "Foreign Student's Distribution by Province",
    selectInput("il","Province", choices = c(df2$Il_Adi,"All"), multiple = TRUE , selected = "All"),
    
    tabPanel(
      title = "Table_Prov",
      DT::dataTableOutput("table1")
    )
  )


)

)




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$table <- DT::renderDataTable({
    if (input$uyruk == "All") {df1  %>%
        slice_max(order_by = Sum_Of_Male_Student, n = as.numeric(input$max_row))} else
          df1 %>% 
      dplyr::filter(Uyruk %in% input$uyruk)  %>%
      slice_max(order_by = Sum_Of_Male_Student, n = as.numeric(input$max_row))
  })
  
  output$plot <- renderPlot({
    if (input$uyruk == "All") {df1  %>%
        slice_max(order_by = Sum_Of_Male_Student, n = as.numeric(input$max_row)) %>%
        dplyr::select(Uyruk,Ratio_Of_Male_Students,Ratio_Of_Female_Students) %>%
        pivot_longer(cols = starts_with("Ratio"), names_to = "Male_Female_Ratio", values_to = "Rate") %>%
        ggplot(aes(x = Uyruk, y = Rate, fill = Male_Female_Ratio)) +
        geom_col()  +
        theme(axis.text.x = element_text(angle=60,vjust=1,hjust=1))} else 
          df1 %>% 
      dplyr::filter(Uyruk %in% input$uyruk)  %>%
      slice_max(order_by = Sum_Of_Male_Student, n = as.numeric(input$max_row)) %>%
      dplyr::select(Uyruk,Ratio_Of_Male_Students,Ratio_Of_Female_Students) %>%
      pivot_longer(cols = starts_with("Ratio"), names_to = "Male_Female_Ratio", values_to = "Rate") %>%
      ggplot(aes(x = Uyruk, y = Rate, fill = Male_Female_Ratio)) +
      geom_col()  +
      theme(axis.text.x = element_text(angle=60,vjust=1,hjust=1))
    
    
  })
  
  output$plot2 <- renderPlot({
    if (input$uyruk == "All") {df1  %>%
        slice_max(order_by = Sum_Of_Male_Student, n = as.numeric(input$max_row)) %>%
        ggplot(aes(x = reorder(Uyruk, - Sum_Of_Students) , y = Sum_Of_Students, fill = Uyruk)) +
        geom_col() +
        theme(axis.text.x = element_text(angle=60,vjust=1,hjust=1)) +
        xlab("") +
        ylab("Sum Of Students") } else 
          
          df1 %>% 
      dplyr::filter(Uyruk %in% input$uyruk)  %>%
      slice_max(order_by = Sum_Of_Male_Student, n = as.numeric(input$max_row)) %>%
      ggplot(aes(x = reorder(Uyruk, - Sum_Of_Students) , y = Sum_Of_Students, fill = Uyruk)) +
      geom_col() +
      theme(axis.text.x = element_text(angle=60,vjust=1,hjust=1))
    
    
    
  })
  
  
  output$table1 <- DT::renderDataTable({
    if (input$il == "All") {df2 } else
          df1 %>% 
      dplyr::filter(İl_Adi %in% input$il)    })
  
}






# Run the application 
shinyApp(ui = ui, server = server)







