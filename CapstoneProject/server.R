##########################################
####   Main Libraries                 ####
##########################################
#install.packages("shiny")
library(shiny)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("dplyr")
library(dplyr)
#install.packages("tidyr")
library(tidyr)
#install.packages("DT")
library(DT)
#install.packages("knitr")
library(knitr)
#install.packages("kableExtra")
library(kableExtra)
#install.packages("ggthemes")
library(ggthemes)
#install.packages("plotly")
library(plotly)
#install.packages("rsconnect")
library(rsconnect)
#install.packages("shinythemes")
library(shinythemes)

##########################################
####   Attaching datasets             ####
##########################################

#data <- read.csv("data/employment_data.csv")
setwd(
  'C:/Users/blaze/Desktop/Uni/Cap-stone_project/CapstoneProject/CapstoneProject'
)
data_real <- readRDS("data/Dataset-of-Diabetes.rds")
data_c <- readRDS("data/framedClasses.rds")
data_h2 <- readRDS("data/preprocessed2.rds")
data_h4 <- read.csv("data/preprocessed4.csv")
test1 <- read.csv("data/anova_HDL_high_LDL_low.csv") #Low LDL and High HDL
test2 <- read.csv("data/anova_HDL_low_LDL_high.csv") #high LDL and low HDL
test3 <- read.csv("data/anova_HDL_low_LDL_low.csv") #Low LDL and low HDL
test4 <- read.csv("data/anova_HDL_high_LDL_high.csv") #high LDL and High HDL
data_h5 <- read.csv("data/preprocessed5.csv")

str(data_real)
#str(data)
## Setting datatables view

opts <- list(
  language = list(url = "//cdn.datatables.net/plug-ins/1.10.19/i18n/English.json"),
  pageLength = 30,
  searchHighlight = TRUE,
  orderClasses = TRUE,
  columnDefs = list(list(
    targets = c(1, 6), searchable = FALSE
  ))
)


##########################################
####   Shiny server                   ####
##########################################

server <- function(session, input, output) {
  
  ##################################
  #### MEDI-SIGHT RELEVANT CODE ####
  ##################################
  
  
  ################################################
  #### Panel: Dashboard>Summary>Tables & Pie Chart ####
  ################################################
  
  output$meddata <- renderDataTable({
    # filtered_DT() %>%
    data_real %>%
      datatable(
        .,
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 10, scrollX = T),
        colnames = c(
          "ID",
          "",
          "Gender",
          "Age",
          "Urea",
          "Creatinine",
          "HbA1c",
          "Cholesterol",
          "TG",
          "HDL",
          "LDL",
          "VLDL",
          "BMI",
          "Condition Status"
        )
      )
    
  })
  
  class_counts <- reactive({
    data_real %>%
      filter(AGE >= input$age_range[1] & AGE <= input$age_range[2]) %>%
      count(AGE, CLASS)
  })
  
  output$agevconditionPieChart <- renderPlot({
      colmap <-
            c(
            # No Diabetes
            "#bdb2ff",
            # Pre-Diabetes
            "#ffc6ff",
            # Diabetes
            "#fdffb6",
            # ITE
            "#caffbf",
            # NAFA DEG
            "#a8dadc"
          )
    ggplot(class_counts(), aes(x="", y=n, fill=CLASS)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      theme_void() +
      labs(title = "Class Counts by Age", fill = "Class") +
      scale_fill_manual(values = c(colmap))
  })
  
  
  ################################################
  #### Panel: Dashboard>Hypotheses> Hypothesis 2: Density Plot ####
  ################################################
  
  
  output$densityplot <- renderPlotly({
    # filter data by diabetes class
    if (input$densityclass == "All") {
      filtered_data <- data_h2
    } else if (input$densityclass == "Non-Diabetic") {
      filtered_data <- data_h2 %>% filter(Diebetic == 0)
    } else {
      filtered_data <- data_h2 %>% filter(Diebetic == 1)
    }
    
    # create density plot
    p <- plot_ly(filtered_data, x = ~Urea, color = ~get(input$Urea_score), type = "histogram")
    
    # customize layout
    p %>% layout(title = "Density Plot of Urea by Diabetes Class",
                 xaxis = list(title = "Urea"),
                 yaxis = list(title = "Density"),
                 barmode = "overlay")
  })
  
  ################################################
  #### Panel: Dashboard>Hypotheses> Hypothesis 4: Scatter Plot ####
  ################################################
  
  output$scatterplot <- renderPlotly({
    filtered_data <- subset(data_h4, Diebetic == input$scatterClass & AGE >= input$age[1] & 
                              AGE <= input$age[2] & BMI >= input$bmi[1] & BMI <= input$bmi[2])
    plot_ly(data = filtered_data, x = ~AGE, y = ~BMI, type = "scatter",
            mode = "markers",  colors = c("#1f77b4"),
            marker = list(size = 8, opacity = 0.8)) %>%
      layout(xaxis = list(title = "Age"),
             yaxis = list(title = "BMI"),
             title = "Interactive Scatter Plot",
             colorway = c("#1f77b4"))
  })
  
  
  
  ################################################
  #### Panel: Dashboard>Hypotheses> Hypothesis 5: ANOVA Tables ####
  ################################################
  
  output$anovaSummaryhl <- renderDataTable({
    test1 %>%
      datatable(
        .,
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 4, scrollX = T),
        colnames = c(
          "",
          "Df",
          "Sum Sq",
          "Mean Sq",
          "F value",
          "Pr(>F)"
        )
      )
    
  })
  
  output$anovaSummarylh <- renderDataTable({
    test2 %>%
      datatable(
        .,
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 4, scrollX = T),
        colnames = c(
          "",
          "Df",
          "Sum Sq",
          "Mean Sq",
          "F value",
          "Pr(>F)"
        )
      )
    
  })
  
  output$anovaSummaryll <- renderDataTable({
    test3 %>%
      datatable(
        .,
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 4, scrollX = T),
        colnames = c(
          "",
          "Df",
          "Sum Sq",
          "Mean Sq",
          "F value",
          "Pr(>F)"
        )
      )
    
  })
  
  output$anovaSummaryhh <- renderDataTable({
    test4 %>%
      datatable(
        .,
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 4, scrollX = T),
        colnames = c(
          "",
          "Df",
          "Sum Sq",
          "Mean Sq",
          "F value",
          "Pr(>F)"
        )
      )
    
  })
  
  output$scatter_plot <- renderPlot({
    data_h5$Diebetic <- as.factor(data_h5$Diebetic)
    ggplot(data_h5, aes(x = LDL, y = HDL, color = Diebetic)) +
      geom_point() +
      scale_color_manual(values = c("red", "green"), labels = c("No", "Yes")) +
      labs(x = "LDL level", y = "HDL level") +
      ggtitle("Relation of LDL and HDL levels with Diabetes") +
      geom_vline(xintercept = input$ldl, linetype = "dashed", color = "blue") +
      geom_hline(yintercept = input$hdl, linetype = "dashed", color = "blue")
  })
  
  ################################################
  #### Panel: Documentation                   ####
  ################################################
  
  getPageDoc <- function() {
    return(includeHTML("gesrmarkdown.html"))
  }
  output$doc <- renderUI({
    getPageDoc()
  })
  
  
  ################################################
  #### Panel: About                           ####
  ################################################
  
  getPageAbo <- function() {
    return(includeHTML("about.html"))
  }
  output$abo <- renderUI({
    getPageAbo()
  })
  
}

