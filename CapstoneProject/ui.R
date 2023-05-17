##########################################
####   Shiny ui                       ####
##########################################
library(shinyWidgets)
library(shiny)
library(plotly)
library(shinythemes)
library(DT)
library(rsconnect)
# ------------------
# Main title section
# ------------------
setwd(
  'C:/Users/blaze/Desktop/Uni/Cap-stone_project/CapstoneProject/CapstoneProject'
)
#data_real <- readRDS("data/Dataset-of-Diabetes.rds")

ui <- navbarPage(
  "Medi-Sight Customer Analysis",
  theme = shinytheme("flatly"),
  tabPanel(
    "Dashboard",
    # App title ----
    titlePanel(div(
      windowTitle = "Medi-SightAnalysis",
      #img(src = "www/croppedheaderimage.jpg", width = "100%", class = "bg"),
    )),
    
    tags$br(),
    
    
    ##########################################
    ####  Panel: Dashboard>Summary             ####
    ##########################################
    
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Summary",
        ################################################
        #### Panel: Dashboard>Summary>Tables & Pie Chart ####
        ################################################
        
        # ------------------
        # ranking $ pie chart section
        # ------------------
        sidebarLayout(
          sidebarPanel(
            h3("Utalised Dataset"),
            h5("Sampled dataset containing 1000 records with an overall focus on patients which have diabetics disease. The dataset primarily features numetically based values, with two columns being character based, although these may be changes for visualisation purposes. "),
            h5("ie, the class column being converted to numeric counterparts.")
            ),
          mainPanel(
            dataTableOutput("meddata")
          ),
        ),
        tags$hr(),
        
        sidebarLayout(
          sidebarPanel(
            h3("Utalised Dataset"),
            sliderInput("age_range", "Select Age Range:",
                        min = min(data_real$AGE), max = max(data_real$AGE),
                        value = c(min(data_real$AGE), max(data_real$AGE)), step = 1),
          ),
          
          mainPanel(
            #tableOutput("MedData"),
            tabsetPanel(
              type = "tabs",
              #tabPanel("Dataset", dataTableOutput("meddata")),
              tabPanel("Class Distribution", plotOutput(outputId = "agevconditionPieChart"))
            ),
            tags$br()
          )
        ),
        tags$hr(),
        
      ),
      
      
      ################################################
      #### Panel: Dashboard>Plots                      ####
      ################################################
      
      tabPanel(
        "Hypotheses",
        
        sidebarLayout(
          # --------------------
          # Medi-Sight K-nearest Neighbours plot section
          # --------------------
          
          
          
          # --------------------
          # Medi-Sight Density plot section
          # --------------------
          
          sidebarPanel(
            radioButtons("densityclass", "Select Diabetes Class", choices = c("All", "Non-Diabetic", "Diabetic"), selected = "All"),
            selectInput("Urea_score", "Variable", choices = c("Urea"), selected = "Urea")
          ),
          mainPanel(
            h3("Distribution Plot"),
            plotlyOutput("densityplot"),
              tags$br(),
              tags$br()
          )
        ),
        tags$hr(),
        
        # --------------------
        # Medi-Sight Scatter plot section
        # --------------------
        sidebarLayout(
          
          sidebarPanel(
            sliderInput("scatterClass", "Select class", min = 0, max = 1, value = 0, step = 1),
            p("0: Non Diabetic, 1: Diabetic"),
            sliderInput("age", "Select Age", min = 20, max = 80, value = c(20, 80), step = 1),
            sliderInput("bmi", "Select BMI", min = 18, max = 45, value = c(18, 45), step = 0.1)
          ),
          mainPanel(
            h3("Scatter Plot"),
            plotlyOutput("scatterplot"),
            tags$br(),
            tags$br()
          )
          
        ),
        tags$hr(),
        
        # --------------------
        # Medi-Sight Anova Table section
        # --------------------
        
        sidebarLayout(
          sidebarPanel(
            h3("Anova Table"),
            sliderInput("ldl", "LDL level:", min = 0, max = 10, value = 5,step = 0.3),
            sliderInput("hdl", "HDL level:", min = 0, max = 10, value = 2.5,step = 0.3),
          ),
          
          # Show a plot of the generated distribution
          mainPanel(
            tabsetPanel(
              type = "tabs",
              tabPanel("Anova Visualisation",plotOutput("scatter_plot")),
              tabPanel("Anova - high HDL and low LDL", dataTableOutput("anovaSummaryhl")),
              tabPanel("Anova  - low HDL and high LDL", dataTableOutput("anovaSummarylh")),
              tabPanel("Anova  - low HDL and low LDL", dataTableOutput("anovaSummaryll")),
              tabPanel("Anova  - high HDL and high LDL", dataTableOutput("anovaSummaryhh"))
            ),
          )
        ),
        
        tags$hr(),
      ),
    )
  ),
  
  
  
  ################################################
  #### Panel: Documentation                   ####
  ################################################

  tabPanel("Report",
           fluidPage(htmlOutput("doc"))),

  ################################################
  #### Panel: About                           ####
  ################################################
  tabPanel("About Us",
           fluidPage(htmlOutput("abo")))
)

