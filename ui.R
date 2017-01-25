# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Município no mapa"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("uf",
                  "Unidade de Federaçao:",
                  choices = unique(sort(dtb_viz$sigla_uf)),
                  selected = NULL),
      
      selectInput("mun","Municipio",
                  choices = NULL,
                  selected = "Escolha uma UF")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       verbatimTextOutput("printar"),
       uiOutput("file_rmd")
    )
  )
))
