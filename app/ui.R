fluidPage(
  
  # mandatory
  Variables$toIncludeList,
  
  # Application title
  titlePanel("EM/GMM 1D - iteration by iteration"),
  tags$p("Unfortunately, it can give NaN/Inf results..."),
  tags$p("Black points could not be assigned to any cluster due to numerical reasons (=Inf) or due to equal probabilities of belonging to clusters."),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons("defaultRadio", label = h3("Input"),
                   choices = list("Default example" = 1, "Upload your own files" = 2), 
                   selected = 1),
      checkboxGroupInput("fixedCheck", label="Fix parameters from now on [when clicking 'Next iteration']?",
                         choices = list("Fix pis" = 1, "Fix mus" = 2, "Fix sigmas" = 3)),
      verbatimTextOutput("dataParamsText")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tags$h2("The pdfs are multiplied by their selection probability and then plotted:"),
      tags$style(HTML("
                  #latex {
                      width:100%;
                      overflow-x:scroll
                      }
                      ")),
      uiOutput("latex"),
      plotOutput("distPlot",height = "570px"),
      actionButton("previousButton","Previous executed iteration"),
      actionButton("nextButton","Next iteration")
    )
  )
)
