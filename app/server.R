function(input, output, session) {
  
  rValues <- reactiveValues(params = list(), myData = list(), t = NA)
  
  observeEvent(input$defaultRadio, {
    if("1" == input$defaultRadio) {
      D <- read.table(Data$data.txt,sep="")[[1]]
      n <- length(D)
      xlim <- c(min(D)-(max(D)-min(D))/10,max(D)+(max(D)-min(D))/10)
      ylim <- c(0,0.25)
      zeros <- rep(0,length(D))
      
      rValues$t <- 0
      shinyjs::disable("previousButton")
      
      rValues$myData <- list(
        D = D,
        n = n,
        xlim = xlim,
        ylim = ylim,
        zeros = zeros
      )
      
      init <- read.table(Data$init.txt,sep="")
      rValues$t <- rValues$t + 1
      rValues$params <- list()
      loglike <- Functions$logLikelihood(D,init[[1]],init[[2]],init[[3]])
      rValues$params[[rValues$t]] <- list(
        k = nrow(init),
        E = matrix(nrow = length(D),ncol = nrow(init)),
        pi = init[[1]],
        mu = init[[2]],
        sigma = init[[3]],
        loglike = loglike
      )
      if(is.infinite(loglike) || is.nan(loglike)) {
        shinyjs::disable("nextButton")
      } else {
        shinyjs::enable("nextButton")
      }
    } else {
      showModal(modalDialog(
        title = "Upload files",
        size = "s",
        tags$a(href = "data.zip","Download sample data"),
        fileInput("dataFileInput","Upload data",accept = "text/plain"),
        fileInput("initFileInput","Upload initial parameters",accept = "text/plain"),
        footer = list(
          actionButton("useFiles","Use files"),
          actionButton("cancelFiles","Cancel")
        ),
        easyClose = FALSE
      ))
      message <- HTML("Please select for 'data' a valid text file: 
                        <br><b>first (and only) column</b> - data (real numbers)<br>
                        Example of contents of such a file:<br>
                        <code>
                        0<br>
                        1<br>
                        5<br>
                        6<br>
                        7<br>
                        </code><br><br>
                        Please select for 'initial parameters' a valid text file: 
                        <br><b>the number of rows</b> - the number of clusters = k</b><br>
                        <b>first column</b> - selection probabilities (pis; their sum = 1)<br>
                        <b>second column</b> - means of normal distributions (mus)<br>
                        <b>third column</b> - standard deviations of normal distributions (sigmas; sqrt(variance); each must be > 0)<br>
                        Example of contents of such a file (2 clusters):<br>
                        <code>
                        0.5 5 1 <br>
                        0.5 6 2 <br>
                        </code>")
        sendSweetAlert(
          session = session,
          title = "Info",
          text = message,
          type = "info",
          html = TRUE
        )
    }
    
  })
  
  observeEvent(input$useFiles, {
    dataFile <- input$dataFileInput
    initFile <- input$initFileInput
    
    if (is.null(dataFile)) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Data file not uploaded.",
        type = "error"
      )
      return()
    }
    
    if (is.null(initFile)) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Initial parameters file not uploaded.",
        type = "error"
      )
      return()
    }
    
    data <- try(read.table(dataFile$datapath,sep=""), 
             silent = TRUE)
    if (class(data) == "try-error" || !Functions$checkDataFile(data)) {
      message <- HTML("Please select for 'data' a valid text file: 
          <br><b>first (and only) column</b> - data (real numbers)<br>
          Example of contents of such a file:<br>
           <code>
           0<br>
           1<br>
           5<br>
           6<br>
           7<br>
           </code>")
      sendSweetAlert(
        session = session,
        title = "Error",
        text = message,
        type = "error",
        html = TRUE
      )
      return()
    }
    
    init <- try(read.table(initFile$datapath,sep=""), 
                silent = TRUE)
    if (class(init) == "try-error" || !Functions$checkInitFile(init)) {
      message <- HTML("Please select for 'initial parameters' a valid text file: 
                      <br><b>the number of rows</b> - the number of clusters = k</b><br>
                      <b>first column</b> - selection probabilities (pis; their sum = 1)<br>
                      <b>second column</b> - means of normal distributions (mus)<br>
                      <b>third column</b> - standard deviations of normal distributions (sigmas; sqrt(variance); each must be > 0)<br>
                      Example of contents of such a file (2 clusters):<br>
                      <code>
                      0.5 5 1 <br>
                      0.5 6 2 <br>
                      </code>")
      sendSweetAlert(
        session = session,
        title = "Error",
        text = message,
        type = "error",
        html = TRUE
      )
      return()
    }
    
    D <- data[[1]]
    n <- length(D)
    xlim <- c(min(D)-(max(D)-min(D))/10,max(D)+(max(D)-min(D))/10)
    ylim <- c(0,0.25)
    zeros <- rep(0,length(D))
    
    rValues$t <- 0
    shinyjs::disable("previousButton")
    
    rValues$myData <- list(
      D = D,
      n = n,
      xlim = xlim,
      ylim = ylim,
      zeros = zeros
    )
    
    rValues$params <- list()
    rValues$t <- rValues$t + 1
    loglike <- Functions$logLikelihood(D,init[[1]],init[[2]],init[[3]])
    rValues$params[[rValues$t]] <- list(
      k = nrow(init),
      E = matrix(nrow = length(D),ncol = nrow(init)),
      pi = init[[1]],
      mu = init[[2]],
      sigma = init[[3]],
      loglike = loglike
    )
    
    if(is.infinite(loglike) || is.nan(loglike)) {
      shinyjs::disable("nextButton")
    } else {
      shinyjs::enable("nextButton")
    }
    
    removeModal()
  })
  
  observeEvent(input$cancelFiles, {
    updateRadioButtons(session, "defaultRadio",
                       selected = "1"
    )
    removeModal()
  })
  
  observeEvent(input$nextButton, {
    if(length(input$fixedCheck) == 3) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "It makes no sense to fix all parameters.",
        type = "error"
      )
      return()
    } 
    
    rValues$params[[rValues$t + 1]] <- Functions$emOneIteration(
      rValues$myData$D,
      rValues$myData$n,
      rValues$params[[rValues$t]]$k,
      rValues$params[[rValues$t]]$E,
      rValues$params[[rValues$t]]$pi,
      rValues$params[[rValues$t]]$mu,
      rValues$params[[rValues$t]]$sigma,
      "1" %in% input$fixedCheck,
      "2" %in% input$fixedCheck,
      "3" %in% input$fixedCheck) 
    rValues$t <- rValues$t + 1
    if(rValues$t - 1 == 1) {
      shinyjs::enable("previousButton")
    }
    loglike <- rValues$params[[rValues$t]]$loglike
    if(is.infinite(loglike) || is.nan(loglike)) {
      shinyjs::disable("nextButton")
    }
  })
  
  observeEvent(input$previousButton, {
    loglike <- rValues$params[[rValues$t]]$loglike
    if(is.infinite(loglike) || is.nan(loglike)) {
      shinyjs::enable("nextButton")
    }
    rValues$params[[rValues$t]] <- NULL
    rValues$t <- rValues$t - 1 
    if(rValues$t - 1 == 0) {
      shinyjs::disable("previousButton")
    }
  })
  
  output$distPlot <- renderPlot({
    Functions$plotEmOneIteration(rValues$myData$D,
                                 rValues$myData$zeros,
                                 rValues$myData$xlim,
                                 rValues$myData$ylim,
                                 rValues$params[[rValues$t]]$k,
                                 rValues$params[[rValues$t]]$pi,
                                 rValues$params[[rValues$t]]$mu,
                                 rValues$params[[rValues$t]]$sigma)
  })
  
  output$dataParamsText <- renderPrint({
    cat("Data:",rValues$myData$D,"\n")
    cat("Iteration: ",rValues$t - 1,"\n")
    cat("'E' is computed according to the EM algorithm, \n    so according to the 'previous' plot.\n")
    cat("Log-likelihood of observed data: ",rValues$params[[rValues$t]]$loglike,"\n   (should grow as the number of iterations grows)\n")
    print(rValues$params[[rValues$t]])
  })
  
  output$latex <- renderUI({
    pi <- rValues$params[[rValues$t]]$pi
    mu <- rValues$params[[rValues$t]]$mu
    sigma <- rValues$params[[rValues$t]]$sigma
    k <- rValues$params[[rValues$t]]$k
    
    text <- "$$"
    for(i in 1:k) {
      text <- paste0(text,pi[i],"\\cdot \\mathcal{N}(x|",mu[i],",",sigma[i],"^2)")
      if(i != k) {
        text <- paste0(text," + ")
      }
    }
    text <- paste0(text,"$$")
    withMathJax(
      helpText(text)
    )
  })
  
}
