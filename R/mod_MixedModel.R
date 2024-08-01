#' MixedModel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

# ui part ----
mod_MixedModel_ui <- function(id){ 
  ns <- NS(id)
  tagList(
    fluidRow(style = "height:5000px",
             box(width = 12, 
                 p("Run analysis with mixed models")
             ),
             
             # Choose the experiment design
             box(width = 12,
                 selectInput(ns("design"), label = h4("Experiment design"), 
                             choices = list("Randomized complete block" = "block", "Split-plot design" = "split", "Alpha lattice" = "lattice"), 
                             selected = "split")
             ),
             
             # Input the file
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Input file",
                 p("The input file is a tab delimited table with a column called 'local' defining the environment, 
                   other called 'gen' defining the genotypes, other called 'harve' defining harvests and other called 'block' defining the block number. 
                   The adjusted phenotypic means should be included in extra columns. Download here an input file example:"),
                 downloadButton(ns("data_example")), hr(),
                 p("Upload here your file:"),
                 fileInput(ns("data_input"), label = h6("File: data.txt"), multiple = F),
                 p("If you do not have an file to be upload you can still check this app features with our example file. The example file is automatically upload, you just need to procedure to the other buttons."),
                 
                 #Input Control
                 hr(),
                 p("Data View:"),
                 box(width = 4,
                     radioButtons(ns("read_data1"), label = p("Select o separator"),
                                  choices = list("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
                                  selected = ",") 
                 ), 
                 box(width = 8,  
                     #title = "Database",
                     #data visualisation
                     tableOutput(ns("dataview"))
                 ),
                 # Read the file
                 hr(),
                 actionButton(ns("read_data"), "Read the file",icon("refresh")), 
                 hr(),
             ),
             
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary", title = "Data Filtering",
                 box(width = 12,
                     radioButtons(ns("filter"), label = p("Would you like to filter your data?"),
                                  choices = c("Yes", "No"),
                                  selected = "No"),
                 ),
                 box(width = 12,
                     checkboxGroupInput(ns("select"), label = p("Choose the harvest to be evaluated:"),
                                        choices = "Press 'Read the file' button to update",
                                        selected = "Press 'Read the file' button to update")
                 ),
                 
                 hr(),
                 actionButton(ns("filter_read"), "Filter the file",icon("refresh")), 
                 
                 hr(),
                 uiOutput(ns("dynamic_filter_boxes")),
                 
                 hr(),
                 actionButton(ns("filter_data"), "Finish",icon("refresh"))
             ),
             
             #Select variables
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Select variables",
                 box(width = 4,
                     radioButtons(ns("trait"), label = p("Choose the traits to be evaluated:"),
                                  choices = "Press 'Read the file' button to update",
                                  selected = "Press 'Read the file' button to update"),
                 ),
                 box(width = 4,
                     radioButtons(ns("local"), label = p("Choose the location to be evaluated:"),
                                        choices = "Press 'Read the file' button to update",
                                        selected = "Press 'Read the file' button to update")
                 ),
                 box(width = 4,
                     radioButtons(ns("harve"), label = p("Choose the harvest to be evaluated:"),
                                        choices = "Press 'Read the file' button to update",
                                        selected = "Press 'Read the file' button to update")
                 ),
                 hr(),
                 actionButton(ns("select_data"), "Finish",icon("refresh"))
             ),
             
             
             #Define the model
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Define the model:",
                 p("If you want to consider a pedigree matrix, please submit an csv file as the example:"),
                 downloadButton(ns("pedigree_example")), hr(),
                 fileInput("pedigree", label = p("Pedigree matrix")),
                 hr(),
                 p("Define the model expression bellow. Here we used 'sommer' package to perform the analysis. Then, consider its syntax."),
                 p("If you uploaded the pedigree matrix above you can add it in the model with the symbol A."),
                 textInput(ns("fixed"), label = p("Fixed:"), value = "Peso ~ Corte + Corte:Bloco"),
                 textInput(ns("random"), label = p("Random:"), value = "~ Genotipo + Corte:Genotipo"),
                 textInput(ns("rcov"), label = p("rcov:"), value = "~ units"), hr(),
                 actionButton(ns("run_analysis"), "Run analysis",icon("refresh")), br(),
                 p("Expand the windows above to access the results")
             ), hr(),
             
             # Results
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Results:",
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Variance components:",
                     DT::dataTableOutput(ns("varcomp_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "AIC and BIC",
                     DT::dataTableOutput(ns("aic_bic_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "BLUPs",
                     box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Residuals vs fitted values",
                         DT::dataTableOutput(ns("blups_out")),
                     ),
                     box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "BLUP by Genotype",
                         plotOutput(ns("plot_blups")),
                     ),
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Download .RData",
                     downloadButton(ns('bn_download'), "Download", class = "butt")
                 )
             )
    )
  )
}

#' MixedModel Server Function
#'
#' @import sommer
#' 
#' @noRd 
#' 
# server part ----
mod_MixedModel_server <- function(input, output, session){
  ns <- session$ns
  ## download input
  output$data_example <- downloadHandler(
    filename =  function() {
      paste("example_data.csv")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$design == "block"){
        dat <- read.csv(system.file("ext","example_inputs/example_blocks.csv", package = "StatGenESALQ"))
      } else {
        dat <- read.csv(system.file("ext","example_inputs/example_lattice.csv", package = "StatGenESALQ"))
      }
      write.csv(dat, file = file, row.names = F)
    } 
  )
  
  # Data processing using .txt and .csv files.
  observeEvent(input$read_data1, {
    observeEvent(input$data_input, {
      if (is.null(input$data_input)) {
        output$dataview <- renderTable({
          return(p("Upload your data"))
        })
      } else {
        df <- read.csv(input$data_input$datapath, sep = input$read_data1)
        output$dataview <- renderTable({
          return(head(df))
        })
      }
    })
  })  
  
  
  ## download pedigree
  output$pedigree_example <- downloadHandler(
    filename =  function() {
      paste("pedigree.csv")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      dat <- read.csv(system.file("ext","example_inputs/example_pedigree.csv", package = "StatGenESALQ"), row.names = 1, header = T)
      write.csv(dat, file = file)
    } 
  )
  
  #Corrigir isso!!!
  button1 <- eventReactive(input$read_data, {
    if (is.null(input$data_input$datapath)) {
      if(input$design == "block"){
        dat <- read.csv(system.file("ext","example_inputs/example_blocks.csv", package = "StatGenESALQ"))
      } else {
        dat <- read.csv(system.file("ext","example_inputs/example_lattice.csv", package = "StatGenESALQ"))
      }
    } else {
      dat <- read.csv(input$data_input$datapath, sep = input$read_data1)
    }
    cat(colnames(dat))
    dat
  })
  
  observe({
    req(input$filter == "Yes")
    choices_trait_temp <- colnames(button1())
    #Choose the trait and the location
    choices_trait <- choices_trait_temp
    names(choices_trait) <- choices_trait_temp
    
    updateCheckboxGroupInput(session, "select",
                             label="Choose the factors to be filtered:",
                             choices = choices_trait)
  })
  
  observeEvent(input$read_data, {
    observeEvent(input$filter_read, {
      output$dynamic_filter_boxes <- renderUI({
        req(input$filter == "Yes")
        dat <- button1()
        print(input$select[1])
        
        if (length(input$select) > 0) {
          n <- length(input$select)
          # print(str(dat))
          for (i in 1:n) {
              dat[[input$select[i]]] <- as.factor(dat[[input$select[i]]])
          }
        } 
        
        num <- length(input$select)
        col_names <- input$select
        
        lapply(seq_len(num), function(i) {
          if(is.factor(dat[[input$select[i]]])) {
            box(
              width = 12,
              checkboxGroupInput(
                ns(paste0("filter", i)),
                label = paste("Select data from", col_names[i], ":"),
                choices = unique(dat[[input$select[i]]]),
                selected = unique(dat[[input$select[i]]])[1]
              )
            )
          }
        })
      })
    })
  })
  
  button3 <- eventReactive(input$filter_data, {
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      
      data <- button1()
      
      if (length(input$select) > 0) {
          n <- length(input$select)
          # print(str(dat))
          for (i in 1:n) {
              data[[input$select[i]]] <- as.factor(data[[input$select[i]]])
          }
        } 
      
        num <- length(input$select)
        col_names <- input$select
       
        for (i in 1:num) {
          data <- data %>%
            filter(data[[input$select[i]]] %in% c(input[[paste0("filter", i)]])) %>%
            droplevels()
        }

      incProgress(0.25, detail = paste("Doing part", 2))
      data
    })
  })

  observeEvent(input$filter_data, {
    choices_trait_temp <- colnames(button1())
    choices_trait <- choices_trait_temp
    names(choices_trait) <- choices_trait_temp
    
    updateRadioButtons(session, "trait",
                       label="Choose the trait to be evaluated:",
                       choices = choices_trait,
                       selected = unlist(choices_trait)[1])
    
    updateRadioButtons(session, "local",
                       label="Choose the locations to be evaluated:",
                       choices = choices_trait)
    
    updateRadioButtons(session, "harve",
                       label="Choose the harvest to be evaluated:",
                       choices = choices_trait)
  })

  # defining the model as a factor
  button2 <- eventReactive(input$run_analysis, {
    req(input$select_data)
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      dat <- button3()
      
      if (ncol(dat) > 0) {
        n <- ncol(dat)
        for (i in 1:n) {
          if (colnames(dat)[i] == input$trait) {
            dat[, i] <- as.double(dat[, i])
          } else {
            dat[, i] <- as.factor(dat[, i])
          }
        }
      } 
      
      if(!is.null(input$pedigree)) A <- read.csv(input$pedigree$datapath, row.names = 1, header = T)
      
      
      # Input the model
      mod <- mmer(fixed = as.formula(input$fixed), 
                  random = as.formula(input$random), 
                  rcov = as.formula(input$rcov),
                  data = dat)
      
      # Results
      summary_mod <- summary(mod)
      
      aic_bic <- data.frame(AIC = mod$AIC, BIC = mod$BIC)
      
      BLUPs <- data.frame(ID = levels(dat[[input$harve]]), BLUPs = mod$U[[input$harve]])
      rownames(BLUPs) <- NULL
      
      incProgress(0.25, detail = paste("Doing part", 2))
      # list(mod,summary_mod, aic_bic, BLUPs)
      list(mod,summary_mod, aic_bic, BLUPs)
    })
  })
  
  output$varcomp_out <- DT::renderDataTable({
    data <- data.frame(button2()[[2]]$varcomp)
    
    # Especifique as colunas que deseja arredondar e o número de casas decimais
    decimal_places1 <- 2  # Especifique o número de casas decimais
    
    # Arredonde as colunas selecionadas
    for (col in 1:3) {
      data[[col]] <- round(as.numeric(data[[col]]), decimal_places1)
    }
    
    DT::datatable(data,  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf'),
                    columnDefs = list(list(className = 'dt-center', targets = '_all')) # Centraliza o texto de todas as colunas
                  ),
                  class = "display")
  })
  
  output$aic_bic_out <- DT::renderDataTable({
    data <- data.frame(button2()[[3]])
    
    # Especifique as colunas que deseja arredondar e o número de casas decimais
    decimal_places1 <- 2  # Especifique o número de casas decimais
    
    # Arredonde as colunas selecionadas
    for (col in 1:2) {
      data[[col]] <- round(as.numeric(data[[col]]), decimal_places1)
    }
    
    # Outputs
    DT::datatable(data,
                  rownames = FALSE,
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Brt',
                    buttons = c('copy', 'csv', 'excel', 'pdf'),
                    columnDefs = list(list(className = 'dt-center', targets = '_all')) # Centraliza o texto de todas as colunas
                  ),
                  class = "display")
  })
  
  output$blups_out <- DT::renderDataTable({
    data <- data.frame(button2()[[4]])
    
    # Especifique as colunas que deseja arredondar e o número de casas decimais
    decimal_places1 <- 2  # Especifique o número de casas decimais
    
    # Arredonde as colunas selecionadas
    for (col in 2) {
      data[[col]] <- round(as.numeric(data[[col]]), decimal_places1)
    }

    # Outputs
    DT::datatable(data,
                  rownames = FALSE,
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf'),
                    columnDefs = list(list(className = 'dt-center', targets = '_all')) # Centraliza o texto de todas as colunas
                  ),
                  class = "display")
  })
  
  output$plot_blups <- renderPlot({
    data <- data.frame(button2()[[4]])
    data[,1] <- as.factor(data[,1])
    data[,2] <- as.numeric(data[,2])
    # Plot the data and analyze the BLUP.
    ggplot(data, 
           aes(x = data[,1], y = data[,2])) +
      geom_point(color = "#cc662f", size = 4) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      labs(x = "Genotipo",
           y = "BLUP") +
      theme_minimal() +
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12))
  })
  
  fn_downloadname <- reactive({
    seed <- sample(1:10,1)
    filename <- paste0("mixedmodel","_",seed,".RData")
    return(filename)
  })
  
  # download profile 
  fn_download <- function() {
    mixedmodel <- button2()[[1]]
    save(mixedmodel, file = fn_downloadname())
  }
  
  # download handler
  output$bn_download <- downloadHandler(
    filename = fn_downloadname,
    content = function(file) {
      fn_download()
      file.copy(fn_downloadname(), file, overwrite=T)
      file.remove(fn_downloadname())
    }
  )
}

## To be copied in the UI
# mod_MixedModel_ui("MixedModel_ui_1")

## To be copied in the server
# callModule(mod_MixedModel_server, "MixedModel_ui_1")