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
    fluidRow(style = "height:8000px",
             box(width = 12, 
                 p("Run analysis with mixed models")
             ),
             
             # Input the file
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Input file",
                 p("If you don't have any data, you can download an example input file here to see how the app works."),
                 downloadButton(ns("data_example")), hr(),
                 p("Upload your file here:"),
                 fileInput(ns("data_input"), label = h6("File: .csv .xls .txt"), multiple = F),
                 p("If you don't have a file to upload, you can still explore the app's features using our example file.
                   The example file will be automatically uploaded, simply press the 'Load file' button to proceed."),
                 
                 # Input Control
                 hr(),
                 box(width = 4,
                     radioButtons(ns("data_view"), label = p("Choose the separator:"),
                                  choices = list("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
                                  selected = ",") 
                 ), 
                 box(width = 8,  
                     tableOutput(ns("dataview"))
                 ),
                 
                 # Read the file
                 hr(),
                 actionButton(ns("data_load"), "Load file", icon("file")), 
             ),
             
             # Data Filtering
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary", title = "Data Filtering",
                 box(width = 12,
                 radioButtons(ns("filter_choice"), label = p("Do you need to filter your data?"),
                              choices = c("Yes", "No"),
                              selected = "No"),
                 p("If you don't need to filter your data, just press the 'Data Filters' button and continue with the next steps.")
                 ),
                 
                 uiOutput(ns("filter_dynamic_factor")),
                 br(),
                 uiOutput(ns("filter_dynamic_button")),
                 br(),
                 uiOutput(ns("filter_dynamic_level")),
                 
                 actionButton(ns("filter_ready"), "Data Filters", icon("filter"))
             ),
             
             # Choose Parameters
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Select variables",
                 box(width = 4,
                     radioButtons(ns("trait"), label = p("Choose the trait to be evaluated:"),
                                  choices = "Press 'Data Filters' button to update",
                                  selected = "Press 'Data Filters' button to update"),
                 ),
                 box(width = 4,
                     radioButtons(ns("fixed_ef"), label = p("Choose the fixed effect to be evaluated:"),
                                  choices = "Press 'Data Filters' button to update",
                                  selected = "Press 'Data Filters' button to update")
                 ),
                 box(width = 4,
                     radioButtons(ns("random_ef"), label = p("Choose the random effect to be evaluated:"),
                                  choices = "Press 'Data Filters' button to update",
                                  selected = "Press 'Data Filters' button to update")
                 ),
                 hr(),
                 actionButton(ns("parameter_choice"), "Model Parameters", icon("check"))
             ),
             
             
             # Define the model
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Define the model",
                 p("If you want to consider a pedigree matrix, please submit an csv file as the example:"),
                 downloadButton(ns("pedigree_example")), hr(),
                 fileInput("pedigree", label = p("Pedigree matrix")),
                 hr(),
                 p("Define the model expression bellow. Here we used 'sommer' package to perform the analysis. Then, consider its syntax."),
                 p("If you uploaded the pedigree matrix above you can add it in the model with the symbol A."),
                 textInput(ns("fixed"), label = p("Fixed:"), value = "Peso ~ Corte + Corte:Bloco"),
                 textInput(ns("random"), label = p("Random:"), value = "~ Genotipo + Corte:Genotipo"),
                 textInput(ns("rcov"), label = p("rcov:"), value = "~ units"), hr(),
                 actionButton(ns("analysis_run"), "Run analysis",icon("refresh")), br(),
                 p("Expand the windows above to access the results")
             ), hr(),
             
             # Results
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Results",
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Variance components",
                     DT::dataTableOutput(ns("varcomp_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "AIC and BIC",
                     DT::dataTableOutput(ns("aic_bic_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "BLUPs",
                     box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Table Visualization",
                         DT::dataTableOutput(ns("blups_table_out")),
                     ),
                     box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Graph Visualization",
                         plotOutput(ns("blups_graph_out")),
                     ),
                 ),
                 hr(),
                 # Download
                 downloadButton(ns('download_rdata'), "Download .RData", class = "butt")
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
  
  ## Download input
  output$data_example <- downloadHandler(
    filename =  function() {
      paste("example_data.csv")
    },
    # Content is a function with argument file. content writes the plot to the device
    content = function(file) {
      dat <- read.csv(system.file("ext","example_inputs/example_blocks.csv", package = "StatGenESALQ"))
      write.csv(dat, file = file, row.names = F)
    } 
  )
  
  # Observe the file input and display the data
  observeEvent(input$data_view, {
    observeEvent(input$data_input, {
      if (is.null(input$data_input)) {
        output$dataview <- renderTable({
          return(p("Upload your data"))
        })
      } else {
        df <- read.csv(input$data_input$datapath, sep = input$data_view)
        output$dataview <- renderTable({
          return(head(df))
        })
      }
    })
  })  
  
  # Download example pedigree data
  output$pedigree_example <- downloadHandler(
    filename =  function() {
      paste("pedigree.csv")
    },
    # Content is a function with argument file. content writes the plot to the device
    content = function(file) {
      dat <- read.csv(system.file("ext","example_inputs/example_pedigree.csv", package = "StatGenESALQ"), row.names = 1, header = T)
      write.csv(dat, file = file)
    } 
  )
  
  # Data Loading
  button1 <- eventReactive(input$data_load, {
    if (is.null(input$data_input$datapath)) {
      dat <- read.csv(system.file("ext","example_inputs/example_blocks.csv", package = "StatGenESALQ"))
    } else {
      dat <- read.csv(input$data_input$datapath, sep = input$data_view)
    }
    dat
  })
  
  # Dynamic UI for filtering data
  observeEvent(input$data_load, {
      output$filter_dynamic_factor <- renderUI({
        req(input$filter_choice == "Yes")
        choices_trait_temp <- colnames(button1())
        #Choose the trait and the location
        choices_trait <- choices_trait_temp
        names(choices_trait) <- choices_trait_temp
        
        num <- 1
        
        lapply(seq_len(num), function(i) {
            box(
              width = 12,
              checkboxGroupInput(
                ns("select"),
                label = p("Choose the factors to be filtered:"),
                choices = choices_trait,
                selected = choices_trait[1]
              )
            )
       })
    })
  })
  
  observeEvent(input$data_load, {
    output$filter_dynamic_button <- renderUI({
      req(input$filter_choice == "Yes")
      dat <- button1()
      
      num <- 1

        actionButton(ns("filter_in_process"), "Select levels", icon("plus"))
      
    })
  })
  
  # Reactive filter data
  observeEvent(input$data_load, {
      output$filter_dynamic_level <- renderUI({
        req(input$filter_choice == "Yes")
        dat <- button1()
        
        if (length(input$select) > 0) {
          n <- length(input$select)
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
                # Choose the 'local' levels to select:
                label = paste0("Choose the levels from '", col_names[i], "' to be filtered:"),
                choices = "Press 'Filter' button to update"
              )
            )
          }
        })

    })
  })
  
    observeEvent(input$filter_in_process, {
        req(input$filter_choice == "Yes")
        dat <- button1()

        if (length(input$select) > 0) {
          n <- length(input$select)
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
              updateCheckboxGroupInput(session,
                paste0("filter", i),
                label = paste0("Choose the levels from '", col_names[i], "' to be filtered:"),
                choices = unique(dat[[input$select[i]]])
              )
            )
          }
        })
      })

  # Data Filtering
  button3 <- eventReactive(input$filter_ready, {
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      
      data <- button1()
      
      if (length(input$select) > 0) {
        n <- length(input$select)
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

  # Update choices for analysis
  observeEvent(input$filter_ready, {
    choices_trait_temp <- colnames(button1())
    choices_trait <- choices_trait_temp
    names(choices_trait) <- choices_trait_temp
    
    updateRadioButtons(session, "trait",
                       label="Choose the trait to be evaluated:",
                       choices = choices_trait,
                       selected = unlist(choices_trait)[1])
    
    updateRadioButtons(session, "fixed_ef",
                       label="Choose the fixed effect to be evaluated:",
                       choices = choices_trait)
    
    updateRadioButtons(session, "random_ef",
                       label="Choose the random effect to be evaluated:",
                       choices = choices_trait)
  })
  
  # Analysis function
  button2 <- eventReactive(input$analysis_run, {
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
      
      BLUPs <- data.frame(ID = levels(dat[[input$random_ef]]), BLUPs = mod$U[[input$random_ef]])
      rownames(BLUPs) <- NULL
      
      incProgress(0.25, detail = paste("Doing part", 2))
      # list(mod,summary_mod, aic_bic, BLUPs)
      list(mod,summary_mod, aic_bic, BLUPs)
    })
  })

  # Output for variance components
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
  
  # Output for AIC and BIC
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
  
  # Output for BLUPs - Table
  output$blups_table_out <- DT::renderDataTable({
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
  
  # Output for BLUPs - Graph
  output$blups_graph_out <- renderPlot({
    data <- data.frame(button2()[[4]])
    data[,1] <- as.factor(data[,1])
    data[,2] <- as.numeric(data[,2])
    # Plot the data and analyze the BLUP.
    ggplot(data, 
           aes(x = data[,1], y = data[,2])) +
      geom_point(color = "#cc662f", size = 3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      labs(x = input$random_ef,
        y = "BLUP") +
      theme_minimal() +
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12))
  })
  
  # Download results
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
  output$download_rdata <- downloadHandler(
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