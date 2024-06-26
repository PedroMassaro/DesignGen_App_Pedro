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
                 p("Run analysis with mixed models.")
             ),
             
             # Choose the experiment design
             box(width = 12,
                 selectInput(ns("design"), label = h4("Experiment design"), 
                             choices = list("Randomized complete block" = "block", "Alpha lattice" = "lattice"), 
                             selected = "block")
             ),
             
             # Input the file
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Input file",
                 p("The input file is a tab delimited table with a column called 'local' defining the environment, 
                   other called 'gen' defining the genotypes and other called 'block' defining the block number. 
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
                                  selected = ";") 
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
             
             #Select variables
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Select variables",
                 box(width = 6,
                     radioButtons(ns("trait"), label = p("Choose the traits to be evaluated:"),
                                  choices = "Press 'Read the file' button to update",
                                  selected = "Press 'Read the file' button to update"),
                 ),
                 box(width = 6,
                     checkboxGroupInput(ns("local"), label = p("Choose the location to be evaluated:"),
                                        choices = "Press 'Read the file' button to update",
                                        selected = "Press 'Read the file' button to update")
                 ),
                 box(width = 6,
                     checkboxGroupInput(ns("corte"), label = p("Choose the harvest to be evaluated:"),
                                        choices = "Press 'Read the file' button to update",
                                        selected = "Press 'Read the file' button to update")
                 )
             ),
             
             #Define the model
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Define the model:",
                 p("If you want to consider a pedigree matrix, please submit an csv file as the example:"),
                 downloadButton(ns("pedigree_example")), hr(),
                 fileInput("pedigree", label = p("Pedigree matrix")),
                 hr(),
                 p("Define the model expression bellow. Here we used 'sommer' package to perform the analysis. Then, consider its syntax."),
                 p("If you uploaded the pedigree matrix above you can add it in the model with the symbol A."),
                 textInput(ns("fixed"), label = p("Fixed:"), value = "peso ~ local + local:block"),
                 textInput(ns("random"), label = p("Random:"), value = "~ vsr(dsr(local),gen)"),
                 textInput(ns("rcov"), label = p("rcov:"), value = "~ units"), hr(),
                 # radioButtons(ns("rcov"), label = p("Choose the rcov to be evaluated:"), hr(),
                 #              choices = list("units" = "units", "vsr" = "vsr", "vsc" = "vsc"), 
                 #              selected = "units"),
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
                     DT::dataTableOutput(ns("blups_out"))
                 ),
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
    
    if(any(colnames(button1()) %in% "rep"))
      choices_trait_temp <- colnames(button1())[-c(1:4)] else
        choices_trait_temp <- colnames(button1())[-c(1:3)]
      
      #Choose the trait and the location   
      choices_trait <- choices_trait_temp
      names(choices_trait) <- choices_trait_temp
      
      choices_locations_temp <- unique(button1()[,"local"])
      choices_locations <- choices_locations_temp
      names(choices_locations) <- choices_locations_temp
      
      choices_corte_temp <- unique(button1()[,"corte"])
      choices_corte <- choices_corte_temp
      names(choices_corte) <- choices_corte_temp
      
      updateRadioButtons(session, "trait",
                         label="Choose the trait to be evaluated:",
                         choices = choices_trait,
                         selected = unlist(choices_trait)[1])
      
      updateCheckboxGroupInput(session, "local",
                               label="Choose the locations to be evaluated:",
                               choices = choices_locations)
      
      updateCheckboxGroupInput(session, "corte",
                               label="Choose the harvest to be evaluated:",
                               choices = choices_corte)
  })
  
  # defining the model as a factor
  button2 <- eventReactive(input$run_analysis, {
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      dat <- button1()
      dat$block <- as.factor(dat$block)
      dat$gen <- as.factor(dat$gen)
      dat$local <- as.factor(dat$local)
      dat$corte <- as.factor(dat$corte)
      dat$peso <- as.double(dat$peso)
      
      if(input$design == "block"){
        if(!all(c("local", "block", "gen", "corte") %in% colnames(dat)) | ("rep" %in% colnames(dat)))
          stop(safeError("Randomized complete block design should have columns 'local', 'block' and 'gen'."))
        dat <- dat %>% select(c("local", "gen", "block", "corte",input$trait)) %>% 
          filter(local %in% input$local) %>% droplevels() 
        
      } else {
        if(!all(c("local", "block", "gen", "corte") %in% colnames(dat)))
          stop(safeError("Alpha lattice design should have columns 'local', 'block', 'rep', and 'gen'."))
        dat$rep <- as.factor(dat$rep)
        
        dat <- dat %>% select(c("local", "gen", "block","corte",input$trait)) %>%
          filter(local %in% input$local) %>% droplevels() #%>% 
        # filter(corte %in% input$corte) %>% droplevels()
        
        dat$local <- as.factor(dat$rep)
        #dat$corte <- as.factor(dat$rep)
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
      
      BLUPs <- data.frame(ID = names(mod$U$gen), BLUPs = mod$U$gen)
      
      # B1 <- mod$U$AC:gen$peso
      # B2 <- mod$U$RJ:gen$peso
      # B3 <- mod$U$DF:gen$peso
      # B4 <- mod$U$RO:gen$peso
      # B5 <- mod$U$MS:gen$peso
      #
      # if(mod$random == "local:gen"){
      #   BLUPs <- data.frame(ID = names(mod$U$local:gen$peso), BLUPs = mod$U$local:gen$peso)
      #
      # } else {
      #   BLUPs <- data.frame(ID = names(mod$U$local:gen$peso), BLUPs = rbind(B1, B2, B3, B4, B5))}
      #
      
      # B1 <- data.frame(ID = names(mod$U$AC:gen$peso), BLUPs = mod$U$AC:gen$peso)
      # B2 <- data.frame(ID = names(mod$U$RJ:gen$peso), BLUPs = mod$U$RJ:gen$peso)
      # B3 <- data.frame(ID = names(mod$U$DF:gen$peso), BLUPs = mod$U$DF:gen$peso)
      # B4 <- data.frame(ID = names(mod$U$RO:gen$peso), BLUPs = mod$U$RO:gen$peso)
      # B5 <- data.frame(ID = names(mod$U$MS:gen$peso), BLUPs = mod$U$MS:gen$peso)
      
      AC <- mod$U$`AC:gen`$peso
      RJ <- mod$U$`RJ:gen`$peso
      DF <- mod$U$`DF:gen`$peso
      RO <- mod$U$`RO:gen`$peso
      MS <- mod$U$`MS:gen`$peso

      if(input$random == "local:gen"){
        BLUPs <- data.frame(ID = names(mod$U$`local:gen`), BLUPs = mod$U$`local:gen`)
      } else {
        previa <- cbind(AC, RJ, DF, RO, MS)
        # BLUPs <- data.frame(ID = names(mod$U$`AC:gen`), BLUPs = previa)
        BLUPs <- data.frame(ID = names(mod$U$`AC:gen`$peso), previa)
      }
      incProgress(0.25, detail = paste("Doing part", 2))
      # list(mod,summary_mod, aic_bic, BLUPs)
      list(mod,summary_mod, aic_bic, BLUPs)
    })
  })
  
  output$varcomp_out <- DT::renderDataTable({
    data <- data.frame(button2()[[2]]$varcomp)
    
    # Especifique as colunas que deseja arredondar e o número de casas decimais
    # columns_to_round <- c("Sum.Sq", "Mean.Sq", "F.value", "Pr..F.", "outra_coluna1", "outra_coluna2")
    decimal_places1 <- 2  # Especifique o número de casas decimais
    
    # Arredonde as colunas selecionadas
    for (col in 1:3) {
      data[[col]] <- round(as.numeric(data[[col]]), decimal_places1)
    }
    
    # decimal_places1 <- 5
    # for (col in 5) {
    #   data[[col]] <- round(as.numeric(data[[col]]), decimal_places1)
    # }
    
    # Outputs
    DT::datatable(data,  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
    
  })
  
  output$aic_bic_out <- DT::renderDataTable({
    data <- data.frame(button2()[[3]])
    
    # Especifique as colunas que deseja arredondar e o número de casas decimais
    # columns_to_round <- c("Sum.Sq", "Mean.Sq", "F.value", "Pr..F.", "outra_coluna1", "outra_coluna2")
    decimal_places1 <- 2  # Especifique o número de casas decimais
    
    # Arredonde as colunas selecionadas
    for (col in 1:2) {
      data[[col]] <- round(as.numeric(data[[col]]), decimal_places1)
    }
    
    # decimal_places1 <- 5
    # for (col in 5) {
    #   data[[col]] <- round(as.numeric(data[[col]]), decimal_places1)
    # }
    
    # Outputs
    DT::datatable(data,  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Brt',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  })
  
  output$blups_out <- DT::renderDataTable({
    data <- data.frame(button2()[[4]])
    
    # Especifique as colunas que deseja arredondar e o número de casas decimais
    # columns_to_round <- c("Sum.Sq", "Mean.Sq", "F.value", "Pr..F.", "outra_coluna1", "outra_coluna2")
    decimal_places1 <- 2  # Especifique o número de casas decimais
    
    # Arredonde as colunas selecionadas
    for (col in 2:6) {
      data[[col]] <- round(as.numeric(data[[col]]), decimal_places1)
    }
    
    # decimal_places1 <- 5
    # for (col in 5) {
    #   data[[col]] <- round(as.numeric(data[[col]]), decimal_places1)
    # }
    
    # Outputs
    DT::datatable(data, 
                  rownames = FALSE,
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Brt',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  })
}

## To be copied in the UI
# mod_MixedModel_ui("MixedModel_ui_1")

## To be copied in the server
# callModule(mod_MixedModel_server, "MixedModel_ui_1")