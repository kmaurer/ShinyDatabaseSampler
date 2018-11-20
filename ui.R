library(tidyverse)

### Define page layout with side panel and main panel (in which we will put multiple tabs)
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Shiny Database Sampler"),
  
#------------------------------------------------------------------------------------------------------
### Define objects in side panel
  
  sidebarPanel( 
    ### options trying to slim sidepanel and expand main panel
      # tags$head(
      #   tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
      #   tags$style(type="text/css", "select { max-width: 200px; }"),
      #   tags$style(type="text/css", "textarea { max-width: 185px; }"),
      #   tags$style(type="text/css", ".jslider { max-width: 200px; }"),
      #   tags$style(type='text/css', ".well { padding: 12px; margin-bottom: 5px; max-width: 380px; }"),
      #   tags$style(type='text/css', ".span4 { max-width: 300px; }")
      # ),
      
#---------------------------------------------      
  ### Conditional panels for input.tabs == 'Sample and Summarize' (Tab 1)
    # Data base selection
    conditionalPanel(
      condition = "input.tabs == 'Sample and Summarize'",
      selectInput("dbname", "Choose a database:", 
                  choices = c("Fatal Car Accidents"="fars2008_2017clean",
                              "American Community Survey"="acs2017clean"),
                  selected="acs2017clean")
    ),
    
    # Sampling technique selection and random seed
    conditionalPanel(
      condition = "input.tabs == 'Sample and Summarize'",
      radioButtons("samptype", "Sampling Technique:",
                   list("Simple Random Sample" = "srs",
                        "Stratified Random Sample" = "strat")),
      checkboxInput(inputId="wantseed", label="Set seed for reproducible sample?")
    ),

    conditionalPanel(
      condition = "input.tabs == 'Sample and Summarize' & input.wantseed",
      numericInput("rseed", "Select Seed for Random Draw (1-10000)", 1)
    ),
    
    #If simple random sample radio button selected show sample size selecter
    conditionalPanel(
      condition = "input.tabs == 'Sample and Summarize' & input.samptype == 'srs'",
      numericInput("ndraws", "Number of Simple Random Draws from Database", 50)
      #sliderInput("ndraws", "Number of Simple Random Draws from Database:",min=1, max=10000, value=50)
    ), 
    
    #make stratcol choice list for person in census
    conditionalPanel(
      condition = "input.tabs == 'Sample and Summarize' & input.samptype == 'strat'",
      uiOutput("stratcol")
    ),  


    #If strat random sample radio button selected show sample size selecter
    conditionalPanel(
      condition = "input.tabs == 'Sample and Summarize' & input.samptype == 'strat'",
      numericInput("ndrawsper", "Number of Random Draws Per Stratum from Database", 10)
      #sliderInput("ndrawsper", "Number of Random Draws Per Strata from Database:", min=1, max=1000, value=10)
    ),
    
    # give warning if number of samples improperly filled in
    conditionalPanel(
      condition = "input.tabs == 'Sample and Summarize' & (input.samptype == 'srs' || input.samptype == 'strat')",
      verbatimTextOutput("numwrong")
    ),   

    
    # If on get a sample tab then make sampling button
    conditionalPanel(
      condition = "input.tabs == 'Sample and Summarize' & (input.ndraws<10001 & input.ndraws>0 & input.ndrawsper<1001 & input.ndrawsper>0)",
      actionButton("DrawButton", "Get My Sample!")
    ),
    
      
#---------------------------------------------     
      ### Conditional panels for input.tabs == 'Sample and Summarize' (Tab 1)

      conditionalPanel(
        condition = "input.tabs == 'Visualize'",
        radioButtons("nvar", "Number of Variables in Plot:",
                     list("Univariate" = 1,
                          "Bivariate" = 2),inline = TRUE)
      ),   

      # Select Response Variable and type
      conditionalPanel(
        condition = "input.tabs == 'Visualize'",
        uiOutput("var1")
      ),

      # Select explanatory Variable and type
      conditionalPanel(
        condition = "input.tabs == 'Visualize' & input.nvar==2",
        uiOutput("var2")
      ),

#---------------------------------------------  

      ### Non-Conditional sidepanel items ###      
      #add download csv button
    downloadButton(outputId='downloadData', label='Download Data')

    ),
  
  
#------------------------------------------------------------------------------------------------------
### Define Objects in main panel with multiple tabs
  mainPanel(
    
    tabsetPanel(
      tabPanel("Sample and Summarize", 
        h4("Data Table"),
          dataTableOutput("view")
        # h4("Basic Summary"),
        #   verbatimTextOutput("summary")
      ), 
      tabPanel("Visualize", 
               h4("Basic Plot"),
               plotOutput("MainPlot",height = "600px")
      ), 
      id="tabs"
    )
  )
))