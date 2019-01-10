##### Server Script for ShinyDBSampler #####

#------------------------------------------------------------------------------------------------------
### Preliminaries ###

# Load all necessary libraries
library(shiny)
library(tidyverse)
library(RMySQL)

options(scipen=8)

load(file="ShinyDBSamplerAppInfo.Rdata")

# define data base driver
con <- dbConnect(MySQL(),
                 user='ktmaurer',
                 password='pas$18Maurer',
                 dbname='shinyDBSampler',
                 host='10.34.225.221')

#------------------------------------------------------------------------------------------------------
### Sampling Functions ###

# dataset specific ordinal variable leveling 
order_vars <- function(df, dbtabname){
  for(i in names(dbinfo[[dbtabname]]$ordinal_vars)){
    df[,i] <- factor(df[,i], level=dbinfo[[dbtabname]]$ordinal_vars[[i]])
  } 
  return(df)
}

# function to generate a SRS from a given table (with given integer indexed column)
# n=number of draws, dbtabname=name of sql table , indexcol=unique db row identifier
# con1= connection specification to db, seed=random seed value
# infolistnum=location in dbinfolist to pull index info
SRS <- function(n, dbtabname, seed=NA){
  # select row numbers to draw
  if(!is.na(seed)) set.seed(seed)
  SRSindex <- sample(1:dbinfo[[dbtabname]]$N,n,replace = FALSE)
  # run query to pull draws
  sampall <- dbGetQuery(con,  sprintf("select * from %s WHERE row_names in (%s)",dbtabname, paste(SRSindex, collapse=","))) %>% 
    order_vars(dbtabname)
  return(sampall)
}

# function to generate a Stratified Sample from given table with nper from each group 
# requires stratification column and integer indexed column
StratSampler <-  function(nper, dbtabname, stratcol, seed=NA){
  if(!is.na(seed)) set.seed(seed)
  # Get row numbers and strata values, group and sample (removing too small categories)
  strat_rows <- dbGetQuery(con,sprintf("SELECT %s FROM %s",stratcol, dbtabname))%>% 
    mutate(row_names = 1:n())%>%
    group_by_(stratcol) %>%
    add_tally( ) %>%
    filter(n > nper) %>%
    sample_n(nper)
  # Run query for selected rows
  sampall <- dbGetQuery(con,  sprintf("select * from %s WHERE row_names in (%s)",dbtabname, paste(strat_rows$row_names, collapse=","))) %>%
    arrange_(stratcol) %>% 
    order_vars(dbtabname)
  return(sampall)
}

# summary table print builder
sum_table <- function(sample_data,max_show=12){
  sum_tab <- "NA"
  for(j in 1:ncol(sample_data)){
    if(is.numeric(sample_data[,j])){
      tmp_summary <- summary(sample_data[,j]) 
      sum_tab[j] <- paste0("Variable:",names(sample_data[j]),"  ",paste0(c(names(tmp_summary),"sd"),"=",c(round(tmp_summary,3),round(sd(sample_data[,j]),3)),collapse=",  "))
    } 
    if(is.character(sample_data[,j])|is.factor(sample_data[,j])){
      tmp_summary <- summary(factor(sample_data[,j]))
      if(length(tmp_summary) > max_show) tmp_summary <- c(tmp_summary[1:max_show],"...")
      sum_tab[j] <- paste0("Variable:",names(sample_data[j]),"  ", paste0(names(tmp_summary),":",tmp_summary,collapse=",  "))
    }
  }
  paste(sum_tab, collapse="\n \n")
}


#------------------------------------------------------------------------------------------------------
### Define server logic required to summarize and view the selected dataset ###

shinyServer(function(input, output, session) {
  
  ###Conditional UI items ###
  #Create Stratification dropdown based on database
  output$stratcol <- renderUI({
    selectInput("stratcol", "Select Strata Variable", choices = dbinfo[[input$dbname]]$stratchoices)
  })
  
  ### reactive data object
  sample_data <- reactive({
    input$DrawButton
    isolate(
      if(input$samptype=="srs"){
        SRS(n=input$ndraws, dbtabname=input$dbname, seed=ifelse(input$wantseed,input$rseed,NA)) %>% select(-row_names)
      } else{
        StratSampler(input$ndrawsper,dbtabname=input$dbname,stratcol=input$stratcol, seed=ifelse(input$wantseed,input$rseed,NA)) %>% select(-row_names)
      }
    )
  })
  
  output$view <- renderDataTable({
    sample_data()
  }, options = list(aLengthMenu = c(5, 10, 25, 100), iDisplayLength = 10))
  
  
  # handler for data download (NOTE: will not operate in Rstudio viewer, open in browser to test)
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("data-", Sys.Date(), ".csv", sep="")
    } ,
    content = function(file) {
      write.csv(sample_data(), file, row.names = FALSE)
    }
  )
  
  
  output$summary <- renderPrint({
    writeLines(sum_table(sample_data()))
  })
  
  
  #   # reactive function to generate error reports if numeric fields are improperly filled (and thus sample button disabled)
  textboxInput <- reactive({
    samptype <- input$samptype
    ndraws <- input$ndraws
    rseed <- input$rseed
    ndrawsper <- input$ndrawsper
    if (samptype == "srs"){
      if (is.numeric(ndraws) & ndraws <= 10000 & ndraws >= 1 & is.numeric(rseed) & rseed <= 10000 & rseed >= 1) {
        warnmess <- "Ready to sample"} else {warnmess <- "Please make sure that the number of draws and the random seed are both integers 1-10000"}
      
    }
    if (samptype == "strat"){
      if (is.numeric(ndrawsper) & ndrawsper <= 1000 & ndrawsper >= 1 & is.numeric(rseed) & rseed <= 10000 & rseed >= 1) {
        warnmess <- "Ready to sample"} else {warnmess <- "Number of draws per strata must be an integer 1-1000 and that the random seed is an integer 1-10000"}
    }
    warnmess
  })
  
  
  #   ### Side panel object rendering ###
  #   # place message generated reactively from textboxInput() into a printable format to send to UI
  output$numwrong <- renderText({
    textboxInput()[1]
  })
  
  ### Second Tab Functionality ###
  
  output$var1 <- renderUI({
    input$DrawButton
    isolate(
      selectInput("var1", "Choose a Variable",
                  choices = dbinfo[[input$dbname]]$plot_vars )
    )
  })
  
  output$var2 <- renderUI({
    input$DrawButton
    isolate(
      selectInput("var2", "Choose a Second Variable",
                  choices = dbinfo[[input$dbname]]$plot_vars,
                  selected = dbinfo[[input$dbname]]$plot_vars[2])
    )
  })
  
  # generate plot based on selection characteristics and variables
  output$MainPlot <- renderPlot({
    data_sub <- sample_data()[,c(input$var1, input$var2)] 
    var_names <- dbinfo[[input$dbname]]$plot_vars 
    if(input$nvar == 1){
      if(is.numeric(data_sub[,1])){
        ggplot(data=data_sub) + theme_bw() +
          geom_histogram(aes_string(x=input$var1)) + labs(x=names(var_names)[var_names==input$var1])+
          theme(axis.text=element_text(size=14),
                axis.title=element_text(size=16))
      } else {
        if(input$reorder_check) data_sub[,1] <- factor(data_sub[,1],levels=names(sort(table(data_sub[,1]), decreasing = T)))
        ggplot(data=data_sub) + theme_bw() +
          geom_bar(aes_string(x=input$var1))+coord_flip()+ 
          scale_x_discrete(limits = rev(levels(data_sub[,input$var1])))+ 
          labs(x=names(var_names)[var_names==input$var1])+
          theme(axis.text=element_text(size=14),
                axis.title=element_text(size=16))
      }
    }else{
      if(is.numeric(data_sub[,1]) & is.numeric(data_sub[,2])){
        ggplot(data=data_sub) + theme_bw() +
          geom_point(aes_string(x=input$var2, y=input$var1)) +
          labs(x=names(var_names)[var_names==input$var2],y=names(var_names)[var_names==input$var1])+
          theme(axis.text=element_text(size=14),
                axis.title=element_text(size=16))
      }else if(!is.numeric(data_sub[,1]) & !is.numeric(data_sub[,2])){
        if(input$reorder_check) data_sub[,1] <- factor(data_sub[,1],levels=names(sort(table(data_sub[,1]), decreasing = T)))
        ggplot(data=data_sub) + theme_bw() +
          geom_bar(aes_string(x=input$var1, fill=input$var2))+ coord_flip() +
          scale_x_discrete(limits = rev(levels(data_sub[,input$var1])))+ 
          labs(fill=names(var_names)[var_names==input$var2],
               x=names(var_names)[var_names==input$var1])+
          theme(axis.text=element_text(size=14),
                axis.title=element_text(size=16))
      }else{
        if(input$reorder_check & !is.numeric(data_sub[,1])) data_sub[,1] <- reorder(factor(data_sub[,1]), -data_sub[,2], FUN = median, na.rm=TRUE )
        if(input$reorder_check & !is.numeric(data_sub[,2])) data_sub[,2] <- reorder(factor(data_sub[,2]), -data_sub[,1], FUN = median, na.rm=TRUE)
        ggplot(data=data_sub) + theme_bw() +
          geom_boxplot(aes_string(x=ifelse(is.numeric(data_sub[,1]),input$var2,input$var1),
                                  y=ifelse(is.numeric(data_sub[,2]),input$var2,input$var1)),
                       fill="lightblue") + coord_flip() +
          scale_x_discrete(limits = rev(levels(data_sub[,ifelse(is.numeric(data_sub[,1]),input$var2,input$var1)])))+ 
          labs(y=names(var_names)[var_names==input$var2],
               x=names(var_names)[var_names==input$var1])+
          theme(axis.text=element_text(size=14),
                axis.title=element_text(size=16))
      }
    }
    
  }) 
  #   
  #   ### Run at end of session ###
  #   
  session$onSessionEnded(function() {
    if (!is.null(con)) dbDisconnect(con)
  })
})

