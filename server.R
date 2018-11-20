##### Server Script for ShinyDBSampler #####

#------------------------------------------------------------------------------------------------------
### Preliminaries ###

# Load all necessary libraries
library(shiny)
library(datasets)
library(plyr)
library(DBI)
library(RMySQL)
library(lubridate)
library(ggplot2)

load("dbinfolist.RData")

# define data base driver
con <- dbConnect(MySQL(),
                 user='ktmaurer',
                 password='pas$18Maurer',
                 dbname='shinyDBSampler',
                 host='10.34.225.221')

#------------------------------------------------------------------------------------------------------
### Sampling Functions ###


# function to generate a SRS from a given table (with given integer indexed column)
# n=number of draws, dbtabname=name of sql table , indexcol=unique db row identifier
# con1= connection specification to db, seed=random seed value
# infolistnum=location in dbinfolist to pull index info
SRS <- function(n, dbtabname, seed=NA){
  # select row numbers to draw
  if(!is.na(seed)) set.seed(seed)
  SRSindex <- sample(1:dbinfo[[dbtabname]]$N,n,replace = FALSE)
  # run query to pull draws
  sampall <- dbGetQuery(con,  sprintf("select * from %s WHERE row_names in (%s)",dbtabname, paste(SRSindex, collapse=",")))
  return(sampall)
}

# function to generate a Stratified Sample from given table with nper from each group 
# requires stratification column and integer indexed column
StratSampler <- function(nper, dbtabname, stratcol, indexcol, con1, seed,infolistnum){
  # randomly select row numbers from within groups
  set.seed(seed)
  grpidx <-  dbinfolist$idxlist[[infolistnum]][,c(stratcol,indexcol)]
  groupnames <- unique(grpidx[,1])
  allsampindex <- NULL
  for (i in groupnames){
    groupidxs <- grpidx[which(grpidx[,1]==i) , 2]    
    if(nper >= length(groupidxs)){
      allsampindex <- c(allsampindex, groupidxs)}else{
        allsampindex <- c(allsampindex, sample(groupidxs, nper, replace = FALSE))
      }
  }
  # query to pull draws
  sampall <- dbGetQuery(con1,  sprintf("select * from %s WHERE %s in (%s) ORDER BY %s",dbtabname,indexcol, paste(allsampindex, collapse=","),stratcol))
  # clean up variable types (all keys set to factors)
  keysdat <-  dbinfolist$keylist[[infolistnum]]
  for (j in 1:length(sampall[1,])){
    if (is.character(sampall[,j]) | names(sampall)[j] %in% keysdat) sampall[,j] <- as.factor(as.character(sampall[,j]))
  }
  rm(grpidx)
  return(sampall)
}


#------------------------------------------------------------------------------------------------------
### Define server logic required to summarize and view the selected dataset ###

shinyServer(function(input, output, session) {

  ###Conditional UI items ###
  #Create Stratification dropdown based on database
  output$stratcol <- renderUI({
    selectInput("stratcol", "Select Strata Variable", choices = dbinfo[[input$dbname]]$stratchoices)
  })
  
  # check_strat_var <- function(x, name_vec){
  #   names(x) %in% name_vec
  # }
  
  ### reactive data object
  sample_data <- reactive({
    input$DrawButton
    isolate(
      SRS(n=input$ndraws, dbtabname=input$dbname, seed=ifelse(input$wantseed,input$rseed,NA)) %>%
        select(-row_names)
    )
  
  })
  

  output$view <- renderDataTable({
    sample_data()
  }, options = list(aLengthMenu = c(5, 10, 25, 100), iDisplayLength = 5))
  
  
  output$summary <- renderPrint({
    if (input$samptype == "srs") print(summary(sample_data()), width=500)
  })
  
  
#   
#   
#   ### Define all reactive functions ###
#   # reactive function that executes the sampler with defined attributes: isolated to sample button
#   datasetInput <- reactive({
#     # make a dependency on input$DrawButton
#     input$DrawButton
#     # Use isolate() to avoid dependency on input$obs
#     isolate({
#       #when drawbutton clicked update these global variables
#       dbnam <- tolower(input$dbname)
#       samptype <- input$samptype
#       ndraws <- input$ndraws
#       ndrawsper <- input$ndrawsper
#       stratcol <- input$stratcol
#       rseed <- input$rseed
#       stratsumvar <- input$stratsumvar
#       infolistnum <- 1
#     })
#     
#     #Use global variables to define characteristics used sampling process
#     if (dbnam ==  "recmiler") {dbtabname <- "workouts" ; infolistnum <- 1;  cols <- c(2,3,5,6,4)}
#     if (dbnam ==  "accidents") {dbtabname <- "accidents" ; infolistnum <- 2;   cols <- c(42,47,4:7,43,2,3,52,53,8:10,15,16,39:41,18,44,55,28,25,20,12,21,11,19)}
#     if (dbnam ==  "census") {dbtabname <- "censusperson" ; infolistnum <- 3;   cols <- c(2,3,164,6,7,12,14,18,38,44,52,68,72,74,78,80,87,117,119,130,133,142,144,160,162,163)}
#     
#     #Use global variables to draw sample based on sampling type
#     if (samptype == "srs") dattoss <- SRS(ndraws, dbtabname, rseed)[,cols]
#     if (samptype == "strat") dattoss <- StratSampler(ndrawsper,dbtabname, stratcol, "SRSindex",con1(),rseed,infolistnum)[,cols]
#     dattoss
#   })
#   
#   # # reactive function that changes which db you are connected to: isolated to sample button
#   # con1 <- reactive({
#   #   input$DrawButton
#   #   isolate({
#   #     #kill connection if there is already one open
#   #     if (!is.null(con)) dbDisconnect(con)
#   #     #open new connection
#   #     con <<- dbConnect(db, host="mysql2.stat.iastate.edu", port=3306, password="k@rst3nm", user="kar", dbname="karstenm")
#   #   })
#   #   con
#   # })
#   
#   # reactive function to generate error reports if numeric fields are improperly filled (and thus sample button disabled)
#   textboxInput <- reactive({
#     samptype <- input$samptype
#     ndraws <- input$ndraws
#     rseed <- input$rseed
#     ndrawsper <- input$ndrawsper
#     if (samptype == "srs"){
#       if (is.numeric(ndraws) & ndraws <= 10000 & ndraws >= 1 & is.numeric(rseed) & rseed <= 10000 & rseed >= 1) {
#         warnmess <- "Ready to sample"} else {warnmess <- "Please make sure that the number of draws and the random seed are both integers 1-10000"}
#       
#     } 
#     if (samptype == "strat"){
#       if (is.numeric(ndrawsper) & ndrawsper <= 1000 & ndrawsper >= 1 & is.numeric(rseed) & rseed <= 10000 & rseed >= 1) {
#         warnmess <- "Ready to sample"} else {warnmess <- "Number of draws per strata must be an integer 1-1000 and that the random seed is an integer 1-10000"}        
#     }
#     warnmess
#   })  
#   
#   
#   ### Side panel object rendering ###
#   # place message generated reactively from textboxInput() into a printable format to send to UI
#   output$numwrong <- renderPrint({
#     textboxInput()
#   })
#   

# handler for data download (NOTE: will not operate in Rstudio viewer, open in browser to test)
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("data-", Sys.Date(), ".csv", sep="")
      } ,
    content = function(file) {
      write.csv(sample_data(), file, row.names = FALSE)
    }
  )

   
#   ### First Tab ('Get a Sample') object rendering ###
#   # Show data table with set to have 5 observations per page
  # output$view <- renderDataTable({
  #   datasetInput()
  # }, options = list(aLengthMenu = c(5, 10, 25, 100), iDisplayLength = 5))
#   
#   # Generate a summary of the dataset (on all variables if srs, on one variable if stratified )
  # output$summary <- renderPrint({
  #   isolate({
  #     dbnam <- tolower(input$dbname)
  #     samptype <- input$samptype
  #     stratcol <- input$stratcol
  #   })
  #   if (input$samptype == "srs") print(summary(dat), width=500)
  #   # if (samptype  == "strat") {
  #   #   stratdatout <- list()
  #   #   for(i in which(names(dat)!=stratcol)) {
  #   #     stratdatout[[i]] <- ddply(dat[is.na(dat[,i])==FALSE, ], stratcol, function(x) summary(x[,i]))
  #   #     names(stratdatout)[i] <- names(dat)[i]
  #   #     if (dim(stratdatout[[i]])[2] > 9) stratdatout[[i]] <- cbind(stratdatout[[i]][,1:9], truncated=rep("...", nrow(stratdatout[[i]])))
  #   #   }
  #   #   print(stratdatout, width=500,row.names=FALSE)
  #   # }
  # })
  # 
#   
#   
# 
  ### Second Tab Functionality ###

  output$respvar <- renderUI({
    selectInput("respvar", "Choose a Response Variable to Plot",
                choices = names(sample_data()) )
  })

  output$expvar <- renderUI({
    selectInput("expvar", "Choose a Explanatory Variable to Plot",
                choices = names(sample_data()) )
  })

  # generate plot based on selection characteristics and variables
  output$MainPlot <- renderPlot({
    p1 <- ggplot(data=sample_data())
    if(input$nvar == 1){
      if(is.numeric(sample_data()[,input$respvar])){
        p1 + geom_histogram(aes_string(x=input$respvar), binwidth=1)
      } else {
        p1 + geom_bar(aes_string(x=input$respvar))+coord_flip()
      }
    }else{
      ggplot() + theme_bw()
    }
  })
#   
#   ### Run at end of session ###
#   
  session$onSessionEnded(function() {
    if (!is.null(con)) dbDisconnect(con)
  })
})

