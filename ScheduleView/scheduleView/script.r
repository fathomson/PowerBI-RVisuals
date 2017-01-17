# Copyright (c) ICT GROUP.  All rights reserved.

##PBI_R_VISUAL: VIZGAL_SCVIEW  Graphical display of schedule, planning or other events.
# Visualise schedule, planning or events in one graph
#
# INPUT:
# Resource, User, Start, End
#
# CREATION DATE: 12/08/2016
#
# LAST UPDATE: 01/17/2017
#
# VERSION: 1.0.0
#
# R VERSION TESTED: 3.3.2
#
# AUTHOR: F.A. THOMSON (frank.thomson@ict.nl)


############### LIBRARY DECLARATIONS ###############

libraryRequireInstall = function(packageName, ...)
{
  if (!require(packageName, character.only = TRUE))
    warning(paste("*** The package: '", packageName, "' was not installed ***", sep =
                    ""))
}

libraryRequireInstall("ggplot2")
libraryRequireInstall("grid")
libraryRequireInstall("gridExtra")
libraryRequireInstall("wesanderson")
libraryRequireInstall("RColorBrewer")
libraryRequireInstall("dplyr")

############ INTERNAL FUNCTIONS #########

# plot error message to user
showErrorMessageToUser <- function(message) {
  par(mar = c(0, 0, 0, 0))
  plot(
    c(0, 1),
    c(0, 1),
    ann = F,
    bty = 'n',
    type = 'n',
    xaxt = 'n',
    yaxt = 'n'
  )
  text(
    x = 0.5,
    y = 0.5,
    message,
    cex = 1.6,
    col = "black"
  )
}

# check if date in handable format
# TDOD for next version: support multiple date formats
dateInCorrectFormat <- function(date) {
  d <- try(as.Date(date, format = "%Y-%m-%dT%H:%M:%OS"))
  return (class(d) != "try-error" & !is.na(d))
}


# Get segment size so that they do not overlap on resize / large number of resources.

getSegmentSize = function(numCols, numRows, orientation = "horizontal", maxW = 25, minW = 1)
{
  convFactor = 20 # unit conversion
  hw = par()$din
  if(orientation == "horizontal")
    segSize = hw[2]/numCols
  else
    segSize = 0.5* hw[1]/numRows # take legend into account 
  
  segSize = min(max(segSize*convFactor, minW),maxW)
  
  return(segSize)
}

# Sort the dataset. A-Z (az), Z-A (za), total duration (total_duration) and user count (user_count)

sortDataset = function(dataset, sorting = "az", orientation="horizontal")
{
  switch(sorting,
         "az" = {
           if(settings_orientation == "horizontal"){
              dataset$Resource  <- factor(dataset$Resource, levels= dataset[rev(order(dataset$Resource)), "Resource"])
           } else {
             dataset$Resource  <- factor(dataset$Resource, levels= dataset[order(dataset$Resource), "Resource"])
           }
         },
         "za" = {
           if(settings_orientation == "horizontal"){
             dataset$Resource  <- factor(dataset$Resource, levels= dataset[order(dataset$Resource), "Resource"])
           } else {
             dataset$Resource  <- factor(dataset$Resource, levels= dataset[rev(order(dataset$Resource)), "Resource"])
           }
        
         },
         "total_duration" = {
           dataset$duration <- difftime(dataset$End, dataset$Start, units="secs")
           dataset$duration[is.na(dataset$duration)] <- 0 
           temp <- dataset %>% group_by(Resource) %>% summarize(t=sum(duration)) %>% arrange(t)
           if(settings_orientation == "horizontal"){
             dataset$Resource  <- factor(dataset$Resource, levels= temp$Resource)
           } else {
             dataset$Resource  <- factor(dataset$Resource, levels= rev(temp$Resource))
           }
         },
         "user_count" = {
           temp <- dataset %>% group_by(Resource) %>% summarize(n=n()) %>% arrange(n)
           if(settings_orientation == "horizontal"){
             dataset$Resource  <- factor(dataset$Resource, levels= temp$Resource)
           } else {
             dataset$Resource  <- factor(dataset$Resource, levels= rev(temp$Resource))
           }
         })
  return(dataset)
}

# strText = text to modify 
# strCex = font size 
# abbrTo = very long string will be abbreviated to "abbrTo" characters
# isH = "is horizontal" ?
# maxChar = text smaller than maxChar is replaced by NULL
# partAvailable = which portion of window is available for text, in [0,1]

cutStr2Show = function(strText, strCex = 0.8, abbrTo = 100, isH = TRUE, maxChar = 0, partAvailable = 1)
{

  if(is.null(strText))
    return (NULL)
  
  SCL = 0.094*strCex
  pardin = par()$din
  gStand = partAvailable*(isH*pardin[1]+(1-isH)*pardin[2]) /SCL
  
  # if very very long abbreviate
  if(nchar(strText)>abbrTo && nchar(strText)> 1)
    strText = abbreviate(strText, abbrTo, dot = TRUE)
  
  # if looooooong convert to lo...
  if(nchar(strText)>round(gStand) && nchar(strText)> 1)
    strText = paste(substring(strText,1,floor(gStand)),"...",sep="")
  
  # if shorter than maxChar remove 
  if(gStand<=maxChar)
    strText = NULL
  
  return(strText) 
}


# Calculate and return the number of legend columns as selected by the user

getNcolLegend = function(ncols = "auto"){
  colCount <- 1;
  switch(ncols,
        "auto" = { colCount <- ceiling(nrow(unique(User))/nrow(unique(Resource)))},
        "one" = {colCount <- 1},
        "two" = {colCount <- 2},
        "three" = {colCount <- 3},
        "four" = {colCount <- 4},
        "five" = {colCount <- 5})
  return(colCount)
}
  



############ INPUT / DATE VALIDATION #########

input_valid <- TRUE
dates_valid <- TRUE
message <- ""

if (!exists("Resource")) {
  input_valid <- FALSE
  message <- paste(message, "Resource ")
}
if (!exists("User")) {
  input_valid <- FALSE
  message <- paste(message, "User ")
}
if (!exists("Start")) {
  input_valid <- FALSE
  message <- paste(message, "Start ")
}
if (!exists("End")) {
  input_valid <- FALSE
  message <- paste(message, "End " + settings_orientation)
}





if (input_valid) {
  # Assign columns to dataset
  dataset <- cbind(Resource, User, Start, End)
  # Rename columns
  colnames(dataset) <- c("Resource", "User", "Start", "End")
  # Remvoe NA's
  dataset <- na.omit(dataset)
  dates_valid <-   (dateInCorrectFormat(dataset$Start[1]) &
                      dateInCorrectFormat(dataset$End[1]))
} else {
  dates_valid <- FALSE
  plot.new()
  #showErrorMessageToUser(paste("All four fields required, please add: ", message))
}



############ INPUT / DATE VALIDATION #########
if(input_valid){

if (dates_valid) {
  if(!exists("settings_sorting"))
  {
    settings_sorting = "az";
  }
  
  if (!exists("settings_colorPalette"))
  {
    settings_colorPalette = "Set1";
  }
  
  if (!exists("settings_orientation"))
  {
    settings_orientation= "horizontal";
  }
  if(!exists("settings_legendCols")){
    settings_legendCols = "auto";
  }
  
  dataset$Start <-
    as.POSIXct(dataset$Start, format = "%Y-%m-%dT%H:%M:%OS")
  dataset$End <-
    as.POSIXct(dataset$End,  format = "%Y-%m-%dT%H:%M:%OS")
  
  colourCount <- length(unique(dataset$User))
  getPalette <- colorRampPalette(brewer.pal(8, settings_colorPalette))
  
  segSize = getSegmentSize(length(unique(dataset$Resource)), length(unique(dataset$User)), orientation = settings_orientation)
  
  dataset <- sortDataset(dataset, sorting = settings_sorting, orientation = settings_orientation)
  
  dataset$User <- abbreviate(dataset$User, 30)
  
  ncolLegend <- getNcolLegend(ncols = settings_legendCols);

  if(settings_orientation == "horizontal"){
    ggplot(dataset, aes(x = Start, y = Resource, color = User)) +
      geom_segment(aes(x = Start, xend = End, y = Resource,yend = Resource), size = segSize) +
      xlab("Time") +
      ylab(cutStr2Show(names(Resource), abbrTo = 50)) +
      labs(color=names(User))+
      theme_bw()  +
      scale_colour_manual(values = getPalette(colourCount)) + 
      theme(legend.direction ="vertical",legend.position = "right") + guides(color=guide_legend(ncol=ncolLegend))

  } else {
    ggplot(dataset, aes(x = Resource, y = Start, color = User)) +
      geom_segment(aes(x = Resource, xend = Resource, y = Start,yend = End), size = segSize) +
      ylab("Time") +
      xlab(cutStr2Show(names(Resource), abbrTo = 50)) +
      labs(color=names(User))+
      theme_bw() + 
      scale_colour_manual(values = getPalette(colourCount)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+ guides(color=guide_legend(ncol=ncolLegend))
  }
  
  
} else {
  showErrorMessageToUser(
    paste("Start and/or End date incorrectly formatted, required format: %Y-%m-%dT%H:%M:%OS\n
      More dateformats will be supported in newer version"
    )
    )
}
}
  
