# Copyright (c) ICT GROUP.  All rights reserved.

##PBI_R_VISUAL: VIZGAL_SCVIEW  Graphical display of schedule, planning or other events.
# Visualise schedule, planning or events in one graph
#
# INPUT:
# Name, Type, Start, End
#
# CREATION DATE: 12/08/2016
#
# LAST UPDATE: 12/08/2016
#
# VERSION: 0.0.1
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
  
  if (!exists("settings_colorPalette"))
  {
    settings_colorPalette = "Set1";
  }
  
  if (!exists("settings_orientation"))
  {
    settings_orientation= "horizontal";
  }
  
  dataset$Start <-
    as.POSIXct(dataset$Start, format = "%Y-%m-%dT%H:%M:%OS")
  dataset$End <-
    as.POSIXct(dataset$End,  format = "%Y-%m-%dT%H:%M:%OS")
  
  if(settings_orientation == "horizontal"){
  ggplot(dataset, aes(x = Start, y = Resource, color = User)) +
    geom_segment(aes(x = Start, xend = End, y = Resource,yend = Resource), size = 15) +
    scale_colour_discrete(guide = guide_legend(override.aes = list(size = 10))) +
    xlab("Time") +
    ylab(names(Resource)) +
    labs(color=names(User))+
    theme_bw() + 
    scale_colour_brewer(palette = settings_colorPalette)
  } else {
    ggplot(dataset, aes(x = Resource, y = Start, color = User)) +
      geom_segment(aes(x = Resource, xend = Resource, y = Start,yend = End), size = 15) +
      scale_colour_discrete(guide = guide_legend(override.aes = list(size = 10))) +
      ylab("Time") +
      xlab(names(Resource)) +
      labs(color=names(User))+
      theme_bw() + 
      scale_colour_brewer(palette = settings_colorPalette)
  }
  
  
  
} else {
  showErrorMessageToUser(
    paste(
      "Start and/or End date incorrectly formatted, required format: %Y-%m-%dT%H:%M:%OS\n
      More dateformats will be supported in newer version"
    )
    )
}
}
