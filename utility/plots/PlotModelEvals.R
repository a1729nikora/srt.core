# Plot model applicability evaluation analyses

plot_model_evals <- function(ModelEvalsData, DataSetName, input, plotWidthRange, plotHeightRange, plotPixels) {
  require(ggplot2)
  
  DisplayModels <- input$modelResultsForEval
  DataView <- input$EvalToPlot
  PlotView <- input$ModelEvalPlotType
  PlotFault <- FALSE
  
  # Initialize the model evaluation plot
  
  localEvalsPlot <- ggplot()
  
  # Set up the values that we'll need to create a plot legend
  
  scaleManBreaks <- c()
  scaleManColors <- c()
  
  # Create plot axes
  
  if(DataView == "lnPL") {
    localEvalsPlot <- localEvalsPlot + ggtitle(paste0("Negative ln(Prequential Likelihood) vs. Failure Number for ", DataSetName))
    localEvalsPlot <- localEvalsPlot + xlab("Failure Number")+ylab("-ln(Prequential Likelihood)")
  } else if(DataView == "PLR") {
    localEvalsPlot <- localEvalsPlot + ggtitle(paste0("Prequential Likelihood Ratio vs. Failure Number for ", DataSetName))
    localEvalsPlot <- localEvalsPlot + xlab("Failure Number")+ylab("Prequential Likelihood Ratio")
  } else if(DataView == "UPlot") {
    localEvalsPlot <- localEvalsPlot + ggtitle(paste0("Model Bias (u-plot) for ", DataSetName))
    localEvalsPlot <- localEvalsPlot + xlab("u(i)")+ylab("Cumulative Distribution of u(i)")
  } else if(DataView == "YPlot") {
    localEvalsPlot <- localEvalsPlot + ggtitle(paste0("Model Bias Trend (y-plot) for ", DataSetName))
    localEvalsPlot <- localEvalsPlot + xlab("y(i)")+ylab("Cumulative Distribution of y(i)")
  } else {
    
    # Couldn't identify view of data to display.
    # #print an error message.
    
    #print(msgModelDataViewUnknown)
    PlotFault <- TRUE
  }
  
  # Set up the vectors for the x-axis when drawing curves on the plot.
  
  if(!is.null(plotWidthRange) && !is.null(plotHeightRange)) {
    # We've zoomed in to a subset of the plot.  We don't need to specify the
    # x values for each model.
    
    if ((DataView == "lnPL") || (DataView == "PLR")) {
      X_startPoint <- max(plotWidthRange[1], ModelEvalsData$Failure[1])
    } else if ((DataView == "UPlot") || (DataView == "YPlot")) {
      X_startPoint <- max(plotWidthRange[1], 0)
    }
    X_AxisLinePlotVals <- seq(from=X_startPoint, to=plotWidthRange[2], by=(plotWidthRange[2]-X_startPoint)/(plotPixels-1))
  }
  
  for (modelIndex in DisplayModels) {
    
    # Create plot data, axes, and titles based on the view
    # of the data selected by the user (e.g., prequential likelihood)
    
    # Set up the vectors for the x-axis when drawing curves on the plot.
    # If we haven't already done this for a section of the plot that we're
    # zooming in to, we do it for the whole plot here.
    
    if(is.null(plotWidthRange) && is.null(plotHeightRange)) {
      # We're looking at the entire plot.
      
      if ((DataView == "lnPL") || (DataView == "PLR")) {
        xAxisVals <- ModelEvalsData[["Failure Number"]]
        if (DataView == "lnPL") {
          max_k <- 0
          maxVal <- 0
          for (k in 1:length(DisplayModels)) {
            if (max(ModelEvalsData[[paste(DisplayModels[k], "-lnPL", sep="_")]]) > maxVal) {
              maxVal <- max(ModelEvalsData[[paste(DisplayModels[k], "-lnPL", sep="_")]])
              max_k <- k
            }
          }
          IFVals <- c(0, ModelEvalsData[[paste(DisplayModels[max_k], "-lnPL", sep="_")]])
        } else {
          max_k <- 0
          maxVal <- 0
          for (k in 1:length(DisplayModels)) {
            if (max(ModelEvalsData[[paste(DisplayModels[k], "PL Ratio", sep="_")]]) > maxVal) {
              maxVal <- max(ModelEvalsData[[paste(DisplayModels[k], "PL Ratio", sep="_")]])
              max_k <- k
            }
          }
          IFVals <- c(0, ModelEvalsData[[paste(DisplayModels[max_k], "PL Ratio", sep="_")]])
        }
      } else if ((DataView == "UPlot") || (DataView == "YPlot")) {
        xAxisVals <- c(0:10)/10
        IFVals <- c(0:10)/10
      }
      #X_AxisLinePlotVals <- seq(from=xAxisVals[1], to=xAxisVals[length(xAxisVals)], by=(xAxisVals[length(xAxisVals)]-(xAxisVals[1]-IFVals[1]))/(plotPixels-1))
    }
    
    if(DataView == "lnPL") {
      model_plot_data <- data.frame("Time" = ModelEvalsData[["Failure Number"]], "Failure" = ModelEvalsData[[paste(modelIndex, "-lnPL", sep="_")]], "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModelEvalsData[["Failure Number"]])))
      model_line_data <- model_plot_data
    } else if(DataView == "PLR") {
      model_plot_data <- data.frame("Time" = ModelEvalsData[["Failure Number"]], "Failure" = ModelEvalsData[[paste(modelIndex, "PL Ratio", sep="_")]], "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModelEvalsData[["Failure Number"]])))
      model_line_data <- model_plot_data
    } else if (DataView == "UPlot") {
      model_plot_data <- data.frame("Time" = sort(ModelEvalsData[[paste(modelIndex, "Bias", sep="_")]]), "Failure" = cumsum(rep(1/length(ModelEvalsData[["Failure Number"]]), length(ModelEvalsData[["Failure Number"]]))), "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModelEvalsData[["Failure Number"]])))
      model_line_data <- model_plot_data
    } else if (DataView == "YPlot") {
      model_plot_data <- data.frame("Time" = ModelEvalsData[[paste(modelIndex, "Bias Trend", sep="_")]], "Failure" = cumsum(rep(1/length(ModelEvalsData[["Failure Number"]]), length(ModelEvalsData[["Failure Number"]]))), "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModelEvalsData[["Failure Number"]])))
      model_line_data <- model_plot_data
    } else {
      
      # Couldn't identify view of data to display.
      # #print an error message.
      
      #print(msgModelDataViewUnknown)
      PlotFault <- TRUE
    }
    
    scaleManBreaks <- c(scaleManBreaks, get(paste(modelIndex, "fullname", sep="_")))
    scaleManColors <- c(scaleManColors, get(paste(modelIndex, "plotcolor", sep="_")))
    
    if (PlotView == "points_and_lines") {
      localEvalsPlot <- localEvalsPlot + geom_point(data=model_plot_data,aes(Time,Failure,color=Model)) + geom_line(data=model_line_data, aes(Time,Failure,color=Model,linetype=Model))
    } else if (PlotView == "points") {
      localEvalsPlot <- localEvalsPlot + geom_point(data=model_plot_data,aes(Time,Failure,color=Model))
    } else if (PlotView == "lines") {
      localEvalsPlot <- localEvalsPlot + geom_line(data=model_line_data, aes(Time,Failure,color=Model,linetype=Model))
    } else {
      
      # Couldn't identify the plot type.
      # #print an error message.
      
      #print(paste0("plot_model_results: ", msgPlotTypeUnknown))
      PlotFault <- TRUE
    }
    
  } # End for - draw results for all models
  
  if ((DataView == "UPlot") || (DataView == "YPlot")) {
    # Draw a straight line from the origin to (1,1)
    # if this is a u-plot or a y-plot.
    
    tempPlotData <- data.frame("Time"=cumsum(rep(1/length(ModelEvalsData[["Failure Number"]]), length(ModelEvalsData[["Failure Number"]]))), "Failure"=cumsum(rep(1/length(ModelEvalsData[["Failure Number"]]), length(ModelEvalsData[["Failure Number"]]))), "Model" =rep("Unbiased Ideal", length(ModelEvalsData[["Failure Number"]])))
    
    if (PlotView == "points_and_lines") {
      localEvalsPlot <- localEvalsPlot + geom_point(data=tempPlotData, aes(Time,Failure,color=Model)) + geom_line(data=tempPlotData, aes(Time,Failure,color=Model,linetype=Model))
    } else if (PlotView == "points") {
      localEvalsPlot <- localEvalsPlot + geom_point(data=tempPlotData, aes(Time,Failure,color=Model))
    } else if (PlotView == "lines") {
      localEvalsPlot <- localEvalsPlot + geom_line(data=tempPlotData, aes(Time,Failure,color=Model,linetype=Model))
    } else {
      
      # Couldn't identify the plot type.
      # #print an error message.
      
      #print(paste0("plot_model_results: ", msgPlotTypeUnknown))
      PlotFault <- TRUE
    }
    
  }
  
  #localEvalsPlot <- localEvalsPlot + scale_color_manual("", breaks=scaleManBreaks, values=scaleManColors)
  localEvalsPlot <- localEvalsPlot + theme(legend.position = "bottom", text = element_text(size=14))
  
  if(PlotFault) {
    localEvalsPlot = NULL
  }
  return(localEvalsPlot)
}
