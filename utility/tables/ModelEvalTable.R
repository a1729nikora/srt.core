model_eval_table <- function (in_data, ModelsForTable, input) {
  
  ModelEvalTypes <- c("-lnPL", "PL Ratio", "Bias", "Bias Trend")
  
  ModelEvalChosen <- input$EvalForTable
  k <- 0
  
  # First column holds failure numbers.
  local_ME_Table <- data.frame("Failure"=in_data[["Failure Number"]])
  
  ME_Table_Error <- FALSE
  
  if(length(ModelsForTable) > 0) {
    for(modelID in ModelsForTable) {
      if(ModelEvalChosen == "lnPL") {
        k <- 1
      } else if (ModelEvalChosen == "PLR") {
        k <- 2
      } else if (ModelEvalChosen == "UPlot") {
        k <- 3
      } else if (ModelEvalChosen == "YPlot") {
        k <- 4
      } else {
        # Couldn't identify the type of model evaluation to show.
        # Print an error message.
        
        ME_Table_Error <- TRUE
      }
      if (!(ME_Table_Error)) {
        eval_name <- paste(modelID, ModelEvalTypes[k], sep="_")
        local_ME_Table[[eval_name]] <- as.character(in_data[[eval_name]])
      }
    } # End for - build table entries for all models that were run.
  } else {
    
    # Somehow we don't have any model evaluations to display.
    # #print an error message.
    
    ME_Table_Error <- TRUE
    
  }

  if(ME_Table_Error) {
    local_ME_Table <- data.frame()
  }
  local_ME_Table = round_table(local_ME_Table, 6)
  return(local_ME_Table)
}