library(rootSolve)
library(numDeriv)


run_model_evals <- function(modeled_data, model_results, model_data_range, successful_models, input) {
  evalsList <- c()
  
  ParmInitIntvl <- input$parmEstIntvl
  DataStart <- model_data_range[1]
  DataEnd <- model_data_range[2]
  OffsetFailure <- DataStart-1
  localEstIntvlEnd <- ParmInitIntvl-DataStart+1
  
  #print(successful_models$MLE)  # Debug code
  
  if (("FRate" %in% (names(modeled_data))) && !("FCount" %in% (names(modeled_data)))) {
    # We're dealing with Failure Rates data.
    
    # We'll compute the prequential likelihood, prequential likelihood ratio,
    # model bias, and model bias trend for each model that was successfully run.
    # Only the MLE estimates of model parameters are used to compute these
    # applicability assessments.
    
    for (i in 1:length(successful_models$MLE)) {
      # First get the estimated parameters for the model from the model results table.
      # The names of parameters in this data frame are given by:
      # "<model_ID>_<parameter name>_MLE"
      
      model_ID <- successful_models$MLE[i]
      
      parm_names <- get(paste(successful_models$MLE[i], "params", sep="_"))
      parm_estimates <- matrix(nrow=DataEnd-ParmInitIntvl+1, ncol=length(parm_names))
      parm_estimates <- as.data.frame(parm_estimates)
      names(parm_estimates) <- parm_names
      
      for (k in 1:length(parm_names)) {
        col_name <- paste(model_ID, parm_names[k], "MLE", sep="_")
        parm_estimates[[parm_names[k]]] <- head(tail(as.array(unlist(model_results[[col_name]])), DataEnd-ParmInitIntvl+2), DataEnd-ParmInitIntvl+1)
      }

      # Compute prequential likelihood

      preqLike <- paste(successful_models$MLE[i], "Preq", "lnL", sep="_")
      print(model_ID)         # Debug code
      print(parm_estimates)   # Debug code
      #lnPl <- get(preqLike)
      
      # Compute prequential likelihood ratio
      
      # Copmute model bias
      
      # Compute model bias trend
    }
  } else {
    # We're dealing with Failure Counts data
  }
  return(evalsList)
}
