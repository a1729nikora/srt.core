library(rootSolve)
library(numDeriv)


run_model_evals <- function(modeled_data, model_results, model_data_range, successful_models, input) {
  
  ModelEvalTypes <- c("-lnPL", "PL Ratio", "Bias", "Bias Trend")
  ParmInitIntvl <- input$parmEstIntvl
  DataStart <- model_data_range[1]
  DataEnd <- model_data_range[2]
  OffsetFailure <- DataStart-1
  localEstIntvlEnd <- ParmInitIntvl-DataStart+1
  
  # Set up the data frame in which the results will be returned.

  localEvalsFrame <- matrix(data=NA, nrow=(DataEnd-ParmInitIntvl), ncol=1+(length(ModelEvalTypes)*length(successful_models$MLE)))
  evalNames <- c("Failure Number")
  localEvalsFrame[,1] <- c(localEstIntvlEnd:(DataEnd-1))
  for (k in 1:length(ModelEvalTypes)) {
    evalNames <- c(evalNames, paste(c(successful_models$MLE), ModelEvalTypes[k], sep="_"))
  }
  localEvalsFrame <- as.data.frame(localEvalsFrame)
  names(localEvalsFrame) <- evalNames

  #print(successful_models$MLE)  # Debug code
  
  if (("FRate" %in% (names(modeled_data))) && !("FCount" %in% (names(modeled_data)))) {
    # We're dealing with Failure Rates data.
    
    # We'll compute the prequential likelihood, prequential likelihood ratio,
    # model bias, and model bias trend for each model that was successfully run.
    # Only the MLE estimates of model parameters are used to compute these
    # applicability assessments.
    
    # Set up a vector of minimum ln(prequential likelihood) values.
    # That will be used to compute the prequential likelihood ratio
    # from ln(prequential likelihood).
    
    tempPLmax <- rep(0, (DataEnd-ParmInitIntvl))
    
    for (i in 1:length(successful_models$MLE)) {
      # First get the estimated parameters for the model from the model results table.
      # The names of parameters in this data frame are given by:
      # "<model_ID>_<parameter name>_MLE"
      
      model_ID <- successful_models$MLE[i]
      
      parm_names <- get(paste(successful_models$MLE[i], "params", sep="_"))
      parm_estimates <- matrix(nrow=DataEnd-ParmInitIntvl, ncol=length(parm_names))
      parm_estimates <- as.data.frame(parm_estimates)
      names(parm_estimates) <- parm_names
      
      for (k in 1:length(parm_names)) {
        col_name <- paste(model_ID, parm_names[k], "MLE", sep="_")
        parm_estimates[[parm_names[k]]] <- head(tail(as.array(unlist(model_results[[col_name]])), DataEnd-ParmInitIntvl+2), DataEnd-ParmInitIntvl)
      }

      # Set up parameters to compute model evaluation criteria

      preqLike <- paste(successful_models$MLE[i], "FT", "Preq", "lnL", sep="_")
      modelBias <- paste(successful_models$MLE[i], "FT", "Bias", sep="_")
      modelTrend <- paste(successful_models$MLE[i], "FT", "Bias", "Trend", sep="_")
      print(preqLike)         # Debug code
      print(parm_estimates)   # Debug code
      
      FN <- modeled_data$FRate$FN
      IF <- c(unlist(subset(subset(modeled_data$FRate, modeled_data$FRate$FN >= ParmInitIntvl, select = c(FN, IF, FT)), FN <= DataEnd, select = IF)), use.names=FALSE)
      FT <- c(unlist(subset(subset(modeled_data$FRate, modeled_data$FRate$FN >= ParmInitIntvl, select = c(FN, IF, FT)), FN <= DataEnd, select = FT)), use.names=FALSE)
      FN <- c(unlist(subset(subset(modeled_data$FRate, modeled_data$FRate$FN >= ParmInitIntvl, select = c(FN, IF, FT)), FN <= DataEnd, select = FN)), use.names=FALSE)
      in_fail_data <- data.frame("FT"=FT, "IF"=IF, "FN"=FN)
      #print(in_fail_data)   # Debug code
      
      # Compute prequential likelihood
      
      k <- 1
      tempEval <- get(preqLike)(parm_estimates, in_fail_data)
      localEvalsFrame[[paste(successful_models$MLE[i], ModelEvalTypes[k], sep="_")]] <- tempEval
      tempPLmax <- pmax(tempEval, tempPLmax)
      
      # Compute model bias
      
      k <- 3
      tempEval <- get(modelBias)(parm_estimates, in_fail_data)
      localEvalsFrame[[paste(successful_models$MLE[i], ModelEvalTypes[k], sep="_")]] <- tempEval
      
      # Compute model bias trend
      
      k <- 4
      tempEval <- get(modelTrend)(parm_estimates, in_fail_data)
      localEvalsFrame[[paste(successful_models$MLE[i], ModelEvalTypes[k], sep="_")]] <- tempEval
    }
    for (i in 1:length(successful_models$MLE)) {
      
      # Compute prequential likelihood ratio
      k <- 2
      localEvalsFrame[[paste(successful_models$MLE[i], ModelEvalTypes[k], sep="_")]] <- exp(tempPLmax - localEvalsFrame[[paste(successful_models$MLE[i], ModelEvalTypes[1], sep="_")]])
    }
    #print(localEvalsFrame)  # Debug code
  } else {
    # We're dealing with Failure Counts data
  }
  return(localEvalsFrame)
}
