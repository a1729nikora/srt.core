###############################################################################
#Tab4 Summary Table Section
###############################################################################

  # --------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------
  # ------------------------------------   TAB4 Summary Table   ------------------------------------------
  # ------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------


  tab4_table1_construct <- function(model,data,input){
    if(dataType(names(data))=="FR"){
      #model_params <- try(get(paste(model,get(paste(model,"methods",sep="_"))[1],"MLE",sep="_"))(get(paste("data"))[[get(paste(model,"input",sep="_"))]]),silent=TRUE)
      last_row <- length(ModelResults[,1]) - PredAheadSteps
      model_params_label <- paste(model,"params",sep="_")
      model_params <- as.data.frame(matrix(0, ncol=length(get(model_params_label)), nrow = 1))

      #Generating model_params from ModelResults. If the column is a list, it is converted to numeric
      
      parmNames <- c()
      for (paramNum in 1:length(get(model_params_label))) {
        model_parm_num <- paste0(model, "_", get(model_params_label)[paramNum], "_MLE")
        parmNames <- c(parmNames, model_parm_num)
        model_params[1,paramNum] <- ModelResults[[model_parm_num]][last_row]
      }
      colnames(model_params) <- parmNames
      
      if(typeof(model_params)!="character"){
        # number_fails <- get_prediction_n(model_params,input$modelDetailPredTime,length(get("data")[[get(paste(model,"input",sep="_"))]]))
        #max_lnL <- try(get(paste(model,"lnL",sep="_"))(get("data")[[get(paste(model,"input",sep="_"))]],model_params),silent=TRUE)
        
        #print(data_global()$FRate['FT'])

        if ("FT" %in% get(paste(model,"input",sep="_"))){
            max_lnL <- try(get(paste(model,"FT","lnL",sep="_"))(model_params, parmNames,FALSE,data_global()$FRate$FT),silent=FALSE)
            
        } else if ("IF" %in% get(paste(model,"input",sep="_"))){
            
            max_lnL <- try(get(paste(model,"FT","lnL",sep="_"))(model_params, parmNames,FALSE,data_global()$FRate$IF),silent=FALSE)
            
        } else {
            print("NOTHING found!")
        }
            
        # time_fails <- get_prediction_t(model_params, input$modelDetailPredFailures, length(get("data")[[get(paste(model,"input",sep="_"))]]))
      
        if(length(grep("not found",max_lnL))) {
          count<<-count+1
          tab4_table1[count,1] <<- get(paste0(model, "_fullname"))
          tab4_table1[count,2] <<- "Given model lnL not defined to compute AIC"
          tab4_table1[count,3] <<- "Given model lnL not defined to compute AIC" 
        }
        else if(typeof(max_lnL)!='double') {
          count<<-count+1
          tab4_table1[count,1] <<- get(paste0(model, "_fullname"))
          tab4_table1[count,2] <<- "Non numeral value. Something is not right"
          tab4_table1[count,3] <<- "Non numeral value. Something is not right" 
        }
        else {
            #print(paste0("Length of model_params = ", length(get(paste(model,"params",sep="_")))))
            
          AIC <- aic(length(get(paste(model,"params",sep="_"))),max_lnL)
          
          PSSE <- psse(model,data_global()$FRate$FT,model_params,input$percentData)
          count <<- count+1
          tab4_table1[count,1]<<- get(paste0(model, "_fullname"))
          tab4_table1[count,2]<<- AIC
          tab4_table1[count,3]<<- PSSE
        }
      }
      else if(typeof(model_params)=="character"){
        if(length(grep("not found",model_params))) {
          count<<-count+1
          tab4_table1[count,1] <<- model
          tab4_table1[count,2] <<- "Given-model not defined"
          tab4_table1[count,3] <<- "Given-model not defined" 
        }
        else {
          count<<-count + 1
          tab4_table1[count,1] <<- get(paste0(model, "_fullname"))
          tab4_table1[count,2] <<- "NON-CONV"
          tab4_table1[count,3] <<- "NON-CONV"
        }
      }
    }
    else{
      # -----> FC data should be handled here
    }
  }

  # Download handler for saving model result evaluation tables.

  output$saveModelEvals <- downloadHandler(
    filename = function() {
      data_set_name <- input$dataSheetChoice
      if(input$ModelEvaluationTabset == "Model Evaluation Plot") {
        
        # Save model results plot.
        
        paste(paste0(data_set_name, "_Results_", input$EvalToPlot), input$saveModelEvalsType, sep=".")
      } else if (input$ModelEvaluationTabset == "Model Evaluation Table") {
        
        # Save model results table.
        
        paste(paste0(data_set_name, "_Model_Evals"), "csv", sep=".")
      } else if (input$ModelEvaluationTabset == "Evaluation Summary") {
        
        # Save model evaluation summary.
        
        if(input$saveModelEvalSummaryType == "PDF") {
          paste(paste0(data_set_name, "_Eval_Summary"), "pdf", sep=".")
        } else {
          paste(paste0(data_set_name, "_Eval_Summary"), "csv", sep=".")
        }
      }
    },
    content = function(filespec) {
      if(input$ModelEvaluationTabset == "Model Evaluation Plot") {
        ggsave(filespec, plot=MEPlot, width=20,height=15)
      } else if (input$ModelEvaluationTabset == "Evaluation Summary") {
        tab4_table1_2_save <- tab4_table1
        
        # Turn OutputTable to character representations to avoid
        # difficulties with NA, Inf, and NaN.
        
        TableNames <- names(tab4_table1_2_save)
        for (nameIndex in TableNames) {
          tab4_table1_2_save[[nameIndex]] <- as.character(tab4_table1_2_save[[nameIndex]])
        }
        names(tab4_table1_2_save) <- c("Model", "AIC", "PSSE")
        
        if(length(tab4_table1_2_save) <= 1) {
          tab4_table1_2_save <- data.frame()
        }
        
        if(input$saveModelEvalSummaryType == "PDF") {
          out_put = knit2pdf('Tab4ReportTemplate.Rnw', clean = TRUE)
          file.rename(out_put, filespec) # move pdf to file for downloading
        } else {
          utils::write.csv(tab4_table1_2_save, file=filespec, quote=TRUE, na="NA")
        }
      } else if (input$ModelEvaluationTabset == "Model Evaluation Table") {
        OutputTable <- ModelEvalsFrame
        
        # Turn OutputTable to character representations to avoid
        # difficulties with NA, Inf, and NaN.
        
        TableNames <- names(OutputTable)
        for (nameIndex in TableNames) {
          OutputTable[[nameIndex]] <- as.character(OutputTable[[nameIndex]])
        }
        
        if(length(OutputTable) > 1) {
        } else {
          OutputTable <- data.frame()
        }
        utils::write.csv(OutputTable, file=filespec, quote=TRUE, na="NA")
      }
    }
  )


  output$mytable2 <- DT::renderDataTable({
      source("utility/metrics/GOF.R")
      inFile <- input$file
      if(is.null(inFile)){
        return("Please upload a file")
      }
      
      ModelsToEval <- input$modelResultsForEval

      if(length(ModelsToEval)<=0) {
          return
      }
      
      tab4_table1 <<- data.frame()
      
      # Use the subset of data to which models were applied
      # to do the model evaluation.
      
      in_data_tab4 <- ModeledData
      timeOffset <- ModeledData$FT[1] - ModeledData$IF[1]
      in_data_tab4$FT <- in_data_tab4$FT - timeOffset
      
        if(length(ModelsToEval)>0){
          count <<- 0
          
          for(i in ModelsToEval){
            tab4_table1_construct(i,in_data_tab4,input)
          }

        tab4_table1 <<- data.frame(tab4_table1[1],tab4_table1[2],tab4_table1[3])
        names(tab4_table1) <<- c("Model","AIC","PSSE")
      }
      tab4_table1 = round_table(tab4_table1, 6)

      tab4_table1
    }, filter="top", options = list(scrollX=TRUE, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))))

###############################################################################
#Tab4 Plot Section
###############################################################################

  # A reactive data item that is used to control the height of the model evals
  # plot.  The height is computed based on the width - it the plot is not as high
  # as it is wide, and if the width exceeds a minimum, then the height catches up with
  # the width to make a square plot.
  
  MP_height <- reactive({
    Width <- session$clientData$output_ModelEvaluationPlot_width
    Height <- session$clientData$output_ModelEvaluationPlot_height
    if((Width > Height) && (Width > 400)) {
      Height <- Width*0.75
    }
    Height
  })
  
  # Read the position of the mouse for the model results plot
  
  MPranges <- reactiveValues(x = NULL, y = NULL)
  
  # Event observer for double-click on model results plot.
  # Double click and brush zooms in and out.
  
  observeEvent(input$MEPdblclick, {
    MPbrush <- input$MEP_brush
    if (!is.null(MPbrush)) {
      MPranges$x <- c(MPbrush$xmin, MPbrush$xmax)
      MPranges$y <- c(MPbrush$ymin, MPbrush$ymax)
      
    } else {
      MPranges$x <- NULL
      MPranges$y <- NULL
    }
  })
  
  
# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------
# ------------------------------------   TAB4 Evaluations Plot   ---------------------------------------
# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------
  
  output$ModelEvaluationPlot <- renderPlot({
    data_set <- input$dataSheetChoice
    MEPlot <<-NULL
    if((length(input$modelResultsForEval) > 0) && (input$modelResultsForEval[1] != "None") && (!is.null(ModelEvalsFrame))) {
      MEPlot <<- plot_model_evals(ModelEvalsFrame, data_set, input, MPranges$x, MPranges$y, session$clientData$output_ModelPlot_width)
      if(!is.null(MEPlot)) {
        MEPlot <<- MEPlot + coord_cartesian(xlim = MPranges$x, ylim = MPranges$y)
      }
    }
    MEPlot
  }, height=MP_height)
  

###############################################################################
#Tab4 Evaluation Detail Table Section
###############################################################################
  
# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------
# --------------------------------   TAB4 Evaluations Detail Table   -----------------------------------
# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------

  
  output$mytable3 <- DT::renderDataTable({
    ModelEvalTypes <- c("-lnPL", "PL Ratio", "Bias", "Bias Trend")
    
    ME_Table <- NULL
    
    # Check if modelResultChoice is None and return NULL if true
    if(length(input$modelResultsForEval)==0){
      return(ME_Table)
    }
    if(input$modelResultsForEval[1]=="None"){
      return(ME_Table)
    }
    if(is.null(ModelEvalsFrame)){
      return
    } else if(!is.null(ModelEvalsFrame)) {
      if(length(input$modelResultsForEval) > 0) {
        
        # User has selected at least one set of model evaluations to display as a table.
        
        ME_Table <- model_eval_table(ModelEvalsFrame, input$modelResultsForEval, input)
      }
    }
    ME_Table = round_table(ME_Table, 6)
    ME_Table
  }, filter="top", options = list(scrollX=TRUE, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))))
    