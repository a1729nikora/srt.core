data_or_trend_table <- function(fail_data_in, DataRange, DataOrTrend, TrendTest) {
  
  in_data <- fail_data_in
  
  D_or_T_table <- data.frame()
  TableCreateError <- FALSE
  
  # Are we working with failure counts or failure times data?
  
  if("FCount" %in% names(in_data)){
    # Working with failure counts data
    
    if (DataOrTrend == 1) {
      # Create failure data table
      
      in_data <- in_data$FCount
      
      DataIntervalStart <- DataRange[1]
      DataIntervalEnd <- DataRange[2]
      
      DataColNames <- names(in_data)
      names(in_data) <- gsub("x.", "", DataColNames)
      
      FC <- tail(head(in_data$FC, DataIntervalEnd), (DataIntervalEnd-DataIntervalStart+1))
      CFC <- tail(head(in_data$CFC, DataIntervalEnd), (DataIntervalEnd-DataIntervalStart+1))
      CumT <- tail(head(in_data$T, DataIntervalEnd), (DataIntervalEnd-DataIntervalStart+1))
      
      D_or_T_table <- data.frame("Cumulative Test Time"=CumT, "Failure Counts"=FC, "Cumulative Failure Count"=CFC)
    } else if (DataOrTrend == 2) {
      # Create trend test table
      
      # Since the Laplace Test can't be computed for failure counts data
      # if the intervals are of unequal length, we use the times between
      # failures version of the data even if the original version is
      # failure counts.
      
      in_data <- in_data$FRate
      
      DataColNames <- names(in_data)
      names(in_data) <- gsub("x.", "", DataColNames)
      
      if(DataRange[1]==1){
        DataIntervalStart <- 1
      } else {
        DataIntervalStart <- fail_data_in$FCount$CFC[(DataRange[1]-1)+1]
      }
      DataIntervalEnd <- fail_data_in$FCount$CFC[DataRange[2]]
      
      IF <- tail(head(in_data$IF, DataIntervalEnd), (DataIntervalEnd-DataIntervalStart+1))
      FT <- tail(head(in_data$FT, DataIntervalEnd), (DataIntervalEnd-DataIntervalStart+1))
      FN <- tail(head(in_data$FN, DataIntervalEnd), (DataIntervalEnd-DataIntervalStart+1))
      
      if (TrendTest == "LP") {
        
        # Laplace Test
        
        trendStat <- laplace_trend_test(IF)
        D_or_T_table <- data.frame("Failure Number"=FN, "Times Between Failures"=IF, "Laplace Test Statistic"=trendStat$Laplace_factor)
        
      } else if (TrendTest == "RA") {
        
        # Running Arithmetic Average
        
        trendStat <- running_average_test(IF)
        D_or_T_table <- data.frame("Failure Number"=FN, "Times Between Failures"=IF, "Running Average IF Time"=trendStat$Running_Average)
      } else {
        
        # Couldn't determine trend test type.
        # #print an error message.
        
        TableCreateError <- TRUE
      }
    } else {
      # Couldn't determine whether to create a data or
      # trend test table.
      # #print an error message.
      
      TableCreateError <- TRUE
    }
  } else if ("FRate" %in% names(in_data)) {
    # Working with failure times data
    
    DataIntervalStart <- DataRange[1]
    DataIntervalEnd <- DataRange[2]
    
    in_data <- in_data$FRate
    
    DataColNames <- names(in_data)
    names(in_data) <- gsub("x.", "", DataColNames)
    
    IF <- tail(head(in_data$IF, DataIntervalEnd), (DataIntervalEnd-DataIntervalStart+1))
    FT <- tail(head(in_data$FT, DataIntervalEnd), (DataIntervalEnd-DataIntervalStart+1))
    FN <- tail(head(in_data$FN, DataIntervalEnd), (DataIntervalEnd-DataIntervalStart+1))
    
    if (DataOrTrend == 1) {
      # Create failure data table
      
      D_or_T_table <- data.frame("Failure Number"=FN, "Times Between Failures"=IF, "Failure Time"=FT)
    } else if (DataOrTrend == 2) {
      # Create trend test table
      
      if (TrendTest == "LP") {
        
        # Laplace Test
        
        trendStat <- laplace_trend_test(IF)
        D_or_T_table <- data.frame("Failure Number"=FN, "Times Between Failures"=IF, "Laplace Test Statistic"=trendStat$Laplace_factor)
        
      } else if (TrendTest == "RA") {
        
        # Running Arithmetic Average
        
        trendStat <- running_average_test(IF)
        D_or_T_table <- data.frame("Failure Number"=FN, "Times Between Failures"=IF, "Running Average IF Time"=trendStat$Running_Average)
      } else {
        
        # Couldn't determine trend test type.
        # #print an error message.
        
        TableCreateError <- TRUE
      }
    } else {
      # Couldn't determine whether to create a data or
      # trend test table.
      # #print an error message.
      
      TableCreateError <- TRUE
    }
  } else {
    
    # Couldn't identify input data type.
    # #print an error message.
    
    TableCreateError <- TRUE
  } # Endif - working with failure counts or failure times?
  
  if (TableCreateError) {
    
    # If we've encountered any errors in creating the table,
    # return an empty data frame.
    
    D_or_T_table <- data.frame()
  }
  D_or_T_table = round_table(D_or_T_table, 6)
  return(D_or_T_table)
}
