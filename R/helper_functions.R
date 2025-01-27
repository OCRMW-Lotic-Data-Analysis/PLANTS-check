### For reactable() tables.  Function to make column widths that forces all data on single line.  
# This is somehow not built into the reactable package.  This wrapper also allows the addition of extra 
# reactable::colDef() inputs as a second argument.  
dynamicColWidths <- function(tableData, otherColDefs = NULL){
  
  getColWidths <- function(colName){
    # Width of column name
    ncharColName <- nchar(colName)
    
    # Width of data.  Code below handles if NA count in this colName is some, all, or none.
    # max(NA, rm.na =TRUE) throws a warning without this.  
    nonNAdata <- tableData %>% pull(colName) %>% nchar() %>% na.omit()
    
    if (length(nonNAdata) > 0){
      ncharVal <- nonNAdata %>% max()
    } else if (length(nonNAdata) == 0) {
      ncharVal <- 0
    }
    
    # When grouping rows, add a few characters for the EvaluationID appended row count (e.g. "XE-SS-5141_2013-08-23 (10)")
    if (colName == "EvaluationID" | colName == "PlotID"){
      ncharVal <- ncharVal + 3
    }
    
    maxLength <- max(ncharColName, ncharVal, na.rm = TRUE) * 12 # 12 = pixels per character (font size?)
    
    reactable::colDef(minWidth = maxLength)
  }
  
  # Make list of column widths.  set_names names the list elements properly in purrr:map().
  singleLineColWidths <- set_names(names(tableData)) %>% purrr::map(getColWidths)
  
  # Merge column width settings and other colDefs.
  allColDefsMerged <- list_merge(singleLineColWidths, !!!otherColDefs) # '!!!' splices 2nd list into first
  
  # Fix attributes.  the S3 class attribute isn't allowed through with list_merge.
  # Each element is assigned the class here manually.  There must be a better way.
  
  # Function to fix attributes
  setColDefAttr <- function(x, elemName){
    y <- x[elemName]
    attr(y[[elemName]], "class") <- "colDef"
    return(y)
  }
  
  # Apply function to all elements.
  allColDefs <- map(names(allColDefsMerged), ~setColDefAttr(allColDefsMerged,.x)) %>% flatten()
}