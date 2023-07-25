#' Run a XML nodeset to Data frame
#' @name xmlToDF
#' @aliases xmlToDF
#' @description A different way of doing \code{\link{xmlToDataFrame}}
#' @param doc XML document to be parsed
#' @param xpath XPath expression to extract the dataset 
#' @param usewhich (logical) use [which(logical),] versus [logical, ] for
#' subsetting
#' @param verbose (logical) for things to be printed (default = TRUE)
#' @param isXML (logical) FALSE if doc is a character
#' @param ... arguments to be passed to \code{\link{xmlParse}}
#' @export
#' @seealso \code{\link{xmlParse}}, \code{\link{xmlToDataFrame}}
#' @return A data.frame with the number of columns being the unique field
#' names from all nodes
xmlToDF = function(doc, xpath, usewhich = TRUE, verbose=TRUE, 
                   isXML = TRUE, ...){
  
  if (isXML){
    ### make sure an XML document
    stopifnot(inherits(doc, "XMLAbstractDocument"))
  } else {
    doc <- xmlParse(doc, ...)
  }  
  
  #### get the records for that form
  nodeset <- getNodeSet(doc, xpath)
  
  ## get the field names
  var.names <- lapply(nodeset, names)
  
  ## get the total fields that are in any record
  fields = unique(unlist(var.names))
  
  if(is.null(fields)){
    df = fields
    
  } else {
    ## extract the values from all fields
    dl = lapply(fields, function(x) {
      if (verbose) print(paste0("  ", x))
      xpathSApply(doc, paste0(xpath, "/", x), xmlValue)
    })
    
    ## make logical matrix whether each record had that field
    name.mat = t(sapply(var.names, function(x) fields %in% x))
    df = data.frame(matrix(NA, nrow=nrow(name.mat), ncol=ncol(name.mat)))
    names(df) = fields
    
    len_nammat = ncol(name.mat)
    ## fill in that data.frame
    for (icol in 1:len_nammat){
      rep.rows = name.mat[, icol]
      if (usewhich) rep.rows = which(rep.rows)
      df[rep.rows, icol] = dl[[icol]]
    }
  }
  
  return(df)
}