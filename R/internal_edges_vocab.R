internal_edges_vocab<-
  function(biopax){
    #' @keywords internal
    
    vocab_inst_df<-
      biopax$dt[grepl("vocabulary"
                      ,class
                      ,ignore.case = TRUE) &
                  property_value!=""] %>% 
      unique
    
    all2vocab_edges<-
      biopax$dt[property_attr_value %in% vocab_inst_df$id
                ,.(from=id
                   ,to=property_attr_value
                   ,fromclass=class
                   ,type="vocab")]
    
    all2vocab_edges$type<-
      vocab_inst_df[match(all2vocab_edges$to,id)]$class %>% 
      gsub("Vocabulary|Code"
           ,""
           ,.)
    
    #for all those edges that have a class 
    #with a word "feature" in it
    #or is equal to word "evidence"
    #we need to find instances that refer to them, and replace those "from" ids
    #i.e. get the id of the object referring to the xref, i.e. the actual physical entity
    #get those rows as a logical vector
    refs_logi<-
      grepl("feature|^evidence$"
            ,all2vocab_edges$fromclass
            ,ignore.case = TRUE) 
    
    #get all those ids and combine them using ; as separator
    all2vocab_edges[refs_logi]$from<-
      all2vocab_edges[refs_logi]$from %>% 
      lapply(FUN=function(pid){
        getReferencingIDs(biopax=biopax,id=pid,recursive=FALSE)
      }) %>% 
      sapply(paste,collapse=";")
    
    #remove auxiliary class column
    all2vocab_edges$fromclass<-NULL
    
    #lengthen df by splitting those combined ids
    all2vocab_edges<-
      all2vocab_edges %>% 
      split_cols_lengthen_df(colsToSplit = "from"
                             ,patternToSplit = ";")
    
    return(all2vocab_edges)
    
  }