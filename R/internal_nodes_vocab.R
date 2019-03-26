internal_nodes_vocab<-
  function(biopax){
    #' @keywords internal
    
    vocab_nodes<-
      biopax$dt[grepl("vocabulary"
                      ,class
                      ,ignore.case = TRUE) &
                  property_value!=""][
                    ,.(label=paste(property_value
                                   ,collapse="\n")
                       ,type=class)
                    ,by=id]
    vocab_nodes$type<-
      vocab_nodes$type %>% 
      gsub("Vocabulary|Code"
           ,""
           ,.)
    
    return(vocab_nodes)
  }