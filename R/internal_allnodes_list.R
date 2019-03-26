internal_allnodes_list<-
  function(pw_biopax){
    #' @keywords internal
    
    nodes<-list()
    
    ####################################################################################################
    ######################## physical entities nodes
    nodes$physent<-
      suppressWarnings(RIGplotbiopax:::internal_nodes_physent(pw_biopax))
    
    ####################################################################################################
    ######################## dbid dbid nodes
    nodes$dbid_df<-
      suppressWarnings(RIGplotbiopax:::internal_nodes_dbid(pw_biopax))
    
    ####################################################################################################
    ######################## vocabulary nodes   
    nodes$vocab_df<-
      suppressWarnings(RIGplotbiopax:::internal_nodes_vocab(pw_biopax))
    
    ####################################################################################################
    ######################## left-right components node df
    nodes$lr_df<-
      suppressWarnings(RIGplotbiopax:::internal_nodes_lr(pw_biopax))
    
    return(nodes)
  }