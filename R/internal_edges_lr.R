internal_edges_lr<-
  function(biopax){
    #' @keywords internal
    
    edges<-list()
    
    node_lr_classes<-
      biopax$dt[property %in% c("left"
                                ,"right"
                                ,"product")]$class %>% 
      unique
    
    edges$physent2lr_df<-
      biopax$dt[class %in% node_lr_classes
                & property=="left"
                ,.(from=property_attr_value
                   ,to=id
                   ,type="left")]
    edges$lr2physent_df<-
      biopax$dt[class %in% node_lr_classes
                & property %in% c("right"
                                  ,"product")
                ,.(from=id
                   ,to=property_attr_value
                   ,type="right")]
    
    alledges<-
      edges %>% 
      do.call(rbind.fill
              ,.) %>% 
      unique
    
    return(alledges)
  }