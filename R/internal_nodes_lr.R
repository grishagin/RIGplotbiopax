internal_nodes_lr<-
  function(biopax){
    #' @keywords internal
    
    node_lr_classes<-
      biopax$dt[property %in% c("left"
                                ,"right"
                                ,"product")]$class %>% 
      unique
    
    lr_nodes<-
      biopax$dt[class %in% node_lr_classes
                ,.(label=""
                   #rather not have labels -- for clarity
                   #label=paste(property_value[property_value!="" & property!="spontaneous"],collapse="|")
                   ,spontaneous=property_value[property=="spontaneous"]
                   ,type=class)
                ,by=id] %>% 
      unique
    return(lr_nodes)
  }