internal_nodes_lr<-
    function(biopax){
        node_lr_classes<-
            biopax$dt[property %in% c("left"
                                      ,"right"
                                      ,"product")]$class %>% 
            unique
        
        lr_nodes<-
            biopax$dt[class %in% node_lr_classes
                      ,.(label=paste(property_value[property_value!=""
                                                    & property!="spontaneous"]
                                     ,collapse="|")
                         ,spontaneous=property_value[property=="spontaneous"]
                         ,type=class)
                      ,by=id] %>% 
            unique
        return(lr_nodes)
    }