internal_nodes_physent<-
    function(biopax){
        ######################## physical entities and node df
        node_physent_classes<-
            c("Pathway"
              ,"Protein"
              ,"Complex"
              ,"SmallMolecule"
              ,"PhysicalEntity"
              ,"RNA"
              ,"DNA"
            )
        
        #physical entity nodes with names
        physent_nodes<-
            biopax$dt[class %in% node_physent_classes &
                          property_value!=""
                      ,.(label=paste(property_value
                                     ,collapse="\n")
                         ,type=class)
                      ,by=id] %>% 
            unique
        
        return(physent_nodes)
    }