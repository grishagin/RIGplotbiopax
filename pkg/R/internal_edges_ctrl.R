internal_edges_ctrl<-
    function(biopax){
        
        edges<-list()
        
        ######################## controls df
        node_control_classes<-
            biopax$dt[property %in% c("controller"
                                      ,"controlled"
                                      ,"controlType")]$class %>% 
            unique
        
        #get df of controller and controlled components
        controls_df<-
            biopax$dt[class %in% node_control_classes
                      ,.(cer=property_attr_value[property=="controller"]
                         ,ced=property_attr_value[property=="controlled"]
                         ,ct=property_value[property=="controlType"]
                         ,catdir=property_value[property=="catalysisDirection"]
                         ,xref=property_attr_value[property=="xref"]
                         ,evidence=property_attr_value[property=="evidence"]
                      )
                      ,by=id]
        
        if(nrow(controls_df)<1){
            return(NULL)
        }
        
        #find the ids that the evidence ids are referring to, and merge them using ; as separator
        #the reason's that vocabulary function will refer evidence ids to the control components
        #whereas we are excluding them from being physical nodes
        controls_df$evidence<-
            controls_df$evidence %>% 
            lapply(FUN=function(pid){
                getReferencedIDs(biopax=biopax
                                 ,id=pid
                                 ,recursive=FALSE)
            }) %>% 
            sapply(paste,collapse=";")
        
        #lengthen df by splitting those combined ids (likely not needed but just in case)
        controls_df<-
            controls_df %>% 
            split_cols_lengthen_df(colsToSplit = "evidence"
                                   ,patternToSplit = ";"
                                   ,at_once = FALSE)
        
        
           
        #split up catalysis direction by dash
        controls_df<-
            controls_df %>% 
            split_col_widen_df(colToSplit = "ct"
                               ,split = "-"
                               ,newcolnames = c("type"
                                                ,"label")) 
        
        controls_df[is.na(type)]$type<-
            "UNKNOWNCTRL"
        
        # controllers to controlled edges
        edges$cer2ced_df<-
            controls_df[!is.na(cer) &
                            !is.na(ced)][,.(from=cer
                                            ,to=ced
                                            ,type
                                            ,label)]
        
        # controlleds to xref edges
        edges$ced2dbid_df<-
            controls_df[!is.na(xref) &
                            !is.na(ced)][,.(from=ced
                                            ,to=xref
                                            ,type="dbid")]
        # controllers to xref edges
        # for when the controlleds are NA
        edges$cer2dbid_df<-
            controls_df[!is.na(xref) &
                            !is.na(cer) &
                            is.na(ced)][,.(from=cer
                                            ,to=xref
                                            ,type="dbid")]
        
        # controlleds to evidence edges
        edges$ced2ev_df<-
            controls_df[!is.na(evidence) &
                            !is.na(ced)][,.(from=ced
                                            ,to=evidence
                                            ,type="Evidence")] 
        # controllers to evidence edges
        # for when the controlleds are NA
        edges$cer2ev_df<-
            controls_df[!is.na(evidence) &
                            !is.na(cer) &
                            is.na(ced)][,.(from=cer
                                            ,to=evidence
                                            ,type="Evidence")]
        
        alledges<-
            edges %>% 
            do.call(rbind.fill
                    ,.) %>% 
            unique
        
        return(alledges)
    }