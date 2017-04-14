internal_merge_mult_node_refs<-
    function(nodes_edges_list
             ,ntype){
        allnodes<-
            nodes_edges_list[[1]]
        alledges<-
            nodes_edges_list[[2]]
        
        alledges$newto<-NA_character_
        alledges[type %in% ntype]$newto<-
            alledges[type %in% ntype
                     ,.(newto=paste0(from
                                     ,paste(substr(ntype,1,2)
                                            ,collapse="")))]$newto
        
        #make up a dict of what to replace with what
        merge_dict<-
            alledges[type %in% ntype
                     ,.(to
                        ,newto)]
        
        #and then replace those edges' from values
        alledges$from<-
            alledges$from %>% 
            mapvalues(from=merge_dict$to
                      ,to=merge_dict$newto) 
        
        alledges[!is.na(newto)]$to<-
            alledges[!is.na(newto)]$newto
        
        
        #replace allnodes id values...
        allnodes$id<-
            allnodes$id %>% 
            mapvalues(from=merge_dict$to
                      ,to=merge_dict$newto) 
        
        #...and merge their labels accordingly -- by id
        allnodes<-
            allnodes %>% 
            merge_cols_shorten_df(colKey = "id"
                                  ,colsToMerge = "label"
                                  ,patternToMerge = "\n"
                                  ,return_other_cols=TRUE)
        
        return(list(allnodes=unique(allnodes)
                    ,alledges=unique(alledges)))
    }

       