internal_split_mult_node_refs<-
    function(nodes_edges_list
             ,ntype){
        
        allnodes<-
            nodes_edges_list[[1]]
        alledges<-
            nodes_edges_list[[2]]
       #find all "to" nodes of the desired type
        #make individual ids for them
        # alledges[type %in% ntype
        #          ,newto := paste0(paste(substr(ntype,1,2)
        #                                 ,collapse="")
        #                           ,1:length(to))]
        alledges$newto<-NA_character_
        alledges[type %in% ntype]$newto<-
            alledges[type %in% ntype
                 ,.(newto=paste0(paste(substr(ntype,1,2)
                                        ,collapse="")
                                  ,1:length(to)))]$newto
        #and a dictionary
        #also merge the dict by the old id
        #such that there are no repetitions
        replace_dict<-
            alledges[type %in% ntype
                     ,.(to
                        ,newto)] %>% 
            merge_cols_shorten_df(colKey="to"
                                  ,patternToMerge=";")

        #replace "to" ids and remove auxiliary column
        # alledges[!is.na(newto)
        #          ,to := newto]
        alledges[!is.na(newto)]$to<-
            alledges[!is.na(newto)]$newto
        
        alledges$newto<-NULL
        
        #replace the "from" nodes' ids
        alledges$from<-
            alledges$from %>% 
            mapvalues(from=replace_dict$to
                      ,to=replace_dict$newto)
        
        print("boo")

        #now split up the newly replaced from ids
        alledges<-
            alledges %>% 
            split_cols_lengthen_df(colsToSplit = "from"
                                   ,patternToSplit = ";"
                                   ,at_once = FALSE) %>% 
            as.data.table
        
        
        #do the same thing to nodes df
        allnodes$id<-
            allnodes$id %>% 
            mapvalues(from=replace_dict$to
                      ,to=replace_dict$newto)
        
        allnodes<-
            allnodes %>% 
            split_cols_lengthen_df(colsToSplit = "id"
                                   ,patternToSplit = ";") %>% 
            as.data.table

        return(list(allnodes=allnodes
                    ,alledges=alledges))
    }
