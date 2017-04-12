make_allnodes_alledges<-
    function(nodes_list
             ,edges_list
             ,exclude_ids=NULL){
        
        #' @title 
        #' Make Dataframe with All Nodes and All Edges
        #' @description 
        #' Make fully annotated dataframes with all nodes and all edges required to make a graph.
        #' @param nodes_list List containing dataframes with all nodes.
        #' @param edges_list List containing dataframes with all edges.
        #' @param exclude_ids Component ids to exclude from graph.
        #' 
        #' @author 
        #' Ivan Grishagin

        #combine all nodes list items
        allnodes<-
            nodes_list %>% 
            do.call(rbind.fill
                    ,.) %>% 
            unique %>% 
            #exclude unwanted ids
            filter(!id %in% exclude_ids) %>% 
            as.data.table
        
        #combine all edges list items
        #also take only those edges, that are in the nodes df
        alledges<-
            edges_list %>% 
            do.call(rbind.fill
                    ,.) %>% 
            as.data.table %>% 
            .[!is.na(from) | 
                  !is.na(to)] %>% 
            .[from %in% allnodes$id & 
                  to %in% allnodes$id] %>% 
            unique
        
        #ensure that all vocabulary nodes (evidence and such) have unique ids
        #to avoid edge mishmash when multiple nodes connect to the same vocabulary node
        old2new_vocab<-
            alledges[to %in% nodes_list$vocab_df$id
                     ,.(old_to=to
                        ,new_to=paste0(from,to))]
        
        #replace vocabulary ids
        allnodes$id<-
            allnodes$id %>% 
            mapvalues(from=old2new_vocab$old_to
                      ,to=old2new_vocab$new_to)
        
        #replace vocabulary ids
        alledges$from<-
            alledges$from %>% 
            mapvalues(from=old2new_vocab$old_to
                      ,to=old2new_vocab$new_to)
        alledges$to<-
            alledges$to %>% 
            mapvalues(from=old2new_vocab$old_to
                      ,to=old2new_vocab$new_to)
        
        #make up a dictionary of replacements for nodes and edges
        #because nodes HAVE to be referred to by an INTEGER
        node_char2int<-
            data.frame(from=allnodes$id
                       ,to=1:nrow(allnodes))
        #replace char ids with int ids
        allnodes$id<-
            1:nrow(allnodes)
        
        
        #add edges ids
        alledges$id<-
            1:nrow(alledges)
        #now replace all edges references with integers, same as with nodes
        alledges$from<-
            alledges$from %>% 
            mapvalues(from=node_char2int$from
                      ,to=node_char2int$to) %>% 
            as.integer
        
        alledges$to<-
            alledges$to %>% 
            mapvalues(from=node_char2int$from
                      ,to=node_char2int$to) %>% 
            as.integer
        
        ################add formatting to nodes and edges
        #node and edge properties
        node_props<-
            internal_props_node()
        edge_props<-
            internal_props_edge()
        
        #format nodes
        allnodes<-
            node_props[match(allnodes$type
                             ,node_props$type),] %>% 
            dplyr::select(-type) %>% 
            cbind(allnodes
                  ,.) %>% 
            as.data.table
        #replace peripheries for spontaneous nodes
        allnodes[spontaneous==TRUE]$peripheries<-2
        
        #remove auxiliary columns
        allnodes$type<-NULL
        allnodes$spontaneous<-NULL
        
        #format edges
        alledges<-
            edge_props[match(alledges$type
                             ,edge_props$type),] %>% 
            dplyr::select(-type) %>% 
            cbind(alledges
                  ,.)
        
        #remove auxiliary columns
        alledges$type<-NULL

        #add minimum length for all edges
        alledges$minlen<-1
        
        return(list(allnodes=allnodes
                    ,alledges=alledges))
    }