make_allnodes_alledges<-
  function(pw_biopax
           ,exclude_ids=NULL
           ,verbose=FALSE){
    #' @export
    #' @import data.table
    #' @title 
    #' Make Dataframe with All Nodes and All Edges
    #' @description 
    #' Make fully annotated dataframes with all nodes and all edges required to make a graph.
    #' @param pw_biopax Pathway as a BioPAX object.
    #' @param exclude_ids Component ids to exclude from graph.
    #' @param verbose Boolean. Show all warnings?
    #' 
    #' @author 
    #' Ivan Grishagin
    
    #prepare raw lists of nodes and edges
    nodes_list<-
      pw_biopax %>% 
      internal_allnodes_list
    edges_list<-
      pw_biopax %>% 
      internal_alledges_list
    
    #combine all nodes list items
    allnodes<-
      nodes_list %>% 
      do.call(rbind.fill
              ,.) %>% 
      #exclude unwanted ids
      filter(!id %in% exclude_ids) %>% 
      unique
    
    #get all dbid/vocab node ids
    dbid_vocab_ids<-
      allnodes$id[allnodes$type %in% c("dbid"
                                       ,"CellularLocation"
                                       ,"SequenceModification"
                                       ,"Evidence"
                                       ,"Interaction"
                                       ,"RelationshipType")] %>% 
      unique
    
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
      #also exclude orphan vocab/dbid pairs (not referred to by anything)
      .[!from %in% dbid_vocab_ids[!dbid_vocab_ids %in% to]]
    unique
    
    #also exclude other unwanted ids
    allnodes<-
      allnodes %>% 
      #do not take into account lone dbid/vocabulary nodes (i.e. taxonomy)
      #i.e. dbid nodes not mentioned among edges at all
      filter(!((!id %in% c(alledges$from
                           ,alledges$to)) &
                 id %in% dbid_vocab_ids)) %>% 
      as.data.table
    
    ##################################################################################
    #ensure that all vocabulary nodes (evidence and such) have unique ids
    #to avoid edge mishmash when multiple nodes connect to the same vocabulary node
    #also, unite multiple vocabulary/dbid nodes, which are referred by the same entity node,
    #into neat lists
    nodes_edges_list<-
      RIGplotbiopax:::internal_split_mult_node_refs(list(allnodes
                                                         ,alledges)
                                                    ,ntype=unique(nodes_list$vocab_df$type)
                                                    ,verbose=verbose) %>% 
      RIGplotbiopax:::internal_split_mult_node_refs(ntype="dbid"
                                                    ,verbose=verbose) %>% 
      RIGplotbiopax:::internal_merge_mult_node_refs(ntype=unique(nodes_list$vocab_df$type)
                                                    ,verbose=verbose
                                                    ,sep="\n") %>% 
      RIGplotbiopax:::internal_merge_mult_node_refs(ntype="dbid"
                                                    ,verbose=verbose
                                                    ,sep="|") 
    
    allnodes<-
      nodes_edges_list$allnodes
    alledges<-
      nodes_edges_list$alledges
    
    ##################################################################################
    
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
                ,to=node_char2int$to
                ,warn_missing = verbose) %>% 
      as.integer
    
    alledges$to<-
      alledges$to %>% 
      mapvalues(from=node_char2int$from
                ,to=node_char2int$to
                ,warn_missing = verbose) %>% 
      as.integer
    
    ##################################################################################
    ################ add formatting to nodes and edges
    #node and edge properties
    node_props<-
      RIGplotbiopax:::internal_props_node()
    edge_props<-
      RIGplotbiopax:::internal_props_edge()
    
    #format nodes
    allnodes<-
      node_props[match(allnodes$type
                       ,node_props$type),] %>% 
      dplyr::select(-type) %>% 
      cbind(allnodes
            ,.) %>% 
      as.data.table
    #replace peripheries for spontaneous nodes
    allnodes[spontaneous=="TRUE"]$peripheries<-2
    
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