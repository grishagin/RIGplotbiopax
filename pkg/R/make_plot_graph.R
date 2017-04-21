make_plot_graph<-
    function(allnodes
             ,alledges
             ,pw_id=NULL
             ,pw_name="Pathway"){  
        
        #' @title 
        #' Make and Plot BioPAX Graph
        #' @description 
        #' Using dataframes of nodes and edges make and plot the graph.
        #' @param allnodes Dataframe with all nodes.
        #' @param alledges Dataframe with all edges.
        #' @param to_html Return html file? Has active tooltips.
        #' @param to_svg Return svg file?
        #' 
        #' @author 
        #' Ivan Grishagin
        
        ########################################################
        #replace quotation marks with backticks to avoid error
        allnodes$label<-
            allnodes$label %>% 
            gsub("'"
                 ,"`"
                 ,.)
        
        ########################################################
        #discover all nodes that link to dbid nodes
        dbid_ids<-
            allnodes[tooltip=="ID"]$id %>% 
            unique
        
        #find all nodes that refer to those dbid nodes
        to_dbid_df<-
            alledges[to %in% dbid_ids
                     ,.(physent_id=paste0(from
                                          ,"OLD")
                        ,dbid_label=allnodes[id==to]$label)
                     ,by=to]
        
        #remove dbid nodes from the allnodes and alledges
        allnodes<-
            allnodes[!id %in% dbid_ids]
        alledges<-
            alledges[!to %in% dbid_ids]
        
        ########################################################
        #since some nodes were removed, replace ids!
        #ensure that old ids have distinct names
        allnodes$id<-
            allnodes$id %>% 
            paste0("OLD")
        alledges$from<-
            alledges$from %>% 
            paste0("OLD")
        alledges$to<-
            alledges$to %>% 
            paste0("OLD")
        
        #make up a dictionary of replacements for nodes and edges
        #to ensure that nodes ids are the same as their row numbers
        node_id_dict<-
            data.frame(from=allnodes$id
                       ,to=1:nrow(allnodes))
        #replace char ids with int ids
        allnodes$id<-
            node_id_dict$to
        #replace edges ids
        alledges$id<-
            1:nrow(alledges)
        
        #now replace all edges references with integers, same as with nodes
        alledges$from<-
            alledges$from %>% 
            mapvalues(from=node_id_dict$from
                      ,to=node_id_dict$to) %>% 
            as.integer
        alledges$to<-
            alledges$to %>% 
            mapvalues(from=node_id_dict$from
                      ,to=node_id_dict$to) %>% 
            as.integer
        
        #replace all nodes referring to former dbid nodes
        to_dbid_df$physent_id<-
            to_dbid_df$physent_id %>% 
            mapvalues(from=node_id_dict$from
                      ,to=node_id_dict$to) %>% 
            as.integer
        
        #prepare the final replacement dictionary 
        #to alter the svg string
        #to add attributes to xml tags
        to_dbid_df<-
            to_dbid_df[,.(from_tag=paste0("<g id=\"a_node"
                                          ,physent_id
                                          ,"\"")
                          ,to_tag=paste0("<g id=\"a_node"
                                         ,physent_id
                                         ,"\" class=\"clickable\" data=\""
                                         ,dbid_label
                                         ,"\""))]
        
        ########################################################
        #generate svg code via dot
        graph_svg<-
            #first, make diagrammer graph
            create_graph(nodes_df = allnodes
                         ,edges_df = alledges) %>% 
            add_global_graph_attrs("overlap", "false", "graph") %>% 
            delete_global_graph_attrs("layout","graph") %>% 
            add_global_graph_attrs("layout","dot","graph") %>% 
            add_global_graph_attrs("tooltip"
                                   ,pw_name
                                   ,"graph") %>% 
            #generate dot, then grviz code, and then svg string
            generate_dot %>% 
            grViz %>% 
            export_svg %>% 
            #replace selected node tags with their modified versions
            mgsub(pattern=to_dbid_df$from_tag
                  ,replacement=to_dbid_df$to_tag
                  ,text.var = .
                  ,trim=FALSE)
        
        
        #save the file
        filename<-
            paste0(Sys.Date()
                   ,"_"
                   ,pw_id
                   ,".svg")
        
        writeLines(graph_svg
                   ,filename
                   ,useBytes = TRUE)

    }