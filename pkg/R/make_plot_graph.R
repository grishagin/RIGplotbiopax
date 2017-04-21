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
        
        #find all nodesthat refer to those id nodes
        #find their tags inthe svg string
        #and modify those tags by adding attributes to them 
        to_dbid_df<-
            alledges[to %in% dbid_ids
                     ,.(physent_id=from
                        ,from_tag=paste0("<g id=\"a_node"
                                         ,from
                                         ,"\"")
                        ,to_tag=paste0("<g id=\"a_node"
                                       ,from
                                       ,"\" class=\"clickable\" data=\""
                                       ,allnodes[id==to]$label
                                       ,"\""))
                     ,by=to]
        
        #remove dbid nodes from the allnodes and alledges
        allnodes<-
            allnodes[!id %in% dbid_ids]
        alledges<-
            alledges[!to %in% dbid_ids]
        
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