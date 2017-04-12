make_plot_graph<-
    function(allnodes
             ,alledges
             ,to_html=TRUE
             ,to_svg=TRUE){  
        
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
        
        #replace quotation marks with backticks to avoid error
        allnodes$label<-
            allnodes$label %>% 
            gsub("'"
                 ,"`"
                 ,.)
        
        #create the graph
        #svg/html generation via diagrammer
        graph_diag<-
            create_graph(nodes_df = allnodes
                         ,edges_df = alledges) %>% 
            add_global_graph_attrs("overlap", "false", "graph") %>% 
            delete_global_graph_attrs("layout","graph") %>% 
            add_global_graph_attrs("layout","dot","graph") %>% 
            add_global_graph_attrs("tooltip",pw_to_plot_name,"graph") 
      
        
        if(to_html){
            #filenames
            filename_diag_html<-
                paste(Sys.Date()
                      ,pw_to_plot
                      ,"diagrammer.html"
                      ,sep="_")
            #render graph
            render<-
                render_graph(graph_diag)
            
            #save as html
            saveWidget(render,filename_diag_html)
        }
        if(to_svg){
            filename_diag_svg<-
                paste(Sys.Date()
                      ,pw_to_plot
                      ,"diagrammer.svg"
                      ,sep="_")
            #export
            export_graph(graph_diag
                         ,filename_diag_svg)
        }

    }