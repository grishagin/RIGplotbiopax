plot_pathways<-
    function(biopax
             ,pw_ids
             ,tag=NULL
             ,verbose=FALSE){
        
        #' @title 
        #' Plot Pathways
        #' @description 
        #' Takes in a BioPAX object and pathway ids, and prepares plots in an SVG format.
        #' @param biopax A BioPAX object.
        #' @param pw_ids IDs of pathways to plot.
        #' @param tag Additional tag for the file name (optional).
        #' @param verbose Boolean. Show all warnings?
        #' 
        #' @author 
        #' Ivan Grishagin
        
        ####################################################################################################
        ######################## prepare biopax
        #fix all biopax inconsistencies etc.
        biopax$dt$property_attr_value<-
            biopax$dt$property_attr_value %>% 
            striphash
        
        ####################################################################################################
        #for each pathway, repeat
        for(pw_to_plot in pw_ids){
            message("Plotting pathway "
                    ,pw_to_plot
                    ,"...")
            #extract biopax instances just for a given pathway
            pw_biopax<-
                extract_pathways_from_biopax(biopax = biopax
                                             ,pw_ids = pw_to_plot)
            if(is.null(pw_biopax)){
                next
            }
            
            #get pathway name
            pw_to_plot_name<-
                pw_biopax$dt[pw_biopax$dt$id==pw_to_plot & 
                                 grepl("name",tolower(pw_biopax$dt$property))][
                                     order(-nchar(pw_biopax$dt$property_value))]$property_value[1]

            nodes<-list()
            edges<-list()
            
            ####################################################################################################
            ######################## physical entities nodes
            nodes$physent<-
                suppressWarnings(internal_nodes_physent(pw_biopax))
            
            ####################################################################################################
            ######################## dbid node and edges df
            #dbid nodes
            nodes$dbid_df<-
                suppressWarnings(internal_nodes_dbid(pw_biopax))
            #all entities to dbid edges
            edges$all2dbid_df<-
                suppressWarnings(internal_edges_dbid(pw_biopax))
            
            ####################################################################################################
            #vocabulary nodes   
            nodes$vocab_df<-
                suppressWarnings(internal_nodes_vocab(pw_biopax))
            
            #physical entities to vocabulary edges
            edges$all2vocab_df<-
                suppressWarnings(internal_edges_vocab(pw_biopax))
            
            ####################################################################################################
            ######################## complex components edge df
            #complex components edge dfs
            edges$cplx_df<-
                suppressWarnings(internal_edges_cplx(pw_biopax))
            
            ####################################################################################################
            ######################## pathway components edge df
            #complex components edge dfs
            edges$pw2component_df<-
                suppressWarnings(internal_edges_pw(pw_biopax))
            
            ####################################################################################################
            ######################## left-right components node df
            nodes$lr_df<-
                suppressWarnings(internal_nodes_lr(pw_biopax))
            
            #left-right components edge dfs
            edges$lr_df<-
                suppressWarnings(internal_edges_lr(pw_biopax))
            
            ####################################################################################################
            ######################## control edges df
            #control components edge dfs
            edges$ctrl_df<-
                suppressWarnings(internal_edges_ctrl(pw_biopax))
            ####################################################################################################
            ######################## prepare all nodes and edges do the plotting
            if(verbose){
                allnodes_alledges<-
                    make_allnodes_alledges(nodes_list=nodes
                                           ,edges_list=edges
                                           ,exclude_ids=pw_to_plot
                                           ,verbose=verbose)

                make_plot_graph(allnodes=allnodes_alledges$allnodes
                                ,alledges=allnodes_alledges$alledges
                                ,pw_name=pw_to_plot_name
                                ,tag=paste0(pw_to_plot
                                            ,tag))
            } else {
                suppressWarnings(
                    allnodes_alledges<-
                        make_allnodes_alledges(nodes_list=nodes
                                               ,edges_list=edges
                                               ,exclude_ids=pw_to_plot
                                               ,verbose=verbose))
                
                suppressWarnings(
                    make_plot_graph(allnodes=allnodes_alledges$allnodes
                                    ,alledges=allnodes_alledges$alledges
                                    ,pw_name=pw_to_plot_name
                                    ,tag=paste0(pw_to_plot
                                                ,tag)))
            }
        }
       
        return(NULL)
    }