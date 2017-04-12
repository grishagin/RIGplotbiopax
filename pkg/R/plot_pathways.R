plot_pathways<-
    function(biopax
             ,pw_ids
             ,...){
        
        #' @title 
        #' Plot Pathways
        #' @description 
        #' Takes in a BioPAX object and pathway ids, and  .
        #' @param biopax A BioPAX object.
        #' @param pw_ids IDs of pathways to plot.
        #' @param ... Optional \code{to_html} and \code{to_svg} parameters passed to \code{make_plot_graph}.
        #' 
        #' @author 
        #' Ivan Grishagin
        
        #check for and assign add-l parameters
        dots<-
            list(...)

        if(!is.null(dots$to_html)){
            to_html<-
                dots$to_html
        } else {
            to_html<-TRUE
        }
        if(!is.null(dots$to_svg)){
            to_svg<-
                dots$to_svg
        } else {
            to_svg<-TRUE
        }
        
        ####################################################################################################
        ######################## prepare biopax
        #fix classes' inconsistencies
        biopax$dt[class=="Rna"]$class<-
            "RNA"
        biopax$dt[class=="RnaReference"]$class<-
            "RNAReference"
        biopax$dt[class=="Dna"]$class<-
            "DNA"
        biopax$dt[class=="DnaReference"]$class<-
            "DNAReference"
        
        #remove hash signs from property attr value column (if any)
        biopax$dt$property_attr_value<-
            biopax$dt$property_attr_value %>% 
            striphash
        
        ####################################################################################################
        #for each pathway, repeat
        for(pw_to_plot in pw_ids){
            #extract biopax instances just for a given pathway
            pw_biopax<-
                extract_pathways_from_biopax(biopax = biopax
                                             ,pw_ids = pw_to_plot)
            if(is.null(pw_biopax)){
                next
            }
            #get pathway name
            pw_to_plot_name<-
                pw_biopax$dt[id==pw_to_plot & 
                                 grepl("name",tolower(property))][
                                     order(-nchar(property_value))]$property_value[1]
            
            nodes<-list()
            edges<-list()
            
            ####################################################################################################
            ######################## physical entities nodes
            nodes$physent<-
                internal_nodes_physent(pw_biopax)
            
            ####################################################################################################
            ######################## dbid node and edges df
            #dbid nodes
            nodes$dbid_df<-
                internal_nodes_dbid(pw_biopax)
            #all entities to dbid edges
            edges$all2dbid_df<-
                internal_edges_dbid(pw_biopax)
            
            ####################################################################################################
            #vocabulary nodes   
            nodes$vocab_df<-
                internal_nodes_vocab(pw_biopax)
            
            #physical entities to vocabulary edges
            edges$all2vocab_df<-
                internal_edges_vocab(pw_biopax)
            
            ####################################################################################################
            ######################## complex components edge df
            #complex components edge dfs
            edges$cplx_df<-
                internal_edges_cplx(pw_biopax)
            
            ####################################################################################################
            ######################## pathway components edge df
            #complex components edge dfs
            edges$pw2component_df<-
                internal_edges_pw(pw_biopax)
            
            ####################################################################################################
            ######################## left-right components node df
            nodes$lr_df<-
                internal_nodes_lr(pw_biopax)
            
            #left-right components edge dfs
            edges$lr_df<-
                internal_edges_lr(pw_biopax)
            
            ####################################################################################################
            ######################## control edges df
            #control components edge dfs
            edges$ctrl_df<-
                internal_edges_ctrl(pw_biopax)
            
            ####################################################################################################
            ######################## all nodes and edges 
            allnodes_alledges<-
                make_allnodes_alledges(nodes_list=nodes
                                       ,edges_list=edges
                                       ,exclude_ids=pw_to_plot)
            
            ####################################################################################################
            ######################## do the plotting
            make_plot_graph(allnodes=allnodes_alledges$allnodes
                            ,alledges=allnodes_alledges$alledges
                            ,pw_name=pw_to_plot_name
                            ,pw_id=pw_to_plot
                            ,to_html=to_html
                            ,to_svg=to_svg)
        }
       
        return(NULL)
    }