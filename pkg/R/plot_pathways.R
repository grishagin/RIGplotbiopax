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
        #strip hashes from the biopax property attr value column
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
            #and if there are any sub-pathways from inxight universe,
            #exclude references to all of their components to simplify things
            pw_biopax<-
                extract_pathways_from_biopax(biopax = biopax
                                             ,pw_ids = pw_to_plot
                                             ,exclude_subpw_comp_patt="inxight_pathways")
                
            if(is.null(pw_biopax)){
                next
            }else if(nrow(pw_biopax$dt)>10000){
                #split the pathway up roughly in chuncks of 10000 instances
                #will end up having more 
                #as some instances will be duplicated in each chunck
                Nchunks<-
                    ceiling(nrow(pw_biopax$dt)/10000)
                    
                pw_biopax_list<-
                    pw_biopax %>% 
                    split_pathway_into_chunks(pw_id=pw_to_plot
                                              ,Nchunks=Nchunks)
            }else{
                pw_biopax_list<-
                    list(pw_biopax)
            }
            
            #clean-up to safely reuse the variable
            rm(pw_biopax)
            chunck_num<-0
            
            for(pw_biopax in pw_biopax_list){
                #get pathway name
                pw_name<-
                    pw_biopax$dt[id==pw_to_plot & 
                                     grepl("name",tolower(property))][
                                         order(-nchar(property_value))]$property_value[1]
                if(length(pw_biopax_list)>1){
                    #count which part is being processed
                    chunck_num<-
                        chunck_num+1
                    #make up the part name
                    chunck_tag<-
                        paste0("PART"
                               ,chunck_num
                               ,"of"
                               ,length(pw_biopax_list))
                    
                    #modify the pathway name (if more than one part present)
                    #and inform the user
                    pw_name<-
                        paste(chunck_tag)
                    
                    message("\tProcessing "
                            ,chunck_tag
                            ,"...")
                    }
                ####################################################################################################
                ######################## prepare all nodes and edges do the plotting
                if(verbose){
                    allnodes_alledges<-
                        make_allnodes_alledges(pw_biopax
                                               ,exclude_ids=pw_to_plot
                                               ,verbose=verbose)
                    
                        try(make_plot_graph(allnodes=allnodes_alledges$allnodes
                                            ,alledges=allnodes_alledges$alledges
                                            ,pw_name=pw_name
                                            ,tag=paste0(pw_to_plot
                                                        ,tag
                                                        ,chunck_tag
                                                        ,sep="_")))
                } else {
                    suppressWarnings(
                        allnodes_alledges<-
                            make_allnodes_alledges(pw_biopax
                                                   ,exclude_ids=pw_to_plot
                                                   ,verbose=verbose))
                    
                    suppressWarnings(
                        try(make_plot_graph(allnodes=allnodes_alledges$allnodes
                                            ,alledges=allnodes_alledges$alledges
                                            ,pw_name=pw_name
                                            ,tag=paste(pw_to_plot
                                                       ,tag
                                                       ,chunck_tag
                                                       ,sep="_"))))
                }
            }
           
        }
       
        return(NULL)
    }