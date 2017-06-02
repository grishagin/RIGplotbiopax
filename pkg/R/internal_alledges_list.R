internal_alledges_list<-
    function(pw_biopax){

        edges<-list()
        
        ####################################################################################################
        ######################## all entities to dbid edges
        edges$all2dbid_df<-
            suppressWarnings(RIGplotbiopax:::internal_edges_dbid(pw_biopax))
        
        ####################################################################################################
        ######################## physical entities to vocabulary edges
        edges$all2vocab_df<-
            suppressWarnings(RIGplotbiopax:::internal_edges_vocab(pw_biopax))
        
        ####################################################################################################
        ######################## complex components edge df
        edges$cplx_df<-
            suppressWarnings(RIGplotbiopax:::internal_edges_cplx(pw_biopax))
        
        ####################################################################################################
        ######################## pathway components edge df
        edges$pw2component_df<-
            suppressWarnings(RIGplotbiopax:::internal_edges_pw(pw_biopax))
        
        ####################################################################################################
        ######################## left-right components edge dfs
        edges$lr_df<-
            suppressWarnings(RIGplotbiopax:::internal_edges_lr(pw_biopax))
        
        ####################################################################################################
        ######################## control components edge dfs
        edges$ctrl_df<-
            suppressWarnings(RIGplotbiopax:::internal_edges_ctrl(pw_biopax))
        
        return(edges)
    }