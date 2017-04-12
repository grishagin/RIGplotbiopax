internal_edges_dbid<-
    function(biopax){
        dbid_ids<-
            biopax$dt[property %in% c("id","db")]$id %>% 
            unique
        
        #all entities to dbid edges
        all2dbid_edges<-
            biopax$dt[property_attr_value %in% dbid_ids
                      ,.(from=id
                         ,to=property_attr_value
                         ,class
                         ,type="dbid")]
        
        #for all those edges that have a class with a word "reference" in it
        #we need to find instances that refer to them, and replace those "from" ids
        #i.e. get the id of the object referring to the xref, i.e. the actual physical entity
        #get those rows as a logical vector
        refs_logi<-
            grepl("reference"
                  ,all2dbid_edges$class
                  ,ignore.case = TRUE)
        
        #get all those ids and combine them using ; as separator
        all2dbid_edges[refs_logi]$from<-
            all2dbid_edges[refs_logi]$from %>% 
            lapply(FUN=function(pid){
                getReferencingIDs(biopax=biopax
                                  ,id=pid
                                  ,recursive=FALSE)
            }) %>% 
            sapply(paste,collapse=";")
        
        #remove auxiliary class column
        all2dbid_edges$class<-NULL
        
        #lengthen df by splitting those combined ids
        all2dbid_edges<-
            all2dbid_edges %>% 
            split_cols_lengthen_df(colsToSplit = "from"
                                   ,patternToSplit = ";")
        
        return(all2dbid_edges)
    }