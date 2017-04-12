internal_nodes_dbid<-
    function(biopax){
        dbid_nodes<-
            biopax$dt[property %in% c("id","db")][
                order(id
                      ,property)][
                          ,
                          .(label=paste(property_value
                                        ,collapse=":::")
                            ,type="dbid")
                          ,by=id]
        return(dbid_nodes)
    }