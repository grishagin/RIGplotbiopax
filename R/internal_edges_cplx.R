internal_edges_cplx<-
  function(biopax){
    #' @keywords internal
    
    cplx_edges<-
      biopax$dt[property=="component"
                ,.(from=property_attr_value
                   ,to=id
                   ,type="cplx")]
    return(cplx_edges)
  }