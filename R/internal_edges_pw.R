internal_edges_pw<-
  function(biopax){
    #' @keywords internal
    
    pw2component_edges<-
      biopax$dt[property=="pathwayComponent"
                ,.(from=id
                   ,to=property_attr_value
                   ,type="Pathway")]
    return(pw2component_edges)
  }