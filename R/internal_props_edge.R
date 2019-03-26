internal_props_edge<-
  function(){
    #' @keywords internal
    
    edge_props<-
      data.frame(rel=
                   c(ACTIVATION="Activation"
                     ,INHIBITION="Inhibition"
                     ,UNKNOWNCTRL="Control"
                     ,cplx="Component"
                     ,left="Left"
                     ,right="Right"
                     ,dbid="ID"
                     ,Pathway="Pathway"
                     
                     ,CellularLocation="Cellular Location"
                     ,SequenceModification="Sequence Modification"
                     ,Evidence="Evidence"
                     ,Interaction="Interaction"
                     ,RelationshipType="Relationship Type")
                 ,color=
                   c(ACTIVATION="darkgreen"
                     ,INHIBITION="red3"
                     ,UNKNOWNCTRL="gray40"
                     ,cplx="gray40"
                     ,left="gray40"
                     ,right="gray40"
                     ,dbid="gray40"
                     ,Pathway="gray40"
                     
                     ,CellularLocation="green4"
                     ,SequenceModification="green4"
                     ,Evidence="green4"
                     ,Interaction="green4"
                     ,RelationshipType="green4")
                 ,penwidth=
                   c(ACTIVATION=4
                     ,INHIBITION=4
                     ,UNKNOWNCTRL=4
                     ,cplx=1
                     ,left=2
                     ,right=2
                     ,dbid=1
                     ,Pathway=2
                     
                     ,CellularLocation=1
                     ,SequenceModification=1
                     ,Evidence=1
                     ,Interaction=1
                     ,RelationshipType=1)
                 ,tooltip=
                   c(ACTIVATION="Activation"
                     ,INHIBITION="Inhibition"
                     ,UNKNOWNCTRL="Control"
                     ,cplx="Component"
                     ,left="Left"
                     ,right="Right"
                     ,dbid="ID"
                     ,Pathway="Pathway"
                     
                     ,CellularLocation="Cellular Location"
                     ,SequenceModification="Sequence Modification"
                     ,Evidence="Evidence"
                     ,Interaction="Interaction"
                     ,RelationshipType="Relationship Type")
                 ,arrowhead=
                   c(ACTIVATION="normal"
                     ,INHIBITION="tee"
                     ,UNKNOWNCTRL="diamond"
                     ,cplx="odot"
                     ,left="normal"
                     ,right="normal"
                     ,dbid="none"
                     ,Pathway="odot"
                     
                     ,CellularLocation="invempty"
                     ,SequenceModification="invempty"
                     ,Evidence="invempty"
                     ,Interaction="invempty"
                     ,RelationshipType="invempty")
                 ,arrowsize=
                   c(ACTIVATION=1.5
                     ,INHIBITION=1.5
                     ,UNKNOWNCTRL=1.5
                     ,cplx=1
                     ,left=1
                     ,right=1
                     ,dbid=1
                     ,Pathway=1
                     
                     ,CellularLocation=1
                     ,SequenceModification=1
                     ,Evidence=1
                     ,Interaction=1
                     ,RelationshipType=1)
                 ,style=
                   c(ACTIVATION="solid"
                     ,INHIBITION="solid"
                     ,UNKNOWNCTRL="solid"
                     ,cplx="solid"
                     ,left="solid"
                     ,right="solid"
                     ,dbid="dashed"
                     ,Pathway="dashed"
                     
                     ,CellularLocation="dashed"
                     ,SequenceModification="dashed"
                     ,Evidence="dashed"
                     ,Interaction="dashed"
                     ,RelationshipType="dashed")
      )
    edge_props<-
      edge_props %>% 
      cbind(type=rownames(.)
            ,.)
    return(edge_props)
  }