internal_props_node<-
    function(){
        node_props<-
            data.frame(shape=
                           c(Protein="rectangle"
                             ,Complex="rectangle"
                             ,SmallMolecule="rectangle"
                             ,PhysicalEntity="rectangle"
                             ,RNA="rectangle"
                             ,DNA="rectangle"
                             ,Pathway="oval"
                             
                             ,BiochemicalReaction="circle"
                             ,TemplateReaction="circle"
                             ,ComplexAssembly="circle"
                             ,Transport="circle"
                             ,TransportWithBiochemicalReaction="circle"
                             ,Degradation="circle"
                             
                             ,dbid="rectangle"
                             
                             ,CellularLocation="component"
                             ,SequenceModification="component"
                             ,Evidence="component"
                             ,Interaction="component"
                             ,RelationshipType="component")
                       ,fillcolor=
                           c(Protein="deepskyblue1"
                             ,Complex="mediumpurple3"
                             ,SmallMolecule="gold1"
                             ,PhysicalEntity="lightgray"
                             ,RNA="salmon1"
                             ,DNA="mediumorchid1"
                             ,Pathway="blue1"

                             ,BiochemicalReaction="orange1"
                             ,TemplateReaction="firebrick1"
                             ,ComplexAssembly="steelblue3"
                             ,Transport="limegreen"
                             ,TransportWithBiochemicalReaction="orange1"
                             ,Degradation="darkgray"
                             
                             ,dbid="white"
                             
                             ,CellularLocation="yellowgreen"
                             ,SequenceModification="yellowgreen"
                             ,Evidence="yellowgreen"
                             ,Interaction="yellowgreen"
                             ,RelationshipType="yellowgreen")
                       ,color=
                           c(Protein="deepskyblue1"
                             ,Complex="mediumpurple3"
                             ,SmallMolecule="gold1"
                             ,PhysicalEntity="lightgray"
                             ,RNA="salmon1"
                             ,DNA="mediumorchid1"
                             ,Pathway="blue1"
                             
                             ,BiochemicalReaction="orange1"
                             ,TemplateReaction="firebrick1"
                             ,ComplexAssembly="steelblue3"
                             ,Transport="limegreen"
                             ,TransportWithBiochemicalReaction="limegreen"
                             ,Degradation="darkgray"
                             
                             ,dbid="black"
                             
                             ,CellularLocation="olivedrab"
                             ,SequenceModification="olivedrab"
                             ,Evidence="olivedrab"
                             ,Interaction="olivedrab"
                             ,RelationshipType="olivedrab")
                       ,fontcolor=
                           c(Protein="gray20"
                             ,Complex="whitesmoke"
                             ,SmallMolecule="gray20"
                             ,PhysicalEntity="gray20"
                             ,RNA="gray20"
                             ,DNA="gray20"
                             ,Pathway="whitesmoke"

                             ,BiochemicalReaction="gray20"
                             ,TemplateReaction="whitesmoke"
                             ,ComplexAssembly="whitesmoke"
                             ,Transport="gray20"
                             ,TransportWithBiochemicalReaction="gray20"
                             ,Degradation="whitesmoke"
                             
                             ,dbid="gray50"
                             
                             ,CellularLocation="gray50"
                             ,SequenceModification="gray50"
                             ,Evidence="gray50"
                             ,Interaction="gray50"
                             ,RelationshipType="gray50")
                       # ,style=
                       #     c(Protein="solid"
                       #       ,Complex="solid"
                       #       ,SmallMolecule="solid"
                       #       ,PhysicalEntity="solid"
                       #       ,RNA="solid"
                       #       ,DNA="solid"
                       #       ,Pathway="solid"
                       #       
                       #       ,BiochemicalReaction="solid"
                       #       ,TemplateReaction="solid"
                       #       ,ComplexAssembly="solid"
                       #       ,Transport="solid"
                       #       ,TransportWithBiochemicalReaction="bold"
                       #       ,Degradation="solid"
                       #       
                       #       ,dbid="solid"
                       #       
                       #       ,CellularLocation="solid"
                       #       ,SequenceModification="solid"
                       #       ,Evidence="solid"
                       #       ,Interaction="solid"
                       #       ,RelationshipType="solid")
                       ,penwidth=
                           c(Protein=1
                             ,Complex=1
                             ,SmallMolecule=1
                             ,PhysicalEntity=1
                             ,RNA=1
                             ,DNA=1
                             ,Pathway=1
                             
                             ,BiochemicalReaction=1
                             ,TemplateReaction=1
                             ,ComplexAssembly=1
                             ,Transport=1
                             ,TransportWithBiochemicalReaction=1
                             ,Degradation=1
                             
                             ,dbid=1
                             
                             ,CellularLocation=1
                             ,SequenceModification=1
                             ,Evidence=1
                             ,Interaction=1
                             ,RelationshipType=1)
                       ,peripheries=
                           c(Protein=1
                             ,Complex=1
                             ,SmallMolecule=1
                             ,PhysicalEntity=1
                             ,RNA=1
                             ,DNA=1
                             ,Pathway=1
                             
                             ,BiochemicalReaction=1
                             ,TemplateReaction=1
                             ,ComplexAssembly=1
                             ,Transport=1
                             ,TransportWithBiochemicalReaction=1
                             ,Degradation=1
                             
                             ,dbid=1
                             
                             ,CellularLocation=1
                             ,SequenceModification=1
                             ,Evidence=1
                             ,Interaction=1
                             ,RelationshipType=1)
                       ,tooltip=
                           c(Protein="Protein"
                             ,Complex="Complex"
                             ,SmallMolecule="Small Molecule"
                             ,PhysicalEntity="Physical Entity"
                             ,RNA="RNA"
                             ,DNA="DNA"
                             ,Pathway="Pathway"
                             
                             ,BiochemicalReaction="Biochemical Reaction"
                             ,TemplateReaction="Template Reaction"
                             ,ComplexAssembly="Complex Assembly"
                             ,Transport="Transport"
                             ,TransportWithBiochemicalReaction="Transport with Biochemical Reaction"
                             ,Degradation="Degradation"
                             
                             ,dbid="ID"
                             ,CellularLocation="Cellular Location"
                             ,SequenceModification="Sequence Modification"
                             ,Evidence="Evidence"
                             ,Interaction="Interaction"
                             ,RelationshipType="Relationship Type")
                       ,fixedsize=
                           c(Protein=FALSE
                             ,Complex=FALSE
                             ,SmallMolecule=FALSE
                             ,PhysicalEntity=FALSE
                             ,RNA=FALSE
                             ,DNA=FALSE
                             ,Pathway=FALSE
                             
                             ,BiochemicalReaction=FALSE
                             ,TemplateReaction=FALSE
                             ,ComplexAssembly=FALSE
                             ,Transport=FALSE
                             ,TransportWithBiochemicalReaction=FALSE
                             ,Degradation=FALSE
                             
                             ,dbid=FALSE
                             
                             ,CellularLocation=FALSE
                             ,SequenceModification=FALSE
                             ,Evidence=FALSE
                             ,Interaction=FALSE
                             ,RelationshipType=FALSE)
                       ,height=
                           c(Protein=NA
                             ,Complex=NA
                             ,SmallMolecule=NA
                             ,PhysicalEntity=NA
                             ,RNA=NA
                             ,DNA=NA
                             ,Pathway=NA
                             
                             ,BiochemicalReaction=0.5
                             ,TemplateReaction=0.5
                             ,ComplexAssembly=0.5
                             ,Transport=0.5
                             ,TransportWithBiochemicalReaction=0.5
                             ,Degradation=0.5
                             
                             ,dbid=NA
                             
                             ,CellularLocation=NA
                             ,SequenceModification=NA
                             ,Evidence=NA
                             ,Interaction=NA
                             ,RelationshipType=NA)
                       ,width=
                           c(Protein=NA
                             ,Complex=NA
                             ,SmallMolecule=NA
                             ,PhysicalEntity=NA
                             ,RNA=NA
                             ,DNA=NA
                             ,Pathway=NA
                             
                             ,BiochemicalReaction=0.5
                             ,TemplateReaction=0.5
                             ,ComplexAssembly=0.5
                             ,Transport=0.5
                             ,TransportWithBiochemicalReaction=0.5
                             ,Degradation=0.5
                             
                             ,dbid=NA
                             
                             ,CellularLocation=NA
                             ,SequenceModification=NA
                             ,Evidence=NA
                             ,Interaction=NA
                             ,RelationshipType=NA))

        node_props<-
            node_props %>% 
            cbind(type=rownames(.)
                  ,.)
        return(node_props)
    }




