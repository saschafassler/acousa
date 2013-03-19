SPPselect <- function(acoustic_data){

### function to select species for further calculations
###
### input: -acoustic_data

### load required packages
    require(tcltk)

### select the species used for calculations
    tt<-tktoplevel()
    tl<-tklistbox(tt,height=length(c(levels(acoustic_data$Species))),selectmode="single",background="white")
    tkgrid(tklabel(tt,text="Select species for analysis"))
    tkgrid(tl)
    spp_select_names <- c(levels(acoustic_data$Species))
    select <- tclVar(0)
    for (i in (1:length(c(levels(acoustic_data$Species))))){
    tkinsert(tl,"end",spp_select_names[i])}
    tkselection.set(tl,0)  # Default species is herring
    OK.but <- tkbutton(tt,text="   OK   ",command=function() tclvalue(select) <- spp_select_names[as.numeric(tkcurselection(tl))+1])
    tkgrid(OK.but)
    tkfocus(tt)
    tkwait.variable(select)
    spp_selection <- tclvalue(select)
    tkdestroy(tt)

    spp_selection

}

