modalDialog <- function(title, question, entryInit, entryWidth = 20,
                        returnValOnCancel = "ID_CANCEL") {
    dlg <- tktoplevel()
    tkwm.deiconify(dlg)
    tkgrab.set(dlg)
    tkfocus(dlg)
    tkwm.title(dlg, title)
    textEntryVarTcl <- tclVar(paste(entryInit))
    textEntryWidget <- tkentry(dlg, width = paste(entryWidth),
                               textvariable = textEntryVarTcl)
    tkgrid(tklabel(dlg, text = "       "))
    tkgrid(tklabel(dlg, text = question), textEntryWidget)
    tkgrid(tklabel(dlg, text = "       "))
    ReturnVal <- returnValOnCancel
    
    onOK <- function() {
        ReturnVal <<- tclvalue(textEntryVarTcl)
        tkgrab.release(dlg)
        tkdestroy(dlg)
    }
    onCancel <- function() {
        ReturnVal <<- returnValOnCancel
        tkgrab.release(dlg)
        tkdestroy(dlg)
    }
    OK.but <- tkbutton(dlg, text = "   OK   ", command = onOK)
    Cancel.but <- tkbutton(dlg, text = " Cancel ", command = onCancel)
    tkgrid(OK.but, Cancel.but)
    tkgrid(tklabel(dlg, text = "    "))
    
    tkfocus(dlg)
    tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg)})
    tkbind(textEntryWidget, "<Return>", onOK)
    tkwait.window(dlg)
    
    return(ReturnVal)
}
