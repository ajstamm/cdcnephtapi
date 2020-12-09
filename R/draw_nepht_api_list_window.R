# selectmode = single, extended
draw_nepht_api_list_window <- function(title = "Selection", 
                                       mylist = letters, 
                                       instruction = "Please select one.",
                                       opts = "single", all = FALSE) {
  if (all) {
    mylist <- c("All options", unique(mylist[order(mylist)]))
  } else {
    mylist <- unique(mylist[order(mylist)])
  }
  
  # create frames ####
  tt <- tcltk::tktoplevel()
  tcltk::tktitle(tt) <- title
  # within functions frames must all be created at the start
  tt$frm <- tcltk::tkframe(tt, width = 300, height = 5)
  tt$bound <- tcltk::tkframe(tt$frm, width = 150, height = 110)
  tt$tfbuts <- tcltk::tkframe(tt$frm, width = 300, height = 40)
  
  # list of options ####
  tt$bound$note <- tcltk2::tk2label(tt$bound, text = instruction)
  tt$bound$tl <- tcltk2::tk2listbox(tt$bound, height = 5, background = "white",
                                    values = mylist, selectmode = opts, 
                                    width = 50, scroll = "both")
  tcltk::tkgrid(tt$bound$note, sticky = "w", columnspan = 4, padx = 5)
  tcltk::tkgrid(tt$bound$tl, padx = 10, pady = c(5, 10), sticky = "w",
                row = 2, column = 1)
  
  # functions ####
  myenv <- new.env()
  
  onOk <- function() {
    ind <- as.numeric(tcltk::tkcurselection(tt$bound$tl))
    myvar <- mylist[ind + 1] # list 1
    tcltk::tkdestroy(tt)
    assign("myoptions", list(myvar = myvar), envir=myenv)
  }
  onCancel <- function() {
    tcltk::tkdestroy(tt)
    myvar <- "cancel"
    assign("myoptions", list(myvar = myvar), envir=myenv)
  }
  onBack <- function() {
    tcltk::tkdestroy(tt)
    myvar <- "back"
    assign("myoptions", list(myvar = myvar), envir=myenv)
  }
  
  # buttons ####
  tt$tfbuts$BackBut <- tcltk2::tk2button(tt$tfbuts, text = "< Back",
                                         command = onBack, width = 12)
  tt$tfbuts$OkBut <- tcltk2::tk2button(tt$tfbuts, text = "Next >",
                                       command = onOk, width = 12,
                                       default = "active")
  tt$tfbuts$CancelBut <- tcltk2::tk2button(tt$tfbuts, text = "Cancel",
                                           command = onCancel, width = 12)
  
  tcltk::tkgrid(tt$tfbuts$BackBut, column = 1, row = 1, pady = 5, padx = c(5, 0))
  tcltk::tkgrid(tt$tfbuts$OkBut, column = 2, row = 1, pady = 5)
  tcltk::tkgrid(tt$tfbuts$CancelBut, column = 3, row = 1, pady = 5)
  
  tcltk::tkgrid.configure(tt$tfbuts$BackBut, sticky = "e")
  tcltk::tkgrid.configure(tt$tfbuts$OkBut, sticky = "w")
  tcltk::tkpack(tt$tfbuts, tt$bound, side = "bottom")
  tcltk::tkpack(tt$frm)
  
  # wait for user ####
  tcltk::tkfocus(tt)
  tcltk::tkwait.window(tt) # pauses code to accept user input
  
  return(myenv$myoptions)
  
}
