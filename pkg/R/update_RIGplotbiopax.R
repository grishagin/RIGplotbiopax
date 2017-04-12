update_RIGplotbiopax <-
    function () 
    {
        #' @title
        #' Update RIGplotbiopax from GitHub
        #' @description 
        #' Updates the package. It may be necessary to restart the R session after the update.
        
        #' @author 
        #' Ivan Grishagin
        
        
        unloadNamespace(ns = "RIGplotbiopax")
        devtools::install_github("grishagin/RIGplotbiopax", subdir = "pkg")
    }