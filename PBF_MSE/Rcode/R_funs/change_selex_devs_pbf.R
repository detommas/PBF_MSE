#' Set OM selectivity deviations for forward simulation
#'
#' This function adds selectivity deviations in the
#' ss3.par file with those specified. It then writes a new file 
#'
#' @param seldevs_new A vector of new selectivites deviations.
#' @template par_file_in
#' @template par_file_out
#' @param na #number of ages for which to modify the deviations
#' @return A modified SS3 .par file.
#' @author Cole Monnahan and D. Tommasi (this is modified from an ss3sim function to replace rec devs)

change_selex_devs_pbf <- function(seldevs_new, par_file_in,
                            par_file_out, na){
  
  ## This is the pattern on the line before the vector of current recdevs
  ## Following this pattern are the deviations, one line per age
  pattern <- "# checksum999"
  
  par <- readLines(par_file_in, warn = FALSE)
  
  for(p in 1:9){
    which.line <- grep(pattern=pattern, x=par)+(-18+p*2-1)
    ## grab the old ones, note there is a leading space that needs to be
    ## deleted
    seldevs.old <- par[which.line]
    seldevs.old <- gsub("^\\s+|\\s+$", "", seldevs.old) # remove leading blank
    seldevs.old <- gsub("\\s+", " ", seldevs.old)       # remove >1 blanks
    seldevs.old <- as.numeric(unlist(strsplit(seldevs.old, split= " ")))
    
    ##  Add extra seldevs for the next assessment period for the first sel dev par:
    seldevs_tot <- c(seldevs.old, seldevs_new[,p])
    
    ## replace w/ new seldevs, adding back in that leading space
    par[which.line] <- paste0(" ", seldevs_tot, collapse="")
    
  }

  ## Write it back to file
  writeLines(par, con = par_file_out)
}