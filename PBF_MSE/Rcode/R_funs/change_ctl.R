#' Changes the PBF Stock Synthesis ctl file to
#' 1) reflect the terminal year of the current tstep
#' 2) change the last year of the recruitment deviations
#' 3) set recruitment deviations to be fixed parameters or simple deviations
#' 4) change variance adjustment
#'
#' @param ss_file_in filename of original ctl file to be modified, with full path or relative to working directory
#' @param ss_file_out filename for the new ctl file with full path or relative to working directory
#' @param new_end new end year
#' @param vadj specify if this should left as in assessment model (1) or set to 0 and 1 (2) - option 2 is used to create bootstrap files
#' @return A modified ctl file.
#' @author Desiree Tommasi

change_ctl = function(ss_file_in, ss_file_out, new_end, vadj){
  
  pattern = "# begin and end years of blocks"
  pattern1 = "#do_recdev:"
  pattern2 = "# last year of main recr_devs"
  pattern3 = "#_max_bias_adj_in_MPD"
  pattern4 = "# Input variance adjustments factors: "
  pattern5 = "#_mult_by_lencomp_N"
  pattern6 = "#_last_yr_fullbias_adj_in_MPD"
  pattern7 = "#_end_yr_for_ramp_in_MPD"
  
  ctl = readLines(ss_file_in, warn = FALSE)
  
  new_end_c = as.character(new_end)
  new_end_b = as.character(new_end+1)
  
  #Change block 1
  which.line = grep(pattern=pattern, x=ctl)+1
  blk1.old = ctl[which.line]
  blk1.new = gsub("2021", new_end_c, blk1.old) # replace last year of block 1 
  ctl[which.line] = blk1.new
  #Change block 2
  which.line = grep(pattern=pattern, x=ctl)+2
  blk2.old = ctl[which.line]
  blk2.new = gsub("2021 2021", paste("2021 ",new_end_c, sep=""),blk2.old) # replace last year of block 2 
  ctl[which.line] = blk2.new
  #Change block 3
  which.line = grep(pattern=pattern, x=ctl)+3
  blk3.old = ctl[which.line]
  blk3.new = gsub("2021", new_end_c, blk3.old) # replace last year of block 3 
  ctl[which.line] = blk3.new
  #block 4 stays the same
  #Change block 5
  which.line = grep(pattern=pattern, x=ctl)+5
  blk5.old = ctl[which.line]
  blk5.new = gsub("2021 2021", paste("2021 ",new_end_c, sep=""),blk5.old) # replace last year of block 5 
  ctl[which.line] = blk5.new
  #Change block 6
  which.line = grep(pattern=pattern, x=ctl)+6
  blk6.old = ctl[which.line]
  blk6.new = gsub("2021", new_end_c, blk6.old) # replace last year of block 5 
  ctl[which.line] = blk6.new
  
  #change the recruitment deviations to not sum to 0
  which.line = grep(pattern=pattern1, x=ctl)
  end.old = ctl[which.line]
  end.new = gsub("1", "2", end.old)
  ctl[which.line] = end.new
  
  #change end year of recruitment deviations to new end year - note in the original assessment the rec devs ended 1 yr before the end year
  which.line = grep(pattern=pattern2, x=ctl)
  end.old = ctl[which.line]
  end.new = gsub("2020", new_end_c, end.old)
  ctl[which.line] = end.new
  
  #set bias adjustment to 1 for all years-not used leads to strange patterns in OM
  #which.line = grep(pattern=pattern3, x=ctl)
  #badj.old = ctl[which.line]
  #badj.new = gsub("0.9542", "-1", badj.old)
  #ctl[which.line] = badj.new
    
  #change the last year of bias adjustment
  which.line = grep(pattern=pattern6, x=ctl)
  end.old = ctl[which.line]
  end.new = gsub("2016.7", new_end_c, end.old) 
  ctl[which.line] = end.new
    
  #change the last year of no bias adjustment
  which.line = grep(pattern=pattern7, x=ctl)
  end.old = ctl[which.line]
  end.new = gsub("2020.2", new_end_b, end.old)
  ctl[which.line] = end.new
  
  #Change variance adjustment to additive variables to 0 and for multiplicative to 1
  #before running the bootstrap
  if (vadj == 2) {
  for (j in 9:68){
    which.line = grep(pattern=pattern4, x=ctl)+j
    sur.old = ctl[which.line]
    sur.new = gsub("0", "1", sur.old) # remove leading blank
    ctl[which.line] = sur.new
  }
  }
  
  ## Write it back to file
  writeLines(ctl, con = ss_file_out)
  
  }