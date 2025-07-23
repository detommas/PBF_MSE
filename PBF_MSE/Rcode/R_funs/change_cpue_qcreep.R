#' modify TW LL CPUE (w/in EM func) for q creep robustness test
#' @param cpue_dat CPUE data object in ss dat format
#' @param is_tstep1 flag of whether or not tstep is 1: = TRUE (tstep=1), FALSE (tstep!=1)
#' @return modified CPUE data in ss dat format
#' @author Norio Takahashi

change_cpue_qcreep = function(cpue_dat = NULL, is_tstep1 = NULL) {

  if(is.null(cpue_dat) | is.null(is_tstep1)){
    stop("Either or both of cpue_dat and is_tstep are missing - change_cpue_qcreep")
  }
  
  if(is_tstep1){
    
    modified_cpue_dat <- 
      cpue_dat %>% mutate(obs = if_else(index == 31, obs*(1.02)^(year - 2002), obs))
    
  }else{
    
    modified_cpue_dat <- 
      cpue_dat %>% mutate(obs = obs*(1.02)^(year - 2002))
    
  }
  
  return(modified_cpue_dat)
  
}
  
  