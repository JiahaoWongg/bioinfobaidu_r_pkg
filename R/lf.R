#' Shortcut function for list.files 
#' Shortcut function for list.files. 
#' @title lf 
#' @param mode Running mode, rf|f|r|x. 
#' @param folder Forder to list. 
#' @param pattern File name pattern you want to save. 
#' @param remove Path name pattern you want to remove. 
#' @param include.dirs Whether include directory. 
#' @param ignore.case Whether ignore case when matching pattern. 
#' @author Jiahao Wang 
#' @examples 
#' lf("x", R.home()) 
#' lf("f", R.home()) 
#' lf("r", R.home()) 
#' lf("rf", R.home()) 
#' lf("rf", R.home(), pattern = "R$") 
#' lf("rf", R.home(), pattern = "R$", remove = c("base")) 
#' lf("rf", R.home(), pattern = "R$", remove = c("base"), ignore.case = TRUE) 
#' lf("x", R.home()) 
#' lf("x", R.home()) 
#' @return File paths. 
#' @export 
lf <- function(type = "x", folder, pattern = "", remove = NULL, include.dirs = FALSE, ignore.case = FALSE){ 
    if(!type %in% c("rf", "f", "r", "x")) 
        stop("\nOnly support below types: 
            \t- rf: recursive = TRUE, full.names = TRUE 
            \t-  f: recursive = TRUE, full.names = FALSE 
            \t-  r: recursive = FALSE, full.names = TRUE 
            \t-  x: recursive = FALSE, full.names = FALSE\n") 
    res = switch(type, 
            rf = list.files(folder, pattern, full.names = TRUE, recursive = TRUE, include.dirs = include.dirs, ignore.case = ignore.case), 
            f  = list.files(folder, pattern, full.names = TRUE, recursive = FALSE, include.dirs = include.dirs, ignore.case = ignore.case), 
            r  = list.files(folder, pattern, full.names = FALSE, recursive = TRUE, include.dirs = include.dirs, ignore.case = ignore.case), 
            x  = list.files(folder, pattern, full.names = FALSE, recursive = FALSE, include.dirs = include.dirs, ignore.case = ignore.case) 
    ) 
    if(!is.null(remove)){ 
        for(x in remove){ 
            res = res[!grepl(x, res)] 
        } 
    } 
    return(res) 
} 
