#' Enhanced version of 'head'. 
#' Default only print 5x5 field for two-dimensional data 
#' or first 5 element for one-dimension data. 
#' And print dimension of data in first line. 
#' It also support specified range. 
#' Don't worry about going out of range, it can fix it! 
#'  
#'  
#' @title hd 
#' @param obj data object. 
#' @param x Number to print of horizontal direction, defaut 5. 
#' @param y Number to print of vertical direction, defaut 5. 
#' @author Jiahao Wang 
#' @examples 
#' hd(LETTERS, 10) 
#' hd(LETTERS, 24:100) 
#' df = get(data(package = "ggplot2", "diamonds")) 
#' rownames(df) = paste0("row_", 1:nrow(df)) 
#' colnames(df) = paste0("col_", 1:ncol(df)) 
#' hd(df) 
#' hd(df, 3, 5) 
#' hd(df, 3:5, 7:9) 
#' hd(df, -10:-nrow(df), -3:-5) 
#' hd(df, (nrow(df) - 2):10^6, (ncol(df) - 2):10^6) 
#' @return A part of input data. 
#' @export 
hd <- function(obj, x = 5, y = NULL){ 
 
    check_len <- function(idx, max){ 
        if(length(idx) > 1){ 
            if(max(idx) > max){ 
                idx_res = idx[idx <= max] 
            }else{ 
                idx_res = idx 
            } 
        }else{ 
            if(idx > max){ 
                idx_res = 1:max 
            }else{ 
                idx_res = 1:idx 
            } 
        } 
        return(idx_res) 
    } 
 
    if(is.null(y)) 
        y = x 
 
    dims = is.null(dim(obj)) 
    if(!dims){ 
 
        cat("dim:", nrow(obj), "×", ncol(obj), "\n") 
 
        idx_x = check_len(x, nrow(obj)) 
        idx_y = check_len(y, ncol(obj)) 
        res = data.frame(obj[idx_x, idx_y]) 
        if(!is.null(rownames(obj))) 
            rownames(res) = rownames(obj)[idx_x] 
        if(!is.null(colnames(obj))) 
            colnames(res) = colnames(obj)[idx_y] 
    } else{ 
        cat("dim:", length(obj), "\n") 
        idx_x = check_len(x, length(obj)) 
        res = obj[idx_x] 
        if(!is.null(names(obj))) 
            names(res) = names(obj)[idx_x] 
    } 
    return(res) 
} 
