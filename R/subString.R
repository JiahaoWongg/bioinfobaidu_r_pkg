#' Multifunctional String Processing. 
#' String extraction, string splitting, string concatenation, reverse manipulation. 
#'  
#' @title subString 
#' @param strings One or more strings. 
#' @param idx Positions of the fields to extract, one or more numbers. 
#' @param sep Specified delimiter. 
#' @param rev Whether to extract in reverse order. 
#' @param collapse Delimiter used to reassemble the strings after splitting. 
#' @author Jiahao Wang 
#' @examples 
#' strings = c("TCGA-A1-A0SB-10B-01D-A141-01", "TCGA-A7-A26E-01A-11D-A272-09") 
#' strings = c(strings, "TCGA-E2-A3DX-01A-21D-A20S-09") 
#' subString(strings, 14:15) 
#' subString(strings, 20) 
#' subString(strings, 1:2, rev = TRUE) 
#' subString(strings, 2, sep = "-") 
#' subString(strings, 2, sep = "-", rev = TRUE) 
#' string2 = c("GENE1 ENSG0000000001", "GENE2 ENSG0000000002", "GENE3 ENSG0000000003") 
#' subString(string2, 1, " ") 
#' subString(string2, 2, " ") 
#' subString(strings, 1:3, sep = "-", collapse = "-") 
#' subString(strings, c(1, 3, 5), sep = "-", collapse = "-") 
#' subString(strings, c(2, 4, 6), sep = "-", collapse = "-nb-", rev = TRUE) 
#' 
#' @return One or more strings. 
#' @export 
subString <- function(strings, idx, sep = NULL, rev = FALSE, collapse = NULL){ 
 
	strings = as.character(strings) 
	if(is.null(sep)){ 
		if(rev){ 
			res = as.character(sapply(strings, function(x) paste(rev(rev(strsplit(x, "")[[1]])[idx]), collapse = ""))) 
		} else { 
			res = as.character(sapply(strings, function(x) paste(strsplit(x, "")[[1]][idx], collapse = ""))) 
		} 
	} else{ 
		if(rev){ 
			res = sapply(strsplit(strings, sep), function(x) paste(rev(rev(x)[idx]), collapse = collapse)) 
		} else { 
			res = sapply(strsplit(strings, sep), function(x) paste(x[idx], collapse = collapse))	 
		} 
	} 
	return(res) 
} 
