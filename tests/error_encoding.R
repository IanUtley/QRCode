source("../error_encoding.R")
result=sapply(FUN=GFLog,Poly(7))
result
if (identical(result,c(0,87,229,146,149,238,102,21))) {
    message("Ok")
}
