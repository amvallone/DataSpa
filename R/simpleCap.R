
# Coloca la primer letra en mayuscula de un vector de caracteres


simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(substring(s, 1, 1), tolower(substring(s, 2)),
          sep = "", collapse = " ")
}