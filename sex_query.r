# sex function allows query on cell line and return corresponding sex information. F -> female; M -> male; U-> unknown

sex <- function(alist) {
   x <- lapply(alist, function(cell_line) {
  
                    sexdata <- sextable()
                   
                    if (cell_line %in% sexdata$cell_line)
                    {
                      
                      subset <- sexdata[sexdata$cell_line == cell_line,]
                      result <- unique(subset$sex)
                    }
                    
                    else
                    {
                      result <- NA
                    }
                    
                    return(as.vector(result))
                    }
                 )
   
   ans <- unlist(x)
   return(ans)
}


##sex(c("143B","MONOMAC6","aaa","bbb"))
