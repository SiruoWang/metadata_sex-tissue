# cross check both CCLE and SRA database and create a new table with known gender information 
# fields: run_accession, cell_line, sex

tissuetable <- function(){
        # meta_cl_tissue: subset data frame from metadata with both cell line and tissue fields filled
        metasub2<-data.frame(cell_line = metadata$cell_line, run_accession = metadata$run_accession, tissue = metadata$tissue)
        meta_cl_tissue<-metasub2[!is.na(metasub2$cell_line) & !is.na(metasub2$tissue),]
        
        # data1 & data2 come from CCLE database
        data1 <- data.frame(cell_line = CCLE$Cell.line.primary.name,tissue = CCLE$Histology,stringsAsFactors=FALSE)
        data2 <- data.frame(cell_line = CCLE$Cell.line.aliases,tissue = CCLE$Histology,stringsAsFactors=FALSE)
        
        newsra <- data.frame(cell_line = metadata$cell_line, run_accession = metadata$run_accession, tissue = metadata$tissue,stringsAsFactors=FALSE)
        
        step1<-merge(newsra,data1,by="cell_line",all=FALSE,stringsAsFactors=FALSE)
        step2<-merge(newsra,data2,by="cell_line",all=FALSE,stringsAsFactors=FALSE)
        
        
        # data: 
        CCLE_data <- rbind(step1,step2)
        
        CCLE_data <- data.frame(run_accession = CCLE_data$run_accession, cell_line = CCLE_data$cell_line, tissue = CCLE_data$tissue.y)
        tissuetable <- rbind(CCLE_data,meta_cl_tissue)
        tissuetable <- subset(tissuetable,!duplicated(tissuetable$run_accession))
        
        return(tissuetable)
}

#tissuetable()
