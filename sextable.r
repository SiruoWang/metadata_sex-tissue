# CCLE database: http://www.broadinstitute.org/ccle/data/browseSamples?actionMethod=pages%2Fhome.xhtml%3AbrowseSamplesBean.checkSkipFirstStep%28%29&conversationPropagation=begin

#####################################################################################################
# cross check both CCLE and SRA database and create a new table with known gender information 
# fields: run_accession, cell_line, sex

sextable <- function(){

        # meta_cl_sex: subset data frame from metadata with both cell line and gender fields filled
        metasub1<-data.frame(cell_line = metadata$cell_line, run_accession = metadata$run_accession,sex = metadata$sex,row.names=NULL)
        meta_cl_sex<-metasub1[!is.na(metasub1$cell_line) & !is.na(metasub1$sex),]
        
        # data1 & data2 come from CCLE database
        data1 <- data.frame(cell_line = CCLE$Cell.line.primary.name,sex = CCLE$Gender)
        data2 <- data.frame(cell_line = CCLE$Cell.line.aliases,sex = CCLE$Gender)
        
        newsra <- data.frame(cell_line = metadata$cell_line, run_accession = metadata$run_accession, sex = metadata$sex)
        
        step1<-merge(newsra,data1,by="cell_line",all=FALSE)
        step2<-merge(newsra,data2,by="cell_line",all=FALSE)
        
        # data: 
        CCLE_data <- rbind(step1,step2)
        
        CCLE_data <- data.frame(run_accession = CCLE_data$run_accession, cell_line = CCLE_data$cell_line, sex = CCLE_data$sex.y)
        sextable <- rbind(CCLE_data,meta_cl_sex)
        sextable <- subset(sextable,!duplicated(sextable$run_accession))
        
        return(sextable)
  }

#sextable()
