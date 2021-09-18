library(tidyverse)
library(data.table)
library(plyr)
library(naniar)
library(stringi)


load_ocene <- function(data_dir, pandemic) {
    grades <- c("I", "II", "III", "IV")
    res <- data.frame()
    
    for (grade in grades) {
        files_of_grade <- list.files(path = data_dir,
                                     pattern = paste("^", grade, "-", sep = ""))
        
        for (file in files_of_grade) {
            data <- data.frame(fread(paste(data_dir,file,sep = ""), header = TRUE))
            data <- data %>% select(-c(1))
            data <- head(data, -1)
            
            odeljenje <- str_split(file, pattern = "-")[[1]][2]
            odeljenje <-
                str_split(odeljenje, pattern = "\\.")[[1]][1]
            
            data$razred <- c(grade)
            data$odeljenje <- c(odeljenje)
            data$pandemija <- c(pandemic)
            
            data <- data[, colSums(is.na(data)) < nrow(data)]
            
            
            names(data) <-
                stri_trans_general(colnames(data), "Serbian-Latin/BGN")
            names(data) <- lapply(names(data), tolower)
            
            res <- bind_rows(res, data)
            
        }
    }
    
    res <- res[, colSums(is.na(res)) < nrow(res)]
    
    year1 <- res %>% filter(razred == "I")
    year1 <- year1[, colSums(is.na(year1)) < nrow(year1)]
    year2 <- res %>% filter(razred == "II")
    year2 <- year2[, colSums(is.na(year2)) < nrow(year2)]
    year3 <- res %>% filter(razred == "III")
    year3 <- year3[, colSums(is.na(year3)) < nrow(year3)]
    year4 <- res %>% filter(razred == "IV")
    year4 <- year4[, colSums(is.na(year4)) < nrow(year4)]
    
    res <- list()
    res[[1]] <- year1
    res[[2]] <- year2
    res[[3]] <- year3
    res[[4]] <- year4
    return(res)
}
