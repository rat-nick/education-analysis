library(tidyverse)
library(data.table)
library(plyr)
library(naniar)
library(stringi)

setwd("data/covid/ocene/")

grades <- c("I", "II", "III", "IV")
res <- data.frame()
all_cols <- data.frame()

for (grade in grades) {
    
    files_of_grade <- list.files(
        path = ".",
        pattern = paste("^", grade, "-", sep=""))
    
    for (file in files_of_grade){
        data <- data.frame(fread(file, header = TRUE))
        data <- data %>% select(-c(1))
        data <- head(data, -1)
        
        data$grade <- c(grade)
        data$pandemic <- c(TRUE)
        
        data <- data[ ,colSums(is.na(data))<nrow(data)]
        
        
        names(data) <- stri_trans_general(colnames(data), "Serbian-Latin/BGN")
        names(data) <- lapply(names(data), tolower)
        cols <- colnames(data)
        res <- bind_rows(res,data)
        all_cols <- c(all_cols, cols)
    }
}

all_cols <- unique(all_cols)
res <- res[, colSums(is.na(res))<nrow(res)]


year1 <- res %>% filter(grade == "I") 
year1 <- year1[, colSums(is.na(year1))<nrow(year1)]
year2 <- res %>% filter(grade == "II")
year2 <- year2[, colSums(is.na(year2))<nrow(year2)]
year3 <- res %>% filter(grade == "III")
year3 <- year3[, colSums(is.na(year3))<nrow(year3)]
year4 <- res %>% filter(grade == "IV")
year4 <- year4[, colSums(is.na(year4))<nrow(year4)]

vis_miss(year1)
vis_miss(year2)
