library(tidyverse)
library(openssl)
library(stringr)
load_grades <- function(data_dir, is_pandemic) {
    res <- data.frame()
    
    covid_file_names <- dir(data_dir)
    
    for (f in covid_file_names) {
        str <- f %>% substr(1, nchar(f) - 4) %>% str_split(pattern = "-")
        str <- str[[1]]
        
        godina <- str[1]
        odeljenje <- str[2]
        
        path <- paste(data_dir, f, sep = "")
        
        data <-
            read.csv(
                path,
                header = T,
                stringsAsFactors = F,
                skip = 1,
                blank.lines.skip = T
            )
        
        gradjansko <-  "Грађанско.васпитање..обавезни.изборни."
        veronauka <- "Верска.настава...православни.катихизис..обавезни.изборни."
        
        drop <- c(gradjansko, veronauka)
        
        data <- data[, colSums(is.na(data)) < 0.8 * nrow(data)]
        
        data <- data[, !names(data) %in% drop]
        
        
        data$ucenik <- md5(paste(data[, 1], stri_rand_strings(1,4)))
        data <- select(data, -1)
        data <- head(data, -1)
        
        class_data <- data.frame()
        predmeti <- setdiff(names(data), c("ucenik"))
        
        for (predmet in predmeti) {
            tmp <- separate_rows(data %>% select(ucenik, predmet), predmet) %>%
                drop_na() %>%
                gather(predmet, key = 'predmet', value = 'ocena') %>% 
                filter(ocena != "")
            tmp$godina <- godina
            tmp$odeljenje <- odeljenje
            tmp$pandemija <- is_pandemic
            class_data <- rbind(tmp)
            
        }
        res <- rbind(res, class_data)
        res$ocena <- as.numeric(res$ocena)
    }
    return(res)
}
res <- load_grades("data/covid/ocene/", T)
