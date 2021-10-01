library(tidyverse)
library(openssl)
library(stringi)
load_grades <- function(data_dir, is_pandemic) {
    res <- data.frame()
    
    covid_file_names <- dir(data_dir)
    
    for (f in covid_file_names) {
        str <- f %>% substr(1, nchar(f) - 4) %>% str_split(pattern = "-")
        str <- str[[1]]
        
        godina <- str[1]
        odeljenje <- str[2]
        
        print(f)
        
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
        veronauka <-
            "Верска.настава...православни.катихизис..обавезни.изборни."
        
        drop <- c(gradjansko, veronauka)
        
        data <- data[, colSums(is.na(data)) < 0.9 * nrow(data)]
        
        data <- data[, !names(data) %in% drop]
        
        
        data$ucenik <- md5(paste(data[, 1], stri_rand_strings(1, 4)))
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
            class_data <- rbind(class_data, tmp)
            
        }
        res <- rbind(res, class_data)
        res$ocena <- as.numeric(res$ocena)
    }
    return(res)
}

covid_data <- load_grades("data/covid/ocene/", T)
pre_covid_data <- load_grades("data/pre-covid/ocene/", F)

all_data <- rbind(covid_data, pre_covid_data)

write.csv(all_data, file = "all_grades.csv", row.names = F)

library(janitor)

load_attendance <- function(data_dir, is_pandemic) {
    res <- data.frame()
    
    file_names <- dir(data_dir)
    
    for (f in file_names) {
        path <- paste(data_dir, f, sep = "")
        
        str <-
            f %>% substr(1, nchar(f) - 4) %>% str_split(pattern = "-")
        str <- str[[1]]
        
        godina <- str[1]
        odeljenje <- str[2]
        
        path <- paste(data_dir, f, sep = "")
        
        df1 <- read_csv(path, skip = 1, n_max = 4)
        
        names(df1) <- c(
            "polugodiste",
            "ucenika sa izostancima",
            "ucenika bez izostanaka",
            "ucenika sa opravdanim",
            "broj opravdanih",
            "ucenika sa neopravdanim",
            "broj neopravdanih",
            "ucenika sa neregulisanim",
            "broj neregulisanih",
            "ukupno izostanaka",
            "broj izostanaka po uceniku"
        )
        df1 <- df1[-c(1), ]
        df1$polugodiste <-
            if_else(df1$polugodiste == "Прво",
                    1,
                    if_else(df1$polugodiste == "Друго", 2, 0))
        df1$godina <- c(godina)
        df1$odeljenje <- c(odeljenje)
        df1$pandemija <- is_pandemic
        res <- rbind(res, df1)
    }
    return(res)
}

df1 <- load_attendance("data/covid/prisustvo/", T)
df2 <- load_attendance("data/pre-covid/prisustvo/", F)

df <- rbind(df1, df2)

write.csv(df, file = "summary_attendance.csv", row.names = F)
