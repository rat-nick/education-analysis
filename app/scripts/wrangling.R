library(tidyverse)

#data <- read.csv("all_grades.csv")

avgGradeByStudent <- function(data) {
    df <- data %>%
        group_by(ucenik, predmet) %>%
        summarise(avg = mean(ocena))
    return(df)
}

