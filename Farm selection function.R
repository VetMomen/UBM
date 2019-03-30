library(readxl)
library(dplyr)
library(tidyr)

farms<-read_excel("./data sets/farms.xlsx")

farm_selection<-function(df,size){
        idx<-sample(nrow(df),size,replace = FALSE)
        return(df[idx,])
}
set.seed(1845)

first_wave<-farm_selection(farms,20)
View(first_wave)

second_wave<-farm_selection(df = anti_join(farms,first_wave),20)
View(second_wave)

third_wave<-farm_selection(df = anti_join(farms,rbind(first_wave,second_wave)),10)
View(third_wave)


fourth_wave<-farm_selection(df = anti_join(farms,rbind(first_wave,second_wave,third_wave)),20)

fivth_wave<-farm_selection(df = anti_join(farms,rbind(first_wave,second_wave,third_wave,fourth_wave)),10)
