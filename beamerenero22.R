# beamer 2022 enero
library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)
library(writexl)
library(tidyr)
# install.packages("officer") # Install
# install.packages("Rtools")
library(officer) # Load
# install.packages("flextable")
library(flextable)
# install.packages("magrittr")
library(magrittr)
library(RColorBrewer)

paul22<-read_xlsx("Ventas Lansier 2022 DataPaul.xlsx",sheet = "datapaul",
                  col_types = c("numeric","text","text","numeric","text","text","numeric","text","text","text",
                                "numeric","numeric","numeric","numeric","numeric","numeric","text","text","text","numeric",
                                "text","numeric","numeric","text","numeric","text","text","text","text","numeric","text","text"))

str(paul22)
unique(paul22$periodo)
# filtering data
paul22<-paul22 %>% filter(periodo=="202201")
paul22<-paul22 %>% filter(tipocl!="SIMILASAN")
# QUITANDO PRODUCTOS COVID
paul22<-paul22 %>% filter(tipoart!="C")
unique(paul22$subtotal)

#bar chart
paul22 %>% filter(tipocl=="DISTRIBUIDORES")%>%
  ggplot(aes(x=tipocl, y=subtotal,fill=tipoart)) + geom_bar(stat = "identity",width = 0.4)+scale_fill_brewer(palette="Set1")


# pie chart entre tipoart para distribuidoras
# tengo que eliminar los NA porque no permiten realizar un calculo
piechart<-paul22 %>% filter(tipocl=="DISTRIBUIDORES") %>% drop_na(subtotal) %>%  group_by(tipoart) %>% summarise(subtotal=sum(subtotal))

piechart %>% ggplot(aes(x="", y=subtotal, fill=tipoart)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0,direction = -1) +
  theme_void()



