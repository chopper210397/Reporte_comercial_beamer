library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)
library(writexl)
# install.packages("officer") # Install
# install.packages("Rtools")
library(officer) # Load
# install.packages("flextable")
library(flextable)
# install.packages("magrittr")
library(magrittr)
# install.packages("devtools")
# library(devtools)
# find.package("devtools")
# find_rtools()

datapaul21<-read_xlsx("C:\\Users\\LBarrios\\OneDrive - Laboratorios Lansier\\Laptop_antigua\\escritorio\\historico_venta_lansier\\Ventas Lansier 2021 DataPaul.xlsx",
          sheet="datapaul")
datapaul20<-read_xlsx("C:\\Users\\LBarrios\\Desktop\\ventalansier2020.xlsx")
str(datapaul21)
unique(datapaul21$periodo)
unique(datapaul21$mes)

datapaul21$periodo<-ymd(datapaul21$periodo,truncated = 2)
datapaul20$periodo<-ymd(datapaul20$periodo,truncated = 2)
# eliminando productos covid y seleccionando solo canal distribuidores, essalud e instituciones
datapaul21nocovid<-datapaul21 %>%
  filter(!(artdes %in% c("VEDMLN001","VESOGVT001","VESOGVT002","VESOTR001","VESOTR002","VESOTR003","VESOTR004")),
         tipocl %in% c("ESSALUD","DISTRIBUIDORES","INSTITUCIONES") )

datapaul20nocovid<-datapaul20 %>%
  filter(!(artdes %in% c("VEDMLN001","VESOGVT001","VESOGVT002","VESOTR001","VESOTR002","VESOTR003","VESOTR004")),
         tipocl %in% c("ESSALUD","DISTRIBUIDORES","INSTITUCIONES") )
# creando año y mes para la data


# 
# datapaul21<-datapaul21 %>% mutate(año=substr(datapaul21$periodo,0,4))
# datapaul21<-datapaul21 %>% mutate(mes=substr(datapaul21$periodo,5,6))
# 
# datapaul20<-datapaul20 %>% mutate(año=substr(datapaul20$periodo,0,4))
# datapaul20<-datapaul20 %>% mutate(mes=substr(datapaul20$periodo,5,6))
# 

# aqui estamos generando la data para cada slide

#---------------------------------- primer slide ----------------------------------#
# PARA TENER LA VENTA TOTAL DEL MES DE DICIEMBRE SE ELIMINO LOS PRODUCTOS COVID Y 
# SOLO SE SELECCIONÓ LAS FUENTES DISTRIBUIDORES, ESSALUD E INSTITTUCIONES

# VENTA TOTAL MES DICIEMBRE
totalmes<-datapaul21nocovid %>%
   filter(mes=="12") %>%
   summarise(soles=sum(subtotal),presupuesto=sum(ppsol),cumplimiento=sprintf("%.2f",sum(subtotal)/sum(ppsol)*100))
 
 # VENTA CANAL PRIVADO
 privadomes<-datapaul21nocovid %>%
   filter(mes=="12",
          tipocl %in% c("DISTRIBUIDORES") ) %>%
   summarise(soles=sum(subtotal),presupuesto=sum(ppsol),cumplimiento=sprintf("%.2f",sum(subtotal)/sum(ppsol)*100))
 # VENTA CANAL ESSALUD
 essaludmes<-datapaul21nocovid %>%
   filter(mes=="12",
          tipocl %in% c("ESSALUD") ) %>%
   summarise(soles=sum(subtotal),presupuesto=sum(ppsol),cumplimiento=sprintf("%.2f",sum(subtotal)/sum(ppsol)*100))
# VENTA INSTITUCIONES
 institucionmes<-datapaul21nocovid %>%
   filter(mes=="12",
          tipocl %in% c("INSTITUCIONES") ) %>%
   summarise(soles=sum(subtotal),presupuesto=sum(ppsol),cumplimiento=sprintf("%.2f",sum(subtotal)/sum(ppsol)*100))
# YTD
 # DISTRIBUIDORES
ytd21<-datapaul21nocovid %>% summarise(acumuladoytdañoactual=sum(subtotal))
ytd20<-datapaul20nocovid %>% summarise(acumuladoytdañoanterior=sum(subtotal))
ytdprivado21<-datapaul21nocovid %>% filter(tipocl=="DISTRIBUIDORES") %>%  summarise(acumuladoytdañoactual=sum(subtotal))
ytdprivado20<-datapaul20nocovid %>% filter(tipocl=="DISTRIBUIDORES") %>%  summarise(acumuladoytdañoactual=sum(subtotal))

ytdessalud21<-datapaul21nocovid %>% filter(tipocl=="ESSALUD") %>%  summarise(acumuladoytdañoactual=sum(subtotal))
ytdessalud20<-datapaul20nocovid %>% filter(tipocl=="ESSALUD") %>%  summarise(acumuladoytdañoactual=sum(subtotal))

ytdinstitu21<-datapaul21nocovid %>% filter(tipocl=="INSTITUCIONES") %>%  summarise(acumuladoytdañoactual=sum(subtotal))
ytdinstitu20<-datapaul20nocovid %>% filter(tipocl=="INSTITUCIONES") %>%  summarise(acumuladoytdañoactual=sum(subtotal))


privadomes<-privadomes %>% mutate(crecimientoytdinteranual=sprintf("%.2f",(ytdprivado21/ytdprivado20*100)-100))
essaludmes<-essaludmes %>% mutate(crecimientoytdinteranual=sprintf("%.2f",(ytdessalud21/ytdprivado20*100)-100))
institucionmes<-institucionmes %>% mutate(crecimientoytdinteranual=sprintf("%.2f",(ytdprivado21/ytdprivado20*100)-100))



  
totalmes<-totalmes %>% mutate(crecimientoytdinteranual=sprintf("%.2f",(ytd21/ytd20*100)-100))
totalmes[1,5]<-datapaul21nocovid %>% summarise(ytdcumplimiento=sprintf("%.2f",sum(subtotal)/sum(ppsol)*100))

privadomes<-privadomes %>% mutate(ytd21=ytdprivado21)
privadomes<-privadomes %>% mutate(crecimientoytdinteranual=sprintf("%.2f",((ytdprivado21/ytdprivado20*100)-100)))

essaludmes<-essaludmes %>% mutate(ytd21=ytdessalud21)
essaludmes<-essaludmes %>% mutate(crecimientoytdinteranual=sprintf("%.2f",((ytdessalud21/ytdessalud20*100)-100)))

institucionmes<-institucionmes %>% mutate(ytd21=ytdinstitu21)
institucionmes<-institucionmes %>% mutate(crecimientoytdinteranual=sprintf("%.2f",((ytdinstitu21/ytdinstitu20*100)-100)))



#---------------------------------- segundo slide ----------------------------------#
# GRÁFICO DISTRIBUIDORES
dataagrupada<-datapaul21nocovid  %>% group_by( periodo,tipocl) %>%
  summarise(soles=sum(subtotal/1000000),cumplimiento=as.numeric( sprintf("%.2f",sum(subtotal)/sum(ppsol)*100)))

dataagrupada  %>%
  ggplot(mapping=aes(x=periodo,y=soles,color=tipocl))+
  geom_line(size=1.2)+geom_point(color="black")+theme_light()+labs(y="Millones de soles")+facet_wrap(~tipocl,ncol = 2,scales = "free")+
  theme(legend.position="none")

ggsave("solesbycanal.jpg", dpi = 300)


dataagrupada  %>%
  ggplot(mapping=aes(x=periodo,y=cumplimiento,color=tipocl))+
  geom_line(size=1.3)+
  geom_point(color="black",size=2)+theme_light()+labs(y="% de cumplimiento")+facet_grid(cols=vars(tipocl))+
  theme(legend.position="none")+geom_hline(yintercept = 100,color="red",linetype="dashed")

ggsave("cumplimientobycanal.jpg", dpi = 300)




dataagrupada20<-datapaul20nocovid  %>% group_by( periodo,tipocl) %>%
  summarise(soles=sum(subtotal/1000000),cumplimiento=as.numeric( sprintf("%.2f",sum(subtotal)/sum(ppsol)*100)))

# estos graficos no estan mal solo que no son los slides en si
# dataagrupada20  %>%
#   ggplot(mapping=aes(x=periodo,y=cumplimiento,color=tipocl))+
#   geom_line(size=1.3)+
#   geom_point(color="black",size=2)+theme_light()+labs(y="% de cumplimiento")+facet_grid(cols=vars(tipocl))+
#   theme(legend.position="none")+geom_hline(yintercept = 100,color="red",linetype="dashed")
# 
# dataagrupada20  %>%
#   ggplot(mapping=aes(x=periodo,y=cumplimiento,color=tipocl))+
#   geom_line(size=1.3)+geom_line(data=dataagrupada,mapping = aes(x=dataagrupada20$periodo,y=cumplimiento))+
#   geom_point(color="black",size=2)+theme_light()+labs(y="% de cumplimiento")+facet_grid(cols=vars(tipocl))+
#   theme(legend.position="none")+geom_hline(yintercept = 100,color="red",linetype="dashed")

#---------------------------------- tercer slide ----------------------------------#
# cumplimiento por producto acumulado a diciembre

pres_2 <- read_pptx() %>%
  add_slide() %>% 
  ph_with(value = "Hello world", location = ph_location_type(type = "title")) %>% 
  ph_with(value = head(iris), location = ph_location_type(type = "body")) 

print(pres_2, target = "pptx_example_2.pptx")
