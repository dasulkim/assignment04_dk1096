library(readxl)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(tibble)
library(WDI)
library(dplyr)
library(ggrepel)
library(dp)
library(sjPlot)
library(writexl)
penn <- read_xlsx("pwt100.xlsx", sheet = 3)
wdi <- read_xlsx("data/WDI.xlsx", sheet =1)
polity <- read_xls("data/p5v2018.xls")
test <- read_xlsx("data/informaldata.xlsx")
  
informal1 <- read_xlsx("data/informal.xlsx", sheet =2) 
informal1<- informal1 %>%     
  pivot_longer(cols = starts_with(c("19", "20")), 
             names_to = "year",
             values_to = "DGE_p")

informal2 <- read_xlsx("data/informal.xlsx", sheet =3)
  informal2<- informal2 %>%     
  pivot_longer(cols = starts_with(c("19", "20")), 
               names_to = "year",
               values_to = "MIMIC_p")

informal3 <- read_xlsx("data/informal.xlsx", sheet =4)
  informal3<- informal3 %>%     
  pivot_longer(cols = starts_with(c("19", "20")), 
               names_to = "year",
               values_to = "SEMP_p")

informal4 <- read_xlsx("data/informal.xlsx", sheet =5)
  informal4<- informal4 %>%     
  pivot_longer(cols = starts_with(c("19", "20")), 
               names_to = "year",
               values_to = "Pension_p")

informal5 <- read_xlsx("data/informal.xlsx", sheet =6) 
  informal5<- informal5 %>%     
  pivot_longer(cols = starts_with(c("19", "20")), 
               names_to = "year",
               values_to = "Infemp_p")

informal6 <- read_xlsx("data/informal.xlsx", sheet =7)
  informal6<- informal6 %>%     
  pivot_longer(cols = starts_with(c("19", "20")), 
               names_to = "year",
               values_to = "Infsize_p")

informal7 <- read_xlsx("data/informal.xlsx", sheet =8)
  informal7<- informal7 %>%     
  pivot_longer(cols = starts_with(c("19", "20")), 
               names_to = "year",
               values_to = "WBentp1")

informal8 <- read_xlsx("data/informal.xlsx", sheet =9)
  informal8<- informal8 %>%     
  pivot_longer(cols = starts_with(c("19", "20")), 
               names_to = "year",
               values_to = "WBentp2")

  informal9 <- read_xlsx("data/informal.xlsx", sheet =10)
  informal9<- informal9 %>%     
    pivot_longer(cols = starts_with(c("19", "20")), 
                 names_to = "year",
                 values_to = "WBentp3")
  
  informal10 <- read_xlsx("data/informal.xlsx", sheet =11)
  informal10<- informal10 %>%     
    pivot_longer(cols = starts_with(c("19", "20")), 
                 names_to = "year",
                 values_to = "WBentp4")
  
informal11 <- read_xlsx("data/informal.xlsx", sheet =12)
  informal11<- informal11 %>%     
    pivot_longer(cols = starts_with(c("19", "20")), 
                 names_to = "year",
                 values_to = "WVS")


informaldata <- left_join(x=informal1, y=informal2, by=c("year", "Code"), keep=FALSE)
informaldata <- left_join(x=informaldata, y=informal3, by=c("year", "Code"))
informaldata <- left_join(x=informaldata, y=informal4, by=c("year", "Code"))
informaldata <- left_join(x=informaldata, y=informal5, by=c("year", "Code"))
informaldata <- left_join(x=informaldata, y=informal6, by=c("year", "Code"))
informaldata <- left_join(x=informaldata, y=informal7, by=c("year", "Code"))
informaldata <- left_join(x=informaldata, y=informal8, by=c("year", "Code"))
informaldata <- left_join(x=informaldata, y=informal9, by=c("year", "Code"))
informaldata <- left_join(x=informaldata, y=informal10, by=c("year", "Code"))
informaldata <- left_join(x=informaldata, y=informal11, by=c("year", "Code"))

informaldata <- select(informaldata, c(-5, -7, -9, -11, -13, -15, -17, 
                                       -19, -21, -23))
                      
colnames(informaldata)[2] <- "scode"

informaldata <- as.data.frame(informaldata) %>% 
informaldata[, 3] <- as.numeric(unlist(informaldata$year))

new <- left_join(x = new, y = informaldata, by= c("year", "scode"))

colnames(new)[4] <- "country"


######-------------------------------------------------------------------------

new<- new[, !duplicated(colnames(new))]


WDI <- WDI(
  country = "all",
  indicator = c("DT.ODA.OATL.KD",
  "DT.ODA.ODAT.CD",
  "DT.ODA.ALLD.KD",
  "DC.DAC.TOTL.CD",
  "GC.DOD.TOTL.GD.ZS",
  "GC.XPN.TOTL.GD.ZS",
  "NE.EXP.GNFS.ZS",
  "BX.KLT.DINV.WD.GD.ZS",
  "NY.GDP.MKTP.KD",
  "NY.GDP.PCAP.KD",
  "NY.GDP.PCAP.KD.ZG",
  "NE.CON.GOVT.ZS",
  "SE.XPD.TOTL.GD.ZS",
  "MS.MIL.XPND.GD.ZS",
  "NY.GDP.PETR.RT.ZS",
  "BX.TRF.PWKR.DT.GD.ZS",
 "GC.TAX.TOTL.GD.ZS",
  "SE.PRM.TENR",
  "SP.URB.TOTL.IN.ZS",
  "SP.POP.TOTL",
  "SI.POV.DDAY",
  "SI.POV.LMIC",
  "SI.POV.UMIC"),
  start = 1960,
  end = 2020,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en")
colnames(WDI)[27] <- "scode"

new <- left_join(x = WDI, y=polity, by=c("year", "scode"))
new <- left_join(x = new, y = informaldata, by = c("year", "scode"))


### Graph ####
## 1. geom_boxplot

g1 <- new %>% 
  drop_na(region, MIMIC_p) %>% 
  ggplot(aes(x = region, y = MIMIC_p, group=region, fill=region))+
  geom_boxplot() +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_brewer(palette="BuPu") +
  labs(x = "",
       y = "The Share of Informal Economy (% of GDP)",
       title = "The Share of Informal Economy by Region",
       subtitle = "Source: World Bank",
       caption = "The share of informal economy is calculated using multiple indicators multiple causes model-based (MIMIC) estimates of informal output (% of official GDP)"
  )

table(new$country.x)
## 2. geom_line
new %>% 
  mutate(oda_gdp = DT.ODA.ODAT.CD/NY.GDP.MKTP.KD*100) %>% 
  filter(country.x == "Mozambique", year >=1990) %>%
  drop_na(oda_gdp) %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = oda_gdp)) +
  geom_line(aes(y = MIMIC_p), color = "blue") +
  theme_minimal()+
  labs(x = "Year",
       y = "The Share of net ODA (black) and informal economy (blue) (% of GDP)",
       title = "The Share of ODA and Informal Economy in Ethiopia (% of GDP)",
       caption = "Source: World Bank")

#oda/gdp - mimic
new %>% 
  mutate(oda_gdp = DT.ODA.ODAT.CD/NY.GDP.MKTP.KD*100) %>% 
  filter(!(region==c("Aggregates","North America") & income=="High Income"), year==2010) %>% 
  group_by(country.x) %>% 
  summarize(odagdpm = mean(oda_gdp, na.rm=TRUE), 
            mimicm = mean(MIMIC_p, na.rm =TRUE)) %>% 
  ggplot(aes(x=odagdpm, y=mimicm))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlim(0, 20)


## 3. geom_point
new %>% 
  filter(region == "Sub-Saharan Africa",
         year ==2010,
         !(income == "High income")) %>% 
  mutate(pop = SP.POP.TOTL/1000000,
         oda_gdp = DT.ODA.ODAT.CD/NY.GDP.MKTP.KD*100) %>% 
  ggplot(aes(x = oda_gdp, y=DGE_p, col = income, size = pop)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  xlim(0, 70) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70))+
  theme_classic()+
  theme(plot.title = element_text(hjust=0.5),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        axis.ticks=element_blank(),
        panel.border=element_blank())+
  labs( x= "The Share of ODA (% of GDP)",
        y = "The Share of Informal Economy (DGE) (%of GDP)",
        size = "Population (in millions)",
        col = "Income Group",
        subtitle = "Source: World Bank",
        caption = "Dynamic general equilibrium model-based (DGE) estimates of informal output (% of official GDP)",
        title = "Informal Economy and ODA in Sub-Saharan Africa in 2010")

table(new$region)

## 4.geom_jitter, geom_smooth
 new %>% 
  filter(!(region=="Aggregates")) %>% 
  mutate(pop = SP.POP.TOTL/1000000) %>% 
  ggplot(aes(x=SE.PRM.TENR, y=Infsize_p)) +
  geom_jitter(aes(col=income, size=pop)) +
  geom_smooth(aes(col=income), method="lm", se=F)+
  theme_bw() +
  labs(x= "Primary School Enrollment Rate (% of primary school age children)",
       y = "The Share of Informal Economy (MIMIC) (%of GDP)",
       size = "Population (in millions)",
       col = "Income Group",
       subtitle = "Source: World Bank",
       caption = "The Share of Informal Economy: employment outside the formal sector (% of total employment; International Labour Organization; hamonized series)",
       title = "Informal Economy and Primary School Enrollment Rate from 1990 to 2000")
 

