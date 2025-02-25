library(tidyverse)
library(lattice)
library(readxl)
library (lme4)

setwd("C:/Users/battl/Dropbox (Personal)/Shrub allometry/Data")

in.file <- read_xlsx("Shrub_Allometry_Data_27October2020.xlsx")


ggplot (in.file, aes(x = log(CA.m2), y = log(AGL.g)))+
  geom_point()+
  geom_smooth()

chk.spp.3 <- lm(log(AGL.g)~log(CA.m2)* Species, in.file)

chk.spp.2 <- lm(log(AGL.g)~log(CA.m2)+ Species, in.file)


chk.spp.1 <- lm(log(AGL.g)~log(CA.m2), in.file)

AIC (chk.spp.1, chk.spp.2, chk.spp.3)




in.McGinnis <- read_xlsx("McGinnis_R1.xlsx", sheet = "R_data")
#should be 153 obs

in.Lutz.plant <- read_csv("Lutz_fd_plant_jjb.csv")
in.Lutz.biomass <- read_csv("Lutz_ShrubBiomass_jjb.csv")
in.Lutz.biomass <- mutate (in.Lutz.biomass, WOOD_mass= as.double (WOOD_mass),
                           BARK_mass = as.double(BARK_mass))
#should be 171 obs
in.Lutz <- left_join(in.Lutz.plant, in.Lutz.biomass, by = c("PlantID"="PlantID"))

in.Huff<-read_csv("Huff_R1.csv")
#should be 180 obs

in.Battles <- read_csv("Battles_R1.csv")
#should be 30 obs

#### Parse McGinnis
parse.McGinnis <- select (in.McGinnis, SppAllom, Site, Author, D1.cm, D2.cm, H.cm, Vol.cm3)

###parse Huff
parse.Huff<- select (in.Huff, SppAllom, Site, Author, hh, CW1.cm, CW2.cm, volume,Dbio3.g, Dbio1_3.g, 
                    Dbio25_1.g, Dbio0_25.g, Leaf.g)
names.fuel <- c("Species", "Site", "Author", "Hgt.cm", "CW1.cm", "CW2.cm", "Vol.cm3","Dbio3.g", 
                 "Dbio1_3.g", "Dbio25_1.g", "Dbio0_25.g", "Leaf.g")

names(parse.Huff)<-names.fuel


parse.McGinnis <- mutate (in.McGinnis, Dbio3.g=0)
parse.McGinnis <- select (parse.McGinnis, SppAllom, Site, Author, H.cm, D1.cm, D2.cm,Vol.cm3, 
                          Dbio3.g, hundred.hour.live.g, ten.hour.live.g, one.hour.live.g, 
                          leaves.live.g)
names (parse.McGinnis)<-names.fuel

fuel.dat <- rbind.data.frame (parse.McGinnis, parse.Huff)
fuel.dat <- fuel.dat %>% replace (is.na(.),0)

fuel.dat <- mutate (fuel.dat, Hgt.m = Hgt.cm/100, CW1.m=CW1.cm/100, CW2.m = CW2.cm/100,
                    Stem.g = Dbio3.g + Dbio1_3.g + Dbio25_1.g + Dbio0_25.g, 
                    AGL.g = Leaf.g + Stem.g)

fuel.sum.dat <- select (fuel.dat, Species, Site, Author, Hgt.m, CW1.m, CW2.m, Leaf.g, Stem.g, AGL.g)


check.fuel <- fuel.sum.dat %>%
  group_by (Species, Author) %>%
  summarize (CW.x = mean (CW1.m), AGL.x = mean (AGL.g))

### parse Lutz
parse.Lutz <- in.Lutz%>% replace (is.na (.),0)
parse.Lutz <- mutate (parse.Lutz, Stem.g = WOOD_mass+BARK_mass)
parse.Lutz <- select (parse.Lutz, Spp4, Site, Author,Length_m,Crown_NS_m, Crown_EW_m,
                      FOLIAGE_mass,Stem.g, TOTAL_ABOVE_GROUND_biomass)
 
names.Lutz <- c("Species", "Site", "Author", "Hgt.m","CW1.m", "CW2.m","Leaf.g", "Stem.g", 
                 "AGL.g")
names (parse.Lutz)<- names.Lutz

parse.Lutz

###
allom.3 <- rbind.data.frame (fuel.sum.dat, parse.Lutz)
allom.3 <- mutate (allom.3, CA.m2 = ((CW1.m*CW2.m)/4)^2*pi)
names (allom.3)


### parse Battles

parse.Battles <- mutate (in.Battles, Species=Spp4, Hgt.m = NA, CW1.m = NA, CW2.m=NA, Leaf.g=NA, Stem.g = NA, 
                         AGL.g = AGL_kg*1000, CA.m2 = area_m2)
parse.Battles <- select (parse.Battles, Species, Site, Author, Hgt.m, CW1.m, CW2.m, Leaf.g,
                         Stem.g, AGL.g, CA.m2)

allom.4 <- rbind.data.frame (allom.3, parse.Battles)

allom.4 <- filter (allom.4, AGL.g>0)
allom.4 <- filter (allom.4, CA.m2>0)

allom.spp<-allom.4 %>%
  group_by (Species,Author) %>%
  summarize (N = n()) %>%
  ungroup
  
pick.spp <- c("ARPA", "ARVI","CECO","RISP", "CEIN", "CEVE", "CHSE","SYMP")

allom.spp.8 <- filter (allom.4, Species %in% pick.spp)

xyplot (log(AGL.g)~log(CA.m2)|Species, allom.spp.8)


see.full <-lm (log(AGL.g) ~ log(CA.m2)*Site, data = allom.spp.8)
see.add <-lm (log(AGL.g) ~ log(CA.m2)+Site, data = allom.spp.8)
see.nosite <-lm (log(AGL.g) ~ log(CA.m2), data = allom.spp.8)
AIC (see.full, see.add, see.nosite)

summary (see.nosite)
summary (see.full)
summary (see.add)


allom.spp.8i <- mutate(allom.spp.8, 
  iARPA = ifelse(Species == "ARPA", 1,0),
  iARVI = ifelse(Species == "ARVI", 1,0),
  iCECO = ifelse(Species == "CECE", 1,0),
  
  
  Species == "CECO" ~1,
  Species == "RISP" ~1,
  Species == "CEIN" ~1,
  Species == "CECE" ~1,
  Species == "CHSE" ~1,
  Species == "SYMP"~ 1))
  

test.nls <- nls (AGL.g ~ a*CA.m2^b, 
           data = allom.spp.8,
           start = c(a=1000, b=0.6))
           

test.nlme <- nlme (AGL.g ~ a*CA.m2^b, 
                 data = allom.spp.8,
                 fixed = b,
                 random = a,
                 start = c(a=1000, b=0.6))


           fixed = B1
           
           write_csv(allom.sum, "see3.csv")
           
summary (lm (AGL.g~ CA.m2 + Species, data = allom.spp.8))


library (lme4)




power.f = deriv(~k + a*CA.m2^b, namevec=c('k', 'a', 'b'), 
                function.arg=c('CA.m2','k', 'a','b'))

fit.nlmer = nlmer(AGL.g ~ power.f(CA.m2, k, a, b) ~ k|Species, 
                  start=list(nlpars=c(k=0.2, a=15, b=0.6)), data=allom.spp.8)


### example
x = 1:100
y1 = 7 + 2 * x^0.4 + rnorm(100); plot(y1, main='y1')  # unit 1
y2 = 4 + 2 * x^0.4 + rnorm(100); plot(y2, main='y2')  # unit 2
y3 = 1 + 2 * x^0.4 + rnorm(100); plot(y3, main='y3')  # unit 3
D = data.frame(y=c(y1, y2, y3), id=rep(c(1,2,3), each=100), time=rep(x, 3))
plot(y ~ time, D, main='all')  # combined data


power.f = deriv(~k + a*time^b, namevec=c('k', 'a', 'b'), 
                function.arg=c('time','k', 'a','b'))

lmer = nlmer(y ~ power.f(time, k, a, b) ~ k|id, 
             start=list(nlpars=c(k=2, a=1, b=1)), data=D)



first.pass.power<-
  allom.spp.8%>%
  select(Species,AGL.g,CA.m2) %>%
  split(.$Species) %>%
  map(~nls (AGL.g ~ a*CA.m2^b,start = c(a=1000, b=0.6), data=.))%>%
# map(~ lm(log(AGL.g) ~ log(CA.m2), data=.)) %>%
  map_dfr(tidy, .id = "Species")



df %>% 
  group_by(key) %>% 
  nest() %>% 
  mutate(models = map(data, ~ lm(y~., data = .x))) %>% 
  mutate(summaries = map(models, glance)) %>% 
  unnest(summaries)

allom.spp.8 %>%
  group_by (Species) %>%
  nest() %>%
  mutate (models =  map (data, ~ lm (log(AGL.g)~log(CA.m2))))%>%
  mutate(summaries = map(models, glance)) %>% 
  unnest(summaries)



in.Harrington <- read_csv("Harrington_R1.csv")

in.Harrington <- mutate (in.Harrington, AGL.g = AGL.kg*1000)

NODE.nls <- nls (AGL.g ~ a*CA.m2^b, 
                     data = allom.spp.8,
                     start = c(a=1000, b=0.6))
