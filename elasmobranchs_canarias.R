
################Canary Islands batoids; code by Fernando Tuya################
library(tidyverse)
library(ggpubr)
library(ggthemes)
library(ggstatsplot)
library(ggmap)
library(fishualize)

##############################ROV data######################
#read data "ROV" 
data_ROV=read.table("./data/ROVs.txt", sep="\t", header=TRUE)
data_ROV[is.na(data_ROV)] = 0
View(data_ROV)

# reorder data so the three groups are arranged following their geography
data1_ROV=data_ROV %>%
  mutate(Grupo = fct_relevel(Grupo, "Western", "Central", "Eastern"))

# plotting data after transformation using box plots  
data1_ROV%>%
  mutate(Total_batoids_raiz= sqrt(Total_batoids)) %>%
  as_tibble(data1_ROV) %>%
  ggstatsplot::ggbetweenstats(Grupo, Total_batoids_raiz,
                              plot.type = "box",type = "parametric", pairwise.comparisons = TRUE, pairwise.display = "significant",p.adjust.method = "bonferroni",
                              xlab = " ", ylab = "Total abundance",
                              ggtheme = Theme1) + theme_bw(base_size = 20) +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())
ggsave("ROV.tiff")

#Data analysis via GLM
library(MASS)
model_rov = glm.nb(Total_batoids~Grupo, data=data_ROV)
summary(model_rov)
attach(data_ROV)
Group=as.factor(Group)
Grupo2=relevel(Group, ref="Western")
model_rov = glm.nb(Total_batoids~Grupo2, data=data_ROV)
summary(model_rov)
Grupo3=relevel(Group, ref="Eastern")
model_rov = glm.nb(Total_batoids~Grupo3, data=data_ROV)
summary(model_rov)

#plotting species per group
#read data "UVC_species"
data_ROVs_species=read.table("./data/ROVs_species.txt", sep="\t", header=TRUE)
View(data_ROVs_species)
data_ROVs_species%>%
  ggbarplot(x = "Species", y = "Abundance",
            fill = "Group") +
            coord_flip() +
  xlab(" ") + ylab(" Total number of observations") +
  theme_bw(base_size = 20) +
  theme(axis.text.y = element_text(face = "italic")) +
theme(legend.position="bottom") +
  theme(legend.title=element_blank())
ggsave("ROVs_species.tiff")

#mapping observaions 
#get basemap from OSM - export
canary_map=get_stamenmap(
  bbox= c(left= -18.572, bottom= 26.693 , right= -12.057, top=30.051),
  maptype= "watercolor", zoom = 8)
ggmap(canary_map) 

data_map=read.table("./data/batoids.txt", sep="\t", header=TRUE)

ggmap(canary_map) + 
  geom_point(data= data_map,
             aes(x= Longitude, y= Latitude),
             size = 1.5) + theme_map ()

#####################UVC data #############################
data_UVC=read.table("./data/UVC.txt", sep="\t", header=TRUE)
data_UVC[is.na(data_UVC)] = 0

# reorder data so the three groups are arranged following their geography and plotting
data_UVC %>%
  mutate(Group = fct_relevel(Group, "Western", "Central", "Eastern"))%>%
  ggstatsplot::ggbetweenstats(Group, Abun,
                              plot.type = "box",type = "parametric", pairwise.comparisons = FALSE, pairwise.display = "significant",p.adjust.method = "bonferroni",
                              xlab = " ", ylab = "Total abundance",
                              ggtheme = Theme1) + theme_bw(base_size = 20) +
theme(legend.position="bottom") +
theme(legend.title=element_blank())
ggsave("uvc.tiff")

# data analysis with only island groups
library(MASS)
model_uvc = glm.nb(Abun~Group, data=data_UVC)
summary(model_uvc)
attach(data_UVC)
Group=as.factor(Group)
Group2=relevel(Group, ref="Western")
model_uvc = glm.nb(Abun~Group2, data=data_UVC)
summary(model_uvc)

# data analysis with 4 predictors
model = glm.nb(Abun~Group + Habitat + Shelf.area, data=data_UVC)
summary(model)
plot(model)
write.csv(model, file = "model.csv")
attach(data_UVC)
Group=as.factor(Group)
Group2=relevel(Group, ref="Western")
model_uvc = glm.nb(Abun~Group2, data=data_UVC)
summary(model)

# data analysis without fish farm data and data analysis
data_nojaulas=data_UVC %>%
  filter(Habitat != "Farm (c.nodosa y green algae)") %>%
  filter(Habitat != "Farms fondo arenoso")

model_uvc_nojaulas = glm.nb(Abun~Group, data=data_nojaulas)
summary(model_uvc_nojaulas)
attach(model_uvc_nojaulas)
Group=as.factor(Group)
Group2=relevel(Group, ref="Eastern")
model_uvc_nojaulas = glm.nb(Abun~Group2, data=data_nojaulas)
summary(model_uvc_nojaulas)

write.csv(data_nojaulas, "data.csv")

# plotting species per island group (total abundances)
data_UVC_species=read.table("./data/UVC_species.txt", sep="\t", header=TRUE)

data_UVC_species%>%
  ggbarplot(x = "Species", y = "Abundance",
          fill = "Group",
          sort.by.groups = TRUE) +     
          coord_flip() +
  xlab(" ") + ylab(" Total number of observations") +
  theme_bw(base_size = 20) +
  theme(axis.text.y = element_text(face = "italic")) +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  geom_text(x=8, y=60, label="(a)", size= 8)
ggsave("uvc_species_a.tiff")  

# plotting species per island group (relative abundances)
data_UVC_species_relative=read.table("./data/UVC_species_relative.txt", sep="\t", header=TRUE)

data_UVC_species_relative%>%
  ggbarplot(x = "Species", y = "Relative_abundance",
            fill = "Group",
            sort.by.groups = TRUE) +     
  coord_flip() +
  xlab(" ") + ylab("Relative number of observations") +
  theme_bw(base_size = 20) +
  theme(axis.text.y = element_text(face = "italic")) +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  geom_text(x=8, y=50, label="(b)", size= 8)
ggsave("uvc_species_b.tiff") 

#Abundances per habitat on the central islands 
data_UVC%>%
  filter(Taxa != 0) %>%
  filter(Group == "Central") %>% 
  group_by(Habitat) %>%
  count()

#Abundances per habitat on the eastern islands
data_UVC%>%
  filter(Taxa != 0) %>%
  filter(Group == "Eastern") %>% 
  group_by(Habitat) %>%
  count()

#Abundances per habitat on the western islands
data_UVC%>%
  filter(Taxa != 0) %>%
  filter(Group == "Western") %>% 
  group_by(Habitat) %>%
  count()

data_UVC_habitats %>%
  group_by(Habitat)%>%
  count()

#testing of proportions
prop.test(x=c(15, 65, 9, 35), n=c(124, 124, 124, 124))

#Plotting data per habitat
data_habitat=read.table("./data/UVC_habitats.txt", sep="\t", header=TRUE)  
data_habitat %>% 
ggbarplot(x = "Group", y = "Proportion",
            fill = "Habitat",
            sort.by.groups = TRUE,
            x.text.angle = 90) +     
  coord_flip() +
  xlab(" ") + ylab("Proportion of individuals (%)") +
  theme_bw(base_size = 20) +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())
ggsave("uvc_habitat.tiff")

#nº sites per island group

data_UVC %>%
  filter(Group =="Eastern") %>%
  group_by(Site) %>%
  count()
data_UVC %>%
  filter(Group =="Western") %>%
  group_by(Site) %>%
  count()
data_UVC %>%
  filter(Group =="Central") %>%
  group_by(Site) %>%
  count()

#rarefraction curves: species density
rare=read.table("./data/Rarefraction.txt", sep="\t", header=TRUE)
ggplot(data=rare, aes(x=Samples, y=S, group= Group)) + 
  geom_line(size = 2, aes(color=Group)) +
  theme_classic() + xlab("Counts") + ylab("Number of species") +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank()) +
  geom_text(x=0, y=8.5, label="(a)", size= 8) +
  geom_ribbon(aes(ymin = Low, ymax = High), alpha = 0.1)
ggsave("rarefraction.tiff")

#rarefraction curves: species richness
ggplot(data=rare, aes(x=Ind, y=S, group= Group)) + 
  geom_line(size = 2, aes(color=Group)) +
  theme_classic() + xlab("Number of observed individuals") + ylab("Number of species") +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank()) +
  geom_text(x=0, y=8.5, label="(b)", size= 8) +
  geom_ribbon(aes(ymin = Low, ymax = High), alpha = 0.1)
ggsave("rarefraction2.tiff")

#####################PROMAR community science data###############
data_Promar=read.table("./data/Promar.txt", sep="\t", header=TRUE)

# reorder data so the three groups are arranged following their geography and plotting
plot=data_Promar %>%
  group_by(Species, Group)%>%
  summarise(Total = n())%>%
  ggbarplot(x = "Species", y = "Total",
            fill = "Group",
            sort.by.groups = TRUE,
            x.text.angle = 90) +     
  coord_flip() +
  xlab(" ") + ylab(" Total number of observations") +
  theme_bw(base_size = 20) +
  theme(axis.text.y = element_text(face = "italic")) +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())
  ggsave("promar.tiff")

data_Promar %>%
  group_by(Group)%>%
  count()

#testing of proportions
prop.test(x=c(93, 258, 10), n=c(361, 361, 361))
