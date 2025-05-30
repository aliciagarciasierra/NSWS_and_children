
##################################################################
######### MERGE CHILDREN AND MOTHER ######################
###################################################################

#------------------------------------------------------------- #
#---------------- OPEN DATASETS ---------------- #
#------------------------------------------------------------- #

load("data_mothers.Rda")
mothers <- data_long %>%
  rename(motherID = ID)%>%
  rename(sex_mother = sex)%>%
  rename(race_mother = race)

load("data_children.Rda")
children<- data_long %>%
  rename(sex_child = sex)%>%
  rename(race_child = race)%>%
  rename(birthyear_child = birthyear)

#------------------------------------------------------------- #
#---------------- MERGE ---------------- #
#------------------------------------------------------------- #

# Merge
merged_data<-merge(mothers, children, by.x=c("motherID", "year"), by.y=c("motherID", "year"),all.x=TRUE)

#Select only women with children
data<- merged_data[ which(merged_data$childID!="NA"), ]

#Check number of observations

n_distinct(data$childID) #10234 children
n_distinct(data$motherID) #4297 mothers

# Number of children per family
children %>%
  distinct(motherID, childID) %>%
  count(motherID, name = "n_children") %>%
  count(n_children, name = "n_families")
      
#------------------------------------------------------------- #
#---------------- SAVE DATA ---------------- #
#------------------------------------------------------------- #

save(data, file="data.Rda")
