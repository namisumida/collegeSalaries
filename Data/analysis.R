# Import data
degrees <- read.csv(file.choose())
collegetypes <- read.csv(file.choose())
collegeregions <- read.csv(file.choose())

# Rename colnames
colnames(degrees) <- c("major", "starting_median", "mid_median", "starting_mid_change", "mid_10", "mid_25", "mid_75", "mid_90")
colnames(collegetypes) <- c("school", "school_type", "starting_median", "mid_median", "mid_10", "mid_25", "mid_75", "mid_90")
colnames(collegeregions) <- c("school", "school_region", "starting_median", "mid_median", "mid_10", "mid_25", "mid_75", "mid_90")

# Reformat collegetypes - it has the same school listed twice if it has multiple types 
collegetypes2 <- unique(collegetypes[c(1,3:8)])
collegetypes2$school <- as.character(collegetypes2$school)
i = 1;
for (i in 1:nrow(collegetypes2)) {
  currSchool <- collegetypes2$school[i]
  origRow <- which(collegetypes$school==currSchool)
  collegetypes2$type_engineering[i] <- ifelse(collegetypes$school_type[origRow]=="Engineering", 1, 0)
  collegetypes2$type_ivy[i] <- ifelse(collegetypes$school_type[origRow]=="Ivy League", 1, 0)
  collegetypes2$type_libarts[i] <- ifelse(collegetypes$school_type[origRow]=="Liberal Arts", 1, 0)
  collegetypes2$type_party[i] <- ifelse(collegetypes$school_type[origRow]=="Party", 1, 0)
  collegetypes2$type_state[i] <- ifelse(collegetypes$school_type[origRow]=="State", 1, 0)
}
summary(degrees)
summary(collegetypes)
summary(collegeregions)

# Merge types and regions
colleges <- merge(collegetypes2, collegeregions[c('school', 'school_region')], by="school", all.x=TRUE)
summary(colleges)

# Remove dollar signs and turn into ints
for (i in 2:7) {
  colleges[,i] <- as.numeric(gsub("\\$|,", "", colleges[,i]))
}
for (i in c(2,3,5,6,7,8)) {
  degrees[,i] <- as.numeric(gsub("\\$|,", "", degrees[,i]))
}

# Analysis of colleges
# Type
output <- list()
type_colindex <- c(8,9,10,11,12) # engineering, ivy, liberal alrts, party, state
for (type in type_colindex) {
  sub <- subset(colleges, colleges[,type]==1)
  name <- ifelse(type==8, "engineering", ifelse(type==9, "ivy", ifelse(type==10, "liberal arts", ifelse(type==11, "party", "state"))))
  output_list <- c(name, 
                   nrow(sub),
                   mean(sub$starting_median, na.rm=TRUE), 
                   mean(sub$mid_10, na.rm=TRUE), 
                   mean(sub$mid_25, na.rm=TRUE),
                   mean(sub$mid_median, na.rm=TRUE),
                   mean(sub$mid_75, na.rm=TRUE),
                   mean(sub$mid_90, na.rm=TRUE))
  output <- rbind(output, output_list)
}
# Region
colleges$school_region <- as.character(colleges$school_region)
for (region in unique(colleges$school_region)) {
  sub <- subset(colleges, colleges$school_region==region)
  name <- region
  output_list <- c(name, 
                   nrow(sub),
                   mean(sub$starting_median, na.rm=TRUE), 
                   mean(sub$mid_10, na.rm=TRUE), 
                   mean(sub$mid_25, na.rm=TRUE),
                   mean(sub$mid_median, na.rm=TRUE),
                   mean(sub$mid_75, na.rm=TRUE),
                   mean(sub$mid_90, na.rm=TRUE))
  output <- rbind(output, output_list)
}
# Region AND type
for (type in type_colindex) {
  type_sub <- subset(colleges, colleges[,type]==1)
  type_name <- ifelse(type==8, "engineering", ifelse(type==9, "ivy", ifelse(type==10, "liberal arts", ifelse(type==11, "party", "state"))))
  for (region in unique(colleges$school_region)) {
    sub <- subset(type_sub, type_sub$school_region==region)
    region_name <- region
    output_list <- c(paste(type_name, region_name, sep="_"), 
                     nrow(sub),
                     mean(sub$starting_median, na.rm=TRUE), 
                     mean(sub$mid_10, na.rm=TRUE), 
                     mean(sub$mid_25, na.rm=TRUE),
                     mean(sub$mid_median, na.rm=TRUE),
                     mean(sub$mid_75, na.rm=TRUE),
                     mean(sub$mid_90, na.rm=TRUE))
    output <- rbind(output, output_list)
  }
}
