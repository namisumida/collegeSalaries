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
