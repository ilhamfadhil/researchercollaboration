path <- file.path("I:", "R", "Network Analysis", "Publication Collaboration")

# import data for peer reviewed journal
prj_csv <- read.csv(paste(path, "Peer-reviewed Journals.csv", sep = "/"),
                    header = T,
                    na.strings = "",
                    stringsAsFactors = F)
con_csv <- read.csv(paste(path, "Conference Proceedings.csv", sep = "/"),
                    header = T,
                    na.strings = "",
                    stringsAsFactors = F)

# using only complete observation
prj_name <- prj_csv[complete.cases(prj_csv[,1:2]),1:2]
prj_name$authors <- as.character(prj_name$authors)

con_name <- con_csv[complete.cases(con_csv[,1:2]), 1:2]
con_name$authors <- as.character(con_name$authors)

# split author's name using comma and create nested list
prj <- strsplit(prj_name$authors, ",")
con <- strsplit(con_name$authors, ",")

# create edgelist and stored in object by iteration in nested list

aut_prj <- matrix(ncol = 2,nrow = 0)
for(i in seq(length(prj))) {
  if (length(prj[[i]]) > 2) {
    temp <- combn(prj[[i]], 2, simplify = F)
    temp <- matrix(unlist(temp), ncol = 2, byrow = T)
    aut_prj <- rbind(aut_prj, temp)
  }
  else if (length(prj[[i]]) == 2) {
    temp <- rbind(rev(prj[[i]]), prj[[i]])
    aut_prj <- rbind(aut_prj, temp)
  }
}

aut_con <- matrix(ncol = 2,nrow = 0)
for(i in seq(length(con))) {
  if (length(con[[i]]) > 2) {
    temp <- combn(con[[i]], 2, simplify = F)
    temp <- matrix(unlist(temp), ncol = 2, byrow = T)
    aut_con <- rbind(aut_con, temp)
  }
  else if (length(con[[i]]) == 2) {
    temp <- rbind(rev(con[[i]]), con[[i]])
    aut_con <- rbind(aut_con, temp)
  }
}

# tidying & filtering same author's edge
aut_prjdata <- as.data.frame(aut_prj)
names(aut_prjdata) <- c("Source", "Target")
aut_prjdata$Source <- as.character(aut_prjdata$Source)
aut_prjdata$Target <- as.character(aut_prjdata$Target)
aut_prjdata_clean <- aut_prjdata[aut_prjdata$Source != aut_prjdata$Target, ] 

aut_condata <- as.data.frame(aut_con)
names(aut_condata) <- c("Source", "Target")
aut_condata$Source <- as.character(aut_condata$Source)
aut_condata$Target <- as.character(aut_condata$Target)
aut_condata_clean <- aut_condata[aut_condata$Source != aut_condata$Target, ]

# Create combination data of conference and peer reviewed journal
aut_condata_clean$Type <- c("Conference")
aut_prjdata_clean$Type <- c("Peer reviewed Journal")
aut_totdata <- rbind(aut_prjdata_clean,aut_condata_clean)
write.csv(aut_totdata, file = paste(path, "edgelist_total.csv", sep = "/"), row.names = F)