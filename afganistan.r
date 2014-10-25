require(XML)
require(lubridate)
require(ggplot2)
print("Pages:")
url <- "http://securitydata.newamerica.net/drones/pakistan/analysis"
table <- readHTMLTable(url)
data <- table[[1]]
print(1)

for(i in 1:19) {
  url <- paste("http://securitydata.newamerica.net/drones/pakistan/analysis?page=",
               i, sep = "")
  table <- readHTMLTable(url)
  data <- rbind(data, table[[1]])
  print(i+1)
}


names(data) <- c("Date", "Location", "Total killed", "Militants killed",
                 "Civilians killed", "Unknown killed", "Target organization")

#Sys.setlocale(category="LC_TIME", locale="en_US.UTF-8")
Sys.setlocale("LC_TIME", "English")

years <- sapply(data$Date, 
            function(x) year(as.Date(as.character(x), format = "%A, %B %d, %Y")))

days <- sapply(data$Date, 
               function(x) day(as.Date(as.character(x), format = "%A, %B %d, %Y")))

months <- sapply(data$Date, 
                  function(x) month(as.Date(as.character(x), format = "%A, %B %d, %Y")))
            
data$years <- years
data$days <- days
data$months <- months
data$Date <- NULL
                      
#min_max killing                      
for (i in c("Total killed", "Militants killed","Civilians killed", "Unknown killed")){
  data[[i]]<- gsub("\\.| civilians| unknown| militants| killed| total|Between ", "", data[[i]]);
  data[[i]] <- gsub(" and ", ",", data[[i]]);
  mmin <- vector();
  mmax <- vector();
  lapply(data[[i]], function(x) { 
    q <- (strsplit(x,",")); 
    mmin <<- c(mmin, q[[1]][1]);
    mmax <<- c(mmax, q[[1]][2]);
  } );
  data[[paste(i,"min")]] <- mmin;
  data[[paste(i,"max")]] <- mmax;
}

data$"Total killed" <- NULL
data$"Militants killed" <- NULL
data$"Civilians killed" <- NULL
data$"Unknown killed" <- NULL
 
for(i in match("Total killed min",names(data)):ncol(data)) {
  data[,i] <- as.numeric(data[,i])
}


table(data$"Target organization")
table(data$years)
                      
## Graphics
qplot(data$"Target organization", data$month, geom=c("boxplot", "jitter"), xlab="Target organization", ylab="Months")
qplot(data$"Target organization", data$years, geom=c("boxplot", "jitter"),xlab="Target organization", ylab="Years")
qplot(data$"Target organization", data$days, geom=c("boxplot", "jitter"), xlab="Target organization", ylab="Days of months")
qplot(data$"Target organization", (data$"Total killed max" + data$"Total killed min")/2, 
                                   geom=c("boxplot", "jitter"), xlab="Target organization", ylab="Approximate number of total killed")
qplot(data$"Target organization", (data$"Militants killed max" + data$"Militants killed min")/2, 
      geom=c("boxplot", "jitter"), xlab="Target organization", ylab="Approximate number of militants killed")

qplot(data$"Target organization", (data$"Civilians killed max" + data$"Civilians killed min")/2, 
      geom=c("boxplot", "jitter"), xlab="Target organization", ylab="Approximate number of civilians killed")
                      