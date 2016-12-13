# import stops data and calculate one-way correlations

options(scipen=6) # display numbers up to 6 decimal points w/o scientific notation
options(stringsAsFactors=FALSE)



#library(MASS)
library(pscl) # for pseudo-R2

setwd(dirname(parent.frame(2)$ofile)) # set current working dir to source directory (only works when run as source)

df <- read.csv("stops-dataset-v7.csv", header=TRUE, sep=",", strip.white=TRUE)
df$Transit.Connections. <- NULL # get rid of the stations dataset 0 - 1 score for connections (we use a count instead)

###############################################
#  Calculate summary stats for all attributes
###############################################

# table.str <- function(obj) {
#   
#   res <- paste(names(obj)[1], ": ", obj[1], sep="")
#   for(k in 2:length(obj)) 
#     res <- paste(res, ", ", names(obj)[k], ": ", obj[k], sep="")
#   if(length(obj)==2 && names(obj)[1]=="0" && names(obj)[2]=="1") 
#     res <- paste("True: ", obj[2], ", False: ", obj[1], sep="")
#   
#   return (res)
# }
# 
# col.stats <- data.frame(name=character(0), type=character(0), mean=numeric(0), median=numeric(0), min=numeric(0), max=numeric(0))
# for(col in colnames(df)) {
#   if(!is.character(df[[col]])) {
#     if(is.numeric(df[[col]]) && min(df[[col]])>=0 && max(df[[col]])<=1 && length(levels(factor(df[[col]]))) < 10){
#       t <- "ord"
#       med <- table.str(table(df[[col]]))
#     }
#     else {
#       t <- typeof(df[[col]])
#       med <- median(df[[col]])
#     }
# #    if(length(table(df[[col]]))==2) {
# #      med <- paste(table(df[[col]])[1], "/", table(df[[col]])[2], sep="")
# #    } else
# 
#     col.stats <- rbind(col.stats, data.frame(
#       name=col, type=t, mean=round(mean(df[[col]]),2), median=med, min=min(df[[col]]), max=max(df[[col]])
#     ))  
#   }
# }
# write.csv(col.stats, file="output/attribute-stats.csv")


hist(df$Daily.Ridership, breaks=25, xlab="Average Daily Ridership", main="Histogram of Avg. Daily Ridership by Station\n(All Stations)", sub="N=75")

# remove anomalous stations - North side and CBD
north.side.stations <- c("Allegheny Station", "North Side")
cbd.stations <- c("Steel Plaza", "Gateway", "Wood Street") #cbd.stations <- c("First Avenue", "Steel Plaza", "Penn Station", "Gateway", "Wood Street")
df <- df[!(df$Station.Name %in% c(north.side.stations, cbd.stations)),]

hist(df$Daily.Ridership, xlab="Average Daily Ridership", main="Histogram of Avg. Daily Ridership by Station\n(Outliers Removed)", sub="N=70")

# what distribution does this look like?
library(MASS); library(fitdistrplus)
par(mfrow=c(1,1))
hist(df$Daily.Ridership, pch=20, breaks=7, prob=TRUE, xlab="Daily Ridership", main="Histogram of Daily Ridership")
fit <- fitdistr(df$Daily.Ridership, densfun="normal")
curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col="red", lwd=2, add=TRUE)
fit <- fitdistr(df$Daily.Ridership, densfun="negative binomial")
curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col="blue", lwd=2, add=TRUE)
legend("topright", inset=.05, title="Distributions", c("Normal", "Negative Binomial") , lwd=2, lty=c(1, 1, 1, 1, 2), col=c("red", "blue"))

# Eliminate station characteristics without any variation
for(i in names(df)) {
  if(length(table(df[[i]])) == 1) {
    print(paste("Removed attribute",i,"without variance from dataset."))
    df[[i]] <- NULL
  }
}

# for each ordered factor, create a box plot and get a one-way regression multiple correlation coefficient
# generate box and whiskers plots for all variables
pdf(file="output/pair-wise-plots.pdf", paper="letter", w=8, h=9)
par(mfrow=c(3,3))
par(mar = c(5.1, 4.1, 4.1, 2.1)) 
for(i in names(df[,5:ncol(df)])) {
  if(is.numeric(df[[i]]) && min(df[[i]])>=0 && max(df[[i]])<=1 && length(levels(factor(df[[i]]))) < 10) {
    boxplot(df$Daily.Ridership ~ ordered(df[[i]]), data = df, xlab=i, ylab="Daily Ridership")
  } else if(is.numeric(df[[i]])) {
    plot(df$Daily.Ridership ~ df[[i]], xlab=i, ylab="Daily Ridership")
  }
}
dev.off()
#file.show("correlations.pdf")
# corr.df <- data.frame(VarName=factor(), type=character(), MCC=numeric(), pval=numeric())
# for(i in names(df)[5:77]) {
#   #if(class(df[[i]])[1] %in% c("ordered","logical") ) {
#   if(is.numeric(df[[i]]) && min(df[[i]])>=0 && max(df[[i]])<=1 && length(levels(factor(df[[i]]))) < 10) {
#     boxplot(df$Daily.Ridership ~ ordered(df[[i]]), data = df, xlab=i, ylab="Daily Ridership")
# 
#     if(length(levels(df[[i]]))==1){
#       corr.df <- rbind(corr.df, data.frame(VarName=i, type="ord.factor", MCC=0, pval=NA))
#     } else {
#       model.lm <- lm(df$Daily.Ridership ~ df[[i]], data = df)
#       rsq <- summary(model.lm)$r.squared
#       a <- anova(model.lm)
#       p <- a[["Pr(>F)"]][1]
#       p <- round(p, digits = 5)
#       cat(paste(i, ' coefficient of determination'), sqrt(rsq))
#       corr.df <- rbind(corr.df, data.frame(VarName=i, type=typeof(df[[i]]), MCC=sqrt(rsq), pval=p))
#     }
#       
#   } else if(is.numeric(df[[i]])) {
#     plot(df$Daily.Ridership ~ df[[i]], xlab=i, ylab="Daily Ridership")
# 
#     c <- cor(df$Daily.Ridership, df[[i]], use="pairwise.complete.obs")
#     a <- aov(df$Daily.Ridership ~ df[[i]], data = df)
#     p <- summary(a)[[1]][["Pr(>F)"]][1]
#     p <- round(p, digits = 5)
#     corr.df <- rbind(corr.df, data.frame(VarName=i, type="numeric", MCC=sqrt(abs(c)), pval=p))
#   }
# }


# before first pass cutoff
#corr.df <- corr.df[order(corr.df$MCC), ]
#print(corr.df)

# elim variables where MCC less than 0.25
# first.pass.elimination <- corr.df[corr.df$MCC < 0.25,]
# print("First pass characteristic elimination for lack of correllation with outcome variable:")
# print(first.pass.elimination$VarName)
# fp.corr.df <- corr.df[corr.df$MCC > 0.25,]
# 
# remaining.variables <- levels(as.factor(fp.corr.df$VarName))
# df <- df[, c('Station.Name', 'StopID', remaining.variables)]

#create full correlation matrix
library(polycor)
#first, convert station characteristics to ordered factors
odf <- df[,c("Daily.Ridership", colnames(df[,-which(colnames(df) %in% c("Daily.Ridership", "StopID"))]) )]
for(i in names(df)) {
  if(is.numeric(df[[i]]) && min(df[[i]])>=0 && max(df[[i]])<=1 && length(levels(factor(df[[i]]))) < 10) {
    odf[[i]] <- ordered(df[[i]])
  } else if (is.character(df[[i]])) {
    odf[[i]] <- NULL
  }
}
mat <- hetcor(odf)
corrs <- as.data.frame(mat$correlations)
write.csv(corrs, file="output/correlation-matrix-full.csv")


# build a ml model for each variable, recording its coefficient, p-value, and the model
# pseudo r2, then throw out anything with a pval > 0.1
vars <- data.frame(variable=character(0),loglik=numeric(0), pR2=numeric(0), B=numeric(0), pval=numeric(0) )
for(i in colnames(df[,5:ncol(df)])) {
  f = formula(paste("Daily.Ridership ~", i))
  m1 <- glm.nb(f, data=df)
  
  coefs <- summary(m1)$coefficients
  ll <- round(summary(m1)$twologlik, 1)
  r2 <- pR2(m1)['r2ML']
  b <- round(coefs[2, 1], 2)
  p <- round(coefs[2, 4],3)
  
  vars <- rbind(vars, data.frame(variable=i, loklik=ll, pR2=r2, B=b, pval=p))
}
elim.cols <- vars$variable[vars$pval > 0.1]
for(i in elim.cols) print(paste("Removing attribute ", i, "with no ridership explanatory power"))
df <- df[ , which(!colnames(df) %in% elim.cols)]


# should I log transform Daily Ridership? - No, I'll use a count model instead
# hist(df$Daily.Ridership, breaks=15)
# df$Log.Daily.Ridership <- log(df$Daily.Ridership)
# hist(df$Log.Daily.Ridership, breaks=15)

#build a baseline linear model to predict ridership based demographic and TLOS variables
m1 <- lm(Daily.Ridership ~ Jobs2014LODES + WalkScore + Pop2013 + Transit.Connections + PNR.Free + Ticket.Vending.Machine, data=df)
summary(m1)
m1 <- lm(Daily.Ridership ~ Pop2013 * Jobs2011, data=df)

# investigate relationship between vacant units and other demographic characteristics of interest
m1 <- lm(Vacant2013 ~ Pop2013 + HU2013 + RenterOccupied2013 + Carless.Households + Median.Household.Income, data=df)
summary(m1)
# 
# model.res <- resid(m1)
# plot(df$Jobs2014LODES, model.res)
# 
# ml1 <- update(m1, Log.Daily.Ridership ~ .)
# summary(ml1)
# model.res <- resid(ml1)
# plot(df$Jobs2014LODES, model.res)
# 
# 
# df$PopPlusJobs2014 <- df$Pop2014ACS + df$Jobs2014LODES
# m3 <- update(m1, . ~ . - Jobs2014LODES + PopPlusJobs2014)
# summary(m3)

# loop through all 23 remaining station characteristics and build a model for each
# station.characteristics <- c("Safety.Security.Cameras.Present.",	"Safety.Call.Box.",	
#                              "Shelter.Space.for.Wheelchair.",	"Accessible.Signage.",	"Accessible.Boarding.",	"Is.Station.Accessible.",	
#                              "Amenities.Inbound.platform.shelter.from.above.",	"Amenities.Outbound.platform.shelter.from.above.",	"Amenities.Outbound.shelter.from.sides.",	"Outbound.passenger.seats.",	"Amenities.Trash.Receptacles.",	"Ticket.Vending.Machine",	
#                              "Safe.way.to.enter.and.exit.the.station.",	"Visibility.Shed.Rank",	"Directional.Signage.",	"PA.VMS",	
#                              "Art.",	"Sense.of.Place.",	"Appearance.Scale",	
#                              "Ratio.of.Spaces..Bike.1.or.greater.",	"Advertising.Present.")


# # early model development code--test one var at a time
# last <- NULL
# baseline.r2 <- round(summary(m1)$r.squared, 3)
# lm.output = data.frame(attribute = "baseline", coeficient=0, pval=0, r2=baseline.r2, delta=0)
# m2 <- m1
# 
# for(characteristic in station.characteristics) {
#   add.formula <- paste(". ~ . +", characteristic)
#   m2 <- update(m2, add.formula)
#   if(!is.null(last)) {
#     subtract.formula <- paste(". ~  . -", last)
#     m2 <- update(m2, subtract.formula)
#   }
#   
#   c <- round(summary(m2)$coefficients[6,1], 1)
#   p <- round(summary(m2)$coefficients[6,4], 4)
#   r <- round(summary(m2)$r.squared, 3)
#   d <- round(summary(m2)$r.squared - baseline.r2,3)
#   row <- c(characteristic, c, p, r, d)
# 
#   lm.output <- rbind(lm.output, row)
#   
#   last <- characteristic
# }
# write.csv(lm.output, file="output/linear-model-coefs.csv")
# 
# # Let's look at some count models
# # should we use Poisson or NBD? 
# library(AER) # for overdispersion test
# rd <- glm(Daily.Ridership ~ Jobs2014LODES + Pop2014ACS + WalkScore + Transit.Connections + PNR.Free, data=df, family=poisson)
# dispersiontest(rd)
# # very strong evidence of overdispersion, so let's use NBD
# nbm <- glm.nb(Daily.Ridership ~ Jobs2014LODES + Pop2014ACS + WalkScore + Transit.Connections + PNR.Free, data=df)
# summary(nbm)
# library(Hmisc)
# pR2(nbm)
# 
# last <- NULL
# baseline.ll <- round(summary(nbm)$twologlik, 1)
# nb.output = data.frame(attribute = "baseline", coeficient=0, pval=0, LL=baseline.ll, delta=0)
# m2 <- nbm
# 
# for(characteristic in station.characteristics) {
#   add.formula <- paste(". ~ . +", characteristic)
#   m2 <- update(m2, add.formula)
#   if(!is.null(last)) {
#     subtract.formula <- paste(". ~  . -", last)
#     m2 <- update(m2, subtract.formula)
#   }
#   
#   c <- round(summary(m2)$coefficients[7,1], 1)
#   p <- round(summary(m2)$coefficients[7,4], 5)
#   ll <- round(summary(m2)$twologlik, 3)
#   d <- round(summary(m2)$twologlik - baseline.ll,3)
#   row <- c(characteristic, c, p, ll, d)
#   
#   nb.output <- rbind(nb.output, row)
#   
#   last <- characteristic
# }
# write.csv(nb.output, file="output/negbin-model-coefs.csv")
# 
# let's build a correlation matrix for our subset
# sub.df <- df[df$Daily.Ridership > 1500, c("Daily.Ridership", station.characteristics)]
# sub.df <- sapply(sub.df, as.numeric)
# sub.corrs <- cor(sub.df)
# #sub.corr.matrix <- hetcor(sub.df)
# #sub.corrs <- sub.corr.matrix$correlations
# write.csv(sub.corrs, file="output/subset-correlation-matrix-1500-plus.csv")

# ******************************************************
# build and test regression models of up to three attributes
# ******************************************************
attrbs <- colnames(df[,5:ncol(df)])
column.names <- vector(mode="character", length=2*length(attrbs))
for(i in 1:length(attrbs)) {
  column.names[i*2 - 1] <- paste(attrbs[i], ".value", sep="")
  column.names[i*2] <- paste(attrbs[i], ".pval", sep="")
}
column.names <- c("loglik", "pR2", "(Intercept).value", "(Intercept).pval", column.names)

# create a vector with one entry for every formula to build a model for
formulas = vector(mode="character", length=0)
for(i in 1:length(attrbs)) {
  new.formula <- paste("Daily.Ridership ~ ", attrbs[[i]])
  formulas <- c(formulas, new.formula)
}
combinations <- t(combn(attrbs,2))
for(i in 1:nrow(combinations)) {
  new.formula <- paste("Daily.Ridership ~ ", combinations[i,1], "+", combinations[i,2])
  formulas <- c(formulas, new.formula)
}
combinations <- t(combn(attrbs,3))
for(i in 1:nrow(combinations)) {
  new.formula <- paste("Daily.Ridership ~ ", combinations[i,1], "+", combinations[i,2], "+", combinations[i,3])
  formulas <- c(formulas, new.formula)
}

# for each formula, return a row with the 2xloglik, pseudo R2, and ceof value + p-values for each value
#row.template <- vector(mode="numeric", length=length(column.names))
row.template <- as.vector(rep(NA, length(column.names)))
names(row.template) <- column.names
models <- as.data.frame(t(row.template))

for(i in 1:length(formulas)) {
  f = formula(formulas[i])
  try(m1 <- glm.nb(f, data=df))
  new.row <- row.template
  new.row["loglik"] <- round(summary(m1)$twologlik, 1)
  new.row["pR2"] <- pR2(m1)['r2ML']
  
  coefs <- summary(m1)$coefficients
  for(j in 1:nrow(coefs)) {
    #coef.names <- c("(Intercept)", attr(terms(f), "term.labels"))
    coef.names <- row.names(coefs)
    new.row[[paste(coef.names[j], ".value", sep="")]] <- round(coefs[j, 1], 2)
    new.row[[paste(coef.names[j], ".pval", sep="")]] <- round(coefs[j, 4], 3)
  }
  models <- rbind(models, as.data.frame(t(new.row)))
}

write.table(models, file="output/nb-models-pairwise.csv",      na = "",  row.names = FALSE, col.names = TRUE,  sep = ",")

models <- models[order(models$loglik, decreasing = TRUE), ] # sort by increasing log likelihood

#model.results <- vector(mode="numeric", length=length(attrbs))
model.results <- matrix(ncol=2, nrow=length(attrbs),dimnames=list(attrbs,c("count","avg.pval")))

for(i in 1:length(attrbs)) {
  model.results[i,1] <- sum(!is.na(models[1:500, i*2 + 3]))
  model.results[i,2] <- round(mean(models[1:500, i*2 + 4], na.rm=TRUE),4)
}
#model.results.df <- data.frame(attribute=names(model.results), inclusion.count=model.results)
model.results.df <- as.data.frame(model.results)
model.results.df <- model.results.df[order(model.results.df$count,decreasing=TRUE),]

#elim.cols <- as.vector(model.results.df[model.results.df$inclusion.count <= 2, "attribute"])
elim.cols <- rownames(model.results.df[model.results.df$avg.pval > 0.1, ])
for(i in elim.cols) print(paste("Removing attribute ", i, " due to p-values"))

new.attrbs <- rownames(model.results.df[model.results.df$avg.pval < 0.1, ])

# ******************************************************
# Elimination Round 4: build and test models with 3 - 6 explanatory variables
# ******************************************************
odf <- df[,c("Daily.Ridership", new.attrbs)]
for(i in names(odf)) {
  if(is.numeric(odf[[i]]) && min(odf[[i]])>=0 && max(odf[[i]])<=1 && length(levels(factor(odf[[i]]))) < 10) {
    odf[[i]] <- ordered(odf[[i]])
  } else if (is.character(odf[[i]])) {
    odf[[i]] <- NULL
  }
}
mat <- hetcor(odf)
corrs <- as.data.frame(mat$correlations)
write.csv(corrs, file="output/correlation-matrix-significant-attrbs.csv")

# Add WalkScore and TVMs to base model, and select up to four new attribtues
base.attrbs <- c("(Intercept)", "WalkScore", "Ticket.Vending.Machine")
other.attrbs <- new.attrbs[!(new.attrbs %in% c("WalkScore", "Ticket.Vending.Machine"))]
all.attrbs <- c(base.attrbs, other.attrbs)

base.formula <- "Daily.Ridership ~ WalkScore + Ticket.Vending.Machine"

# create a vector with one entry for every formula to build a model for
formulas = as.vector(base.formula, mode="character")
for(i in 1:length(other.attrbs)) {
  new.formula <- paste(base.formula, other.attrbs[[i]], sep = " + ")
  formulas <- c(formulas, new.formula)
}
combinations <- t(combn(other.attrbs,2))
for(i in 1:nrow(combinations)) {
  new.formula <- paste(base.formula, combinations[i,1], combinations[i,2], sep = " + ")
  formulas <- c(formulas, new.formula)
}
combinations <- t(combn(other.attrbs,3))
for(i in 1:nrow(combinations)) {
  new.formula <- paste(base.formula, combinations[i,1], combinations[i,2], combinations[i,3], sep=" + ")
  formulas <- c(formulas, new.formula)
}
combinations <- t(combn(other.attrbs,4))
for(i in 1:nrow(combinations)) {
  new.formula <- paste(base.formula, combinations[i,1], combinations[i,2], combinations[i,3], combinations[i,4], sep=" + ")
  formulas <- c(formulas, new.formula)
}


column.names <- vector(mode="character", length=2*length(all.attrbs))
for(i in 1:length(all.attrbs)) {
  column.names[i*2 - 1] <- paste(all.attrbs[i], ".value", sep="")
  column.names[i*2] <- paste(all.attrbs[i], ".pval", sep="")
}
column.names <- c("loglik", "pR2", "aic", column.names)

# for each formula, return a row with the 2xloglik, pseudo R2, and ceof value + p-values for each value
row.template <- as.vector(rep(NA, length(column.names)))
names(row.template) <- column.names
models <- as.data.frame(t(row.template))

for(i in 1:length(formulas)) {
  f = formula(formulas[i])
  try({
    m1 <- glm.nb(f, data=df)
    new.row <- row.template
    new.row["loglik"] <- round(summary(m1)$twologlik, 1)
    new.row["pR2"] <- pR2(m1)['r2ML']
    new.row["aic"] <- round(summary(m1)$aic, 1)
    coefs <- summary(m1)$coefficients
    for(j in 1:nrow(coefs)) {
      #coef.names <- c("(Intercept)", attr(terms(f), "term.labels"))
      coef.names <- row.names(coefs)
      new.row[[paste(coef.names[j], ".value", sep="")]] <- round(coefs[j, 1], 2)
      new.row[[paste(coef.names[j], ".pval", sep="")]] <- round(coefs[j, 4], 3)
    }
    models <- rbind(models, as.data.frame(t(new.row)))
  })
}

models <- models[order(models$loglik, decreasing = TRUE), ] # sort by increasing log likelihood

m2.results <- matrix(ncol=2, nrow=length(all.attrbs),dimnames=list(all.attrbs,c("count","avg.pval")))
for(i in 1:length(all.attrbs)) {
  m2.results[i,1] <- sum(!is.na(models[1:500, i*2 + 2]))
  m2.results[i,2] <- round(mean(models[1:500, i*2 + 3], na.rm=TRUE),4)
}
m2.results.df <- as.data.frame(m2.results)
m2.results.df <- m2.results.df[order(m2.results.df$count,decreasing=TRUE),]


# m2.results <- vector(mode="numeric", length=length(all.attrbs[-1]))
# names(m2.results) <- all.attrbs[-1]
# for(i in 1:length(all.attrbs[-1])) {
#   m2.results[i] <- sum(!is.na(models[1:100, i*2 + 4]))
# }
# m2.results.df <- data.frame(attribute=names(m2.results), inclusion.count=m2.results)
# m2.results.df <- m2.results.df[order(m2.results.df$inclusion.count,decreasing=TRUE),]

write.table(models, file="output/nb-models-2.csv", na = "",  row.names = FALSE, col.names = TRUE,  sep = ",")

# attributes most frequently included:
# WalkScore, TVM, Is.Station.Accessible., Transit.Connections, Amenities.Trash.Receptacles.
# Ratio.of.Spaces..Bike.1.or.greater., Safety.Security.Cameras.Present., Pop2013, Visibility.Shed.Rank
# Outbound.passenger.seats.
# Directional.Signage.

elim.cols.2 <- rownames(m2.results.df[m2.results.df$avg.pval >= 0.1, ])
for(i in elim.cols.2) print(paste("Removing attribute ", i, " due to p-values"))

final.attrbs <- rownames(m2.results.df[m2.results.df$avg.pval < 0.1, ])

# ******************************************************
# iteration 2: build and test models with 3 - 6 explanatory variables
# ******************************************************
odf <- df[,c("Daily.Ridership", final.attrbs[2:length(final.attrbs)])]
for(i in names(odf)) {
  if(is.numeric(odf[[i]]) && min(odf[[i]])>=0 && max(odf[[i]])<=1 && length(levels(factor(odf[[i]]))) < 10) {
    odf[[i]] <- ordered(odf[[i]])
  } else if (is.character(odf[[i]])) {
    odf[[i]] <- NULL
  }
}
mat <- hetcor(odf)
corrs <- as.data.frame(mat$correlations)
write.csv(corrs, file="output/correlation-matrix-round-4-attrbs.csv")


# more experimentation by hand
# Safety.Security.Cameras.Present. + 


m0 <- glm.nb(Daily.Ridership ~ WalkScore + Ticket.Vending.Machine + Amenities.Trash.Receptacles. + Vacant2013 + Is.Station.Accessible. + PNR + Safety.Security.Cameras.Present. + Outbound.passenger.seats. + Ratio.of.Spaces..Bike.1.or.greater.	+ Visibility.Shed.Rank, data=df)
summary(m0)

m1 <- m0
m1 <- update(m1, . ~ . - Is.Station.Accessible.)
m1 <- update(m1, . ~ . - Ratio.of.Spaces..Bike.1.or.greater.)
m1 <- update(m1, . ~ . - Amenities.Trash.Receptacles.)
summary(m1)
# stop here
m1 <- update(m1, . ~ . - Outbound.passenger.seats.)
m1 <- update(m1, . ~ . - Transit.Connections)
m1 <- update(m1, . ~ . - Is.Station.Accessible.)
m1 <- update(m1, . ~ . - Directional.Signage.)
m1 <- update(m1, . ~ . - Visibility.Shed.Rank)
m1 <- update(m1, . ~ . - Pop2013)
m1 <- update(m1, . ~ . - Safety.Security.Cameras.Present.)
m1 <- update(m1, . ~ . - WalkScore)



odf <- df[,c("Daily.Ridership", attr(terms(f), "term.labels"))]
for(i in names(odf)) {
  if(is.numeric(odf[[i]]) && min(odf[[i]])>=0 && max(odf[[i]])<=1 && length(levels(factor(odf[[i]]))) < 10) {
    odf[[i]] <- ordered(odf[[i]])
  } else if (is.character(odf[[i]])) {
    odf[[i]] <- NULL
  }
}
mat <- hetcor(odf)
corrs <- as.data.frame(mat$correlations)
write.csv(corrs, file="output/correlation-matrix-final-attrbs.csv")

#direct output to a file
#sink("output/final-model.txt", append=FALSE, split=FALSE)

#m2 <- glm.nb(Daily.Ridership ~ Pop2013k + Transit.Connections + Ticket.Vending.Machine + 
#               Visibility.Shed.Rank + Ratio.of.Spaces..Bike.1.or.greater. + Safety.Security.Cameras.Present.,  data = df, link = log)

# ****
# this is the final model used in the paper
# ****
f = formula(Daily.Ridership ~ WalkScore + Ticket.Vending.Machine + Vacant2013 + PNR + Safety.Security.Cameras.Present. + Outbound.passenger.seats. + Visibility.Shed.Rank)
m2 <- glm.nb(f, data = df)
summary(m2, correlation = TRUE)
pR2(m2)
est <- cbind(Estimate = coef(m2), confint(m2))
round(exp(est),3)

for(i in attr(terms(f), "term.labels"))
  cat( i, ": ", round(mean(df[[i]]),2), " ", min(df[[i]]), "-", max(df[[i]]) , "\n" )

# return output to terminal
#sink()

#model validation plot with the fitted data
# from http://stats.stackexchange.com/questions/92253/effectively-using-coefficients-from-poisson-regression?noredirect=1&lq=1
r <- resid(m2)
ft<-fitted.values(m2,pch=19)
logft <- log(ft)
#par(mfrow=c(1,3))
par(mfrow=c(1,1))
par(mfrow=c(1,3))
plot(r ~ logft,ylab="Residuals",xlab="Fitted values (log scale)",main="Model 2: Residuals vs log fitted values", pch=19)
hist(r,main="",xlab="Residuals")
qqnorm(r); qqline(r, lwd=2)


predictions <- data.frame(station.names=df$Station.Name, daily.ridership=df$Daily.Ridership, predicted.ridership=round(ft,0))
predictions$error <- predictions$predicted.ridership - predictions$daily.ridership
write.table(predictions, file="output/predictions-and-errors.csv", sep=",", row.names = FALSE, col.names = TRUE)
par(mfrow=c(1,2))
hist(df$Daily.Ridership, main="Histogram of Observed Daily Ridership", xlab="Observed Ridership by Station")
hist(ft, main="Histogram of Predicted Daily Ridership", xlab="Predicted Ridership by Station")

#plot independent variables versus residuals
par(mfrow=c(2,3))
#plot(r ~ log(df$Pop2013k), xlab="Log Pop 2013", ylab="Residuals")
for(iv in labels(terms(f))[1:6]) {
  if(min(df[[iv]])==0 & max(df[[iv]])==1) {
    boxplot(r ~ df[[iv]], data=df, xlab=iv, ylab="Residuals")  
  }
  else{
    plot(df[[iv]], r, xlab=iv, ylab="Residuals", pch=19, col="blue")
  }
}

with(df[df$PNR.Free > 0, ], plot(Daily.Ridership ~ PNR.Free, pch=19, col="blue", main="Daily Ridership and \nFree Park-and-Ride Parking Spaces", xlab="Free Parking Spaces at Station", ylab="Daily Ridership"))

# clustering
library(cluster)
#area.stats <- df[,c(4, which(names(df)=="Avg..Annual.Crime.Rate"):ncol(df))]
area.stats <- df[,c(4, which(names(df)=="Transit.Connections"):ncol(df))]
row.names(area.stats) <- df$Station.Name
D = daisy(area.stats, metric='gower')
H.fit <- hclust(D, method="ward.D2")
pdf(file="output/clusters.pdf", w=16, h=6)
par(mfrow=c(1,1))
plot(H.fit)
par(mfrow=c(1,3))
for(i in 2:4){
  groups <- cutree(H.fit, k=i)
  clusplot(area.stats, groups, color=TRUE, shade=TRUE, labels=2, lines=0, main=paste("Station Groups:", i, "Clusters"))
}
dev.off()

groups <- cutree(H.fit, k=2)
head(groups)
s1 <- df[df$Station.Name %in% names(groups[groups==1]),]
s2 <- df[df$Station.Name %in% names(groups[groups==2]),]
m1 <- glm.nb(f,  data = s1, link = log)
m2 <- glm.nb(f,  data = s2, link = log)

summary(m1)
summary(m2)

est1 <- cbind(Estimate = coef(m1), confint(m1))
est2 <- cbind(Estimate = coef(m2), confint(m2))
est <- cbind(est1, est2)
round(exp(est),3)


m3 <- glm.nb(Daily.Ridership ~ WalkScore + Ticket.Vending.Machine + Vacant2013, data = df)
summary(m3)
m4 <- update(m3, . ~ . + Is.Station.Accessible.)
m4 <- update(m3, . ~ . + Amenities.Trash.Receptacles.)
m4 <- update(m3, . ~ . + Ratio.of.Spaces..Bike.1.or.greater.)
m4 <- update(m3, . ~ . + Sense.of.Place.)
summary(m4)
est <- cbind(Estimate = coef(m4), confint(m4))
round(exp(est),4)

# recommendations
odf <- (df[order(df$Daily.Ridership, decreasing=TRUE), ])
odf <- odf[odf$Outbound.passenger.seats.==0, ]
odf$StopID
head(odf)
odf[odf$Daily.Ridership > 100 & odf$Safety.Security.Cameras.Present.==0, c("Station.Name", "Daily.Ridership", "Avg..Annual.Crime.Rate")]
predict(m2)

# predict change in ridership from 20-point bump in walkscore
pdf <- data.frame(df$Station.Name, df$Daily.Ridership, predicted.ridership=exp(predict(m2)))
mdf <- df
mdf$WalkScore <- mdf$WalkScore + 20
pdf2 <- data.frame(df$Station.Name, df$Daily.Ridership, predicted.ridership=exp(predict(m2, newdata=mdf)))
pdf3 <- as.data.frame(cbind(station=mdf$Station.Name, ws=mdf$WalkScore, p1=pdf$predicted.ridership, p2=pdf2$predicted.ridership))
head(pdf3)
pdf3$ws <- as.numeric(pdf3$ws)
pdf3$delta <- as.numeric(as.numeric(pdf3$p2) - as.numeric(pdf3$p1))
pdf3 <- pdf3[order(pdf3$delta, decreasing=TRUE),]
head(pdf3[pdf3$ws < 70,])

