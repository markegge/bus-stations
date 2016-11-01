# import stops data and calculate one-way correlations

options(scipen=6) # display numbers up to 6 decimal points w/o scientific notation
if(!require("polycor")){
  install.packages("polycor", repos='http://cran.us.r-project.org')
  library(polycor)
}

setwd(dirname(parent.frame(2)$ofile)) # set current working dir to source directory (only works when run as source)
df <- read.csv("stops-dataset.csv", header=TRUE, sep=",", strip.white=TRUE)

for(i in names(df)) {
  if(is.numeric(df[[i]]) && min(df[[i]])>=0 && max(df[[i]])<=1 && length(levels(factor(df[[i]]))) < 10) {
    df[[i]] <- ordered(df[[i]])
  }
}

#sink(file="correlations.pdf")
pdf("correlations.pdf")
corr.df <- data.frame(VarName=factor(), type=character(), MCC=numeric(), pval=numeric())
for(i in names(df)) {
  if(class(df[[i]])[1]=="ordered") {
    boxplot(df$Daily.Ridership ~ df[[i]], data = df, ylab="Daily Ridership", xlab=i)
    if(length(levels(df[[i]]))>1){
      model.lm <- lm(df$Daily.Ridership ~ df[[i]], data = df)
      rsq <- summary(model.lm)$r.squared
      a <- anova(model.lm)
      p <- a[["Pr(>F)"]][1]
      p <- round(p, digits = 5)
      cat(paste(i, ' coefficient of determination'), sqrt(rsq))
      corr.df <- rbind(corr.df, data.frame(VarName=i, type="ord.factor", MCC=sqrt(rsq), pval=p))
    } else {
      corr.df <- rbind(corr.df, data.frame(VarName=i, type="ord.factor", MCC=0, pval=NA))
    }
      
  } else if(is.numeric(df[[i]])) {
    c <- cor(df$Daily.Ridership, df[[i]], use="pairwise.complete.obs")
    a <- aov(df$Daily.Ridership ~ df[[i]], data = df)
    p <- summary(a)[[1]][["Pr(>F)"]][1]
    p <- round(p, digits = 5)
    corr.df <- rbind(corr.df, data.frame(VarName=i, type="numeric", MCC=sqrt(abs(c)), pval=p))
  }
}

#sink()
#file.show("correlations.pdf")
dev.off()


# for each ordered factor, create a box plot and get a one-way regression multiple correlation coefficient
#cor(df$Daily.Ridership, df$WalkScore, use="pairwise.complete.obs")
#boxplot(Daily.Ridership ~ Amenities.Trash.Receptacles., data = df, ylab="Daily Ridership")
#class(df$Amenities.Trash.Receptacles.)
#model.lm <- lm(formula = Daily.Ridership ~ Amenities.Trash.Receptacles., data = df)
#summary(model.lm)$r.squared

# before first pass cutoff
corr.df <- corr.df[order(corr.df$MCC), ]
write.csv(corr.df, file="correlations-baseline.csv")
print(corr.df)

# elim variables where MCC less than 0.25 and ANOVA f-test p-value greater than 0.1
fp.corr.df <- corr.df[(corr.df$MCC > 0.25 & corr.df$pval < 0.1),]
#fp.corr.df <- fp.corr.df[fp.corr.df$pval < 0.2,]
print(fp.corr.df)


a <- c('Station.Name')
b <- levels(droplevels(fp.corr.df$VarName))
c <- c(a, b)
fp.df <- df[, b]

mat <- hetcor(fp.df)
corrs <- mat$correlations
write.csv(corrs, file="correlation-matrix-full.csv")
