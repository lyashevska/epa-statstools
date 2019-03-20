
library(lme4)
library(effects)
library(optimx)
# require(devtools)
# install_version("effects", version = "4.0-0")

set.seed(4322)

dat <- read.csv(file = 'data.csv', row.names=1)

head(dat)

cols = c("x1", "x2")
dat[, paste0(cols, "_", "sc")] <- scale(dat[ ,cols])
summary(dat)

summary(glm(ifelse(dat$y>0,1,0) ~
            x1_sc +
            x2_sc +
            year,
            data = dat,
        family = binomial(link = logit)))

m.bin.full.re <- glmer(ifelse(dat$y>0,1,0) ~
             x1_sc +
             (1|year) ,
           data = dat,
#            control = glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
           family = binomial(link = logit))

summary(m.bin.full.re)

ranef.bin.dat<-as.data.frame(ranef(m.bin.full.re))[c(3,4)]
colnames(ranef.bin.dat)[2] <- "reyear"

head(ranef.bin.dat)

# merge with dat by column year
dat.bin.re<-merge(dat,ranef.bin.dat, by.x = "year", by.y = "grp")

head(dat.bin.re)

m.bin.full <- glm(reyear ~
                  x2_sc,
           data = dat.bin.re,
           family = gaussian)

summary(m.bin.full)

plot(allEffects(m.bin.full.re))

# drop rows with nas
no.na.dat <- na.omit(dat)

summary(glm(y ~
            year +
            x1_sc +
            x2_sc,
            data = subset(no.na.dat, y>0),
            family = Gamma(link = log)))

m.gamma.full.re <- glmer(y ~
             x1_sc +
             (1|year),
            data = subset(no.na.dat, y>0),
            control = glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
           family = Gamma(link = log))

summary(m.gamma.full.re)


plot(allEffects(m.gamma.full.re))


# extract random effect
ranef.gamma.dat<-as.data.frame(ranef(m.gamma.full.re))[c(3,4)]

colnames(ranef.gamma.dat)[2] <- "reyear"

dat.gamma.re <- merge(subset(no.na.dat, y>0),ranef.gamma.dat, by.x = "year", by.y = "grp")

# fit the rest of the variables against ranef.year
m.gamma.full <- glm(reyear ~
            x2_sc,
           data = dat.gamma.re,
           family = gaussian)

summary(m.gamma.full)
