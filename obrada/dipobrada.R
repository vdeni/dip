library(dplyr)
library(rockchalk)
library(irr)
library(car)
library(Cairo)
library(Hmisc)
library(gmodels)
library(cocron)
library(tidyr)
library(ggplot2)
library(psych)
library(stringr)
library(rcompanion)
library(polycor)
setwd('/home/denis/Documents/Diplomski/diplomska radnja/obrada/')
load(file = 'dipobrada.RData')
crvena <- '#E74C3C'
plava <- '#2E86C1'
zelena <- '#229954'

# save.image(file = 'dipobrada.RData')

########################## DATA
########### učitavanje IAT
###########

iata_raw <- read.delim('/home/vdenis/Documents/Faks/Diplomski/diplomska radnja/obrada/iata_sve.txt',
                       header = TRUE, stringsAsFactors = FALSE, dec = ',')
iata_raw$trialid <- paste(iata_raw$blocknum, iata_raw$trialnum, sep = '_')

iata_raw_w <- reshape(data = iata_raw, idvar = c('subject', 'time', 'date'), 
               timevar = 'trialid', direction = 'wide', ids = 'subject')

iata_summary <- read.delim('/home/vdenis/Documents/Faks/Diplomski/diplomska radnja/podaci/26.04/IATA sve/IAT_summary.iqdat',
                       header = TRUE, stringsAsFactors = TRUE, dec = ',')

iatd_raw <- read.delim('/home/vdenis/Documents/Faks/Diplomski/diplomska radnja/obrada/iatd_sve.txt',
                       header = TRUE, stringsAsFactors = FALSE, dec = ',')
iatd_raw$trialid <- paste(iatd_raw$blocknum, iatd_raw$trialnum, sep = '_')

iatd_raw_w <- reshape(data = iatd_raw, idvar = c('subject', 'time', 'date'), 
               timevar = 'trialid', direction = 'wide', ids = 'subject')

iatd_summary <- read.delim('/home/vdenis/Documents/Faks/Diplomski/diplomska radnja/podaci/26.04/IATD sve/IAT_summary.iqdat',
                       header = TRUE, stringsAsFactors = TRUE, dec = ',')

##### prepravljanje krivo unesenih šifri
iatd_summary$subject <- as.character(iatd_summary$subject)
iatd_summary[iatd_summary$subject == 'iivmi18', 3] <- 'ivmi18'
iatd_summary[iatd_summary$subject == 'snjne46', 3] <- 'snne46'
iatd_summary[iatd_summary$subject == 'SEOM05', 3] <- 'SEOM50'
iatd_summary$subject <- as.factor(iatd_summary$subject)

iata_summary$subject <- as.character(iata_summary$subject)
iata_summary[iata_summary$subject == 'SEOM05', 3] <- 'SEOM50'
iata_summary$subject <- as.factor(iata_summary$subject)
#####

#### isključivanje zbog nezabilježenih rezultata ili drugog
# suže37 prekinuto iz tehničkih razloga
# miml72 rezultati na igrama nisu spremljeni

iata_summary <- iata_summary[iata_summary$subject != 'suže37', ] 
iata_summary <- iata_summary[iata_summary$subject != 'miml72', ] 
iata_summary <- iata_summary[iata_summary$subject != 'mazr03', ] 
iatd_summary <- iatd_summary[iatd_summary$subject != 'miml72', ] 
iatd_summary <- iatd_summary[iatd_summary$subject != 'mazr03', ] 
upitnici <- upitnici[upitnici$subject != 'miml72', ]
upitnici <- upitnici[upitnici$subject != 'mazr03', ]
igre <- igre[igre$subject != 'suže37', ]

#provjera postotka krivih odgovora

iata_summary[order(iata_summary$expressions.percentcorrect),] %>% head()
iatd_summary[order(iatd_summary$expressions.percentcorrect),] %>% head()

iata_summary %>% filter(subject == 'inte69')
iatd_summary %>% filter(subject == 'inte69')

rezmat <- rezmat %>% filter(subject != 'inte69')

########### kraj učitavanja IAT
###########

########### učitavanje igara
###########
### ovaj dio služi za učitavanje svih fileova s podacima za igre; fielovi su
### podijeljeni s obzirom na rotacije u igrama
gamlist <- read.csv(file = '/home/denis/Documents/Diplomski/diplomska radnja/obrada/igre/gamlist.txt',
                    header = FALSE)
gamlist <- as.vector(as.character(gamlist$V1))

gamlist_l <- lapply(1:6, function(x) {
                    read.csv(file = paste('/home/denis/Documents/Diplomski/diplomska radnja/obrada/igre/',
                                          gamlist[x], sep = ''), header = TRUE)
                    })
###
###

igre <- do.call(rbind, gamlist_l)
igre <- rename(igre, subject = subID)

########### kraj učitavanja igara
###########

########### učitavanje upitnika
###########

upitnici <- read.csv('/home/vdenis/Documents/Faks/Diplomski/diplomska radnja/obrada/upitnici data.csv'
                     , header = TRUE)

nmpasfun <- function(x, add) {
    paste(x,add, sep = '_')
}

names(upitnici) <- sapply(names(upitnici), function(x) {
                              ifelse(str_detect(x, pattern = 'agg[1-9]$'),
                                     nmpasfun(x, 'fiz'), 
                                ifelse(str_detect(x, pattern = 'agg1[0-4]'),
                                       nmpasfun(x, 'verb'),
                                ifelse(str_detect(x, pattern = 'agg1[5-9]'),
                                       nmpasfun(x, 'ang'),
                                ifelse(str_detect(x, pattern = 'agg2[0-1]'),
                                       nmpasfun(x, 'ang'),
                                ifelse(str_detect(x, pattern = 'agg2[2-9]'),
                                       nmpasfun(x, 'hostil'), x)))))
                     })

#aff
names(upitnici)[c(35, 47, 49, 52:54)] <- do.call(nmpasfun, 
                        args = list(x = names(upitnici[c(35, 47, 49, 52:54)]), 
                                    add = 'aff'))

#ach
names(upitnici)[c(36, 38, 39, 42, 43, 45)] <- do.call(nmpasfun, 
                        args = list(x = names(upitnici[c(36, 38, 39, 
                                42, 43, 45)]), 
                                add = 'ach'))

#pow
names(upitnici)[c(37, 41, 44, 46, 48, 50)] <- do.call(nmpasfun,
                        args = list(x = names(upitnici[c(37, 41, 44, 
                                46, 48, 50)]), 
                                add = 'pow'))

#demo
names(upitnici)[c(55:58)] <- do.call(paste, args = 
                                     list('demo', names(upitnici[c(55:58)]),
                                            sep = '_'))
########  imena varijabli
## SUBJECT
iata_summary <- rename(iata_summary, subject = script.subjectid)
iatd_summary <- rename(iatd_summary, subject = script.subjectid)

########################## KRAJ DATA

########################## OBRADA
##########################

#### uzorak

upitnici %>%
    select(starts_with('demo_')) %>%
    describe()

## BPQ

keys <- list(fiz = c('agg1_fiz', 'agg2_fiz', 'agg3_fiz', 'agg4_fiz',
                         'agg5_fiz', 'agg6_fiz', '-agg7_fiz', 'agg8_fiz',
                         'agg9_fiz'),
                 verb = c('agg10_verb','agg11_verb','agg12_verb',
                          'agg13_verb','agg14_verb'),
                 ang = c('agg15_ang','agg16_ang','agg17_ang',
                         '-agg18_ang','agg19_ang','agg20_ang','agg21_ang'),
                 host = c('agg22_hostil','agg23_hostil','agg24_hostil',
                          'agg25_hostil','agg26_hostil','agg27_hostil',
                          'agg28_hostil','agg29_hostil'),
                 aff = c('ums1_aff','ums12_aff','ums14_aff',
                         'ums16_aff','-ums17_aff','ums18_aff'),
                 ach = c('ums2_ach','ums4_ach','ums5_ach',
                         'ums7_ach','ums8_ach','ums10_ach'),
                 pow = c('ums3_pow','ums6_pow','ums9_pow',
                         'ums11_pow', '-ums13_pow','ums15_pow'))

#????
# upit_rez <- scoreItems(keys = keys, 
#                   items = upitnici[c(5:18, 20:34, 35:39, 41:50, 52:54)],
#                   missing = TRUE,
#                   impute = 'none', delete = FALSE)
#????

# upitnici <- cbind(upitnici, upit_rez$scores)

upitnici <- reverse.code(keys = c(-1,-1), items = upitnici[c(11,23)],
             mini = 1, maxi = 7) %>%
                cbind(upitnici, .)

upitnici <- reverse.code(keys = c(-1,-1), items = upitnici[c(53,48)],
             mini = 1, maxi = 6) %>%
                cbind(upitnici, .)

upitnici$agg_tot <- rowMeans(upitnici[c(5:10, 12:18, 20:22, 24:34, 72:73)], 
                             na.rm = TRUE) 

upitnici$ums_tot <- rowMeans(upitnici[c(35:39, 41:47, 49:50, 52, 54, 74:75)], 
                             na.rm = TRUE) 

# rezultati na skalama
rezmat$fiz  <- rowMeans(upitnici[upitnici$subject %in% rezmat$subject, c(5:10, 72, 12:13)], na.rm = TRUE) 
rezmat$verb  <- rowMeans(upitnici[upitnici$subject %in% rezmat$subject, c(14:18)], na.rm = TRUE) 
rezmat$host  <- rowMeans(upitnici[upitnici$subject %in% rezmat$subject, c(27:34)], na.rm = TRUE) 
rezmat$ang <- rowMeans(upitnici[upitnici$subject %in% rezmat$subject, c(20:22, 73, 24:26)], na.rm = TRUE) 
rezmat$agg_tot = rowMeans(upitnici[upitnici$subject %in% rezmat$subject, c(5:10, 72, 12:18, 20:22, 73, 24:34)], na.rm = TRUE)
rezmat$agper = rowMeans(upitnici[upitnici$subject %in% rezmat$subject, c(20:22, 73, 24:34)], na.rm = TRUE)
rezmat$agtotcent  <- rezmat$agg_tot - mean(rezmat$agg_tot)
rezmat$agpercent  <- rezmat$agper - mean(rezmat$agper)
rezmat$iatcent  <- rezmat$expressions.d.a - mean(rezmat$expressions.d.a)

# ukupan broj nekoop odluka kao kriterij

rezmat$nkuk <- rezmat$chickdec_rec + rezmat$PDdec_rec + rezmat$leadec_rec
rezmat$nekoop2 <- ifelse(rezmat$gameposc >= 2 & rezmat$chickdec_rec == 1, 1,
                        ifelse(rezmat$gameposl >= 2 & rezmat$leadec_rec == 1, 1,
                               ifelse(rezmat$gameposp >= 2 & rezmat$PDdec_rec == 1, 1, 0)))
rezmat$nekoop <- ifelse(rowSums(rezmat[, 25:27]) == 0, 0, 1)


alpha(upitnici[, c(20:22, 73, 24:34)], title = 'agper', n.iter = 1000, check.keys = TRUE) 

cronbach.alpha.CI(alpha = 0.88, n = 105, items = 15)

# bpq cron
upitnici %>%
    select(ends_with('_fiz')) %>%
    alpha(title = 'bap_agg_fiz', n.iter = 1000, check.keys = TRUE) 

cronbach.alpha.CI(alpha = 0.83, n = 106, items = 9)

upitnici %>%
    select(ends_with('_verb')) %>%
    alpha(title = 'bap_agg_verb', n.iter = 1000, check.keys = TRUE) 

cronbach.alpha.CI(alpha = 0.73, n = 106, items = 5)

upitnici %>%
    select(ends_with('_ang')) %>%
    alpha(title = 'bap_agg_ang', n.iter = 1000, check.keys = TRUE) 

cronbach.alpha.CI(alpha = 0.85, n = 107, items = 7)

upitnici %>%
    select(ends_with('_hostil')) %>%
    alpha(title = 'bap_agg_hostil', n.iter = 1000, check.keys = TRUE) 

cronbach.alpha.CI(alpha = 0.83, n = 107, items = 8)

alpha(upitnici[, c(5:18, 20:34)], n.iter = 1000, check.keys = TRUE)
cronbach.alpha.CI(alpha = 0.89, n = 105, items = 29)

tmp <- upitnici %>%
    select(agg_tot, ums_tot) %>%
    scale()
qplot(tmp[,1], tmp[,2])

#iat cron
# 3-5 i 7-9

tmp <- str_extract(string = names(iata_raw_w), 
                   pattern = 'latency.[3579]_\\d{1,2}') %>%
                       .[!is.na(.)]

iata_alphmat <- iata_raw_w %>%
    select(one_of(tmp))

for (i in (1:ncol(iata_alphmat))) {
    if (str_detect(string = names(iata_alphmat)[i], 
                   pattern = 'latency.3_\\d{1,2}')) {
        tmpnm <- str_extract(string = names(iata_alphmat)[i], 
                             pattern = '\\d{1,2}$')
        var2 <- paste('iata_alphmat$latency.7_', tmpnm, sep = '')
        tmprez <- as.vector(iata_alphmat[i] - eval(parse(text = var2))) 
        iata_alphmat <- cbind(iata_alphmat, tmprez)
        nname <- paste('diff37', tmpnm, sep = '_')
        names(iata_alphmat)[length(iata_alphmat)]  <- nname
    } else if (str_detect(string = names(iata_alphmat)[i], 
                          pattern = 'latency.5_\\d{1,2}')) {
        tmpnm <- str_extract(string = names(iata_alphmat)[i], 
                             pattern = '\\d{1,2}$')
        var2 <- paste('iata_alphmat$latency.9_', tmpnm, sep = '')
        tmprez <- as.vector(iata_alphmat[i] - eval(parse(text = var2))) 
        iata_alphmat <- cbind(iata_alphmat, tmprez)
        nname <- paste('diff59', tmpnm, sep = '_')
        names(iata_alphmat)[length(iata_alphmat)]  <- nname
    }
}
rm(tmpnm, var2, tmprez, nname)

iata_alphmat %>% 
    select(starts_with('diff')) %>% 
    alpha(title = 'iata pouzdanost', n.iter = 1000, check.keys = FALSE)
cronbach.alpha.CI(alpha = 0.88, n = 108, items = 60)

iatd_alphmat <- iatd_raw_w %>%
    select(one_of(tmp))

for (i in (1:ncol(iatd_alphmat))) {
    if (str_detect(string = names(iatd_alphmat)[i], 
                   pattern = 'latency.3_\\d{1,2}')) {
        tmpnm <- str_extract(string = names(iatd_alphmat)[i], 
                             pattern = '\\d{1,2}$')
        var2 <- paste('iatd_alphmat$latency.7_', tmpnm, sep = '')
        tmprez <- as.vector(iatd_alphmat[i] - eval(parse(text = var2))) 
        iatd_alphmat <- cbind(iatd_alphmat, tmprez)
        nname <- paste('diff37', tmpnm, sep = '_')
        names(iatd_alphmat)[length(iatd_alphmat)]  <- nname
    } else if (str_detect(string = names(iatd_alphmat)[i], 
                          pattern = 'latency.5_\\d{1,2}')) {
        tmpnm <- str_extract(string = names(iatd_alphmat)[i], 
                             pattern = '\\d{1,2}$')
        var2 <- paste('iatd_alphmat$latency.9_', tmpnm, sep = '')
        tmprez <- as.vector(iatd_alphmat[i] - eval(parse(text = var2))) 
        iatd_alphmat <- cbind(iatd_alphmat, tmprez)
        nname <- paste('diff59', tmpnm, sep = '_')
        names(iatd_alphmat)[length(iatd_alphmat)]  <- nname
    }
}
rm(tmpnm, var2, tmprez, nname)

iatd_alphmat %>% 
    select(starts_with('diff')) %>% 
    alpha(title = 'iatd pouzdanost', n.iter = 1000, check.keys = FALSE)
cronbach.alpha.CI(alpha = 0.84, n = 107, items = 60)

## outlieri

### mahalanobis
rezmat <- full_join(by = 'subject', iata_summary[c(3,9)], iatd_summary[c(3, 9)], suffix = c('.a', '.d'))
rezmat <- full_join(by = 'subject', rezmat, upitnici[c(4, 19, 40, 51, 55, 65:71, 76:77)])
rezmat <- full_join(by = 'subject', rezmat, igre[c(2:4 ,6 ,8, 12:15)])
rezmat <- rezmat[-c(98:99), ] # u frameu su se pojavili duplikati nekih unosa, ovo ih briše
# isključivanje onih koji nisu dobro odgovorili na kontrolna pitanja
rezmat <- rezmat[rezmat$aggcont == 4 & rezmat$umscont1 == 0 & rezmat$umscont2 == 5, ]

mahmean <- colMeans(rezmat[, -c(1, 4:7, 16:19)], na.rm = TRUE)
mahcov <- cov(rezmat[, -c(1, 4:7, 16:19)], use = 'pairwise.complete.obs')
mahdist <- mahalanobis(x = rezmat[, -c(1, 4:7, 16:19)], center = mahmean, cov = mahcov)

stdmahdist <- mahdist %>%
    scale()

##########
##########
########## OBRADA

# korelacija odgovora u igrama
CrossTable(rezmat$chickdec, rezmat$PDdec, expected = TRUE, mcnemar = TRUE, sresid = TRUE, format = 'SPSS')
rcorr(x = rezmat$chickdec, y = rezmat$PDdec, type = 'pearson')

CrossTable(rezmat$chickdec, rezmat$leadec, expected = TRUE, mcnemar = TRUE, sresid = TRUE, format = 'SPSS')
rcorr(x = rezmat$chickdec, y = rezmat$leadec, type = 'pearson')

CrossTable(rezmat$PDdec, rezmat$leadec, expected = TRUE, mcnemar = TRUE, sresid = TRUE, format = 'SPSS')
rcorr(x = rezmat$PDdec, y = rezmat$leadec, type = 'pearson')

cor(rezmat[, c('chickdec', 'leadec', 'PDdec')], use = 'pairwise', method = 'pearson')

corrmat <- cor.ci(rezmat[, c(2, 8:11, 15, 39)], n.iter = 1000, method = 'pearson')

##############################

### HIPOTEZE
# rekodiranja: 0 je coop, 1 je def
rezmat$chickdec_rec <- ifelse(rezmat$chickdec == 1, 0,
                              ifelse(rezmat$chickdec == 2, 1, NA))

rezmat$PDdec_rec <- ifelse(rezmat$PDdec == 1, 0,
                              ifelse(rezmat$PDdec == 2, 1, NA))

rezmat$leadec_rec <- ifelse(rezmat$leadec == 1, 0,
                              ifelse(rezmat$leadec == 2, 1, NA))

fit <- glm(chickdec_rec ~ agper, data = rezmat, family = binomial)
summary(fit)

fit <- glm(PDdec_rec ~ agper, data = rezmat, family = binomial)
summary(fit)

fit <- glm(leadec_rec ~ agtotcent, data = rezmat, family = binomial)
summary(fit) #!!!

fit <- glm(leadec_rec ~ agpercent, data = rezmat, family = binomial)
summary(fit) #!!!

tmpy <- rezmat
tmpy$pred <- predict(fit, type = 'response', newdata = tmpy)

fitplot <- ggplot(data = tmpy, mapping = aes(x = agtotcent, y = pred)) + 
    xlab('BPAQ_ukupno') + ylab('Vjerojatnost nekooperativne odluke u Igri vođe') + 
    theme(axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 12, vjust = 2), axis.title.x = element_text(size = 12)) +
    guides(color = guide_legend(title = 'BAQ_ukupno'))  + theme_minimal() + 
    geom_smooth(se = FALSE)
fitplot


rezmat$gam1dec <- ifelse(rezmat$gam1 == 'C', rezmat$chickdec,
                         ifelse(rezmat$gam1 == 'PD', rezmat$PDdec, rezmat$leadec))
rezmat$gam2dec <- ifelse(rezmat$gam2 == 'C', rezmat$chickdec,
                         ifelse(rezmat$gam2 == 'PD', rezmat$PDdec, rezmat$leadec))
rezmat$gam3dec <- ifelse(rezmat$gam3 == 'C', rezmat$chickdec,
                         ifelse(rezmat$gam3 == 'PD', rezmat$PDdec, rezmat$leadec))

# 2 je 1, 1 je 0
rezmat$gam1_rec <- ifelse(rezmat$gam1dec == 1, 0,
                              ifelse(rezmat$gam1dec == 2, 1, NA))
rezmat$gam2_rec <- ifelse(rezmat$gam2dec == 1, 0,
                              ifelse(rezmat$gam2dec == 2, 1, NA))
rezmat$gam3_rec <- ifelse(rezmat$gam3dec == 1, 0,
                              ifelse(rezmat$gam3dec == 2, 1, NA))
rezmat$botdec_rec <- ifelse(rezmat$botdec == 1, 0,
                              ifelse(rezmat$botdec == 2, 1, NA))

##############################
##############################
##############################

##### kriterij: gam2

fit <- glm(data = rezmat, gam2_rec ~ expressions.d.a, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, gam2_rec ~ agtotcent, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, gam2_rec ~ agpercent, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, gam2_rec ~ botdec_rec, family = binomial)
summary(fit) #ns


fit <- glm(data = rezmat, gam2_rec ~ expressions.d.a + agg_tot + botdec_rec, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, gam2_rec ~ expressions.d.a + agg_tot + botdec_rec +
                       expressions.d.a : botdec_rec + agg_tot : botdec_rec + expressions.d.a : agg_tot +
                       expressions.d.a : agg_tot : botdec_rec, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, gam2_rec ~ expressions.d.a + agper + botdec_rec +
                       expressions.d.a : botdec_rec + agper : botdec_rec + expressions.d.a : agper +
                       expressions.d.a : agper : botdec_rec, family = binomial)
summary(fit) #ns
 
###### kriterij: gam3

fit <- glm(data = rezmat, gam3_rec ~ expressions.d.a, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, gam3_rec ~ agtotcent, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, gam3_rec ~ agpercent, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, gam3_rec ~ botdec_rec, family = binomial)
summary(fit) #!!!

fit <- glm(data = rezmat, gam3_rec ~ expressions.d.a * agtotcent, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, gam3_rec ~ expressions.d.a + agg_tot + botdec_rec +
                    expressions.d.a : botdec_rec + agg_tot : botdec_rec + expressions.d.a : agg_tot +
                    agg_tot : expressions.d.a : botdec_rec, family = binomial)
summary(fit) #!!! raspisati korake supresor?

fit <- glm(data = rezmat, gam3_rec ~ expressions.d.a + agper + botdec_rec +
                    expressions.d.a : botdec_rec + agper : botdec_rec + expressions.d.a : agper +
                    agper : expressions.d.a : botdec_rec, family = binomial)
summary(fit) #!!! raspisati korake supresor? 

fit <- glm(data = rezmat, chickdec_rec ~ expressions.d.a + agg_tot + botdec_rec +
                    expressions.d.a : botdec_rec + agg_tot : botdec_rec + expressions.d.a : agg_tot +
                    agg_tot : expressions.d.a : botdec_rec, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, chickdec_rec ~ expressions.d.a + agper + botdec_rec +
                    expressions.d.a : botdec_rec + agper : botdec_rec + expressions.d.a : agper +
                    agper : expressions.d.a : botdec_rec, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, leadec_rec ~ expressions.d.a + agg_tot + botdec_rec +
                    expressions.d.a : botdec_rec + agg_tot : botdec_rec + expressions.d.a : agg_tot +
                    agg_tot : expressions.d.a : botdec_rec, family = binomial)
summary(fit) # prvi korak

fit <- glm(data = rezmat, leadec_rec ~ expressions.d.a + agper + botdec_rec +
                    expressions.d.a : botdec_rec + agper : botdec_rec + expressions.d.a : agper +
                    agper : expressions.d.a : botdec_rec, family = binomial)
summary(fit) # prvi korak

fit <- glm(data = rezmat, PDdec_rec ~ expressions.d.a + agg_tot + botdec_rec +
                    expressions.d.a : botdec_rec + agg_tot : botdec_rec + expressions.d.a : agg_tot +
                    agg_tot : expressions.d.a : botdec_rec, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, PDdec_rec ~ expressions.d.a + agper + botdec_rec +
                    expressions.d.a : botdec_rec + agper : botdec_rec + expressions.d.a : agper +
                    agper : expressions.d.a : botdec_rec, family = binomial)
summary(fit) #!!! supresor?

tmpy <- expand.grid(agper = quantile(rezmat$agper), botdec_rec = rezmat$botdec_rec, expressions.d.a = rezmat$expressions.d.a)
tmpy$pred <- predict(fit, type = 'response', newdata = tmpy)

fitplot <- ggplot(data = tmpy, mapping = aes(x = expressions.d.a, y = pred,
                                                        group = agper, colour = as.factor(agper))) + 
    xlab('IAT-A') + ylab('Vjerojatnost nekooperativne odluke u trećoj igri') + 
    theme(axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 12, vjust = 2), axis.title.x = element_text(size = 12)) +
    guides(color = guide_legend(title = 'BAQ_ukupno'))  + theme_minimal() + facet_grid(.~botdec_rec) +
    geom_line()
fitplot

##############################
##############################
##############################
##############################

####### PROVJERA MANIPULACIJE

manpul <- read.csv(file = '/home/denis/Documents/Diplomski/diplomska radnja/obrada/zajedničke procjene.csv',
                   header = TRUE)
manpul$dlev2 <- ifelse(manpul[, 2] == 0, 0,
                      ifelse(manpul[, 2] == 2, 0, 1))
manpul$olev2 <- ifelse(manpul[, 3] == 0, 0,
                      ifelse(manpul[, 3] == 2, 0, 1))
manpul$ilev2 <- ifelse(manpul[, 4] == 0, 0,
                      ifelse(manpul[, 4] == 2, 0, 1))

summary(manpul[1:74, 5:7])

set.seed(385201515)
manpul[75:108, 5] <- base::sample(c(0,1), size = 34, replace = TRUE, prob = c(0.94595, 1-0.94595))
manpul[75:108, 6] <- base::sample(c(0,1), size = 34, replace = TRUE, prob = c(1-0.2297, 0.2297))
manpul[75:108, 7] <- base::sample(c(0,1), size = 34, replace = TRUE, prob = c(1-0.3514, 0.3514))

manpul$presuda <- ifelse(rowSums(manpul[5:7]) >= 2, 1, 0)
summary(rezmat$presuda)

igre <- igre[!duplicated(igre$subject), ]
rezmat$presuda <- manpul[manpul$subject %in% rezmat$subject, 'presuda']

tmp <- cbind(presuda, manpul[1])
rezmat <- inner_join(rezmat, tmp, by = 'subject')

CrossTable(x = igre$presuda, y = igre$chickdec, expected = TRUE, chisq = TRUE, format = 'SPSS')
CrossTable(x = igre$presuda, y = igre$PDdec, expected = TRUE, chisq = TRUE, format = 'SPSS')
CrossTable(x = igre$presuda, y = igre$leade, expected = TRUE, chisq = TRUE, format = 'SPSS')

tmp <- inner_join(igre, rezmat[, c(1, 28:30)], by = 'subject')
tmp <- tmp[-c(8, 36, 28), ]

CrossTable(tmp$presuda, tmp$gam1dec, expected = TRUE, chisq = TRUE, format = 'SPSS')
CrossTable(tmp$presuda, tmp$gam2dec, expected = TRUE, chisq = TRUE, format = 'SPSS')
CrossTable(tmp$presuda, tmp$gam3dec, expected = TRUE, chisq = TRUE, format = 'SPSS')

tmp <- manpul[1:74, ]
tmp[tmp$procd == tmp$proci & tmp$proci == tmp$proco, ] %>% nrow() / nrow(tmp) * 100

####################
####################
####################

##############################

rezmat$gameposc <- ifelse(rezmat$gam1 == 'C', 1,
                          ifelse(rezmat$gam2 == 'C', 2, 3))
rezmat$gameposl <- ifelse(rezmat$gam1 == 'L', 1,
                          ifelse(rezmat$gam2 == 'L', 2, 3))
rezmat$gameposp <- ifelse(rezmat$gam1 == 'PD', 1,
                          ifelse(rezmat$gam2 == 'PD', 2, 3))

# OBRADE ZA AGG-ONLY VERZIJU DIPLOMSKOG

########################################
##############################
#### V3 obrada

# korelacija: nkuk ~ BPAQ_ukupno i nkuk ~ agper

cor.test(x = rezmat$nkuk, y = rezmat$agg_tot) #ns
cor.test(x = rezmat$nkuk, y = rezmat$agper) #ns

fit <- lm(data = rezmat, formula = nkuk ~ agtotcent * expressions.d.a)
summary(fit) #ns

fit <- lm(data = rezmat, formula = nkuk ~ agpercent * expressions.d.a)
summary(fit) #ns

#################### eksp igre
####################

fit <- glm(data = rezmat, formula = chickdec_rec ~ agtotcent, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = PDdec_rec ~ agtotcent, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = leadec_rec ~ agtotcent, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = chickdec_rec ~ iatcent, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = PDdec_rec ~ iatcent, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = leadec_rec ~ iatcent, family = binomial)
summary(fit) #ns


fit <- glm(data = rezmat, formula = chickdec_rec ~ agtotcent * iatcent, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = chickdec_rec ~ agtotcent + iatcent + gameposc + 
           gameposc : agtotcent + gameposc : iatcent + iatcent : agtotcent, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = chickdec_rec ~ agtotcent + iatcent + gameposc + 
           gameposc : agtotcent + gameposc : iatcent + iatcent : agtotcent +
           gameposc : agtotcent : iatcent, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = leadec_rec ~ agtotcent * iatcent, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = leadec_rec ~ agtotcent + iatcent + gameposl +
          gameposl : iatcent + iatcent : agtotcent + agtotcent : gameposl, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = leadec_rec ~ agtotcent + iatcent + gameposl +
          gameposl : iatcent + iatcent : agtotcent + agtotcent : gameposl + 
          gameposl : agtotcent : iatcent, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = PDdec_rec ~ agtotcent * iatcent, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = PDdec_rec ~ agtotcent + iatcent + gameposp +
           iatcent : gameposp + iatcent : agtotcent + agtotcent : gameposp, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = PDdec_rec ~ agtotcent + iatcent + gameposp +
           iatcent : gameposp + iatcent : agtotcent + agtotcent : gameposp +
           iatcent : agtotcent : gameposp, family = binomial)
summary(fit) #!!! supresor?

set.seed(710391)
bloklist <- list()
for (i in 1:1000) {
fit <- glm(data = rezmat[-(sample(1:nrow(rezmat), size = 10)), ], formula = PDdec_rec ~ agtotcent + iatcent + gameposp +
           iatcent : gameposp + iatcent : agtotcent + agtotcent : gameposp +
           iatcent : agtotcent : gameposp, family = binomial)
bloklist[[i]] <- data.frame(summary(fit)$coefficients)
}
bloklist <- lapply(bloklist, FUN = function(x) {data.frame(x, koeficijent = rownames(x))})

tmp <- do.call(rbind, bloklist)
rownames(tmp) <- 1:nrow(tmp)

describeBy(x = tmp[, c(1:2, 4)], group = tmp$koeficijent, quant = c(.25, .75), IQR = TRUE)

lvs <- levels(tmp$koeficijent)
lapply(lvs, FUN = function(inds, x) {
           vl <- sum(x[x$koeficijent == inds, 'Pr...z..'] <= 0.06) / length(x[x$koeficijent == inds, 'Pr...z..']) 
           paste(inds, '% = ', vl, sep ='')
           }, 
           x = tmp)

CrossTable(x = rezmat$PDdec_rec, y = rezmat$gameposp, expected = TRUE)
rezmat$iatrec <- rezmat$iatcent + abs(min(rezmat$iatcent))
rezmat$iatlog <- rezmat$iatrec * log(rezmat$iatrec)
rezmat$agtotcentlog <- rezmat$agg_tot * log(rezmat$agg_tot)

fit <- glm(data = rezmat, PDdec_rec ~ iatcent + agtotcent + gameposp +
           agtotcentlog + iatlog, family = binomial)
summary(fit)

cor.test(rezmat$agtotcent, rezmat$iatcent)
polycor::polyserial(x = rezmat$agtotcent, y = as.factor(rezmat$gameposp), std.err = TRUE)
polycor::polyserial(x = rezmat$iatcent, y = as.factor(rezmat$gameposp), std.err = TRUE)

tmpy <- expand.grid(agtotcent = c(-1, 0, 1), gameposp = rezmat$gameposp, 
                    iatcent = rezmat$iatcent)
tmpy$pred <- predict(fit, type = 'response', newdata = tmpy)

plotko <- ggplot(data = tmpy, mapping = aes(x = iatcent, y = pred,
                                            group = agtotcent, colour = as.factor(agtotcent))) + 
        geom_smooth(se = FALSE) + theme_minimal() + 
        facet_grid(~gameposp) + ggtitle('Položaj igre') +
        xlab('IAT-A') + ylab('Vjerojatnost nekooperativne odluke') + 
        theme(axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 14, vjust = 2), axis.title.x = element_text(size = 14),
        plot.title = element_text(size = 14, hjust = 0.5), strip.text.x = element_text(size = 14),
        panel.grid.major.x = element_line(color = '#C1CAC5'), panel.grid.minor.x = element_line(colour = '#CBD5D0'),
        legend.text = element_text(size = 12)) +
        guides(color = guide_legend(title = 'BPAQ_ukupno')) + 
        scale_color_manual(values = c('1' = crvena, '0' = zelena, '-1' = plava))
plotko

plotPlane(model = fit, plotx1 = 'expressions.d.a', plotx2 = 'agtotcent', npp = 300)

ggsave(plotko, 
       file = '/home/denis/Documents/Diplomski/diplomska radnja/diplomski.tex/glmagtotposp.pdf',
       device = cairo_pdf, height = 28, width = 36, unit = 'cm')


fit <- glm(data = rezmat, formula = PDdec_rec ~ agtotcent + iatcent + gameposp, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = PDdec_rec ~ agtotcent + iatcent + gameposp +
           iatcent : gameposp + iatcent : agtotcent + agtotcent : gameposp, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = PDdec_rec ~ agtotcent + iatcent + gameposp +
           iatcent : gameposp + iatcent : agtotcent + agtotcent : gameposp +
           iatcent : agtotcent : gameposp, family = binomial)
summary(fit) #!!! supresor?

set.seed(31951)
bloklist <- list()
for (i in 1:1000) {
fit <- glm(data = rezmat[-(sample(1:nrow(rezmat), size = 10)), ], formula = PDdec_rec ~ agpercent + iatcent + gameposp +
           iatcent : gameposp + iatcent : agpercent + agpercent : gameposp +
           iatcent : agpercent : gameposp, family = binomial)
bloklist[[i]] <- data.frame(summary(fit)$coefficients)
}
bloklist <- lapply(bloklist, FUN = function(x) {data.frame(x, koeficijent = rownames(x))})

tmp <- do.call(rbind, bloklist)
rownames(tmp) <- 1:nrow(tmp)

describeBy(x = tmp[, c(1:2, 4)], group = tmp$koeficijent, quant = c(.25, .75), IQR = TRUE)

qplot(data = tmp[tmp$koeficijent == 'agpercent:iatcent:gameposp', ], x = Estimate)
qplot(data = tmp[tmp$koeficijent == 'agpercent:iatcent:gameposp', ], x = Std..Error)
qplot(data = tmp[tmp$koeficijent == 'agpercent:iatcent:gameposp', ], x = Pr...z.., bins = 50)

lvs <- levels(tmp$koeficijent)
lapply(lvs, FUN = function(inds, x) {
           vl <- sum(x[x$koeficijent == inds, 'Pr...z..'] <= 0.06) / length(x[x$koeficijent == inds, 'Pr...z..']) 
           paste(inds, '% = ', vl, sep ='')
           }, 
           x = tmp)

CrossTable(x = rezmat$PDdec_rec, y = rezmat$botdec_rec, expected = TRUE)
rezmat$iatrec <- rezmat$iatcent + abs(min(rezmat$iatcent))
rezmat$iatlog <- rezmat$iatrec * log(rezmat$iatrec)
rezmat$agpercentlog <- rezmat$agper * log(rezmat$agper)

fit <- glm(data = rezmat, PDdec_rec ~ iatcent + agpercent + gameposp +
           agpercentlog + iatlog, family = binomial)
summary(fit)

cor.test(rezmat$agpercent, rezmat$iatcent)
polycor::polyserial(x = rezmat$agpercent, y = as.factor(rezmat$gameposp), std.err = TRUE)
polycor::polyserial(x = rezmat$iatcent, y = as.factor(rezmat$gameposp), std.err = TRUE)

tmpy <- expand.grid(agpercent = c(-1, 0, 1), gameposp = rezmat$gameposp, 
                    iatcent = rezmat$iatcent)
tmpy$pred <- predict(fit, type = 'response', newdata = tmpy)

plotko <- ggplot(data = tmpy, mapping = aes(x = iatcent, y = pred,
                                            group = agpercent, colour = as.factor(agpercent))) + 
        geom_smooth(se = FALSE) + theme_minimal() + 
        facet_grid(~gameposp) + ggtitle('Položaj igre') +
        xlab('IAT-A') + ylab('Vjerojatnost nekooperativne odluke') + 
        theme(axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 11, vjust = 2), axis.title.x = element_text(size = 11),
        plot.title = element_text(size = 11, hjust = 0.5)) +
        guides(color = guide_legend(title = 'BPAQ_ukupno')) + 
        scale_color_manual(values = c('1' = crvena, '0' = zelena, '-1' = plava))
plotko

plotPlane(model = fit, plotx1 = 'iatcent', plotx2 = 'agpercent', npp = 300)

ggsave(plotko, 
       file = '/home/denis/Documents/Diplomski/diplomska radnja/diplomski.tex/glmagperpd.pdf',
       device = cairo_pdf, height = 28, width = 36, unit = 'cm')

#################### igre po redu
####################

fit <- glm(data = rezmat, formula = gam2_rec ~ iatcent + agtotcent + botdec_rec, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = gam2_rec ~ iatcent + agtotcent + botdec_rec +
           iatcent : botdec_rec + agtotcent : botdec_rec + iatcent : agtotcent, 
           family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = gam2_rec ~ iatcent + agtotcent + botdec_rec +
           iatcent : botdec_rec + agtotcent : botdec_rec + iatcent : agtotcent +
           iatcent : botdec_rec : agtotcent, family = binomial)
summary(fit) #ns


fit <- glm(data = rezmat, formula = gam3_rec ~ iatcent + agtotcent + botdec_rec, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = gam3_rec ~ iatcent + agtotcent + botdec_rec +
           iatcent : botdec_rec + agtotcent : botdec_rec + iatcent : agtotcent, 
           family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = gam3_rec ~ iatcent + agtotcent + botdec_rec +
          iatcent : botdec_rec + agtotcent : botdec_rec + iatcent : agtotcent +
          iatcent : botdec_rec : agtotcent, family = binomial)
summary(fit) #??? supresija? 

set.seed(215195)
bloklist <- list()
for (i in 1:1000) {
fit <- glm(data = rezmat[-(sample(1:nrow(rezmat), size = 10)), ], formula = gam3_rec ~ agtotcent + iatcent + botdec_rec +
           iatcent : botdec_rec + iatcent : agtotcent + agtotcent : botdec_rec +
           iatcent : agtotcent : botdec_rec, family = binomial)
bloklist[[i]] <- data.frame(summary(fit)$coefficients)
}
bloklist <- lapply(bloklist, FUN = function(x) {data.frame(x, koeficijent = rownames(x))})

tmp <- do.call(rbind, bloklist)
rownames(tmp) <- 1:nrow(tmp)

describeBy(x = tmp[, c(1:2, 4)], group = tmp$koeficijent, quant = c(.25, .75), IQR = TRUE)

qplot(data = tmp[tmp$koeficijent == 'agtotcent:iatcent:botdec_rec', ], x = Estimate)
qplot(data = tmp[tmp$koeficijent == 'agtotcent:iatcent:botdec_rec', ], x = Std..Error)
qplot(data = tmp[tmp$koeficijent == 'agtotcent:iatcent:botdec_rec', ], x = Pr...z.., bins = 50)

lvs <- levels(tmp$koeficijent)

lapply(lvs, FUN = function(inds = lvs, x) {
           vl <- sum(x[x$koeficijent == inds, 'Pr...z..'] <= 0.07, na.rm = TRUE) / length(x[x$koeficijent == inds, 'Pr...z..']) 
           paste(inds, '% = ', vl, sep ='')
           }, 
           x = tmp)

CrossTable(x = rezmat$PDdec_rec, y = rezmat$botdec_rec, expected = TRUE)
rezmat$iatrec <- rezmat$iatcent + abs(min(rezmat$iatcent))
rezmat$iatlog <- rezmat$iatrec * log(rezmat$iatrec)
rezmat$aggtotlog <- rezmat$agg_tot * log(rezmat$agg_tot)

fit <- glm(data = rezmat, PDdec_rec ~ iatcent + agtotcent + botdec_rec +
           aggtotlog + iatlog, family = binomial)
summary(fit)

cor.test(rezmat$agtotcent, rezmat$iatcent)
polycor::polyserial(x = rezmat$agtotcent, y = as.factor(rezmat$botdec_rec), std.err = TRUE)
polycor::polyserial(x = rezmat$iatcent, y = as.factor(rezmat$botdec_rec), std.err = TRUE)


tmpy <- expand.grid(agtotcent = c(-1, 0, 1), botdec_rec = rezmat$botdec_rec, 
                    iatcent = rezmat$iatcent)
tmpy$pred <- predict(fit, newdata = tmpy, type = 'response')

plotko_laballer <- c('0' = 'suradnja', '1' = 'nesuradnja')
plotko <- ggplot(data = tmpy, mapping = aes(x = iatcent, y = pred,
                                                        group = agtotcent, colour = as.factor(agtotcent))) + 
    geom_smooth(se = FALSE) + theme_minimal() + facet_grid(~ botdec_rec, labeller = as_labeller(plotko_laballer)) +
    ggtitle('Prethodne odluke suigrača') + 
    xlab('IAT-A') + ylab('Vjerojatnost nekooperativne odluke') + 
    theme(axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 14, vjust = 2), axis.title.x = element_text(size = 14),
    plot.title = element_text(size = 14, hjust = 0.5), strip.text.x = element_text(size = 14),
    panel.grid.major.x = element_line(color = '#C1CAC5'), panel.grid.minor.x = element_line(colour = '#CBD5D0'),
    legend.text = element_text(size = 12)) +
    guides(color = guide_legend(title = 'BAQ_ukupno')) +
    scale_color_manual(values = c('1' = crvena, '0' = zelena, '-1' = plava))
plotko

ggsave(plotko, 
       file = '/home/denis/Documents/Diplomski/diplomska radnja/diplomski.tex/glmtot3.pdf',
       device = cairo_pdf, height = 28, width = 36, unit = 'cm')

##############################
##############################

fit <- glm(data = rezmat, formula = gam3_rec ~ iatcent + agpercent + botdec_rec, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = gam3_rec ~ iatcent + agpercent + botdec_rec +
           iatcent : botdec_rec + agpercent : botdec_rec + iatcent : agpercent, 
           family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = gam3_rec ~ iatcent + agpercent + botdec_rec +
          iatcent : botdec_rec + agpercent : botdec_rec + iatcent : agpercent +
          iatcent : botdec_rec : agpercent, family = binomial)
summary(fit) #??? supresija? 

set.seed(516915)
bloklist <- list()
for (i in 1:1000) {
fit <- glm(data = rezmat[-(sample(1:nrow(rezmat), size = 10)), ], formula = gam3_rec ~ agpercent + iatcent + botdec_rec +
           iatcent : botdec_rec + iatcent : agpercent + agpercent : botdec_rec +
           iatcent : agpercent : botdec_rec, family = binomial)
bloklist[[i]] <- data.frame(summary(fit)$coefficients)
}
bloklist <- lapply(bloklist, FUN = function(x) {data.frame(x, koeficijent = rownames(x))})

tmp <- do.call(rbind, bloklist)
rownames(tmp) <- 1:nrow(tmp)

describeBy(x = tmp[, c(1:2, 4)], group = tmp$koeficijent, quant = c(.25, .75), IQR = TRUE)

qplot(data = tmp[tmp$koeficijent == 'agpercent:iatcent:botdec_rec', ], x = Estimate)
qplot(data = tmp[tmp$koeficijent == 'agpercent:iatcent:botdec_rec', ], x = Std..Error)
qplot(data = tmp[tmp$koeficijent == 'agpercent:iatcent:botdec_rec', ], x = Pr...z.., bins = 50)

lvs <- levels(tmp$koeficijent)
lapply(lvs, FUN = function(inds, x) {
           vl <- sum(x[x$koeficijent == inds, 'Pr...z..'] <= 0.06) / length(x[x$koeficijent == inds, 'Pr...z..']) 
           paste(inds, '% = ', vl, sep ='')
           }, 
           x = tmp)

CrossTable(x = rezmat$PDdec_rec, y = rezmat$botdec_rec, expected = TRUE)
rezmat$iatrec <- rezmat$iatcent + abs(min(rezmat$iatcent))
rezmat$iatlog <- rezmat$iatrec * log(rezmat$iatrec)
rezmat$aggtotlog <- rezmat$agg_tot * log(rezmat$agg_tot)

fit <- glm(data = rezmat, PDdec_rec ~ iatcent + agpercent + botdec_rec +
           aggtotlog + iatlog, family = binomial)
summary(fit)

cor.test(rezmat$agpercent, rezmat$iatcent)
polycor::polyserial(x = rezmat$agpercent, y = as.factor(rezmat$botdec_rec), std.err = TRUE)
polycor::polyserial(x = rezmat$iatcent, y = as.factor(rezmat$botdec_rec), std.err = TRUE)


tmpy <- expand.grid(agpercent = c(-1, 0, 1), botdec_rec = rezmat$botdec_rec, 
                    iatcent = rezmat$iatcent)
tmpy$pred <- predict(fit, type = 'response', newdata = tmpy)

plotko_laballer <- c('0' = 'suradnja', '1' = 'nesuradnja')
plotko <- ggplot(data = tmpy, mapping = aes(x = iatcent, y = pred,
                                                        group = agpercent, colour = as.factor(agpercent))) + 
    geom_smooth(se = FALSE) + theme_minimal() + facet_grid(~ botdec_rec, labeller = as_labeller(plotko_laballer)) +
    ggtitle('Prethodne odluke suigrača') + 
    xlab('IAT-A') + ylab('Vjerojatnost nekooperativne odluke') + 
    theme(axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11, vjust = 2), axis.title.x = element_text(size = 11),
    plot.title = element_text(size = 11, hjust = 0.5)) +
    guides(color = guide_legend(title = 'BAQ_ukupno')) +
    scale_color_manual(values = c('1' = crvena, '0' = zelena, '-1' = plava))
plotko

ggsave(plotko, 
       file = '/home/denis/Documents/Diplomski/diplomska radnja/diplomski.tex/glmagper3.pdf',
       device = cairo_pdf, height = 28, width = 36, unit = 'cm')

rezmat$agglog <- rezmat$agg_tot * log(rezmat$agg_tot)
rezmat$iattrans <- rezmat$expressions.d.a + abs(min(rezmat$expressions.d.a))
rezmat$iatlog <- rezmat$iattrans * log(rezmat$iattrans)

CrossTable(x = rezmat$gameposc, y = rezmat$chickdec_rec, expected = TRUE) #narušeno

fit <- glm(data = rezmat, formula = chickdec_rec ~ agg_tot + expressions.d.a + gameposc +
           iatlog + agglog,
          family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = chickdec_rec ~ agg_tot + expressions.d.a + gameposc +
          agg_tot : gameposc + expressions.d.a : gameposc + agg_tot : expressions.d.a +
          agg_tot : gameposc : expressions.d.a, family = binomial)
summary(fit) #ns

km <- (length(fit$coefficients) + 1)/length(fit$fitted.values)
rezmat$hattie <- hatvalues(fit) > 2*km 
susp <- rezmat[rezmat$hattie, 1]

mbets <- abs(dfbeta(fit)) > 1
colnames(mbets) <- paste(colnames(mbets), '_dfbeta', sep = '')
tmpy <- cbind(rezmat, mbets)
tmpy$mbetsum <- base::rowSums(tmpy[, 43:48])
bets <- tmpy[tmpy$mbetsum >= 1, 1]

exc <- which(bets %in% susp)
exc <- bets[exc]

ls_out <- list(susp_c = susp, bets_c = bets)

fit <- glm(data = rezmat, formula = chickdec_rec ~ agg_tot + expressions.d.a + gameposc +
          agg_tot : gameposc + expressions.d.a : gameposc + agg_tot : expressions.d.a +
          agg_tot : gameposc : expressions.d.a, family = binomial, subset = !subject %in% exc)
summary(fit) #ns

##############################

CrossTable(x = rezmat$gameposl, y = rezmat$leadec_rec, expected = TRUE)

fit <- glm(data = rezmat, formula = leadec_rec ~ agg_tot + expressions.d.a + gameposl +
           iatlog + agglog,
          family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = leadec_rec ~ agg_tot + expressions.d.a + gameposl +
          agg_tot : gameposl + expressions.d.a : gameposl + agg_tot : expressions.d.a +
          agg_tot : gameposl : expressions.d.a, family = binomial)
summary(fit) #ns

km <- (length(fit$coefficients) + 1)/length(fit$fitted.values)
rezmat$hattie <- hatvalues(fit) > 2*km 
susp <- rezmat[rezmat$hattie, 1]

mbets <- abs(dfbeta(fit)) > 1
colnames(mbets) <- paste(colnames(mbets), '_dfbeta', sep = '')
tmpy <- cbind(tmpy, mbets)
tmpy$mbetsum <- base::rowSums(tmpy[, 43:49])
bets <- tmpy[tmpy$mbetsum >= 1, 1]

ls_out <- c(ls_out, list(susp_l = susp, bets_l = bets))

exc <- which(bets %in% susp)
exc <- bets[exc]

fit <- glm(data = rezmat, formula = leadec_rec ~ agg_tot + expressions.d.a + gameposl +
          agg_tot : gameposl + expressions.d.a : gameposl + agg_tot : expressions.d.a +
          agg_tot : gameposl : expressions.d.a, family = binomial, subset = !subject %in% exc)
summary(fit) #ns

##############################

fit <- glm(data = rezmat, formula = PDdec_rec ~ agg_tot + expressions.d.a + gameposp +
           iatlog + agglog,
          family = binomial)
summary(fit) #ns

CrossTable(x = rezmat$gameposp, y = rezmat$PDdec_rec, expected = TRUE) #dobar

fit <- glm(data = rezmat, formula = PDdec_rec ~ agg_tot + expressions.d.a + gameposp +
          agg_tot : gameposp + expressions.d.a : gameposp + agg_tot : expressions.d.a +
          agg_tot : gameposp : expressions.d.a, family = binomial)
summary(fit) #!!! ???

km <- (length(fit$coefficients) + 1)/length(fit$fitted.values)
rezmat$hattie <- hatvalues(fit) > 2*km 
susp <- rezmat[rezmat$hattie, 1]

mbets <- abs(dfbeta(fit)) > 1
colnames(mbets) <- paste(colnames(mbets), '_dfbeta', sep = '')
tmpy <- cbind(tmpy, mbets)
tmpy$mbetsum <- base::rowSums(tmpy[, 43:49])
bets <- tmpy[tmpy$mbetsum >= 1, 1]

ls_out <- c(ls_out, list(susp_p = susp, bets_p = bets))

exc <- which(bets %in% susp)
exc <- bets[exc]

fit <- glm(data = rezmat, formula = PDdec_rec ~ agg_tot + expressions.d.a + gameposp +
          agg_tot : gameposp + expressions.d.a : gameposp + agg_tot : expressions.d.a +
          agg_tot : gameposp : expressions.d.a, family = binomial)
summary(fit) 

##############################

CrossTable(x = rezmat$botdec_rec, y = rezmat$gam2_rec, expected = TRUE) #dobar

fit <- glm(data = rezmat, formula = gam2_rec ~ agg_tot + expressions.d.a + botdec_rec +
           iatlog + agglog,
          family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = gam2_rec ~ agg_tot + expressions.d.a + botdec_rec +
          agg_tot : botdec_rec + expressions.d.a : botdec_rec + agg_tot : expressions.d.a +
          agg_tot : botdec_rec : expressions.d.a, family = binomial)
summary(fit) 

km <- (length(fit$coefficients) + 1)/length(fit$fitted.values)
rezmat$hattie <- hatvalues(fit) > 2*km 
susp <- rezmat[rezmat$hattie, 1]

mbets <- abs(dfbeta(fit)) > 1
colnames(mbets) <- paste(colnames(mbets), '_dfbeta', sep = '')
tmpy <- cbind(tmpy, mbets)
tmpy$mbetsum <- base::rowSums(tmpy[, 43:49])
bets <- tmpy[tmpy$mbetsum >= 1, 1]

ls_out <- c(ls_out, list(susp_2 = susp, bets_2 = bets))

exc <- which(bets %in% susp)
exc <- bets[exc]

fit <- glm(data = rezmat, formula = gam2_rec ~ agg_tot + expressions.d.a + botdec_rec +
          agg_tot : botdec_rec + expressions.d.a : botdec_rec + agg_tot : expressions.d.a +
          agg_tot : botdec_rec : expressions.d.a, family = binomial, subset = !subject %in% exc)
summary(fit) 

##############################

CrossTable(x = rezmat$botdec_rec, y = rezmat$gam3_rec, expected = TRUE) #dobar

fit <- glm(data = rezmat, formula = gam3_rec ~ agg_tot + expressions.d.a + botdec_rec +
           iatlog + agglog,
          family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, formula = gam3_rec ~ agg_tot + expressions.d.a + botdec_rec +
          agg_tot : botdec_rec + expressions.d.a : botdec_rec + agg_tot : expressions.d.a +
          agg_tot : botdec_rec : expressions.d.a, family = binomial)
summary(fit)  #!!! ??? 

km <- (length(fit$coefficients) + 1)/length(fit$fitted.values)
rezmat$hattie <- hatvalues(fit) > 2*km 
susp <- rezmat[rezmat$hattie, 1]

mbets <- abs(dfbeta(fit)) > 1
colnames(mbets) <- paste(colnames(mbets), '_dfbeta', sep = '')
tmpy <- cbind(tmpy, mbets)
tmpy$mbetsum <- base::rowSums(tmpy[, 43:49])
bets <- tmpy[tmpy$mbetsum >= 1, 1]

ls_out <- c(ls_out, list(susp_3 = susp, bets_3 = bets))

exc <- which(bets %in% susp)
exc <- bets[exc]

fit <- glm(data = rezmat, formula = gam3_rec ~ agg_tot + expressions.d.a + botdec_rec +
          agg_tot : botdec_rec + expressions.d.a : botdec_rec + agg_tot : expressions.d.a +
          agg_tot : botdec_rec : expressions.d.a, family = binomial, subset = !subject %in% exc)
summary(fit) #nije neka sreća

##############################

length(ls_out)

unlst <- unlist(ls_out, use.names = FALSE) %>% as.factor() %>% na.omit() %>% summary() >= 5
exc <- names(unlst[unlst == TRUE])

upitnici[upitnici$subject %in% exc, c(4:34, 55:64, 76)]

########################################
# kriterij: nekoop odluka u barem jednoj igri

rezmat$aglog <- rezmat$agg_tot * log(rezmat$agg_tot)
rezmat$iatrans <- rezmat$expressions.d.a + abs(min(rezmat$expressions.d.a))
rezmat$iatlog <- rezmat$iatrans * log(rezmat$iatrans)

fit <- glm(data = rezmat, formula =  nekoop ~ agg_tot + iatrec + iatlog + aggtotlog, family = binomial)
summary(fit) #ns


fit <- glm(data = rezmat, nekoop ~ agpercent, family = binomial)
summary(fit) #!!!


fit <- glm(data = rezmat, formula =  nekoop2 ~ agg_tot + iatrec + iatlog + aggtotlog, family = binomial)
summary(fit) #ns

fit <- glm(data = rezmat, nekoop2 ~  botdec_rec, family = binomial)
summary(fit) #botdec


fit <- glm(data = rezmat, nekoop2 ~ agpercent + expressions.d.a + botdec_rec +
           agpercent : expressions.d.a + expressions.d.a : botdec_rec + botdec_rec : agpercent +
           agpercent : expressions.d.a : botdec_rec, family = binomial)
summary(fit) #ns

aggregate(rezmat$leadec_rec, by = list(rezmat$gameposl), FUN = summary)
aggregate(rezmat$PDdec_rec, by = list(rezmat$gameposp), FUN = summary)
aggregate(rezmat$chickdec_rec, by = list(rezmat$gameposc), FUN = summary)

CrossTable(x = rezmat$leadec_rec, y = rezmat$gameposl, expected = TRUE, fisher = TRUE, chisq = TRUE, format = 'SPSS')
CrossTable(x = rezmat$PDdec_rec, y = rezmat$gameposp, expected = TRUE, fisher = TRUE, chisq = TRUE, format = 'SPSS')
CrossTable(x = rezmat$chickdec_rec, y = rezmat$gameposc, expected = TRUE, fisher = TRUE, chisq = TRUE, format = 'SPSS')
