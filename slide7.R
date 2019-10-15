library(foreign) # read NZESin SPSS files
library(dplyr) # general data manipulation
library(sas7bdat) # AES in a mix of SAS files and Rdata
library(ggplot2)
library(binom)
library(tidyr)
# data source https://dataverse.ada.edu.au/dataset.xhtml?persistentId=doi:10.4225/87/WDBBAS

# NZES files in nzes subfolder
nz17 <- suppressWarnings(read.spss("../elect/nzes/NZES2017Release14-07-19.sav",
                                   to.data.frame = TRUE, add.undeclared.levels = "sort"))
nz17meta <- data.frame(varnames = attributes(nz17)$names, eyear=2017,
                       descriptions = attributes(nz17)$variable.labels,
                       stringsAsFactors = FALSE)
nz14 <- suppressWarnings(read.spss("../elect/nzes/NZES2014GeneralReleaseApril16.sav",
                                   to.data.frame = TRUE, add.undeclared.levels = "sort"))
nz14meta <- data.frame(varnames = attributes(nz14)$names, eyear=2014,
                       descriptions = attributes(nz14)$variable.labels,
                       stringsAsFactors = FALSE)
nz11 <- suppressWarnings(read.spss("../elect/nzes/NZES2011_Jun_18_rel.sav",
                                   to.data.frame = TRUE, add.undeclared.levels = "sort"))
nz11meta <- data.frame(varnames = attributes(nz11)$names, eyear=2011,
                       descriptions = attributes(nz11)$variable.labels,
                       stringsAsFactors = FALSE)
nz08 <- suppressWarnings(read.spss("../elect/nzes/2008NZES_Release.sav",
                                   to.data.frame = TRUE, add.undeclared.levels = "sort"))
nz08meta <- data.frame(varnames = attributes(nz08)$names, eyear=2008,
                       descriptions = attributes(nz08)$variable.labels,
                       stringsAsFactors = FALSE)
nz05 <- suppressWarnings(read.spss("../elect/nzes/NZES_Release_05.sav",
                                   to.data.frame = TRUE, add.undeclared.levels = "sort"))
nz05meta <- data.frame(varnames = attributes(nz05)$names, eyear=2005,
                       descriptions = attributes(nz05)$variable.labels,
                       stringsAsFactors = FALSE)
nz02 <- suppressWarnings(read.spss("../elect/nzes/nzes_02_1.1.por",
                                   to.data.frame = TRUE, add.undeclared.levels = "sort"))
nz02meta <- data.frame(varnames = attributes(nz02)$names, eyear=2002,
                       descriptions = attributes(nz02)$variable.labels,
                       stringsAsFactors = FALSE)
nzmeta <- bind_rows(nz02meta, nz05meta, nz08meta, nz11meta, nz14meta, nz17meta)

nzpol17 <- nz17 %>% 
  summarise(polarised = sum((rnatlike == "0. Strongly Dislike" & !is.na(rnatlike) |
                               rlablike == "0. Strongly Dislike" & !is.na(rlablike) |
                               rgrnlike == "0. Strongly Dislike" & !is.na(rgrnlike) |
                               rnzflike == "0. Strongly Dislike" & !is.na(rnzflike)) * rwt),
            tot = sum(rwt),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2017)
nzpol14 <- nz14 %>% 
  summarise(polarised = sum((dnatlike == "Strongly dislike" & !is.na(dnatlike) |
                               dlablike == "Strongly dislike" & !is.na(dlablike) |
                               dgrnlike == "Strongly dislike" & !is.na(dgrnlike) |
                               dnzflike == "Strongly dislike" & !is.na(dnzflike)) * dwtfin),
            tot = sum(dwtfin),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2014)
nzpol11 <- nz11 %>% 
  summarise(polarised = sum((jnatlike == "Strongly dislike" & !is.na(jnatlike) |
                               jlablike == "Strongly dislike" & !is.na(jlablike) |
                               jgrnlike == "Strongly dislike" & !is.na(jgrnlike) |
                               jnzflike == "Strongly dislike" & !is.na(jnzflike)) * jfinwt),
            tot = sum(jfinwt),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2011)
nzpol08 <- nz08 %>% filter(!is.na(ZZWT6)) %>% 
  summarise(polarised = sum((znatlk == "Strong dislike" & !is.na(znatlk) |
                               zlablk == "Strong dislike" & !is.na(zlablk) |
                               zgrnlik == "Strong dislike" & !is.na(zgrnlik) |
                               znzflik == "Strong dislike" & !is.na(znzflik)) * ZZWT6),
            tot = sum(ZZWT6),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2008)
nzpol05 <- nz05 %>% filter(!is.na(YPWT)) %>% 
  summarise(polarised = sum((ynatlk == "Strong Dislike" & !is.na(ynatlk) |
                               ylablk == "Strong Dislike" & !is.na(ylablk) |
                               ygrnlk == "Strong Dislike" & !is.na(ygrnlk) |
                               ynzlk == "Strong Dislike" & !is.na(ynzlk)) * YPWT),
            tot = sum(YPWT),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2005)
nzpol02 <- nz02 %>% filter(!is.na(ALZWT)) %>% 
  summarise(polarised = sum((WNTLK == "Strongly Dislike" & !is.na(WNTLK) |
                               WLBLK == "Strongly Dislike" & !is.na(WLBLK) |
                               WGRLK == "Strongly Dislike" & !is.na(WGRLK) |
                               WNZLK == "Strongly Dislike" & !is.na(WNZLK)) * ALZWT),
            tot = sum(ALZWT),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2002)

polarnz <- bind_rows(nzpol02,nzpol05,nzpol08,nzpol11,nzpol14,nzpol17) %>% mutate(Country="nz")

# Australia

load("../elect/aes/2. Australian Election Study, 2016.RData")
aupol16 <- x %>% select(B18_1:B18_4, wt_enrol) %>%
  filter(!is.na(wt_enrol)) %>% 
  summarise(polarised = sum((B18_1 == 0 & !is.na(B18_1) |
                               B18_2 == 0 & !is.na(B18_2) |
                               B18_3 == 0 & !is.na(B18_3) |
                               B18_4 == 0 & !is.na(B18_4)) * wt_enrol),
            tot = sum(wt_enrol),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2016)

load("../elect/aes/2. Australian Election Study, 2013.RData")
aupol13 <- x %>% select(b17lib:b17grn, weight) %>%
  filter(!is.na(weight)) %>% 
  summarise(polarised = sum((b17lib == 0 & !is.na(b17lib) |
                               b17alp == 0 & !is.na(b17alp) |
                               b17nat == 0 & !is.na(b17nat) |
                               b17grn == 0 & !is.na(b17grn)) * weight),
            tot = sum(weight),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2013)

aupol10 <- read.sas7bdat("../elect/aes/2. Australian Election Study, 2010.sas7bdat") %>% 
  select(b19lib:b19grn, weight2) %>% 
  filter(!is.na(weight2)) %>% 
  summarise(polarised = sum((b19lib == 0 & !is.na(b19lib) |
                               b19alp == 0 & !is.na(b19alp) |
                               b19nat == 0 & !is.na(b19nat) |
                               b19grn == 0 & !is.na(b19grn)) * weight2),
            tot = sum(weight2),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2010)

aupol07 <- read.sas7bdat("../elect/aes/2. Australian Election Study, 2007.sas7bdat") %>% 
  select(b26lib:b26grn) %>% 
  summarise(polarised = sum((b26lib == 0 & !is.na(b26lib) |
                               b26alp == 0 & !is.na(b26alp) |
                               b26nat == 0 & !is.na(b26nat) |
                               b26grn == 0 & !is.na(b26grn))),
            tot = n(),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2007)
aupol04 <- read.sas7bdat("../elect/aes/2. Australian Election Study, 2004.sas7bdat") %>% 
  select(b25lib:b25grn) %>% 
  summarise(polarised = sum((b25lib == 0 & !is.na(b25lib) |
                               b25alp == 0 & !is.na(b25alp) |
                               b25nat == 0 & !is.na(b25nat) |
                               b25grn == 0 & !is.na(b25grn))),
            tot = n(),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2004)
load("../elect/aes/2. Australian Election Study, 2001.RData")
aupol01 <- x %>% select(B25LIB:B25GRN) %>%
  summarise(polarised = sum((B25LIB == 0 & !is.na(B25LIB) |
                               B25ALP == 0 & !is.na(B25ALP) |
                               B25NAT == 0 & !is.na(B25NAT) |
                               B25GRN == 0 & !is.na(B25GRN))),
            tot = n(),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2001)

polarau <- bind_rows(aupol01,aupol04,aupol07,aupol10,aupol13,aupol16) %>% mutate(Country="au")
nzau <- bind_rows(polarnz,polarau)

binom_int <- function(x,df) {
  bc68 <- binom.confint(df$polarised[x], df$tot[x], conf.level = 0.68268, methods = "wilson")
  bc95 <- binom.confint(df$polarised[x], df$tot[x], conf.level = 0.95, methods = "wilson")
  return(data.frame(L68 = bc68$lower, U68 = bc68$upper, L95 = bc95$lower,
                    U95 = bc95$upper))
}

cinfs <- bind_rows(lapply(1:nrow(nzau), binom_int, df=nzau)) *100

gold_variations <- colorRampPalette(c(pal_otago[2],"white"))(3)
blue_variations <- colorRampPalette(c(pal_otago[1],"white"))(3)
cicolours <- c(gold_variations[1:2],blue_variations[1:2])
grf <- nzau %>% bind_cols(cinfs) %>% select(Country, Year, percent, L68:U95) %>% 
  gather(interv, yval, L68:U95) %>%
  separate(interv, into=c("LorU","conint"), sep=1, convert = T) %>% spread(LorU, yval) %>% 
  mutate(`Country &\nConfidence\nInterval` = paste0(Country," ",conint,"%")) %>%
  arrange(Year,desc(conint))

slide <- ggplot(grf, aes(x=Year,y=percent,
                         xmin=Year, xmax=Year+.35, ymin=L, ymax=U,
                         fill=`Country &\nConfidence\nInterval`)) + 
  geom_rect() + scale_fill_manual(values=cicolours) + xlab("\nElection study year\n") +
  ggtitle("NZES or AES respondents maximally disliking any of the main 4 parties") + 
  ylab("\nPercent of respondents\n") + theme_otago_ppt +
  labs(caption = "source: New Zealand Election Study, Australian Election Study\n") 
#print(slide)


