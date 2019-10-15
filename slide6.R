library(foreign) # read NZESin SPSS files
library(dplyr) # general data manipulation
library(sas7bdat) # AES in a mix of SAS files and Rdata
library(ggplot2)
library(tidyr)
library(binom)
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

nzdisl17 <- nz17 %>% 
  summarise(polarised = sum((rnatlike == "0. Strongly Dislike" & !is.na(rnatlike) |
                               rlablike == "0. Strongly Dislike" & !is.na(rlablike) |
                               rgrnlike == "0. Strongly Dislike" & !is.na(rgrnlike) |
                               rnzflike == "0. Strongly Dislike" & !is.na(rnzflike)) * rwt),
            tot = sum(rwt),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2017)
nzdisl14 <- nz14 %>% 
  summarise(polarised = sum((dnatlike == "Strongly dislike" & !is.na(dnatlike) |
                               dlablike == "Strongly dislike" & !is.na(dlablike) |
                               dgrnlike == "Strongly dislike" & !is.na(dgrnlike) |
                               dnzflike == "Strongly dislike" & !is.na(dnzflike)) * dwtfin),
            tot = sum(dwtfin),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2014)
nzdisl11 <- nz11 %>% 
  summarise(polarised = sum((jnatlike == "Strongly dislike" & !is.na(jnatlike) |
                               jlablike == "Strongly dislike" & !is.na(jlablike) |
                               jgrnlike == "Strongly dislike" & !is.na(jgrnlike) |
                               jnzflike == "Strongly dislike" & !is.na(jnzflike)) * jfinwt),
            tot = sum(jfinwt),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2011)
nzdisl08 <- nz08 %>% filter(!is.na(ZZWT6)) %>% 
  summarise(polarised = sum((znatlk == "Strong dislike" & !is.na(znatlk) |
                               zlablk == "Strong dislike" & !is.na(zlablk) |
                               zgrnlik == "Strong dislike" & !is.na(zgrnlik) |
                               znzflik == "Strong dislike" & !is.na(znzflik)) * ZZWT6),
            tot = sum(ZZWT6),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2008)
nzdisl05 <- nz05 %>% filter(!is.na(YPWT)) %>% 
  summarise(polarised = sum((ynatlk == "Strong Dislike" & !is.na(ynatlk) |
                               ylablk == "Strong Dislike" & !is.na(ylablk) |
                               ygrnlk == "Strong Dislike" & !is.na(ygrnlk) |
                               ynzlk == "Strong Dislike" & !is.na(ynzlk)) * YPWT),
            tot = sum(YPWT),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2005)
nzdisl02 <- nz02 %>% filter(!is.na(ALZWT)) %>% 
  summarise(polarised = sum((WNTLK == "Strongly Dislike" & !is.na(WNTLK) |
                               WLBLK == "Strongly Dislike" & !is.na(WLBLK) |
                               WGRLK == "Strongly Dislike" & !is.na(WGRLK) |
                               WNZLK == "Strongly Dislike" & !is.na(WNZLK)) * ALZWT),
            tot = sum(ALZWT),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2002)

dislikenz <- bind_rows(nzdisl02,nzdisl05,nzdisl08,nzdisl11,nzdisl14,nzdisl17) %>% mutate(Feeling="dislike")

nzlike17 <- nz17 %>%  
  mutate(nat = as.numeric(rnatlike), lab = as.numeric(rlablike),
         grn = as.numeric(rgrnlike), nzf = as.numeric(rnzflike)) %>% 
  select(rwt,rjacknum,nat:nzf) %>%
  gather(party,liking, nat:nzf) %>% filter(!is.na(liking) & liking < 12) %>%
  arrange(rjacknum,rwt, desc(liking)) %>% group_by(rjacknum, rwt) %>% slice(1) %>% ungroup() %>%
  summarise(polarised = sum((liking == 11 & !is.na(liking)) * rwt),
            tot = sum(rwt),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2017)
nzlike14 <- nz14 %>% 
  mutate(nat = as.numeric(dnatlike), lab = as.numeric(dlablike),
         grn = as.numeric(dgrnlike), nzf = as.numeric(dnzflike)) %>% 
  select(dwtfin,djacknum,nat:nzf) %>%
  gather(party,liking, nat:nzf) %>% filter(!is.na(liking) & liking < 12) %>%
  arrange(djacknum,dwtfin, desc(liking)) %>% group_by(djacknum, dwtfin) %>% slice(1) %>% ungroup() %>%
  summarise(polarised = sum((liking == 11 & !is.na(liking)) * dwtfin),
            tot = sum(dwtfin),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2014)
nzlike11 <- nz11 %>% 
  mutate(nat = as.numeric(jnatlike), lab = as.numeric(jlablike),
         grn = as.numeric(jgrnlike), nzf = as.numeric(jnzflike)) %>% 
  select(jfinwt,Case11,nat:nzf) %>%
  gather(party,liking, nat:nzf) %>% filter(!is.na(liking) & liking < 12) %>%
  arrange(Case11,jfinwt, desc(liking)) %>% group_by(Case11, jfinwt) %>% slice(1) %>% ungroup() %>%
  summarise(polarised = sum((liking == 11 & !is.na(liking)) * jfinwt),
            tot = sum(jfinwt),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2011)
nzlike08 <- nz08 %>% filter(!is.na(ZZWT6)) %>% 
  mutate(nat = as.numeric(znatlk), lab = as.numeric(zlablk),
         grn = as.numeric(zgrnlik), nzf = as.numeric(znzflik)) %>% 
  select(ZZWT6,zquno,nat:nzf) %>%
  gather(party,liking, nat:nzf) %>% filter(!is.na(liking) & liking < 12) %>%
  arrange(zquno,ZZWT6, desc(liking)) %>% group_by(zquno, ZZWT6) %>% slice(1) %>% ungroup() %>%
  summarise(polarised = sum((liking == 11 & !is.na(liking)) * ZZWT6),
            tot = sum(ZZWT6),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2008)
nzlike05 <- nz05 %>% filter(!is.na(YPWT)) %>% 
  mutate(nat = as.numeric(ynatlk), lab = as.numeric(ylablk),
         grn = as.numeric(ygrnlk), nzf = as.numeric(ynzlk)) %>% 
  select(YPWT,case05,nat:nzf) %>%
  gather(party,liking, nat:nzf) %>% filter(!is.na(liking) & liking < 12) %>%
  arrange(case05,YPWT, desc(liking)) %>% group_by(case05, YPWT) %>% slice(1) %>% ungroup() %>%
  summarise(polarised = sum((liking == 11 & !is.na(liking)) * YPWT),
            tot = sum(YPWT),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2005)
nzlike02 <- nz02 %>% filter(!is.na(ALZWT)) %>% 
  mutate(nat = as.numeric(WNTLK), lab = as.numeric(WLBLK),
         grn = as.numeric(WGRLK), nzf = as.numeric(WNZLK)) %>% 
  select(ALZWT,CASE,nat:nzf) %>%
  gather(party,liking, nat:nzf) %>% filter(!is.na(liking) & liking < 12) %>%
  arrange(CASE,ALZWT, desc(liking)) %>% group_by(CASE, ALZWT) %>% slice(1) %>% ungroup() %>%
  summarise(polarised = sum((liking == 11 & !is.na(liking)) * ALZWT),
            tot = sum(ALZWT),
            percent = round(100 * polarised/tot, 2)) %>% mutate(Year=2002)
likenz <- bind_rows(nzlike02,nzlike05,nzlike08,nzlike11,nzlike14,nzlike17) %>% mutate(Feeling="like")
poled <- bind_rows(dislikenz,likenz)

binom_int <- function(x,df) {
  bc68 <- binom.confint(df$polarised[x], df$tot[x], conf.level = 0.68268, methods = "wilson")
  bc95 <- binom.confint(df$polarised[x], df$tot[x], conf.level = 0.95, methods = "wilson")
  return(data.frame(L68 = bc68$lower, U68 = bc68$upper, L95 = bc95$lower,
                    U95 = bc95$upper))
}

cinfs <- bind_rows(lapply(1:nrow(poled), binom_int, df=poled)) *100

magenta_variations <- colorRampPalette(c(pal_otago[4],"white"))(3)
blue_variations <- colorRampPalette(c(pal_otago[1],"white"))(3)
cicolours <- c(blue_variations[1:2],magenta_variations[1:2])
grf <-poled %>% bind_cols(cinfs) %>% select(Feeling, Year, percent, L68:U95) %>% 
  gather(interv, yval, L68:U95) %>%
  separate(interv, into=c("LorU","conint"), sep=1, convert = T) %>% spread(LorU, yval) %>% 
  mutate(`Feeling &\nConfidence\nInterval` = paste0(Feeling," ",conint,"%"),
         offset = ifelse(Feeling == "like", .35,0)) %>%
  arrange(Year,desc(conint))

slide <- ggplot(grf, aes(x=Year,y=percent,
                  xmin=Year+offset-.35, xmax=Year+offset, ymin=L, ymax=U,
                  fill=`Feeling &\nConfidence\nInterval`)) + 
  geom_rect() + scale_fill_manual(values=cicolours) + xlab("\nElection study year\n") +
  ggtitle("NZES responsents maximally liking or disliking the main 4 parties") + 
  ylab("\nPercent of respondents\n") + theme_otago_ppt +
  labs(caption = "source: New Zealand Election Study 2002 to 2017. Datasets: http://www.nzes.org\n") 
#print(slide)
