library(deplyr)
library(manifestoR)

mp_setapikey(key = "4d0e6feea68b86ff367a41fb26d4836a") 
#manifesto.setcachelocation("C:/Users/merz/Desktop/scaling")

mpds <- mp_maindataset()
westeurope <- filter(mpds, country<70) %>% 
   filter(date > 198000) %>%
   select(1:76,rile)

allpers <- select(westeurope, starts_with("per")) %>%
   select(-pervote) %>%
   select(1:56)


### vanilla test

westeurope$vanilla.inv <- vanilla(westeurope, invert=1)
westeurope$vanilla <- vanilla(westeurope, invert=0)


### how to scale positions

# clean data, drop if progtype estimates


# re-aggregate to common coding scheme

# franzmann kaiser
sample <- mpds %>% filter(country==41, date==199809) ## crashes if you use a country or election which has no weights, better error checking.
vars <- grep("per\\d{3}$", names(sample), value=TRUE)
fk <- franzmann(sample,vars=vars,basevalues=FALSE,smoothing=FALSE)
s <- cbind(sample,fk)
franzmann(sample,vars=vars,basevalues=TRUE,smoothing=FALSE)
franzmann(sample,vars=vars,basevalues=FALSE,smoothing=TRUE) # does not work yet
franzmann(sample,vars=vars,basevalues=TRUE,smoothing=TRUE) # does not work yet



###




## test whether ja



**mds lr core (1944 till october 73, 21 oecd; ignore esp, prt, grc, turk, isr, mex, sri lanka, northern ireland and all cee)

USE ALL.
COMPUTE filter_$=(edate >= DATE.DMY(1,1,44) AND edate <= DATE.DMY(1,10,73))  AND (country~=52) AND (country~=72) AND (country~=73) AND (country~=74) AND (country~=171) AND (avg_vote>=2 OR absseat>0) AND (progtype~=3).
VALUE LABELS filter_$  0 'Not Selected' 1 'Selected'.
FORMAT filter_$ (f1.0).

select