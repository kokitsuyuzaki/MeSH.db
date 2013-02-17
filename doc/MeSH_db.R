### R code from vignette source 'MeSH_db.Rnw'

###################################################
### code chunk number 1: MeSH_db.Rnw:199-200
###################################################
library(MeSH.db)


###################################################
### code chunk number 2: MeSH_db.Rnw:207-208
###################################################
ls("package:MeSH.db")


###################################################
### code chunk number 3: MeSH_db.Rnw:217-218
###################################################
cols(MeSHMAPCOUNTS)


###################################################
### code chunk number 4: MeSH_db.Rnw:225-226
###################################################
keytypes(MeSHMAPCOUNTS)


###################################################
### code chunk number 5: MeSH_db.Rnw:234-236
###################################################
k <- keys(MeSHMAPCOUNTS, keytype="MAPNAME")
head(k)


###################################################
### code chunk number 6: MeSH_db.Rnw:243-245
###################################################
select(MeSHMAPCOUNTS, keys=k[1,], cols=c("MAPNAME","COUNT"),
       keytype="MAPNAME")


###################################################
### code chunk number 7: MeSH_db.Rnw:252-253
###################################################
select(MeSHMAPCOUNTS, keys=k[1,], cols=c("MAPNAME","COUNT"))


###################################################
### code chunk number 8: MeSH_db.Rnw:264-265
###################################################
cols(MeSHTERM)


###################################################
### code chunk number 9: MeSH_db.Rnw:272-273
###################################################
keytypes(MeSHTERM)


###################################################
### code chunk number 10: MeSH_db.Rnw:281-284
###################################################
CF <- select(MeSHTERM, keys="Cystic Fibrosis",
            cols=c("MESHID", "MESHTERM", "MESHCATEGORY"), keytype="MESHTERM")
CF


###################################################
### code chunk number 11: MeSH_db.Rnw:294-296 (eval = FALSE)
###################################################
## select(MeSHSYNONYM, keys=CF[1,1],
##        cols=c("MESHID","MESHSYNONYM"), keytype="MESHTERM")


###################################################
### code chunk number 12: MeSH_db.Rnw:300-301
###################################################
writeLines(strwrap(capture.output(select(MeSHSYNONYM, keys=CF[1,1], cols=c("MESHID","MESHSYNONYM")))))


###################################################
### code chunk number 13: MeSH_db.Rnw:309-311
###################################################
select(MeSHQUALIFIER, keys=CF[1,1], 
       cols=c("QUALIFIERID","SUBHEADING","MESHID"), keytype="MESHID")


###################################################
### code chunk number 14: MeSH_db.Rnw:318-321
###################################################
ao <- select(MeSHCAOR, keys=CF[1,1], 
       cols=c("ANCESTORMESHID","OFFSPRINGMESHID"), keytype="OFFSPRINGMESHID")
ao


###################################################
### code chunk number 15: MeSH_db.Rnw:329-330
###################################################
select(MeSHTERM, keys=ao[,1], cols=c("MESHTERM"), keytype="MESHID")


###################################################
### code chunk number 16: MeSH_db.Rnw:338-341
###################################################
pc <- select(MeSHCPCR, keys=CF[1,1],
       cols=c("PARENTMESHID","CHILDMESHID"), keytype="CHILDMESHID")
pc


###################################################
### code chunk number 17: MeSH_db.Rnw:350-352
###################################################
select(MeSHCAOR, keys=CF[1,1], 
       cols=c("ANCESTORMESHID","OFFSPRINGMESHID"), keytype="ANCESTORMESHID")


