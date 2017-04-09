mac.wd <- '~/Dropbox/Chapter2/'
pc.wd <- 'C:/Users/Kiva Oken/Dropbox/Chapter2/'
my.wd <- pc.wd

source(paste(my.wd, "R/functions.R", sep=''))
setwd(paste(my.wd, "data", sep=''))
grouped <- read.csv('cameo/AGG_TABLE_BY_ALL.csv')
biomass.only <- read.csv('cameo/BEST_AVAILABLE_BIOMASS.csv')
require(reshape2)
require(plyr)
require(dplyr)
#Need to smooth time series first to get less noisy lines to compare

mort.nat.props <- data.frame()


# Calculating surplus production ------------------------------------------

sand.lance <- filter(biomass.only, SPECIES=='Northern sand lance') %>% left_join(y=grouped) 
sand.lance$TYPE <- 'Fish'
sand.lance$HABITAT <- 'Demersal'
sand.lance$FGUILD <- 'Planktivore'
sand.lance$LengthGroup <- 'Small'
sand.lance$TaxonGroup <- 'Forage fish'
sand.lance$LANDINGS[is.na(sand.lance$LANDINGS)] <- 0

yf.sole <- filter(biomass.only, SPECIES=='Yellowfin sole')
yf.sole$TYPE <- 'Fish'
yf.sole$HABITAT <- 'Demersal'
yf.sole$FGUILD <- 'Benthivore'
yf.sole$LengthGroup <- 'Small'
yf.sole$TaxonGroup <- 'Pleuronectid'
yf.sole$LANDINGS <- 0
yf.sole$NumSpecies <- 1
yf.sole$rownames <- 1

#
scorpaeniformes <- filter(biomass.only, SPECIES=='Scorpaeniformes', REGION=='sGoSL')
scorpaeniformes$TYPE <- 'Fish'
scorpaeniformes$HABITAT <- 'Demersal'
scorpaeniformes$FGUILD <- 'Omnivore'
scorpaeniformes$LengthGroup <- 'Small'
scorpaeniformes$TaxonGroup <- 'Scorpaenid'
scorpaeniformes$LANDINGS <- 0
scorpaeniformes$NumSpecies <- 1
scorpaeniformes$rownames <- 1

# Discontinuous time series!!!
perciformes <- filter(biomass.only, SPECIES=='Perciformes', REGION=='sGoSL')
perciformes$TYPE <- 'Fish'
perciformes$HABITAT <- 'Demersal'
perciformes$FGUILD <- 'Benthivore'
perciformes$LengthGroup <- 'Small'
perciformes$TaxonGroup <- 'Percoid'
perciformes$LANDINGS <- 0
perciformes$NumSpecies <- 1
perciformes$rownames <- 1
  
capelin <- filter(biomass.only, SPECIES=='Capelin', REGION=='EBS')
capelin$TYPE <- 'Fish'
capelin$HABITAT <- 'Pelagic'
capelin$FGUILD <- 'Planktivore'
capelin$LengthGroup <- 'Small'
capelin$TaxonGroup <- 'Forage fish'
capelin$LANDINGS <- 0
capelin$NumSpecies <- capelin$rownames <- 1

p.sand.lance <- filter(biomass.only, SPECIES=='Pacific sand lance', REGION=='EBS')
p.sand.lance$TYPE <- 'Fish'
p.sand.lance$HABITAT <- 'Demersal'
p.sand.lance$FGUILD <- 'Planktivore'
p.sand.lance$LengthGroup <- 'Small'
p.sand.lance$TaxonGroup <- 'Forage fish'
p.sand.lance$LANDINGS <- 0
p.sand.lance$NumSpecies <- p.sand.lance$rownames <- 1

# 'Atka mackerel' in EBS?



grouped.long.ts <- subset(grouped, REGION!='ESS' | (SPECIES!='Atlantic butterfish' & SPECIES!='Northern sand lance' & SPECIES!='Shad' &
                                                      SPECIES!='Stone crab')) %>% 
  rbind(sand.lance[,names(grouped)], yf.sole[,names(grouped)], scorpaeniformes[,names(grouped)],
        perciformes[,names(grouped)], capelin[,names(grouped)], p.sand.lance[,names(grouped)]) 

smoothed <- with(grouped.long.ts, tapply(BIOMASS, paste(REGION, SPECIES), smoothBiomass))
grouped.long.ts <- grouped.long.ts[order(paste(grouped.long.ts$REGION, grouped.long.ts$SPECIES)),]
grouped.long.ts$smoothed <- unlist(smoothed)

surp.prod <- by(grouped.long.ts, with(grouped.long.ts, paste(REGION, SPECIES)), getSP)
grouped.long.ts$sp <- unlist(surp.prod)


# Not currently used
forage.fish <- subset(grouped.long.ts, (TaxonGroup=='Forage fish' | TaxonGroup=='Clupeid') & SPECIES!='Alewife' & SPECIES!='Pacific salmon')
# years are all sorted, checked 12/9/13

forage.fish$SPECIES <- droplevels(forage.fish$SPECIES)


# sGoSL predation signal ------------------------------------------------
# start year = 1984
# need cetacea, grey seals, harp seals
ecosystem <- 'sGoSL'
gsl.diet <- get.nat.mort.matrix(ncompartment=30, ecosystem=ecosystem)
group.info <- read.group.info(ecosystem=ecosystem)
gsl.mort.nat.init <- gsl.diet * t(aaply(.data=group.info, .margins=1, .fun=expand.group, df=group.info, .expand=FALSE))
Cod <- combine.groups(c("Cod > 35 cm","Cod <= 35 cm"), gsl.mort.nat.init, group.info)
A.Plaice <- combine.groups(c("American plaice <= 35 cm", "American plaice > 35 cm"), gsl.mort.nat.init, group.info)
gsl.mort.nat <- cbind(gsl.mort.nat.init, Cod, A.Plaice)

sgosl <- subset(biomass.only, REGION=='sGoSL' & YEAR>=1984) # Last time series (dogfish) starts in 1984
sgosl$SPECIES <- droplevels(sgosl$SPECIES)

sgosl.preds <- list()
sgosl.preds$'Grey seals' <- fill.pred.list('Grey seal', sgosl)
sgosl.preds$'Cod' <- fill.pred.list('Atlantic cod', sgosl)
sgosl.preds$'Greenland halibut' <- fill.pred.list('Greenland halibut', sgosl)
sgosl.preds$'A.Plaice' <- fill.pred.list('American plaice', sgosl)
sgosl.preds$Flounders <- fill.pred.list(c('Windowpane', 'Winter flounder', 'Witch flounder', 'Yellowtail flounder'), sgosl)
sgosl.preds$Skates <- fill.pred.list(c('Smooth skate', 'Thorny skate', 'Winter skate'), sgosl)
sgosl.preds$Redfish <- fill.pred.list('Atlantic redfishes (NS)', sgosl)
sgosl.preds$'Large demersal feeders' <- fill.pred.list(c('Haddock', 'White hake', 'Atlantic wolffish', 
                                                         'Eelpouts (NS)', 'Marlin-spike grenadier', 'Atlantic halibut', 
                                                         'Longfin hake'), sgosl)
sgosl.preds$'Small demersal feeders' <- fill.pred.list(c('Cunner', 'Longhorn sculpin', 'Shorthorn sculpin', 'Sea raven', 'Perciformes', 'Scorpaeniformes'), sgosl)
sgosl.preds$Capelin <- fill.pred.list('Capelin', sgosl)
sgosl.preds$'Large pelagic feeders' <- fill.pred.list('Spiny dogfish', sgosl)
sgosl.preds$'Piscivorous small pelagics' <- fill.pred.list('Shortfin squid', sgosl)
sgosl.preds$'Planktivorous small pelagics' <- fill.pred.list(c('Atlantic herring', 'Alewife', 'Rainbow smelt'), sgosl)
sgosl.preds$Shrimp <- fill.pred.list('Shrimps (NS)', sgosl)
sgosl.preds$'Large crustaceans' <- fill.pred.list(c('Snow crab', 'Toad crabs (NS)', 'Rock crab'), sgosl)
# Shrimp, large crustaceans have crappy time series and don't eat any important prey


sgosl.predation <- get.predation(cameo.prey=c('Capelin', 'Small dem. feeders',  
                                              'Plank. small pelagics', 'Flounders'), 
                                 ecopath.prey=c('Capelin', 'Small demersal feeders', 
                                                'Planktivorous small pelagics', 'Flounders'),
                                 region='sGoSL', pred.list=sgosl.preds, mort.nat=gsl.mort.nat, group.info=group.info)

# Baltic predation --------------------------------------------------------
# Start year = 1973 for all 3 species
ecosystem <- 'Baltic Sea'
balt.diet <- get.nat.mort.matrix(ncompartment=16, ecosystem=ecosystem)
group.info <- read.group.info(ecosystem=ecosystem)
balt.mort.nat.init <- balt.diet*t(aaply(.data=group.info, .margins=1, .fun=expand.group, df=group.info, .expand=FALSE))
Sprat <- combine.groups(c('Juvenile sprat', 'Adult sprat'), balt.mort.nat.init, group.info)
Herring <- combine.groups(c('Juvenile herring', 'Adult herring'), balt.mort.nat.init, group.info)
Cod <- combine.groups(c('Juvenile cod', 'Adult cod'), balt.mort.nat.init, group.info)
balt.mort.nat <- cbind(balt.mort.nat.init, Sprat, Herring, Cod)

balt <- subset(grouped.long.ts, REGION=='BALT')
balt$SPECIES <- droplevels(balt$SPECIES)

balt.preds <- list()
balt.preds$'Sprat' <- fill.pred.list('Sprat', balt)
balt.preds$'Herring' <- fill.pred.list('Atlantic herring', balt)
balt.preds$'Cod' <- fill.pred.list('Atlantic cod', balt)

balt.predation <- get.predation(cameo.prey=c('Atlantic herring', 'Sprat'), region='BALT', 
                                 ecopath.prey=c('Juvenile herring', 'Juvenile sprat'),
                                 pred.list=balt.preds, mort.nat=balt.mort.nat, group.info=group.info)

# Western Scotian Shelf predation -----------------------------------------
# Start year = 1973 (same for all species) 
#Need: Ceteaceans*, grey seals, seabirds*
ecosystem <- 'WSS' 
wss.diet <- get.nat.mort.matrix(ncompartment=62, ecosystem=ecosystem)
group.info <- read.group.info(ecosystem=ecosystem)
wss.mort.nat.init <- wss.diet*t(aaply(.data=group.info, .margins=1, .fun=expand.group, df=group.info, .expand=FALSE))


Cod <- combine.groups(c('Cod <1', 'Cod 1-3', 'Cod 4-6', 'Cod 7+'), wss.mort.nat.init, group.info)
Silver.Hake <- combine.groups(c('S Hake <25', 'S Hake 25-31', 'S Hake 31+'), wss.mort.nat.init, group.info)
Halibut <- combine.groups(c('Halibut <46', 'Halibut 46-81', 'Halibut 82+'), wss.mort.nat.init, group.info)
Pollock <- combine.groups(c('Pollock <49', 'Pollock 49+'), wss.mort.nat.init, group.info)
D.Piscivores <- combine.groups(c('D piscivores <40', 'D piscivores 40+'), wss.mort.nat.init, group.info)
L.Benthivores <- combine.groups(c('L benthivores <40', 'L benthivores 40+'), wss.mort.nat.init, group.info)
Skates <- combine.groups(c('Skates <49', 'Skates 49+'), wss.mort.nat.init, group.info)
Redfish <- combine.groups(c('Redfish <22', 'Redfish 22+'), wss.mort.nat.init, group.info)
A.Plaice <- combine.groups(c('A plaice <26', 'A plaice 26+'), wss.mort.nat.init, group.info)
Flounders <- combine.groups(c('Flounders <30', 'Flounders 30+'), wss.mort.nat.init, group.info)
Haddock <- combine.groups(c('Haddock <3', 'Haddock 3+'), wss.mort.nat.init, group.info)
L.Sculpin <- combine.groups(c('L sculpin <25', 'L sculpin 25+'), wss.mort.nat.init, group.info)
Herring <- combine.groups(c('Herring <4', 'Herring 4+'), wss.mort.nat.init, group.info)
wss.mort.nat <- cbind(wss.mort.nat.init, Cod, Silver.Hake, Halibut, Pollock, D.Piscivores, L.Benthivores, Skates, 
                      Redfish, A.Plaice, Flounders, Haddock, L.Sculpin, Herring)

wss <- subset(biomass.only, REGION=='WSS' & YEAR>=1973 & YEAR<=2007)
wss$SPECIES <- droplevels(wss$SPECIES)

wss.preds <- list()
wss.preds$'Cod' <- fill.pred.list('Atlantic cod', wss)
wss.preds$'Silver.Hake' <- fill.pred.list('Silver hake', wss)
wss.preds$Halibut <- fill.pred.list(c('Atlantic halibut', 'Greenland halibut'), wss) 
wss.preds$Pollock <- fill.pred.list('Pollock', wss)
wss.preds$'Skates' <- fill.pred.list(c('Skates (NS)', 'Barndoor skate'), wss)
wss.preds$'Redfish' <- fill.pred.list('Atlantic redfishes (NS)', wss)
wss.preds$A.Plaice <- fill.pred.list('American plaice', wss)
wss.preds$Flounders <- fill.pred.list(c('Winter flounder', 'Witch flounder', 'Yellowtail flounder',
                                        'Windowpane','Fourspot flounder'), wss)
wss.preds$'Haddock' <- fill.pred.list('Haddock', wss)
wss.preds$L.Sculpin <- fill.pred.list('Sculpin', wss)
wss.preds$Herring <- fill.pred.list('Atlantic herring', wss)
wss.preds$'Dogfish' <- fill.pred.list('Spiny dogfish', wss)
wss.preds$Seals <- fill.pred.list('Grey seal', wss)
wss.preds$'Other pelagic' <- fill.pred.list(c('Atlantic argentine', 'Shad', 'Alewife', 'Northern sand lance', 'Atlantic butterfish'), wss)
wss.preds$Lobster <- fill.pred.list('American lobster', wss)
wss.preds$Mackerel <- fill.pred.list('Atlantic mackerel', wss)
wss.preds$'Large crabs' <- fill.pred.list(c('Jonah crab', 'Rock crab', 'Deepsea red crab', 'Snow crab', 'Stone crab'), wss)
wss.preds$Scallop <- fill.pred.list('Sea scallop', wss)
wss.preds$Squids <- fill.pred.list('Squids (NS)', wss)
wss.preds$D.Piscivores <- fill.pred.list(c('White hake', 'Cusk', 'Goosefish'), wss)
wss.preds$L.Benthivores <- fill.pred.list(c('Red hake', 'Ocean pout', 'Wolffishes (NS)', 'Lumpfish'), wss)

wss.predation <- get.predation(cameo.prey=c('Atlantic herring', 'Other pelagics'), region='WSS', 
                                 ecopath.prey=c('Herring <4', 'Other pelagic'),
                                 pred.list=wss.preds, mort.nat=wss.mort.nat, group.info=group.info)               

# Eastern Scotian Shelf ---------------------------------------------------
# Start year = 1970 for species of interest, earliest available
ecosystem <- 'ESS'
ess.diet <- get.nat.mort.matrix(ncompartment=39, ecosystem=ecosystem)
rownames(ess.diet)[rownames(ess.diet)=='Juv Cod'] <- 'Juvenile Cod'
colnames(ess.diet)[colnames(ess.diet)=='Juv Cod'] <- 'Juvenile Cod'
group.info <- read.group.info(ecosystem)
ess.mort.nat.init <- ess.diet*t(aaply(.data=group.info, .margins=1, .fun=expand.group, df=group.info, .expand=FALSE))

Cod <- combine.groups(c('Juvenile Cod', 'Adult Cod'), ess.mort.nat.init, group.info)
Silver.Hake <- combine.groups(c('Silver hake', 'Juv Silver Hake'), ess.mort.nat.init, group.info)
Dem.Piscivores <- combine.groups(c('Juv. Dem.Pisc.', 'Dem. Piscivores'), ess.mort.nat.init, group.info)
Dem.Feeders <- combine.groups(c('Large Demersals', 'Juv LDFs'), ess.mort.nat.init, group.info) 
Halibut <- combine.groups(c('Halibut > 65 cm', 'Juv Halibut'), ess.mort.nat.init, group.info)
Crabs <- combine.groups(c('Large Crabs', 'Small Crabs'), ess.mort.nat.init, group.info)
ess.mort.nat <- cbind(ess.mort.nat.init, Cod, Silver.Hake, Dem.Piscivores, Dem.Feeders, 
                      Halibut, Crabs)

ess <- subset(biomass.only, REGION=='ESS' & YEAR>=1970 & YEAR<2008)
ess$SPECIES <- droplevels(ess$SPECIES)

ess.preds <- list()
ess.preds$'Grey Seals' <- fill.pred.list('Grey seal', ess)
ess.preds$'Cod' <- fill.pred.list('Atlantic cod', ess)
ess.preds$'Silver.Hake' <- fill.pred.list('Silver hake', ess)
ess.preds$'Haddock' <- fill.pred.list('Haddock', ess)
ess.preds$'American plaice' <- fill.pred.list('American plaice', ess)
ess.preds$Halibut <- fill.pred.list(c('Atlantic halibut', 'Greenland halibut'), ess)
ess.preds$Flounders <- fill.pred.list(c('Winter flounder', 'Witch flounder', 'Yellowtail flounder', 'Windowpane'), ess)
ess.preds$'Skates' <- fill.pred.list(c('Skates (NS)', 'Barndoor skate'), ess)
ess.preds$'Dogfish' <- fill.pred.list('Spiny dogfish', ess)
ess.preds$'Redfish' <- fill.pred.list('Atlantic redfishes (NS)', ess)
ess.preds$'Pollock' <- fill.pred.list('Pollock', ess)
ess.preds$'Squid' <- fill.pred.list('Squids (NS)', ess)
ess.preds$'Dem.Piscivores' <- fill.pred.list(c('White hake', 'Cusk', 'Goosefish'), ess)
ess.preds$'Dem.Feeders' <- fill.pred.list(c('Wolffishes (NS)', 'Red hake', 'Lumpfish', 'Ocean pout'), ess)
ess.preds$'Small Demersals' <- fill.pred.list('Sculpin', ess)
ess.preds$'Shrimp' <- fill.pred.list('Pandalid Shrimp (NS)', ess)
ess.preds$'Bivalve Mollusc' <- fill.pred.list('Sea scallop', ess)
ess.preds$Sandlance <- fill.pred.list('Northern sand lance', ess)
ess.preds$'Small Pelagics' <- fill.pred.list(c('Atlantic argentine', 'Shad', 'Alewife', 'Atlantic butterfish'), ess)
ess.preds$'Crabs' <- fill.pred.list(c('Snow crab', 'Rock crab', 'Jonah crab'), ess)
ess.preds$Echinoderms <- fill.pred.list('Sea urchin', ess)

ess.predation <- get.predation(cameo.prey=c('Northern sand lance', 'Small pelagics'), region='ESS', 
                                 ecopath.prey=c('Sandlance', 'Small Pelagics'),
                                 pred.list=ess.preds, mort.nat=ess.mort.nat, group.info=group.info)

# Barents Sea -------------------------------------------------------------
# Start year?
ecosystem <- 'Barents Sea'
bs.diet <- get.nat.mort.matrix(ncompartment=41, ecosystem=ecosystem)
group.info <- read.group.info(ecosystem=ecosystem)
bs.mort.nat.init <- bs.diet*t(aaply(.data=group.info, .margins=1, .fun=expand.group, df=group.info, .expand=FALSE))
Herring <- combine.groups(c('Juvenile herring (1-3)', 'Adult herring (4+)'), bs.mort.nat.init, group.info)
Capelin <- combine.groups(c('Juvenile capelin (1)', 'Adult capelin (2+)'), bs.mort.nat.init, group.info)
Cod <- combine.groups(c('Juvenile cod (1-3)', 'Adult cod (4+)'), bs.mort.nat.init, group.info)
Haddock <- combine.groups(c('Juvenile haddock (1-3)', 'Adult haddock (4+)'), bs.mort.nat.init, group.info)
Saithe <- combine.groups(c('Adult saithe', 'Juvenile saithe'), bs.mort.nat.init, group.info)
bs.mort.nat <- cbind(bs.mort.nat.init, Herring, Capelin, Cod, Haddock, Saithe)

bs <- subset(grouped.long.ts, REGION=='BS' & YEAR >=1964)
bs$SPECIES <- droplevels(bs$SPECIES)

bs.preds <- list()
bs.preds$Herring <- fill.pred.list('Atlantic herring', bs)
bs.preds$Capelin <- fill.pred.list('Capelin', bs)
bs.preds$Cod <- fill.pred.list('Atlantic cod', bs)
bs.preds$Haddock <- fill.pred.list('Haddock', bs)
bs.preds$Redfishes <- fill.pred.list(c('Golden redfish', 'Beaked redfish'), bs)
bs.preds$'Benthic piscivores' <- fill.pred.list('Greenland halibut', bs) 
bs.preds$'Prawns & shrimps' <- fill.pred.list('Pandalid shrimps (NS)', bs)
bs.preds$Saithe <- fill.pred.list('Pollock', bs)

bs.predation <- get.predation(cameo.prey=c('Atlantic herring', 'Capelin'), region='BS', 
                                 ecopath.prey=c('Juvenile herring (1-3)', 'Juvenile capelin (1)'),
                                 pred.list=bs.preds, mort.nat=bs.mort.nat, group.info=group.info)

# Gulf of AK --------------------------------------------------------------
# Start year = 1961 or 1977
ecosystem <- 'GoA'
goa.diet <- get.nat.mort.matrix(ncompartment=122, ecosystem=ecosystem)
group.info <- read.group.info(ecosystem=ecosystem)

goa.mort.nat.init <- goa.diet*t(aaply(.data=group.info, .margins=1, .fun=expand.group, df=group.info, .expand=FALSE))
Pollock <- combine.groups(c('W. Pollock_Juv', 'W. Pollock'), goa.mort.nat.init, group.info)
Cod <- combine.groups(c('P. Cod_Juv', 'P. Cod'), goa.mort.nat.init, group.info)
Herring <- combine.groups(c('Herring_Juv', 'Herring'), goa.mort.nat.init, group.info)
Arrowtooth <- combine.groups(c('Arrowtooth_Juv', 'Arrowtooth'), goa.mort.nat.init, group.info)
Halibut <- combine.groups(c('P. Halibut_Juv', 'P. Halibut'), goa.mort.nat.init, group.info)
Flathead <- combine.groups(c('FH. Sole_Juv', 'FH. Sole'), goa.mort.nat.init, group.info)
Sablefish.comb <- combine.groups(c('Sablefish_Juv', 'Sablefish'), goa.mort.nat.init, group.info)
Perch <- combine.groups(c('POP_Juv', 'POP'), goa.mort.nat.init, group.info)
Salmon <- combine.groups(c('Salmon returning', 'Salmon outgoing'), goa.mort.nat.init, group.info)
Mackerel <- combine.groups(c('Atka mackerel', 'Atka mackerel_Juv'), goa.mort.nat.init, group.info)
Shortspine <- combine.groups(c('Shortspine Thorns_Juv', 'Shortspine Thorns'), goa.mort.nat.init, group.info)
  
goa.mort.nat <- cbind(goa.mort.nat.init, Pollock, Cod, Herring, Arrowtooth, Halibut, Flathead,
                      Sablefish.comb, Perch, Salmon, Mackerel, Shortspine)

goa <- subset(biomass.only, REGION == 'GoA' & YEAR >= 1961)
goa$SPECIES <- droplevels(goa$SPECIES)

goa.preds <- list()
goa.preds$Pollock <- fill.pred.list('Walleye pollock', goa)
goa.preds$Cod <- fill.pred.list('Pacific cod', goa)
goa.preds$Herring <- fill.pred.list('Pacific herring', goa)
goa.preds$Arrowtooth <- fill.pred.list('Arrowtooth flounder', goa)
goa.preds$Halibut <- fill.pred.list('Pacific halibut', goa)
goa.preds$Flathead <- fill.pred.list('Flathead sole', goa)
goa.preds$'Dover Sole' <- fill.pred.list('Dover sole', goa)
goa.preds$'Rex Sole' <- fill.pred.list('Rex sole', goa)
goa.preds$Sablefish.comb <- fill.pred.list('Sablefish', goa)
goa.preds$Perch <- fill.pred.list('Pacific ocean perch', goa)
goa.preds$'Northern Rock' <- fill.pred.list('Northern rockfish', goa)
goa.preds$'Dusky Rock' <- fill.pred.list('Dusky rockfish', goa)
goa.preds$'Rougheye Rock' <- fill.pred.list('Rougheye rockfish', goa)
goa.preds$Salmon <- fill.pred.list('Pacific salmon', goa)
goa.preds$'AK Plaice' <- fill.pred.list('Alaska plaice', goa)
goa.preds$Mackerel <- fill.pred.list('Atka mackerel', goa)
goa.preds$'Big skate' <- fill.pred.list('Big skate', goa)
goa.preds$Capelin <- fill.pred.list('Capelin', goa)
goa.preds$Eelpouts <- fill.pred.list('Eelpouts (NS)', goa)
goa.preds$Eulachon <- fill.pred.list('Eulachon', goa)
goa.preds$Greenlings <- fill.pred.list('Greenlings (NS)', goa)
goa.preds$Grenadiers <- fill.pred.list('Grenadiers (NS)', goa)
goa.preds$'Lg. Sculpins' <- fill.pred.list('Large sculpins (NS)', goa)
goa.preds$'Longnose skate' <- fill.pred.list('Longnose skate', goa)
goa.preds$'N. Rock sole' <- fill.pred.list('Northern rock sole', goa)
goa.preds$'Octopi ' <- fill.pred.list('Octopi (NS)', goa)
goa.preds$Sandlance <- fill.pred.list('Pacific sand lance', goa)
goa.preds$Pandalidae <- fill.pred.list('Pandalid shrimps (NS)', goa)
goa.preds$'Salmon shark' <- fill.pred.list('Salmon shark', goa)
goa.preds$'Other sculpins' <- fill.pred.list('Sculpins other (NS)', goa)
goa.preds$'Other Sebastes' <- fill.pred.list('Sebastes other (NS)', goa)
goa.preds$'Sharpchin Rock' <- fill.pred.list('Sharpchin rockfish', goa)
goa.preds$'Shortraker Rock' <- fill.pred.list('Shortraker rockfish', goa)
goa.preds$'Other skates' <- fill.pred.list('Skates (NS)', goa)
goa.preds$'Sleeper shark' <- fill.pred.list('Sleeper shark', goa)
goa.preds$'S. Rock sole' <- fill.pred.list('Southern rock sole', goa)
goa.preds$Dogfish <- fill.pred.list('Spiny dogfish', goa)
goa.preds$'YF. Sole' <- fill.pred.list('Yellowfin sole', goa)
goa.preds$Shortspine <- fill.pred.list('Shortspine thornyhead', goa)

goa.predation <- get.predation(cameo.prey=c('Pacific herring', 'Walleye pollock', 'Capelin'), #'Yellowfin sole', sand lance
                               ecopath.prey=c('Herring_Juv', 'W. Pollock_Juv', 'Capelin'), #'YF. Sole', sand lance
                               region='GoA', pred.list=goa.preds, mort.nat=goa.mort.nat, group.info=group.info)
                       
# Hecate Strait -----------------------------------------------------------

# YEAR should probably start in 1984 (most common start year among spp)
ecosystem <- 'Hecate Strait'
hs.diet <- get.nat.mort.matrix(ncompartment=50, ecosystem=ecosystem)
group.info <- read.group.info(ecosystem=ecosystem)
hs.mort.nat.init <- hs.diet*t(aaply(.data=group.info, .margins=1, .fun=expand.group, df=group.info, .expand=FALSE))

Pollock.comb <- combine.groups(c('Juvenile pollock', 'Pollock'), hs.mort.nat.init, group.info)
Herring <- combine.groups(c('Juvenile herring', 'Adult herring'), hs.mort.nat.init, group.info)
Halibut <- combine.groups(c('Juvenile halibut', 'Adult halibut'), hs.mort.nat.init, group.info)
Cod <- combine.groups(c('Juvenile Pacific cod', 'Adult Pacific cod'), hs.mort.nat.init, group.info)
Sablefish <- combine.groups(c('Juvenile sablefish', 'Adult sablefish'), hs.mort.nat.init, group.info)
Lingcod <- combine.groups(c('Juvenile lingcod', 'Adult lingcod'), hs.mort.nat.init, group.info)
Turbot <- combine.groups(c('Juvenile turbot', 'Adult turbot'), hs.mort.nat.init, group.info)
Flatfish <- combine.groups(c('Juvenile flatfish', 'Adult flatfish'), hs.mort.nat.init, group.info)  
Pisc.rockfish <- combine.groups(c('Juvenile picivorous rockfish', 'Adult picivorous rockfish'),
                           hs.mort.nat.init, group.info)
Plank.rockfish <- combine.groups(c('Juvenile planktivorous rockfish', 'Adult planktivorous rockfish'),
                                 hs.mort.nat.init, group.info)

hs.mort.nat <- cbind(hs.mort.nat.init, Pollock.comb, Herring, Halibut, Cod, Sablefish, Lingcod,
                     Turbot, Flatfish, Pisc.rockfish, Plank.rockfish)

hs <- subset(biomass.only, REGION=='HS' & YEAR >= 1984)
hs$SPECIES <- droplevels(hs$SPECIES)

hs.preds <- list()
hs.preds$Pollock.comb <- fill.pred.list('Walleye pollock', hs)
hs.preds$Herring <- fill.pred.list('Pacific herring', hs)
hs.preds$Halibut <- fill.pred.list('Pacific halibut', hs)
hs.preds$Cod <- fill.pred.list('Pacific cod', hs)
hs.preds$Sablefish <- fill.pred.list('Sablefish', hs)
hs.preds$Lingcod <- fill.pred.list('Lingcod', hs)
hs.preds$Skates <- fill.pred.list('Skates (NS)', hs)
hs.preds$Dogfish <- fill.pred.list('Spiny dogfish', hs)
hs.preds$Ratfish <- fill.pred.list('Spotted ratfish', hs)
hs.preds$Turbot <- fill.pred.list('Arrowtooth flounder', hs)
hs.preds$Flatfish <- fill.pred.list(c('English sole', 'Dover sole', 'Butter sole', 'Petrale sole',
                                      'Rex sole', 'Slender sole', 'Flathead sole', 
                                      'Pacific sanddab', 'Southern rock sole', 'Curlfin sole',
                                      'Sand Sole'), hs)
hs.preds$Pisc.rockfish <- fill.pred.list('Silvergray rockfish', hs)
hs.preds.Plank.rockfish <- fill.pred.list(c('Bocaccio', 'Yellowtail rockfish'), hs)
hs.preds$'Inshore rockfish' <- fill.pred.list('Quillback rockfish', hs)
hs.preds$'Seals, sea lions' <- fill.pred.list(c('Harbour seal', 'Steller sea lions'), hs)
hs.preds$Eulachon <- fill.pred.list('Eulachon', hs)
hs.preds$Odontocetae <- fill.pred.list('Resident killers', hs)

hs.predation <- get.predation(cameo.prey=c('Pacific herring', 'Flatfish'),
                                           #'Slender sole', 'Pacific sanddab',
                                           #'Curlfin sole', 'Rex sole'),  
                               ecopath.prey=c('Juvenile herring', 'Juvenile flatfish'),
                                              #'Juvenile flatfish', 'Juvenile flatfish', 
                                              #'Juvenile flatfish', 'Juvenile flatfish'),
                               region='HS', pred.list=hs.preds, mort.nat=hs.mort.nat, group.info=group.info)                       

# North Sea ---------------------------------------------------------------
# Start year = 1963 (same for all species)
ecosystem <- 'North Sea'
nort <- subset(biomass.only, REGION=='NORT')
nort$SPECIES <- droplevels(nort$SPECIES)

nort.diet <- get.nat.mort.matrix(ncompartment=68, ecosystem=ecosystem)
group.info <- read.group.info(ecosystem=ecosystem)
nort.mort.nat.init <- nort.diet*t(aaply(.data=group.info, .margins=1, .fun=expand.group, df=group.info, .expand=FALSE))

Cod <- combine.groups(c('Juvenile Cod(0-2, 0-40cm)', 'Cod (adult)'), nort.mort.nat.init, group.info)
Herring <- combine.groups(c('Herring (juvenile 0, 1)', 'Herring (adult)'), nort.mort.nat.init,
                          group.info)
Whiting <- combine.groups(c('Juvenile Whiting (0-1, 0-20cm)', 'Whiting (adult)'), nort.mort.nat.init,
                          group.info)
Haddock <- combine.groups(c('Juvenile Haddock (0-1, 0-20cm)', 'Haddock (adult)'), nort.mort.nat.init,
                          group.info)
Saithe <- combine.groups(c('Juvenile Saithe (0-3, 0-40cm)', 'Saithe (adult)'), nort.mort.nat.init,
                         group.info)
nort.mort.nat <- cbind(nort.mort.nat.init, Herring, Whiting, Haddock, Saithe, Cod)

nort.preds <- list()
nort.preds$Herring <- fill.pred.list('Atlantic herring', nort)
nort.preds$Whiting <- fill.pred.list('Whiting', nort)
nort.preds$Haddock <- fill.pred.list('Haddock', nort)
nort.preds$Cod <- fill.pred.list('Atlantic cod', nort)
nort.preds$Plaice <- fill.pred.list('European plaice', nort)
nort.preds$Sandeels <- fill.pred.list('Lesser sandeel', nort)
nort.preds$'Norway pout' <- fill.pred.list('Norway pout', nort)
nort.preds$Sole <- fill.pred.list('Common sole', nort)
nort.preds$Saithe <- fill.pred.list('Pollock', nort)
# Pollock in database = Gadoid or Saithe?

nort.predation <- get.predation(cameo.prey=c('Atlantic herring', 'Lesser sandeel', 'Norway pout', 'Common sole', 'Whiting'),  
                               ecopath.prey=c('Herring (juvenile 0, 1)', 'Sandeels', 'Norway pout', 'Sole', 'Juvenile Whiting (0-1, 0-20cm)'),
                               region='NORT', pred.list=nort.preds, mort.nat=nort.mort.nat, group.info=group.info)

# Gulf of Maine -----------------------------------------------------------
# start year = 1967, first herring year
ecosystem <- 'GoM'
gom <- subset(biomass.only, REGION == 'GoM' & YEAR >= 1967)
gom$SPECIES <- droplevels(gom$SPECIES)

gom.diet <- get.nat.mort.matrix(ncompartment=31, ecosystem=ecosystem)
group.info <- read.group.info(ecosystem=ecosystem)
gom.mort.nat <- gom.diet*t(aaply(.data=group.info, .margins=1, .fun=expand.group, df=group.info, .expand=FALSE))

gom.preds <- list()
gom.preds$"Demersal benthivores" <- fill.pred.list(c('American plaice', 'Atlantic redfishes (NS)', 
                                                      'Atlantic wolffish', 'Fourspot flounder', 
                                                      'Haddock', 'Ocean pout', 'Red hake', 'Scup',
                                                      'Windowpane', 'Winter flounder', 
                                                      'Witch flounder', 'Yellowtail flounder'), gom)
gom.preds$"Small pelagics- commercial" <- fill.pred.list(c('Atlantic butterfish', 'Atlantic herring',
                                                          'Atlantic mackerel'), gom)
gom.preds$"Demersals piscivores" <- fill.pred.list(c('Atlantic cod', 'Atlantic halibut', 
                                                     'Goosefish', 'Pollock', 'Silver hake', 
                                                     'Spiny dogfish', 'Summer flounder', 
                                                     'White hake'), gom)
gom.preds$"Squids" <- fill.pred.list(c('Longfin squid', 'Shortfin squid'), gom)
gom.preds$"Small pelagics- other" <- fill.pred.list('Northern sand lance', gom)
gom.preds$"Demersals omnivores" <- fill.pred.list('Skates (NS)', gom)
# skates are demersal omnivores or benthivores?
gom.predation <- get.predation(cameo.prey=c('Commercial pelagics', # 'Atlantic herring', 
                                            'Other pelagics'),
                                           # 'Atlantic mackerel'),  
                                ecopath.prey=c('Small pelagics- commercial', #'Small pelagics- commercial',
                                               'Small pelagics- other'), #'Small pelagics- commercial'),
                                region='GoM', pred.list=gom.preds, mort.nat=gom.mort.nat, group.info=group.info)

# Georges Bank ------------------------------------------------------------
# Use all years
ecosystem <- 'GB'
gb <- filter(biomass.only, REGION == ecosystem)
gb$SPECIES <- droplevels(gb$SPECIES)

gb.diet <- get.nat.mort.matrix(ncompartment=31, ecosystem=ecosystem)
group.info <- read.group.info(ecosystem=ecosystem)
gb.mort.nat <- gb.diet*t(aaply(.data=group.info, .margins=1, .fun=expand.group, df=group.info, .expand=FALSE))

gb.preds <- list()
gb.preds$"Demersal benthivores" <- fill.pred.list(c('American plaice', 'Atlantic redfishes (NS)', 
                                                       'Atlantic wolffish', 'Fourspot flounder', 
                                                       'Haddock', 'Ocean pout', 'Red hake', 'Scup',
                                                       'Windowpane', 'Winter flounder', 
                                                       'Witch flounder', 'Yellowtail flounder'), gb)
gb.preds$"Small pelagics- commercial" <- fill.pred.list(c('Atlantic butterfish', 'Atlantic herring',
                                                            'Atlantic mackerel'), gb)
gb.preds$"Demersals piscivores" <- fill.pred.list(c('Atlantic cod', 'Atlantic halibut', 
                                                       'Goosefish', 'Pollock', 'Silver hake', 
                                                       'Spiny dogfish', 'Summer flounder', 
                                                       'White hake', 'Smooth dogfish'), gb)
gb.preds$"Squids" <- fill.pred.list(c('Longfin squid', 'Shortfin squid'), gb)
gb.preds$"Small pelagics- other" <- fill.pred.list('Northern sand lance', gb)
gb.preds$"Demersals omnivores" <- fill.pred.list('Skates (NS)', gb)

gb.predation <- get.predation(cameo.prey=c('Commercial pelagics', #'Atlantic herring', 
                                           'Other pelagics'),
                                            # 'Atlantic mackerel'),  
                               ecopath.prey=c('Small pelagics- commercial', #'Small pelagics- commercial',
                                              'Small pelagics- other'), # 'Small pelagics- commercial'),
                               region='GB', pred.list=gb.preds, mort.nat=gb.mort.nat, group.info=group.info)



# Eastern Bering Sea ------------------------------------------------------
# Start year = 1982
ecosystem <- 'EBS'
ebs <- subset(biomass.only, REGION == 'EBS' & YEAR >= 1982)
ebs$SPECIES <- droplevels(ebs$SPECIES)

ebs.diet <- get.nat.mort.matrix(ncompartment=129, ecosystem=ecosystem)
group.info <- read.group.info(ecosystem=ecosystem)
ebs.mort.nat.init <- ebs.diet*t(aaply(.data=group.info, .margins=1, .fun=expand.group, df=group.info, .expand=FALSE))

Pollock <- combine.groups(c('W. Pollock_Juv', 'W. Pollock'), ebs.mort.nat.init, group.info)
Cod <- combine.groups(c('P. Cod_Juv', 'P. Cod'), ebs.mort.nat.init, group.info)
all.herring <- combine.groups(c('Herring_Juv', 'Herring'), ebs.mort.nat.init, group.info)
Arrowtooth <- combine.groups(c('Arrowtooth_Juv', 'Arrowtooth'), ebs.mort.nat.init, group.info)
Kamchatka <- combine.groups(c('Kamchatka fl._Juv', 'Kamchatka fl.'), ebs.mort.nat.init, group.info)
Turbot <- combine.groups(c('Gr. Turbot_Juv', 'Gr. Turbot'), ebs.mort.nat.init, group.info)
Halibut <- combine.groups(c('P. Halibut_Juv', 'P. Halibut'), ebs.mort.nat.init, group.info)
Yellowfin <- combine.groups(c('YF. Sole_Juv', 'YF. Sole'), ebs.mort.nat.init, group.info)
Flathead <- combine.groups(c('FH. Sole_Juv', 'FH. Sole'), ebs.mort.nat.init, group.info)
Rock.sole <- combine.groups(c('N. Rock sole_Juv', 'N. Rock sole'), ebs.mort.nat.init, group.info)
all.sablefish <- combine.groups(c('Sablefish_Juv', 'Sablefish'), ebs.mort.nat.init, group.info)
Mackerel <- combine.groups(c('Atka mackerel_Juv', 'Atka mackerel'), ebs.mort.nat.init, group.info)
Salmon <- combine.groups(c('Salmon returning', 'Salmon outgoing'), ebs.mort.nat.init, group.info)

ebs.mort.nat <- cbind(ebs.mort.nat.init, Pollock, Cod, all.herring, Arrowtooth, Kamchatka, Turbot,
                      Halibut, Yellowfin, Flathead, Rock.sole, all.sablefish, Mackerel, Salmon)

ebs.preds <- list()
ebs.preds$'Sleeper shark' <- fill.pred.list('Sleeper shark', ebs) #
ebs.preds$Pollock <- fill.pred.list('Walleye pollock', ebs) #
ebs.preds$Cod <- fill.pred.list('Pacific cod', ebs) #
ebs.preds$Halibut <- fill.pred.list('Pacific halibut', ebs) #
ebs.preds$Turbot <- fill.pred.list('Greenland halibut', ebs) #
ebs.preds$all.herring <- fill.pred.list('Pacific herring', ebs) #
ebs.preds$Arrowtooth <- fill.pred.list('Arrowtooth flounder', ebs) #
ebs.preds$Kamchatka <- fill.pred.list('Kamchatka flounder', ebs) #
ebs.preds$Yellowfin <- fill.pred.list('Yellowfin sole', ebs) #
ebs.preds$Flathead <- fill.pred.list('Flathead sole', ebs) #
ebs.preds$Rock.sole <- fill.pred.list('Northern rock sole', ebs) #
ebs.preds$'AK Plaice' <- fill.pred.list('Alaska plaice', ebs) #
ebs.preds$'Dover Sole' <- fill.pred.list('Dover sole', ebs) #
ebs.preds$'Rex Sole' <- fill.pred.list('Rex sole', ebs) #
ebs.preds$'Alaska skate' <- fill.pred.list('Alaska skate', ebs) #
ebs.preds$'Other skates' <- fill.pred.list('Skates (NS)', ebs) #
ebs.preds$all.sablefish <- fill.pred.list('Sablefish', ebs) #
ebs.preds$Eelpouts <- fill.pred.list('Eelpouts (NS)', ebs) #
ebs.preds$POP <- fill.pred.list('Pacific ocean perch', ebs) #
ebs.preds$'Northern Rock' <- fill.pred.list('Northern rockfish', ebs) #
ebs.preds$'Dusky Rock' <- fill.pred.list('Dusky rockfish', ebs) #
ebs.preds$'Rougheye Rock' <- fill.pred.list('Rougheye rockfish', ebs) #
ebs.preds$Mackerel <- fill.pred.list('Atka mackerel', ebs) #
ebs.preds$Octopi <- fill.pred.list('Octopi (NS)', ebs) #
ebs.preds$Squids <- fill.pred.list('Squids (NS)', ebs) #
ebs.preds$Salmon <- fill.pred.list('Pacific salmon', ebs) #
ebs.preds$Capelin <- fill.pred.list('Capelin', ebs) #
ebs.preds$Sandlance <- fill.pred.list('Pacific sand lance', ebs) #
ebs.preds$'King Crab' <- fill.pred.list('King crab', ebs)
ebs.preds$Pandalidae <- fill.pred.list('Pandalid shrimps (NS)', ebs) #
ebs.preds$'NP shrimp' <- fill.pred.list('Shrimps (NS)', ebs)
ebs.preds$'Sea stars' <- fill.pred.list('Sea stars (NS)', ebs)
ebs.preds$'Brittle stars' <- fill.pred.list('Brittle stars (NS)', ebs)
ebs.preds$'Hermit crabs' <- fill.pred.list('Hermit crabs (NS)', ebs)
ebs.preds$Corals <- fill.pred.list('Corals (NS)', ebs)
ebs.preds$'Sea Pens' <- fill.pred.list('Sea pens (NS)', ebs)
ebs.preds$Sponges <- fill.pred.list('Sponges (NS)', ebs)
ebs.preds$Bivalves <- fill.pred.list('Bivalves (NS)', ebs)
ebs.preds$Polychaetes <- fill.pred.list('Polychaetes (NS)', ebs)
ebs.preds$Eulachon <- fill.pred.list('Eulachon', ebs) #
ebs.preds$'Lg. Sculpins' <- fill.pred.list('Large sculpins (NS)', ebs) #
ebs.preds$'Other sculpins' <- fill.pred.list('Sculpins other (NS)', ebs) #
ebs.preds$Greenlings <- fill.pred.list('Greenlings (NS)', ebs) #
ebs.preds$Snails <- fill.pred.list('Snails (NS)', ebs)
ebs.preds$Anemones <- fill.pred.list('Anemones (NS)', ebs)
ebs.preds$'Misc. worms' <- fill.pred.list('Worms (NS)', ebs)
ebs.preds$Urochordata <- fill.pred.list('Urochordata (NS)', ebs)
ebs.preds$'Misc. crabs' <- fill.pred.list(c('Crabs other (NS)', 'Snow crab', 'Tanner crab'), ebs)
ebs.preds$'Misc. Flatfish' <- fill.pred.list('Flatfishes (NS)', ebs)
ebs.preds$'Misc. Crustacean' <- fill.pred.list('Crustaceans other (NS)', ebs)
ebs.preds$'Oth. managed forage' <- fill.pred.list('Forage fishes other (NS)', ebs)
ebs.preds$'Scyphozoid Jellies' <- fill.pred.list('Scyphozoid jellies (NS)', ebs)                                               

               
ebs.predation <- get.predation(cameo.prey=c('Pacific herring', 'Capelin', 'Yellowfin sole',
                                            'Pacific sand lance', 'Walleye pollock'),  
                               ecopath.prey=c('Herring_Juv', 'Capelin', 'YF. Sole_Juv',
                                              'Sandlance', 'W. Pollock_Juv'),
                               region='EBS', pred.list=ebs.preds, mort.nat=ebs.mort.nat, group.info=group.info)

                  

# Combining ecosystems into one data frame --------------------------------

all.regions.pred <- list(#sgosl.predation, 
  balt.predation, wss.predation, ess.predation, bs.predation, 
                         goa.predation, hs.predation, nort.predation, gom.predation, gb.predation,
                         ebs.predation)

# Combine all regions, predator signal time series divided by predator species 
all.regions.byspp <- subset(do.call(rbind, lapply(all.regions.pred, function(predation) predation$byspp)),
                            pred.signal > 0)


all.regions.byspp$species.combo <- all.regions.byspp$species
all.regions.byspp$species.combo[which(all.regions.byspp$species=='Redfishes')] <- 'Redfish'
all.regions.byspp$species.combo[which(all.regions.byspp$species=='Turbot')] <- 'Arrowtooth'
all.regions.byspp$species.combo[which(all.regions.byspp$species=='Sablefish.comb')] <- 'Sablefish'
all.regions.byspp$species.combo[which(all.regions.byspp$species=='A.Plaice')] <- 'American plaice'
all.regions.byspp$species.combo[which(all.regions.byspp$species=='Other skates')] <- 'Skates'
all.regions.byspp$species.combo[which(all.regions.byspp$species=='Saithe')] <- 'Pollock'
all.regions.byspp$species.combo[which(all.regions.byspp$species=='L.Sculpin')] <- 'Lg. Sculpins'
# all.regions.byspp$species.combo[which(all.regions.byspp$species=='Longnose skate')] <- 'Skates'
# all.regions.byspp$species.combo[which(all.regions.byspp$species=='Big skate')] <- 'Skates'
# all.regions.byspp$species.combo[which(all.regions.byspp$species=='Seals')] <- 'Seals, sea lions'
# all.regions.byspp$species.combo[which(all.regions.byspp$species=='Grey Seals')] <- 'Seals, sea lions'
all.regions.byspp$species.combo[which(all.regions.byspp$species=='D.Piscivores')] <- 'Dem.Piscivores'
all.regions.byspp$species.combo[which(all.regions.byspp$species=='Demersals - piscivores')] <- 'Dem.Piscivores'

all.regions.byspp$species <- factor(all.regions.byspp$species)
all.regions.byspp$species <- droplevels(all.regions.byspp$species)
all.regions.byspp$species.combo <- factor(all.regions.byspp$species.combo)
all.regions.byspp$prey.spp <- factor(all.regions.byspp$prey.spp)

# Combine all regions, based on Ecopath equilibrium predator biomasses (i.e., snapshot)
mort.props <- do.call(rbind, lapply(all.regions.pred, function(predation) predation$mort.prop))
mort.props <- subset(mort.props, mort>0)
rownames(mort.props) <- NULL

# Associate Ecopath species names with CAMEO species names in mort.props and all.regions.byspp data frames
# Requires some fiddling with factors/characters
join.ecopath.cameo <- read.csv('join_ecopath_cameo.csv', stringsAsFactors = FALSE)
# index <- which(join.ecopath.cameo$CombinedEcopathName == '')
# join.ecopath.cameo$CombinedEcopathName[index] <- join.ecopath.cameo$EcopathName[index]
join.ecopath.cameo <- data.frame(lapply(join.ecopath.cameo, factor))
names(join.ecopath.cameo) <- c('pred', 'CameoName', 'region', 'CombinedEcopathName', 'TL', 'Len')

cast.ecopath.cameo <- dcast(join.ecopath.cameo, formula=pred + region ~ CameoName, 
                            value.var='CombinedEcopathName')
cast.ecopath.cameo[,3] <- TRUE
names(cast.ecopath.cameo)[3] <- 'in.cameo.true'
mort.props.joined <- left_join(mort.props, cast.ecopath.cameo[,1:3]) %>% mutate(in.cameo = !is.na(in.cameo.true)) 

pred.merged <- left_join(x=all.regions.byspp, 
                         y=rename(grouped.long.ts, year = YEAR, region = REGION, prey.spp = SPECIES),
                         by=c('year', 'prey.spp', 'region')) %>%  
                     select(-rownames) %>% group_by(region, prey.spp, year) %>%
  summarize(tot.pred = sum(pred.signal), sp = first(sp), bio.s = first(smoothed), bio = first(BIOMASS))
# checked 5/9/14: taking the first value of sp was ok, because all values of sp are equal, assumed same for smoothed, BIOMASS
pred.merged$region <- factor(pred.merged$region)
pred.merged$prey.spp <- factor(pred.merged$prey.spp)

#' # Adding recruitment data -------------------------------------------------
#' # Not using for this paper, so commented out
#' setwd("~/Dropbox/Chapter2/data/ram")
#' area <- read.csv('area.csv')
#' stock <- read.csv('stock.csv')
#' timeseries <- read.csv('timeseries.csv')
#' herring.areas <- filter(stock, grepl('herring', commonname, ignore.case=TRUE))$areaid
#' # filter(area, areaid %in% herring.areas)
#' 
#' areanames <- c('Southern Gulf of St. Lawrence', # sGoSL herring
#'                'Scotian Shelf and Bay of Fundy', # WSS herring
#'                'North Sea', # NORT herring, Norway pout
#'                'Baltic Areas 22-32', # BALT sprat
#'                'Barents Sea', # BS capelin
#'                'Eastern Baltic', # BALT herring (only covers areas 25-32, missing 22-24
#'                '22-24-IIIa', # Western BALT herring
#'                'Prince Rupert District', # HS herring
#'                'Queen Charlotte Islands', # HS herring
#'                'Hecate Strait', # HS English sole
#'                'Gulf of Alaska' #GoA Dover sole, Rex sole, Walleye pollock
#' )
#' 
#' regnames <- c('sGoSL', 'WSS', 'NORT', 'BALT', 'BS', 'BALT', 'BALT', 'HS', 'HS', 'HS', 'GoA')
#' ram.cameo.match <- data.frame(ram=areanames, cameo=regnames)
#' #ram.cameo.match$species <- c('Atlantic herring', 'Atlantic herring', 'Atlantic herring', 'Sprat', 'Capelin', 'Atlantic herring',
#' #'Atlantic herring', 'Pacific herring', 'Pacific herring')
#' 
#' recruit.data <- select(area, areaname, areaid) %>% 
#'   filter(areaname %in% ram.cameo.match$ram) %>% 
#'   inner_join(stock) %>% inner_join(timeseries) %>%
#'   filter(tsid == 'R-E00') %>% 
#'   filter(scientificname %in% c('Clupea pallasii', 'Clupea harengus',
#'                                'Sprattus sprattus', 'Mallotus villosus', 'Microstomus pacificus',
#'                                'Glyptocephalus zachirus', 'Theragra chalcogramma', 'Parophrys vetulus',
#'                                'Trisopterus esmarkii')) %>%
#'   merge(y=ram.cameo.match, by.x='areaname', by.y='ram')
#' 
#' recruit.data$commonname <- as.character(recruit.data$commonname)
#' recruit.data$commonname[which(recruit.data$commonname %in% c('Herring', 'herring'))] <- 'Atlantic herring'
#' recruit.data <- group_by(recruit.data, cameo, commonname, tsyear) %>% summarize(rec.tot = sum(tsvalue))
#' recruit.data$tsyear <- as.numeric(recruit.data$tsyear)
#' 
#' pred.ts.data <- left_join(pred.merged, select(data.frame(recruit.data), region=cameo, year=tsyear, prey.spp=commonname, rec.tot))

save('pred.ts.data', 'all.regions.byspp', 'mort.props.joined', file = '~/Dropbox/chapter2/R/predation_dfs.RData')

