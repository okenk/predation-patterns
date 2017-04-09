source("~/Dropbox/Chapter2/R/pred-time-series.R")
setwd("~/Dropbox/Chapter2/data")

# 80s ---------------------------------------------------------------------

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


sgosl.predation.80s <- get.predation(cameo.prey=c('Capelin', 'Small dem. feeders',  
                                              'Plank. small pelagics', 'Flounders'), 
                                 ecopath.prey=c('Capelin', 'Small demersal feeders', 
                                                'Planktivorous small pelagics', 'Flounders'),
                                 region='80s', pred.list=sgosl.preds, mort.nat=gsl.mort.nat, group.info=group.info)



# 90s ---------------------------------------------------------------------

ecosystem <- 'sGoSL90s'
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


# Combining into one df ---------------------------------------------------

sgosl.predation.90s <- get.predation(cameo.prey=c('Capelin', 'Small dem. feeders',  
                                              'Plank. small pelagics', 'Flounders'), 
                                 ecopath.prey=c('Capelin', 'Small demersal feeders', 
                                                'Planktivorous small pelagics', 'Flounders'),
                                 region='90s', pred.list=sgosl.preds, mort.nat=gsl.mort.nat, group.info=group.info)


sgosl.80s.90s <- list(sgosl.predation.80s, sgosl.predation.90s)

# Combine all regions, predator signal time series divided by predator species 
sgosl.byspp <- subset(do.call(rbind, lapply(sgosl.80s.90s, function(predation) predation$byspp)),
                            pred.signal > 0)

sgosl.byspp$species <- factor(sgosl.byspp$species)
sgosl.byspp$species <- droplevels(sgosl.byspp$species)
sgosl.byspp$prey.spp <- factor(sgosl.byspp$prey.spp)



# tests_etc on just sgosl -------------------------------------------------

ordered.preds.short <- group_by(sgosl.byspp, region, prey.spp, species) %>% 
  summarize(avg_pred = mean(pred.signal)) %>% arrange(desc(avg_pred)) %>%
  mutate(rank=order(avg_pred, decreasing = TRUE), pred.prop=cumsum(avg_pred/sum(avg_pred))) 

sgosl.ordered.preds <- right_join(ordered.preds.short, sgosl.byspp)


cutoff.pct <- .9
sgosl.fewer.preds <- summarize(ordered.preds.short, cutoff.no = sum(pred.prop<cutoff.pct) + 1) %>% 
  right_join(ordered.preds.short) %>% right_join(sgosl.byspp) %>%
  filter(rank <= cutoff.no)

# Bootstrap null dist'n of VFs
n.boot <- 1000
set.seed(78378)
sgosl.boot.vf <- group_by(sgosl.byspp, region, prey.spp, species) %>%
  # Bootstrap each predator species time series
  do(boot.ts=tsboot(tseries=.$pred.signal, statistic=identity, R=n.boot, sim='scramble')$t) %>% 
  # Calculate the boostrapped VF for each predator assemblage (i.e., for each prey spp)
  group_by(region, prey.spp) %>% do(null.vf.distn=calc.boot.vf(boot.ls=.$boot.ts)) %>%
  mutate(low.quant = quantile(null.vf.distn, probs = .05),
         high.quant = quantile(null.vf.distn, probs = .95))

sgosl.boot.vf.few.pred <- group_by(sgosl.fewer.preds, region, prey.spp, species) %>%
  # Bootstrap each predator species time series
  do(boot.ts=tsboot(tseries=.$pred.signal, statistic=identity, R=n.boot, sim='scramble')$t) %>% 
  # Calculate the boostrapped VF for each predator assemblage (i.e., for each prey spp)
  group_by(region, prey.spp) %>% do(null.vf.distn=calc.boot.vf(boot.ls=.$boot.ts)) %>%
  mutate(low.quant = quantile(null.vf.distn, probs = .05),
         high.quant = quantile(null.vf.distn, probs = .95))

sgosl.compensation <- calc.vf(sgosl.byspp) %>%
  left_join(sgosl.boot.vf) %>% mutate(significant = vf<low.quant | vf>high.quant)
sgosl.compensation$region <- as.factor(compensation$region)

sgosl.compensation.few <- calc.vf(sgosl.fewer.preds) %>%
  left_join(sgosl.boot.vf.few.pred) %>% mutate(significant = vf<low.quant | vf>high.quant)
sgosl.compensation.few$region <- as.factor(sgosl.compensation.few$region)





# Predator index composition ----------------------------------------------

setwd('~/Dropbox/chapter2/figs')

col.scale <- brewer.pal(10, 'Set3')
region.order <- c('80s', '90s')
year.range <- range(sgosl.ordered.preds$year)

mat <- matrix(c(9,9,9,9,
                1,2,3,4,
                10,10,10,10,
                5,6,7,8), byrow=TRUE, ncol=4)

pdf('sgosl-pred-indices.pdf', width = 9, height=9)
layout(mat,heights = rep(c(1,4.5), 2))
par(mar=c(1,.5,1,.5), oma=c(3,0,1,0))
for(reg in region.order) {
  this.region <- filter(sgosl.ordered.preds, region==reg, rank<=10)
  for(prey in unique(this.region$prey.spp)) {
    this.prey <- filter(this.region, prey.spp==prey)
    max.pred <- group_by(this.prey, year) %>% summarize(tot.pred = sum(pred.signal)) %>% 
      with(max(tot.pred))
    with(this.prey, plot(year, pred.signal, type='n', ylim=1.1 * c(0, max.pred), axes=FALSE,
                         ann=FALSE, xlim=year.range))
    box()
    mtext(prey, 3)
    this.pred <- filter(this.prey, rank==1)
    with(this.pred, polygon(x = c(sort(year), rev(sort(year))), 
                            y = c(pred.signal[order(year)], rep(0, nrow(this.pred))),
                            col=col.scale[1]))
    bottom <- this.pred$pred.signal[order(this.pred$year)]
    for(pred in 2:max(this.prey$rank)) {
      this.pred <- filter(this.prey, rank==pred)
      with(this.pred, polygon(x=c(sort(year), rev(sort(year))), 
                              y = c(bottom + pred.signal[order(year)], rev(bottom)),
                              col=col.scale[pred]))
      bottom <- with(this.pred, bottom + pred.signal[order(year)])
    }
    
    if(reg=='90s') axis(1)
  }
}
#### Region Labels ####
par(mar=c(.5,0,0,0))
plot.region.name('sGoSL, 1980s')
plot.region.name('sGoSL, 1990s')
mtext('Year', 1, 2, outer=TRUE)
dev.off()
system2("open",args=c("-a Skim.app","sgosl-pred-indices.pdf"))


# violin plots ------------------------------------------------------------

maxes <- sapply(sgosl.compensation.few$null.vf.distn, function(xx) 
  c(max(abs(density(xx)$x)), max(density(xx)$y))) %>%
  apply(1, max)

cols <- brewer.pal(2, 'Set3') %>% #brewer.pal(length(levels(compensation.few$region)), 'Set3') %>%
  adjustcolor(alpha.f = .5)
cols[2] <- adjustcolor('#CDC673', alpha.f = .5)

pdf('sgosl-violin-plots.pdf', width=4, height=5)
par(mfrow=c(2,1), mar=c(2,1,1,1))
for(spp in 1:nrow(sgosl.compensation.few)) {
  if(spp %% 4 == 1) {
    plot(1,1, type='n', xlim=c(0, 8), ylim=c(0, 3.25), axes=FALSE, ann=FALSE)
    abline(h=1)
     if(spp == 1){
       text(4, 0, 'sGoSL 1980s', adj=c(.5,1), xpd=NA)
    }
  }
  dens <- density(sgosl.compensation.few$null.vf.distn[[spp]])
  if(spp <= 4) {
    if(sgosl.compensation.few$nspp[spp] > 1) {
      polygon(c(dens$y, -rev(dens$y))/4 + (2*(spp))-1, c(dens$x, rev(dens$x)),
              col = 'white', border=NA, xpd=NA)
      polygon(c(dens$y, -rev(dens$y))/4 + (2*(spp))-1, c(dens$x, rev(dens$x)),
              col = cols[sgosl.compensation.few$region[spp]], xpd=NA)
    }
    points(2*(spp)-1, sgosl.compensation.few$vf[spp], pch=16)
    text(2*(spp)-1, 3.15, sgosl.compensation.few$nspp[spp])
  } else {
    if(sgosl.compensation.few$nspp[spp] > 1) {
      polygon(c(dens$y, -rev(dens$y))/4 + (2*(spp-4))-1, c(dens$x, rev(dens$x)),
              col = 'white', border=NA, xpd=NA)
      polygon(c(dens$y, -rev(dens$y))/4 + (2*(spp-4))-1, c(dens$x, rev(dens$x)),
              col = cols[sgosl.compensation.few$region[spp]], xpd=NA)
    }  
    points(2*(spp-4)-1, sgosl.compensation.few$vf[spp], pch=16)
    text(2*(spp-4)-1, 3.15, sgosl.compensation.few$nspp[spp])    
  }
}

text(4, 0, 'sGoSL 1990s', adj=c(.5,1), xpd=NA)
dev.off()
system2("open",args=c("-a Skim.app",'sgosl-violin-plots.pdf'))

