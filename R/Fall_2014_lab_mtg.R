require(gridExtra)
require(ggplot2)
require(dplyr)

source("~/Dropbox/Chapter2/R/tests_etc.R")
setwd("~/Dropbox/Chapter2/figs/Fall_2014_lab_mtg")

plot.data <- mutate(all.regions.byspp, region=factor(region), prey.spp=factor(prey.spp)) %>%
  group_by(region, prey.spp, species.combo, year) %>% 
  summarize(pred.signal = sum(pred.signal)) %>%
  filter(region!='GoA' | prey.spp!='Yellowfin sole') %>% # weird time series
  filter(region!='GB' | (prey.spp!='Atlantic herring' & prey.spp!='Atlantic mackerel')) %>% # redundant ts
  filter(region!='GoM' | (prey.spp!='Atlantic herring' & prey.spp!='Atlantic mackerel')) %>% # redundant ts
  filter(region!='HS' | (prey.spp!='English sole' & prey.spp!='Pacific sanddab' &
                           prey.spp!='Rex sole' & prey.spp!='Slender sole')) # redundant ts
  
  
dominant.preds <- ungroup(plot.data) %>% group_by(region, prey.spp, year) %>%
  summarize(dominant.pred.by.yr = species.combo[which.max(pred.signal)]) %>%
  summarize(species.combo = names(which.max(summary(dominant.pred.by.yr))))

test <- semi_join(plot.data, dominant.preds) %>% # Pick out time series for dominant preds only
  ungroup() %>% group_by(region, prey.spp, species.combo) %>%
  left_join(select(pred.merged, region, prey.spp, year, tot.pred))  %>% # Join with total predation
  mutate(pred.frac = pred.signal/tot.pred) %>% ungroup()

  ggplot(test) + geom_line(aes(x=year, y=pred.frac, col=prey.spp)) + 
      facet_wrap(~ region, scales='free_x')

head(test)
  pdf('all-pred-fields.pdf', width=20, height=15)
ggplot(plot.data) + geom_area(position='stack', aes(x=year, y=pred.signal, fill=species.combo)) +
  facet_wrap(~region + prey.spp, scales='free_y') + theme(text=element_text(size=20)) + 
  ylab('Predator field') + xlab('Year') +
  geom_text(data=portfolio.effects, aes(label=round(port.effect, 2)), x=Inf, y=Inf, vjust=1.2, hjust=1)
dev.off()  

for(sys in unique(plot.data$region)) {
  sys.data <- filter(plot.data, region==sys)
  print(ggplot(sys.data) + geom_area(position='stack', aes(x=year, y=pred.signal, fill=species.combo)) +
    facet_wrap(~prey.spp, scales='free_y') + theme(text=element_text(size=20)) + 
    ylab(paste(sys, 'predator field')) + xlab('Year'))
}

number_ticks <- function(n) {function(limits) pretty(limits, n)}
pred.plot <- ggplot(pred.ts.data) + aes(x = tot.pred) + geom_point() + 
  facet_wrap(~region+prey.spp, scales='free') +
  theme(text=element_text(size=20)) + scale_x_continuous(breaks=number_ticks(3))

pdf('sp-vs-pred.pdf', width=15, height=12)
pred.plot + aes(y=sp) + geom_text(data=pred.test, aes(label=round(sp.mod.tval, 2), col=sp.mod.pval<.05),
                                  x=Inf, y=Inf, vjust=1.2, hjust=1) + xlab('Predator field') + ylab('Surplus production') +
  theme(legend.position='none')
dev.off()

pdf('bio-vs-pred.pdf', width=15, height=12)
pred.plot + aes(y=bio.s) + geom_text(data=pred.test, aes(label=round(bio.s.cor, 2), col=bio.s.pval<.05),
                                  x=Inf, y=Inf, vjust=1.2, hjust=1) + xlab('Predator field') + ylab('Surplus production') +
  theme(legend.position='none')
dev.off()