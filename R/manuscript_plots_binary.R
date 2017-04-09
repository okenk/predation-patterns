mac.wd <- '~/Dropbox/Chapter2/'
pc.wd <- 'C:/Users/Kiva Oken/Dropbox/Chapter2/'
my.wd <- pc.wd

setwd(paste(my.wd, 'R', sep=''))
source('pred-time-series-binary.R')
setwd(paste(my.wd, 'R', sep=''))
source('tests_etc.R')
setwd(paste(my.wd, 'binary/figs', sep=''))
require(RColorBrewer)
require(gplots)
require(ggplot2)
require(colorspace)

cols <- brewer.pal(length(levels(compensation.few$region)), 'Set3')
cols[2] <- '#CDC673'

# Variance factor violin plots --------------------------------------------

maxes <- sapply(compensation.few$null.vf.distn, function(xx) 
  c(max(abs(density(xx)$x)), max(density(xx)$y))) %>%
  apply(1, max)

pdf('violin-plots.pdf', width=6, height=5)
par(mfrow=c(2,1), mar=c(2,1,1,1))
for(spp in 1:nrow(compensation.few)) {
  if(spp == 1 | spp == 13) {
    plot(1,1, type='n', xlim=c(0, 28), ylim=c(0, 3.25), axes=FALSE, ann=FALSE)
    abline(h=1)
    if(spp == 1){
      text(1, 0, 'BALT', adj=c(.5,1), xpd=NA)
      text(4, 0, 'BS', adj=c(.5,1), xpd=NA)
      text(11, 0, 'EBS', adj=c(.5,1), xpd=NA)
      text(18, 0, 'ESS', adj=c(.5,1), xpd=NA)
      text(22, 0, 'GB', adj=c(.5,1), xpd=NA)
      # text(27, 0, 'GoA', adj=c(.5,1), xpd=NA)
    }
  }
  dens <- density(compensation.few$null.vf.distn[[spp]])
  if(spp <= 12) {
    if(compensation.few$nspp[spp] > 1)
      polygon(c(dens$y, -rev(dens$y))/4 + (2*(spp))-1, c(dens$x, rev(dens$x)),
            col = cols[compensation.few$region[spp]], xpd=NA)
    points(2*(spp)-1, compensation.few$vf[spp], pch=16)
    text(2*(spp)-1, 3.15, compensation.few$nspp[spp])
  } else {
    if(compensation.few$nspp[spp] > 1)
      polygon(c(dens$y, -rev(dens$y))/4 + (2*(spp-12))-1, c(dens$x, rev(dens$x)),
            col = cols[compensation.few$region[spp]], xpd=NA)
    points(2*(spp-12)-1, compensation.few$vf[spp], pch=16)
    text(2*(spp-12)-1, 3.15, compensation.few$nspp[spp])    
  }
}
spp <- 18
dens <- density(compensation.few$null.vf.distn[[spp]])
polygon(c(dens$y, -rev(dens$y))/4 + (2*(spp-12))-1, c(dens$x, rev(dens$x)),
        col = cols[compensation.few$region[spp]])
points(2*(spp-12)-1, compensation.few$vf[spp], pch=16)

text(3, 0, 'GoA', adj=c(.5,1), xpd=NA)
text(8, 0, 'GoM', adj=c(.5,1), xpd=NA)
text(12, 0, 'HS', adj=c(.5,1), xpd=NA)
text(19, 0, 'NORT', adj=c(.5,1), xpd=NA)
text(26, 0, 'WSS', adj=c(.5,1), xpd=NA)
dev.off()
system2("open",args=c("-a Skim.app",'violin-plots.pdf'))

# Variance factor scatter plot --------------------------------------------

cols <- brewer.pal(length(levels(compensation.few$region)), 'Set3') 
cols[2] <- '#CDC673'
cols <-rgb2hsv(col2rgb(cols))
cols['s',] <- 1.63 * cols['s',]
cols <- apply(cols,2,function(x) do.call(hsv,as.list(x)))

cex.amt <- 1.5

pdf('variance-factors.pdf', width=5, height=8)
layout(matrix(c(1,2,3,4,5,3), nrow=3), heights = c(5,5,1), widths = c(4,1))
par(mar=c(4,4,1,1))
with(compensation, plot(nspp, vf, ylim=c(0,2.5), xlab='', ylab='', type='n'))
#abline(1.05, -.05)
#abline(.95, .05)
abline(h=1)
with(compensation, points(nspp, vf, pch=19, col=cols[region], lwd=2, cex=cex.amt))
lm(I(vf-1)~I(nspp-1)+0, data=compensation) %>% coef() %>% abline(a=1, col='red', lty=2)
mtext('Number of species', 1, 2.5)
mtext('Variance factor', 2, 2.5)
text(1,2, '(a)', pos=4, cex=cex.amt)

with(compensation.few, plot(nspp, vf, ylim=c(0,2.5), xlab='', ylab='', type='n'))
#abline(1.05, -.05)
#abline(.95, .05)
abline(h=1)
lm(I(vf-1)~I(nspp-1)+0, data=compensation.few) %>% coef() %>% abline(a=1, col='red', lty=2)
with(compensation.few, points(nspp, vf, pch=19, lwd=2, cex=cex.amt, col=cols[region]))
mtext('Number of species', 1, 2.5)
mtext('Variance factor', 2, 2.5)
text(.75, 2, '(b)', pos=4, cex=cex.amt)

par(mar=rep(1, 4))
xpos <- rep(1:5, each=2)
ypos <- rep(2:1, 5)
plot(xpos, ypos, xlim=c(1,5.5), ylim=c(.5, 2.5), col=cols[1:10], pch=15, axes=FALSE, ann=FALSE, cex=3)
text(xpos, ypos, unique(compensation$region), pos = 4, cex=1.25, offset=1)

par(mar=c(4,0,1,1))
xx <- hist(compensation$vf, plot=FALSE, breaks = seq(0, 2.5, by=0.2))
barplot(xx$counts, horiz = TRUE, axes=FALSE)
xx <- hist(compensation.few$vf, plot=FALSE, breaks = seq(0, 2.5, by=0.2))
barplot(xx$counts, horiz = TRUE, axes=FALSE)
dev.off()
system2("open",args=c("-a Skim.app",'variance-factors.pdf'))

# Proportion of mortality captured ----------------------------------------

std.mort.props <- group_by(mort.props.joined, region, prey) %>% 
  mutate(std.mort = mort/sum(mort)) %>%
  summarize(tot.quantified = sum(std.mort[in.cameo==TRUE]),
            max.prop = max(std.mort),
            max.prop2 = sort(std.mort, decreasing = TRUE)[2]) %>%
  filter(region!='BALT' | prey!='Atlantic herring')

pdf('prop-quantified.pdf')
par(mar=c(5,4,1,1))
hist(std.mort.props$tot.quantified, xlab='', main='', ylab='', 
     breaks=seq(0,1,by=.1))
mtext('Proportion of total predation mortality quantified', 1, 2.5)
mtext('Frequency', 2, 2.5)
dev.off()
system2("open",args=c("-a Skim.app",'prop-quantified.pdf'))

xx <- hist(std.mort.props$max.prop, xlab='', main='', ylab='', xlim=c(0,1), plot=FALSE, 
           breaks=seq(0,.9,by=.1))
yy <- hist(std.mort.props$max.prop2, xlab='', main='', ylab='', xlim=c(0,1), plot=FALSE,
             breaks = xx$breaks)
pdf('pred-props.pdf')
par(mar=c(5,4,1,1))
barplot(height = rbind(xx$counts, yy$counts), beside = TRUE, col=c('gray40', 'gray90'),
        xlim=c(0,30))
axis(1, at = 6*0:5+.5, labels = .2*0:5)
legend('topright', legend = c('Top predator', 'Second predator'), text.width = 10,
       pt.bg=c('gray40', 'gray90'), pch = 22, bty='n', y.intersp = 1.2, pt.cex=1.5)
mtext('Proportion of predation mortality\naccounted for', 1, 3.5)
mtext('Frequency', 2, 2.5)
dev.off()
system2("open",args=c("-a Skim.app",'pred-props.pdf'))


# Pred index composition --------------------------------------------------
plot.region.name <- function(region.name, at=c(.6,1.4)) {
  plot(1,1,type='n', axes=FALSE, ann=FALSE)
  axis(1, line = 0, labels = FALSE, at = at)
  text(1,1, region.name, cex=2)
}

col.scale <- brewer.pal(10, 'Set3')
region.order <- c('HS', 'GoA', 'EBS', 'NORT', 'BS', 'WSS', 'GoM', 'GB', 'ESS', 'BALT')
year.range <- range(ordered.preds$year)

mat <- matrix(c(27,27,28,28,28,
                1,2,3,4,5, # HS, GoA
                29,29,29,29,29,
                6,7,8,9,10, # EBS
                30,30,30,30,30,
                11,12,13,14,15, # NORT
                31,31,32,32,37,
                16,17,18,19,38, # BS, WSS
                33,33,34,34,39,
                20,21,22,23,40, # GoM, GB
                35,35,36,41,41,
                24,25,26,42,42), byrow=TRUE, ncol=5) # ESS, BALT

pdf('pred-indices.pdf', width = 9, height=11)
layout(mat,heights = rep(c(1,4.5), 6))
par(mar=c(1,.5,1,.5), oma=c(3,0,1,0))
for(reg in region.order) {
  this.region <- filter(ordered.preds, region==reg, rank<=10, reg.prey!='BALT Atlantic herring')
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
    
    if(reg=='ESS' | reg=='BALT' | (reg=='GB' & prey=='Other pelagics') |
       (reg=='NORT' & prey=='Whiting')) axis(1)
  }
}
#### Region Labels ####
par(mar=c(.5,0,0,0))
region.order <- c('HS', 'GoA', 'EBS', 'NORT', 'BS', 'WSS', 'GoM', 'GB', 'ESS', 'BALT')
plot.region.name('Hecate Str.')
plot.region.name('Gulf of AK')
plot.region.name('E. Bering Sea')
plot.region.name('North Sea')
plot.region.name('Barents Sea')
plot.region.name('W. Scotian Shelf')
plot.region.name('Gulf of ME')
plot.region.name('Georges Bank')
plot.region.name('E. Scotian Shelf')
plot.region.name('Baltic Sea')
mtext('Year', 1, 2, outer=TRUE)
dev.off()
system2("open",args=c("-a Skim.app","pred-indices.pdf"))


# appendix plots ----------------------------------------------------------

pdf('pred-index-by-region.pdf', width=12)
for(reg in region.order) {
xx <- filter(ordered.preds, reg.prey!='BALT Atlantic herring') %>% filter(region==reg, rank<=10) %>%
  ggplot() + geom_area(position='stack', aes(x=year, y=pred.signal, fill=species)) +
  facet_wrap(~region + prey.spp, scales='free_y') + theme(text=element_text(size=16)) +
  ylab('Predator field') + xlab('Year') 
print(xx)
}
dev.off()
system2("open",args=c("-a Skim.app",'pred-index-by-region.pdf'))
