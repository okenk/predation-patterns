require(boot)
all.regions.byspp$reg.prey <- with(all.regions.byspp, paste(region, prey.spp))

# Compensatory dynamics -------------------------------------------------------

calc.vf <- function(df) {
  # Takes a data frame, groups it, and calculate the variance factors for each guild of predators
  # Returns summarized data frame, with variance factor column
  # df must contain certain column names for grouping purposes
  
  var.of.sum <- group_by(df, region, prey.spp, year) %>% 
    summarize(sum.over.spp = sum(pred.signal)) %>%
    summarize(var.of.sum = var(sum.over.spp))
  sum.of.var <- group_by(df, region, prey.spp, species) %>% 
    summarize(var.by.spp = var(pred.signal)) %>%
    summarize(sum.of.var = sum(var.by.spp), nspp = n())
  vf.df <- right_join(var.of.sum, sum.of.var) %>%
    mutate(vf = var.of.sum/sum.of.var) %>% select(-sum.of.var, -var.of.sum)
  return(vf.df)
}

calc.boot.vf <- function(boot.ls) {
  # Calculate variance factor for a set of bootstrapped time series, where each predator
  # species is a matrix element of boot.ls, with rows for bootstrap iterations and columns
  # for year
  # Returns vector of bootstrapped variance factors
  boot.arr <- sapply(boot.ls, identity, simplify = 'array')
  var.of.sum <- apply(boot.arr, c(1,2), sum) %>% apply(1, var) # First sum spp, then take variance
  sum.of.var <- apply(boot.arr, c(1,3), var) %>% apply(1, sum) # First take variance, then sum spp
  vf <- var.of.sum/sum.of.var
  return(vf)
}

# Calculate order of importance of predators based on average contribution to predation
ordered.preds.short <- filter(all.regions.byspp, reg.prey != 'GoA Yellowfin sole',
                        reg.prey != 'BALT Atlantic herring') %>%
  group_by(region, prey.spp, species) %>% 
  summarize(avg_pred = mean(pred.signal)) %>% arrange(desc(avg_pred)) %>%
  mutate(rank=order(avg_pred, decreasing = TRUE), pred.prop=cumsum(avg_pred/sum(avg_pred))) 

ordered.preds <- right_join(ordered.preds.short, all.regions.byspp)


cutoff.pct <- .9
fewer.preds <- summarize(ordered.preds.short, cutoff.no = sum(pred.prop<cutoff.pct) + 1) %>% 
  right_join(ordered.preds.short) %>% right_join(all.regions.byspp) %>%
  filter(rank <= cutoff.no)
  
# Bootstrap null dist'n of VFs
n.boot <- 1000
set.seed(80952308)
boot.vf <- group_by(all.regions.byspp, region, prey.spp, species) %>%
  # Bootstrap each predator species time series
  do(boot.ts=tsboot(tseries=.$pred.signal, statistic=identity, R=n.boot, sim='scramble')$t) %>% 
  # Calculate the boostrapped VF for each predator assemblage (i.e., for each prey spp)
  group_by(region, prey.spp) %>% do(null.vf.distn=calc.boot.vf(boot.ls=.$boot.ts)) %>%
  mutate(low.quant = quantile(null.vf.distn, probs = .05),
         high.quant = quantile(null.vf.distn, probs = .95))

boot.vf.few.pred <- group_by(fewer.preds, region, prey.spp, species) %>%
  # Bootstrap each predator species time series
  do(boot.ts=tsboot(tseries=.$pred.signal, statistic=identity, R=n.boot, sim='scramble')$t) %>% 
  # Calculate the boostrapped VF for each predator assemblage (i.e., for each prey spp)
  group_by(region, prey.spp) %>% do(null.vf.distn=calc.boot.vf(boot.ls=.$boot.ts)) %>%
  mutate(low.quant = quantile(null.vf.distn, probs = .05),
         high.quant = quantile(null.vf.distn, probs = .95))
 
compensation <- filter(all.regions.byspp, prey.spp != 'Yellowfin sole' | region != 'GoA',
                       prey.spp != 'Atlantic herring' | region != 'BALT') %>% calc.vf() %>%
  left_join(boot.vf) %>% mutate(significant = vf<low.quant | vf>high.quant)
compensation$region <- as.factor(compensation$region)

compensation.few <- calc.vf(fewer.preds) %>%
  left_join(boot.vf.few.pred) %>% mutate(significant = vf<low.quant | vf>high.quant)
compensation.few$region <- as.factor(compensation.few$region)




 
# ggplot(data=test, aes(x=mean.top.90, y=variance.factor)) + geom_point(aes(col=region)) +
#   geom_smooth(method='lm') + geom_abline(slope=0, intercept=1)
# ggplot(data=dominant.preds.by.year, aes(x=year, y=shannon.div)) + geom_line() + facet_wrap(~region + prey.spp)
# ggplot(data=dominant.preds.by.year, aes(x=year, y=simpson.index)) + geom_line() + facet_wrap(~region + prey.spp)
# ggplot(data=dominant.preds.by.year, aes(x=year, y=top.90)) + geom_line() + facet_wrap(~region + prey.spp)
# 
# 
# test <- semi_join(all.regions.byspp, dominant.preds) %>% # Pick out time series for dominant preds only
#   ungroup() %>% group_by(region, prey.spp, species.combo) %>%
#   left_join(select(pred.merged, region, prey.spp, year, tot.pred))  %>% # Join with total predation
#   mutate(pred.frac = pred.signal/tot.pred) %>% ungroup()
# 
# pdf('var_fac1.pdf')
# ggplot(compensation, aes(x=nPred, y=variance.factor)) + geom_point(aes(color=region)) + 
#   geom_smooth(method='lm') + ylim(0,2)
# dev.off()
# pdf('var_fac2.pdf')
# ggplot(compensation, aes(x=nPred, y=variance.factor)) + geom_point(aes(color=region)) + geom_smooth(method='lm') + 
#   geom_hline(yintercept = 1) + geom_abline(slope=c(-.05, .05), intercept = c(1.05, .95)) +
#   ylim(0,2)
# dev.off()
# 
# pdf('stacked_plot.pdf', width=16, height=9)
# filter(all.regions.byspp, reg.prey %in% compensation.small$reg.prey, 
#        prey.spp != 'Yellowfin sole' | region != 'GoA') %>% 
#   ggplot() + geom_area(position='stack', aes(x=year, y=pred.signal, fill=species)) +
#   facet_wrap(~region + prey.spp, scales='free_y') + theme(text=element_text(size=20), legend.position='none') + 
#   ylab('Predator field') + xlab('Year')
# dev.off()
# 
# pdf('line_plot.pdf', width=16, height=9)
# filter(all.regions.byspp, reg.prey %in% compensation.small$reg.prey, 
#        prey.spp != 'Yellowfin sole' | region != 'GoA') %>% 
#   ggplot() + geom_line(aes(x=year, y=pred.signal, color=species)) +
#   facet_wrap(~region + prey.spp, scales='free_y') + theme(text=element_text(size=20), legend.position='none') + 
#   ylab('Predator field') + xlab('Year')
# dev.off()
# 
# 
# 
# pdf('dominant_pred.pdf', width=14, height=9)
# ggplot(test) + geom_line(aes(x=year, y=pred.frac)) + 
#   facet_wrap(~ region + prey.spp) +
#   geom_text(data=dominant.preds, x=1945, y=.95, aes(label=species.combo), hjust=0)
# dev.off()
# 
# # Simulations -------------------------------------------------------------
# require(MASS)
# nyrs <- 10000
# max.nspp <- 100
# variance.factor <- numeric(max.nspp)
# variance.factor2 <- numeric(max.nspp)
# spp.cov <- 0.5
# 
# for(nspp in 1:max.nspp) {
#   sig.mat <- matrix(spp.cov, nrow=nspp, ncol=nspp) + diag(1-spp.cov, nspp)
#   dat <- mvrnorm(n = nyrs, Sigma = sig.mat, mu = rep(0, nspp)) # rows=yrs, cols=spp
#   sim.var.tot <- apply(dat, 1, sum) %>% var()
#   sim.var.byspp <- apply(dat, 2, var)
#   sim.sd.byspp <- apply(dat, 2, sd)
#   variance.factor[nspp] <- sim.var.tot/sum(sim.var.byspp)
#   variance.factor2[nspp] <- sim.var.tot/sum(sim.sd.byspp)^2
# }
# 
# n.vec <- 1:max.nspp
# plot(1:max.nspp, variance.factor)
# lines(n.vec, 1+(n.vec^2-n.vec)*spp.cov/n.vec)
# 
# plot(1:max.nspp, variance.factor2, ylim=c(0,1))
# 
# abline(h=.5)
# cor(variance.factor, 1:25)
# # Proportion of predation quantified --------------------------------------
# 
# prop.quant <- group_by(mort.props.joined, region, prey) %>% 
#   summarize(prop = sum(mort[in.cameo == TRUE]) / sum(mort))
# 
# 
