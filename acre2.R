library(secr)
library(statsecol)
data("ocelot2")

#make the traps objects:
ocelot_traps <- list(read.traps(data = tdf_control, detector = "proximity", trapID = "TrapID"),
                     read.traps(data = tdf_disturb, detector = "proximity", trapID = "TrapID"))

lightf_trap <- as.data.frame(traps) %>% mutate(TrapID = row_number()) %>%
  select(TrapID,x,y)

lightf_traps <- read.traps(data=lightf_trap, detector = "proximity", trapID = "TrapID")

# make capthist:

ocelot_ch <- make.capthist(captures = edf, 
                           traps = ocelot_traps,
                           noccasions = c(44,96))


simple_ch <- create.capt(captures = simple_hhn$capt,
                                traps = simple_hhn$traps)

convert.capt.to.secr(simple_ch,traps=simple_hhn$traps)

convert.capt.to.secr(simple_hhn$capt,traps=simple_hhn$traps)

show_demo_options()
convert.traps(simple_hhn$traps)

lightf_ch <- make.capthist(captures = captures, 
                           traps = lightf_traps)

suggest.buffer(lightf_ch)


#make the mask
ocelot_mask <- make.mask(traps = traps(ocelot_ch), # extract traps from CH
                         buffer = 3000,            # rounded up to the nearest 1000m
                         spacing = 500)            # spacing defined above

library(ascr)

str(ascr::lightfooti)

lightfooti.capt <- lightfooti$capt["bincapt"]
lightfooti.traps <- lightfooti$traps

simple.hn.fit <- fit.ascr(capt = lightfooti.capt,
                          traps = lightfooti.traps,
                          detfn = "hhn",
                          mask = lightfooti$mask)

test.ascr(quick = TRUE)

## Not run: 
## Getting some data.
simple.capt <- example.data$capt["bincapt"]
## A simple model.
simple.hn.fit <- fit.ascr(capt = simple.capt, traps = example.data$traps,
                          mask = example.data$mask, fix = list(g0 = 1))
## A simple model with a hazard-rate detection function.
simple.hr.fit <- fit.ascr(capt = simple.capt, traps = example.data$traps,
                          mask = example.data$mask, detfn = "hr")
## Including some bearing information.
bearing.capt <- example.data$capt[c("bincapt", "bearing")]
## Fitting a model with bearing information.
bearing.hn.fit <- fit.ascr(capt = bearing.capt, traps = example.data$traps,
                           mask = example.data$mask, fix = list(g0 = 1))
## Getting some multi-session data.
multi.capt <- lapply(multi.example.data$capt, function(x) x[1])
multi.fit <- fit.ascr(multi.capt, multi.example.data$traps,
                      multi.example.data$mask)

## End(Not run)

