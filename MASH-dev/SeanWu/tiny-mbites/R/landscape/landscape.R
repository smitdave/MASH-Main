
# landscape has to be a list of lists for sites.

site <- list()

# little booleans for quick checks
site$has_f
site$has_l



# habitat stuff
site$id_l
site$prob_l

# local hazard of death
site$haz

# haunt stuff
site$riskQ <- make_RiskQ()



make_site <- function(id,has_f,has_l,move,move_id)
