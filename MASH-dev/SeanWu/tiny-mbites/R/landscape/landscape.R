
# landscape has to be a list of lists for sites.

site <- list()

# little booleans for quick checks
site$has_f
site$has_l

# movement (potentially different depending if the mosy is out for blood or out to oviposit)
site$move_2F
site$move_2F_id

site$move_2L
site$move_2L_id

# habitat stuff
site$id_l
site$prob_l

# local hazard of death
site$haz

# haunt stuff
site$riskQ <- make_RiskQ()
