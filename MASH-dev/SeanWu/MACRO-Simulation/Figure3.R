
par(mfrow = c(2,3), mar = c(5,4,2,2))
plotResidErr(tororo, "a) Tororo")
plotResidErr(kanungu, "b) Kanungu")
plotResidErr(jinja, "c) Jinja")
plotEsim(tororo, "d) E, Tororo", gg.T, "darkred", tpc, TRUE)
plotEsim(kanungu, "e) E, Kanungu", gg.K, "darkblue", kpc, TRUE)
plotEsim(jinja, "f) E, Jinja", gg.J, "darkgreen", jpc, TRUE)
