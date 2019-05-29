// [[Rcpp::plugins(cpp14)]]
#include <Rcpp.h>

#include <memory>

#include "PDG.h"

using pdg_human_ptr = std::unique_ptr<PDG_human>;

// [[Rcpp::export]]
Rcpp::List sim_PDG(const size_t tmax, const int ninf){

  pdg_human_ptr human = std::make_unique<PDG_human>(0.0,false);

  human->begin_infection(ninf);

  std::vector<double> Pt_hist(tmax);
  std::vector<double> Gt_hist(tmax);
  std::vector<int>    MOI_hist(tmax);
  std::vector<double> TE_hist(tmax);
  std::vector<double> pFever_hist(tmax);
  std::vector<double> Imm_hist(tmax);
  std::vector<int>    immCounter_hist(tmax);

  for(size_t tnow = 0; tnow < tmax; tnow++){
    human->update_PDG();

    Pt_hist[tnow] = human->get_Pt();
    Gt_hist[tnow] = human->get_Gt();
    MOI_hist[tnow] = human->get_MOI();
    TE_hist[tnow] = human->get_TE();
    pFever_hist[tnow] = human->get_pFever();
    Imm_hist[tnow] = human->get_Imm();
    immCounter_hist[tnow] = human->get_immCounter();
  }

  return Rcpp::List::create(
    Rcpp::Named("Pt") = Rcpp::wrap(Pt_hist),
    Rcpp::Named("Gt") = Rcpp::wrap(Gt_hist),
    Rcpp::Named("MOI") = Rcpp::wrap(MOI_hist),
    Rcpp::Named("TE ") = Rcpp::wrap(TE_hist),
    Rcpp::Named("pFever") = Rcpp::wrap(pFever_hist),
    Rcpp::Named("Imm") = Rcpp::wrap(Imm_hist),
    Rcpp::Named("immCounter") = Rcpp::wrap(immCounter_hist)
  );
}
