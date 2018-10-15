/* Rcpp API */
#include <Rcpp.h>

/* R's C API */
#include <R.h>
#include <Rinternals.h>

/* RcppProgress */
#include <progress.hpp>
#include <progress_bar.hpp>

/* C++ */
#include <vector>
#include <algorithm>

using namespace Rcpp;
// [[Rcpp::depends(RcppProgress)]]
// [[Rcpp::plugins(cpp11)]]


/* ################################################################################
  State transitions matrix
################################################################################ */

/* filter mosquitos */
bool filter_fn(const Rcpp::StringVector& x){
  if(strcmp(*(x.end()-1),"E") != 0){
    return true;
  } else {
    return false;
  }
}

/* get integer indices of states */
int state_search(const char* state, const bool search){
  /* death */
  if(strcmp(state,"D") == 0){
    return 5;
  /* rest */
  } else if(strcmp(state,"R") == 0){
    return 2;
  /* blood feeding */
  } else if(strcmp(state,"B") == 0){
    if(search){
      return 0;
    } else {
      return 1;
    }
  /* oviposition */
  } else if(strcmp(state,"O") == 0){
    if(search){
      return 3;
    } else {
      return 4;
    }
  } else {
    Rcpp::stop("illegal state (called from 'state_search')");
  }
}

/* go from state to index */
int state_index(const char* state){
  /* death */
  if(strcmp(state,"D") == 0){
    return 5;
  /* rest */
  } else if(strcmp(state,"R") == 0){
    return 2;
  /* blood feeding */
  } else if(strcmp(state,"F") == 0){
    return 0;
  } else if(strcmp(state,"B") == 0){
    return 1;
  /* oviposition */
  } else if(strcmp(state,"L") == 0){
    return 3;
  } else if(strcmp(state,"O") == 0){
    return 4;
  } else {
    Rcpp::stop("illegal state (called from 'state_search')");
  }
}

/* function to calculate empirical transition matrix */
// [[Rcpp::export]]
Rcpp::NumericMatrix Bionomics_StateTransitionCpp(const Rcpp::DataFrame& mosquitos, bool verbose = true){

  /* filter out mosquitos that were still alive at the end of the simulation */
  Rcpp::LogicalVector filter_bool = Rcpp::sapply(Rcpp::as<Rcpp::List>(mosquitos["behavior"]),filter_fn);
  Rcpp::IntegerVector filter = Rcpp::seq(0,filter_bool.size()-1);
  filter = filter[filter_bool];

  /* transition matrix */
  Rcpp::CharacterVector names = Rcpp::CharacterVector::create("F","B","R","L","O","D");
  Rcpp::NumericMatrix M(6,6);
  M.attr("dimnames") = Rcpp::List::create(names, names);

  /* elements of dataframe */
  Rcpp::List behavior = mosquitos["behavior"];
  Rcpp::List search = mosquitos["search"];

  /* progress bar */
  Progress pb(filter.size(), verbose);

  /* iterate over mosquitos */
  for(int i=0; i<filter.size(); i++){

    /* state transitions for this mosquito */
    Rcpp::CharacterVector state_i = Rcpp::as<Rcpp::CharacterVector>(behavior[filter[i]]);
    Rcpp::LogicalVector search_i = Rcpp::as<Rcpp::LogicalVector>(search[filter[i]]);

    /* integer indices */
    int from;
    int to;
    for(int j=0; j<state_i.size()-1; j++){
      from = state_search(state_i[j],search_i[j]);
      to = state_search(state_i[j+1],search_i[j+1]);
      M(from,to) += 1;
    }

    pb.increment();
  }

  int D = state_index("D");
  M(D,D) = 1.0;

  for(int i=0; i<M.nrow(); i++){
    M(i,_) = M(i,_) / Rcpp::sum(M(i,_)); /* normalize density */
  }

  return M;
};


/* ################################################################################
 Lifespans
################################################################################ */

// [[Rcpp::export]]
 Rcpp::NumericVector Bionomics_lifespanCpp(const Rcpp::DataFrame& mosquitos, bool verbose = true){

  /* filter out mosquitos that were still alive at the end of the simulation */
  Rcpp::LogicalVector filter_bool = Rcpp::sapply(Rcpp::as<Rcpp::List>(mosquitos["behavior"]),filter_fn);
  Rcpp::IntegerVector filter = Rcpp::seq(0,filter_bool.size()-1);
  filter = filter[filter_bool];

  /* progress bar */
  Progress pb(filter.size(), verbose);

  /* elements of dataframe */
  Rcpp::List time = mosquitos["time"];

  /* outptu */
  Rcpp::NumericVector out(filter.size());

  /* iterate over mosquitos */
  for(int i=0; i<filter.size(); i++){

    Rcpp::NumericVector time_i = Rcpp::as<Rcpp::NumericVector>(time[filter[i]]);
    out[i] = *(time_i.end()-1) - *(time_i.begin());

    pb.increment();
  }

  /* check for NaNs */
  if(Rcpp::any(Rcpp::is_nan(out) | Rcpp::is_na(out))){
    Rcpp::LogicalVector isnan = Rcpp::is_nan(out) | Rcpp::is_na(out);
    out = out[!isnan];
  }

  return out;
}


 /* ################################################################################
  Cumulative dispersal
################################################################################ */

Rcpp::IntegerVector rle_vals(const Rcpp::IntegerVector& x){

  int n = x.size();

  /* y */
  Rcpp::IntegerVector head = x[Rcpp::seq(1,n-1)];
  Rcpp::IntegerVector tail = x[Rcpp::seq(0,n-2)];
  Rcpp::LogicalVector y = head != tail;

  /* i */
  Rcpp::IntegerVector y_ix  = Rcpp::seq(0,y.size()-1);
  y_ix = y_ix[y];
  y_ix.push_back(n-1);

  return x[y_ix];
}

// [[Rcpp::export]]
Rcpp::NumericVector Bionomics_cumulativeDisperseCpp(const Rcpp::DataFrame& mosquitos, const Rcpp::NumericMatrix& dist, bool verbose = true){

  /* filter out mosquitos that were still alive at the end of the simulation */
  Rcpp::LogicalVector filter_bool = Rcpp::sapply(Rcpp::as<Rcpp::List>(mosquitos["behavior"]),filter_fn);
  Rcpp::IntegerVector filter = Rcpp::seq(0,filter_bool.size()-1);
  filter = filter[filter_bool];
  size_t n =filter.size();

  /* store all of the hops a mosquito took */
  Rcpp::List hops(n);

  /* elements of dataframe */
  Rcpp::List sites_all = mosquitos["sites"];

  /* progress bar */
  Progress pb(filter.size()*2, verbose);

  /* iterate over mosquitos */
  for(size_t i=0; i<n; i++){

    /* sites i visited */
    Rcpp::IntegerVector sites = Rcpp::as<Rcpp::IntegerVector>(sites_all[filter[i]]);
    sites = rle_vals(sites); /* just when they actually moved */

    if(sites.size()==1){
      hops[i] = Rcpp::IntegerVector::create(0);
    /* lived and died here */
    } else {
      hops[i] = sites;
    }

    pb.increment();
  }

  Rcpp::NumericVector out(n);

  /* iterate through all hops */
  for(size_t i=0; i<n; i++){
    Rcpp::IntegerVector sites = Rcpp::as<Rcpp::IntegerVector>(hops[i]);
    size_t n = sites.size();
    if(n>1){
      double d = 0;
      for(size_t j=0; j<n-1; j++){
        d += dist(sites[j]-1,sites[j+1]-1);
      }
      out[i] = d;
    } else {
      out[i] = 0.;
    }

    pb.increment();
  }

  /* return distances */
  return out;
}


 /* ################################################################################
  Absolute dispersal
################################################################################ */

// [[Rcpp::export]]
Rcpp::NumericVector Bionomics_absoluteDisperseCpp(const Rcpp::DataFrame& mosquitos, const Rcpp::NumericMatrix& dist, bool verbose = true){

  /* filter out mosquitos that were still alive at the end of the simulation */
  Rcpp::LogicalVector filter_bool = Rcpp::sapply(Rcpp::as<Rcpp::List>(mosquitos["behavior"]),filter_fn);
  Rcpp::IntegerVector filter = Rcpp::seq(0,filter_bool.size()-1);
  filter = filter[filter_bool];
  size_t n =filter.size();

  /* store all of the starting and ending points mosquitos had */
  Rcpp::NumericVector disperse(n);

  /* elements of dataframe */
  Rcpp::List sites_all = mosquitos["sites"];

  /* progress bar */
  Progress pb(filter.size(), verbose);

  /* iterate over mosquitos */
  for(size_t i=0; i<n; i++){

    /* sites i visited */
    Rcpp::IntegerVector sites = Rcpp::as<Rcpp::IntegerVector>(sites_all[filter[i]]);
    int start = sites[0];
    int end = *(sites.end()-1);

    if(start==end){
      disperse[i] = 0.0;
    } else {
      disperse[i] = dist(start-1,end-1);
    }

    pb.increment();
  }

  /* return distances */
  return disperse;
}



/* ################################################################################
  mosquito blood hosts
################################################################################ */

// [[Rcpp::export]]
Rcpp::IntegerVector Bionomics_humanBloodHostCpp(const Rcpp::DataFrame& mosquitos, const char* who = "human", bool verbose = true){

  /* filter out mosquitos that were still alive at the end of the simulation */
  Rcpp::LogicalVector filter_bool = Rcpp::sapply(Rcpp::as<Rcpp::List>(mosquitos["behavior"]),filter_fn);
  Rcpp::IntegerVector filter = Rcpp::seq(0,filter_bool.size()-1);
  filter = filter[filter_bool];
  size_t n =filter.size();

  /* which type of bloodhosts */
  size_t type;
  if(strcmp(who,"human") == 0){
    type = 1;
  } else if(strcmp(who,"all") == 0) {
    type = 2;
  } else if(strcmp(who,"zoo") == 0){
    type = 3;
  } else {
    Rcpp::stop("'who' argument must be in 'human','all','zoo'; called from 'Bionomics_humanBloodHostCpp'\n ");
  }

  /* output */
  Rcpp::IntegerVector out(n,R_NaN);

  /* progress bar */
  Progress pb(filter.size(), verbose);

  /* elements of dataframe */
  Rcpp::List bloodHosts_all = mosquitos["bloodHosts"];

  /* iterate over mosquitos */
  for(size_t i=0; i<n; i++){

    Rcpp::IntegerVector bh = Rcpp::as<Rcpp::IntegerVector>(bloodHosts_all[filter[i]]);
    switch(type){
      case 1: {
        Rcpp::LogicalVector h = bh > 0;
        out[i] = Rcpp::sum(h);
        break;
      }
      case 2: {
        Rcpp::LogicalVector h = (bh > 0) | (bh == -1);
        out[i] = Rcpp::sum(h);
        break;
      }
      case 3: {
        Rcpp::LogicalVector h = bh == -1;
        out[i] = Rcpp::sum(h);
        break;
      }
    }

    pb.increment();
  }

  /* check for NaNs */
  if(Rcpp::any(Rcpp::is_nan(out) | Rcpp::is_na(out))){
    Rcpp::LogicalVector isnan = Rcpp::is_nan(out) | Rcpp::is_na(out);
    out = out[!isnan];
  }

  return out;
};


/* ################################################################################
 intervals between blood meals & resting periods
################################################################################ */

bool filter_bint_fn(const Rcpp::NumericVector& x){
  if(Rcpp::NumericVector::is_na(x[0])){
    return false;
  } else {
    return true;
  }
}

// [[Rcpp::export]]
Rcpp::List Bionomics_bloodIntervalsCpp(const Rcpp::DataFrame& mosquitos, const char* who = "human", bool verbose = true){

  /* filter out mosquitos that were still alive at the end of the simulation */
  Rcpp::LogicalVector filter_bool = Rcpp::sapply(Rcpp::as<Rcpp::List>(mosquitos["behavior"]),filter_fn);
  Rcpp::IntegerVector filter = Rcpp::seq(0,filter_bool.size()-1);
  filter = filter[filter_bool];
  size_t n =filter.size();

  /* which type of bloodhosts */
  size_t type;
  if(strcmp(who,"human") == 0){
    type = 1;
  } else if(strcmp(who,"all") == 0){
    type = 2;
  } else if(strcmp(who,"zoo") == 0){
    type = 3;
  } else {
    Rcpp::stop("'who' argument must be in 'human','all','zoo'; called from 'Bionomics_bloodIntervalsCpp'\n ");
  }

  /* elements of dataframe */
  Rcpp::List bloodHosts_all = mosquitos["bloodHosts"];
  Rcpp::List timeFeed_all = mosquitos["timeFeed"];

  /* progress bar */
  Progress pb(n, verbose);

  /* bm intervals for each mosquito */
  Rcpp::List bm_intervals(n);
  for(size_t i=0; i<n; i++){

    Rcpp::IntegerVector host = Rcpp::as<Rcpp::IntegerVector>(bloodHosts_all[filter[i]]);
    Rcpp::NumericVector time = Rcpp::as<Rcpp::NumericVector>(timeFeed_all[filter[i]]);

    switch(type){
      case 1: {
        Rcpp::LogicalVector h = host > 0;
        if(Rcpp::sum(h) > 1){
          bm_intervals[i] = Rcpp::diff(Rcpp::as<Rcpp::NumericVector>(time[h]));
        } else {
          bm_intervals[i] = Rcpp::NumericVector::create(NA_REAL);
        }
        break;
      }
      case 2: {
        Rcpp::LogicalVector h = (host > 0) | (host == -1);
        if(Rcpp::sum(h) > 1){
          bm_intervals[i] = Rcpp::diff(Rcpp::as<Rcpp::NumericVector>(time[h]));
        } else {
          bm_intervals[i] = Rcpp::NumericVector::create(NA_REAL);
        }
        break;
      }
      case 3: {
        Rcpp::LogicalVector h = host == -1;
        if(Rcpp::sum(h) > 1){
          bm_intervals[i] = Rcpp::diff(Rcpp::as<Rcpp::NumericVector>(time[h]));
        } else {
          bm_intervals[i] = Rcpp::NumericVector::create(NA_REAL);
        }
        break;
      }
    }

    pb.increment();
  }

  Rcpp::LogicalVector filterout = Rcpp::sapply(bm_intervals,filter_bint_fn);

  return bm_intervals[filterout];
}

/* comparator for finding when mosquitos rested */
Rcpp::LogicalVector cmp_rest(const Rcpp::CharacterVector& x){
  Rcpp::LogicalVector h(x.size());
  for(size_t i=0; i<x.size(); i++){
    if(strcmp(x[i],"R") == 0){
      h[i] = true;
    } else {
      h[i] = false;
    }
  }
  return h;
}

// [[Rcpp::export]]
Rcpp::List Bionomics_restIntervalsCpp(const Rcpp::DataFrame& mosquitos, bool verbose = true){

  /* filter out mosquitos that were still alive at the end of the simulation */
  Rcpp::LogicalVector filter_bool = Rcpp::sapply(Rcpp::as<Rcpp::List>(mosquitos["behavior"]),filter_fn);
  Rcpp::IntegerVector filter = Rcpp::seq(0,filter_bool.size()-1);
  filter = filter[filter_bool];
  size_t n =filter.size();

  /* elements of dataframe */
  Rcpp::List behavior_all = mosquitos["behavior"];
  Rcpp::List time_all = mosquitos["time"];

  /* progress bar */
  Progress pb(n, verbose);

  /* bm intervals for each mosquito */
  Rcpp::List rest_intervals(n);
  for(size_t i=0; i<n; i++){

    Rcpp::CharacterVector behavior = Rcpp::as<Rcpp::CharacterVector>(behavior_all[filter[i]]);
    Rcpp::NumericVector time = Rcpp::as<Rcpp::NumericVector>(time_all[filter[i]]);

    /* find out when the mosquito rested */
    Rcpp::LogicalVector rest = cmp_rest(behavior);
    if(Rcpp::sum(rest) > 0){
      rest_intervals[i] = Rcpp::diff(Rcpp::as<Rcpp::NumericVector>(time[rest]));
    } else {
      rest_intervals[i] = Rcpp::NumericVector::create(NA_REAL);
    }

    pb.increment();
  }

  Rcpp::LogicalVector filterout = Rcpp::sapply(rest_intervals,filter_bint_fn);

  return rest_intervals[filterout];
}


/* ################################################################################
 human biting rate
################################################################################ */

// [[Rcpp::export]]
Rcpp::NumericVector Bionomics_humanBitingProportionCpp(const Rcpp::DataFrame& mosquitos, bool verbose = true){

  /* filter out mosquitos that were still alive at the end of the simulation */
  Rcpp::LogicalVector filter_bool = Rcpp::sapply(Rcpp::as<Rcpp::List>(mosquitos["behavior"]),filter_fn);
  Rcpp::IntegerVector filter = Rcpp::seq(0,filter_bool.size()-1);
  filter = filter[filter_bool];
  size_t n =filter.size();

  /* elements of dataframe */
  Rcpp::List bh_all = mosquitos["bloodHosts"];

  /* progress bar */
  Progress pb(n, verbose);

  /* iterate through mosquitos */
  Rcpp::NumericVector hbr(n);
  for(size_t i=0; i<n; i++){

    Rcpp::IntegerVector bh = Rcpp::as<Rcpp::IntegerVector>(bh_all[filter[i]]);

    if(bh.size() == 1 & bh[0] == 0){
      hbr[i] = R_NaN;
    } else {
      Rcpp::LogicalVector hh = bh > 0;
      double q = (double)(Rcpp::sum(hh));
      q = q / (double)bh.size();
      hbr[i] = q;
    }

    pb.increment();
  }

  Rcpp::LogicalVector nan_f = Rcpp::is_nan(hbr);
  return hbr[!nan_f];
}


/* filter mosquitos for blood feeding rate */
bool filter_bfr_fn(const Rcpp::IntegerVector& x){
  if(x[0]!=0){
    return true;
  } else {
    return false;
  }
}

// [[Rcpp::export]]
Rcpp::List Bionomics_bloodfeedingRateCpp(const Rcpp::DataFrame& mosquitos, bool verbose = true){

  /* filter out mosquitos that were still alive at the end of the simulation and who took at least one blood meal */
  Rcpp::LogicalVector filter_bool = Rcpp::sapply(Rcpp::as<Rcpp::List>(mosquitos["behavior"]),filter_fn);
  Rcpp::LogicalVector filter_bfr_bool = Rcpp::sapply(Rcpp::as<Rcpp::List>(mosquitos["bloodHosts"]),filter_bfr_fn);

  /* combine into single filter */
  Rcpp::LogicalVector filter_bool_both = filter_bool & filter_bfr_bool;
  Rcpp::IntegerVector filter = Rcpp::seq(0,filter_bool_both.size()-1);
  filter = filter[filter_bool_both];
  size_t n = filter.size();

  /* progress bar */
  Progress pb(n, verbose);

  /* elements of dataframe */
  Rcpp::List time_all = mosquitos["time"];
  Rcpp::List timeFeed_all = mosquitos["timeFeed"];

  /* iterate through mosquitos */
  Rcpp::List bfr(n);
  for(size_t i=0; i<n; i++){

    double bday = Rcpp::as<Rcpp::NumericVector>(time_all[filter[i]])[0];
    Rcpp::NumericVector feed_ages = Rcpp::as<Rcpp::NumericVector>(timeFeed_all[filter[i]]);
    feed_ages = feed_ages - bday;
    bfr[i] = feed_ages;

    pb.increment();
  }

  return bfr;
}


/* ################################################################################
 vectorial capacity
################################################################################ */

/* filter mosquitos for blood feeding rate */
bool filter_vc_fn(const Rcpp::IntegerVector& x){
  if(x.size() > 1){
    return true;
  } else {
    return false;
  }
}

// [[Rcpp::export]]
Rcpp::List Bionomics_vectorialCapacityCpp(const Rcpp::DataFrame& mosquitos, const Rcpp::NumericMatrix& dist,
  size_t nhum, size_t EIP, bool unique = false, bool verbose = true){

  /* filter out mosquitos that were still alive at the end of the simulation and who took at least 2 blood meal */
  Rcpp::LogicalVector filter_bool = Rcpp::sapply(Rcpp::as<Rcpp::List>(mosquitos["behavior"]),filter_fn);
  Rcpp::LogicalVector filter_vc_bool = Rcpp::sapply(Rcpp::as<Rcpp::List>(mosquitos["bloodHosts"]),filter_vc_fn);

  /* combine into single filter */
  Rcpp::LogicalVector filter_bool_both = filter_bool & filter_vc_bool;
  Rcpp::IntegerVector filter = Rcpp::seq(0,filter_bool_both.size()-1);
  filter = filter[filter_bool_both];
  size_t n = filter.size();

  /* elements of dataframe */
  Rcpp::List bloodHosts_all = mosquitos["bloodHosts"];
  Rcpp::List timeFeed_all = mosquitos["timeFeed"];
  Rcpp::List siteFeed_all = mosquitos["siteFeed"];
  Rcpp::List probeAndFeed_all = mosquitos["probeAndFeed"];

  /* progress bar */
  Progress pb(n, verbose);

  /* iterate over mosquitos */
  Rcpp::IntegerVector VC(nhum,0);
  // Rcpp::NumericVector VC_dispersion;
  std::vector<double> VC_dispersion;
  for(size_t i=0; i<n; i++){

    Rcpp::IntegerVector bloodHosts = Rcpp::as<Rcpp::IntegerVector>(bloodHosts_all[filter[i]]);
    Rcpp::NumericVector timeFeed = Rcpp::as<Rcpp::NumericVector>(timeFeed_all[filter[i]]);
    Rcpp::IntegerVector siteFeed = Rcpp::as<Rcpp::IntegerVector>(siteFeed_all[filter[i]]);
    Rcpp::LogicalVector probeAndFeed = Rcpp::as<Rcpp::LogicalVector>(probeAndFeed_all[filter[i]]);

    /* check for non-human hosts */
    if(Rcpp::is_true(Rcpp::any(bloodHosts == -1))){
      Rcpp::LogicalVector nonhuman = bloodHosts == -1;
      /* if i only fed on non-human hosts, skip me */
      if(nonhuman.size() == bloodHosts.size()){
        continue;
      /* get rid of non-human host meals */
      } else {
        bloodHosts = bloodHosts[!nonhuman];
        timeFeed = timeFeed[!nonhuman];
        siteFeed = siteFeed[!nonhuman];
        probeAndFeed = probeAndFeed[!nonhuman];
      }
    }

    /* iterate over bites */
    while(timeFeed.size() > 1){

      /* only if the bite was a probing and feeding event
         (feeding needs to occur for human -> mosy transmission)
      */
      if(probeAndFeed[0]){

        /* get indices of secondary bites */
        Rcpp::NumericVector pairTimes = timeFeed[Rcpp::seq(1,timeFeed.size()-1)] - *(timeFeed.begin());
        Rcpp::LogicalVector secondaryBites;
        if(unique){
          Rcpp::LogicalVector uniqueSecondary = bloodHosts[Rcpp::seq(1,bloodHosts.size()-1)] != *(bloodHosts.begin());
          secondaryBites = pairTimes > EIP;
          secondaryBites = secondaryBites & uniqueSecondary;
        } else {
          secondaryBites = pairTimes > EIP;
        }

        /* only if there were secondary bites arising from this bite */
        int nbite = Rcpp::sum(secondaryBites);
        if(nbite > 0){

          /* add to the primary host's VC */
          VC[bloodHosts[0]-1] += nbite;

          /* get spatial distribution of bites */
          Rcpp::IntegerVector secondaryBitesIx = Rcpp::seq(0,secondaryBites.size()-1);
          secondaryBitesIx = secondaryBitesIx[secondaryBites];
          secondaryBitesIx = secondaryBitesIx + 1;

          Rcpp::NumericVector dispersion(secondaryBitesIx.size());
          for(size_t j=0; j<secondaryBitesIx.size(); j++){
            dispersion[j] = dist(siteFeed[0]-1,siteFeed[secondaryBitesIx[j]]-1);
          }

          for(auto it = dispersion.begin(); it != dispersion.end(); it++){
            VC_dispersion.emplace_back(*it);
          }
        }
      }

      /* take off the first bite */
      bloodHosts.erase(bloodHosts.begin());
      timeFeed.erase(timeFeed.begin());
      siteFeed.erase(siteFeed.begin());
      probeAndFeed.erase(probeAndFeed.begin());
    } /* finish iterating over bites */

    pb.increment();
  }

  // return VC;
  return Rcpp::List::create(Rcpp::Named("VC")=VC,
                            Rcpp::Named("dispersion")=VC_dispersion);
};


/* ################################################################################
 egg production & oviposition
################################################################################ */

// [[Rcpp::export]]
Rcpp::List Bionomics_lifetimeOvipositionCpp(const Rcpp::DataFrame& mosquitos, const Rcpp::NumericMatrix& dist, bool verbose = true){

  /* filter out mosquitos that were still alive at the end of the simulation */
  Rcpp::LogicalVector filter_bool = Rcpp::sapply(Rcpp::as<Rcpp::List>(mosquitos["behavior"]),filter_fn);
  Rcpp::IntegerVector filter = Rcpp::seq(0,filter_bool.size()-1);
  filter = filter[filter_bool];
  size_t n = filter.size();

  /* elements of dataframe */
  Rcpp::List ovipositionBatchSize_all = mosquitos["ovipositionBatchSize"];
  Rcpp::List sites_all = mosquitos["sites"];
  Rcpp::List ovipositionSites_all = mosquitos["ovipositionSites"];

  /* progress bar */
  Progress pb(n, verbose);

  /* iterate over mosquitos */
  Rcpp::NumericVector lifeEgg(n);
  std::vector<double> Egg_dispersion;
  for(size_t i=0; i<n; i++){

    Rcpp::IntegerVector eggBatch = Rcpp::as<Rcpp::IntegerVector>(ovipositionBatchSize_all[filter[i]]);
    Rcpp::IntegerVector sites = Rcpp::as<Rcpp::IntegerVector>(sites_all[filter[i]]);
    Rcpp::IntegerVector oviSites = Rcpp::as<Rcpp::IntegerVector>(ovipositionSites_all[filter[i]]);

    lifeEgg[i] = Rcpp::sum(eggBatch);

    if(lifeEgg[i] > 0){
      size_t natal = sites[0];
      for(size_t j=0; j<oviSites.size(); j++){
        Egg_dispersion.emplace_back(dist(natal-1,oviSites[j]-1));
      }
    }

    pb.increment();
  }

  return Rcpp::List::create(Rcpp::Named("lifetime")=lifeEgg,
                            Rcpp::Named("dispersion")=Egg_dispersion);
};


/* filter mosquitos */
bool filter_oviint_fn(const Rcpp::NumericVector& x){
  if(x.size() > 1){
    return true;
  } else {
    return false;
  }
}

// [[Rcpp::export]]
Rcpp::NumericVector Bionomics_ovipositionIntervalCpp(const Rcpp::DataFrame& mosquitos, bool verbose = true){

  /* filter out mosquitos that were still alive at the end of the simulation and who oviposited at least twice */
  Rcpp::LogicalVector filter_bool = Rcpp::sapply(Rcpp::as<Rcpp::List>(mosquitos["behavior"]),filter_fn);
  Rcpp::LogicalVector filter_ovi_bool = Rcpp::sapply(Rcpp::as<Rcpp::List>(mosquitos["ovipositionTimes"]),filter_oviint_fn);

  /* combine into single filter */
  Rcpp::LogicalVector filter_bool_both = filter_bool & filter_ovi_bool;
  Rcpp::IntegerVector filter = Rcpp::seq(0,filter_bool_both.size()-1);
  filter = filter[filter_bool_both];
  size_t n = filter.size();

  /* elements of dataframe */
  Rcpp::List ovipositionTimes_all = mosquitos["ovipositionTimes"];

  /* progress bar */
  Progress pb(n, verbose);

  /* iterate over mosquitos */
  std::vector<double> interval;
  for(size_t i=0; i<n; i++){

    Rcpp::NumericVector oviTimes = Rcpp::as<Rcpp::NumericVector>(ovipositionTimes_all[filter[i]]);
    Rcpp::NumericVector intervalDiff = Rcpp::diff(oviTimes);
    for(auto it = intervalDiff.begin(); it != intervalDiff.end(); it++){
      interval.emplace_back(*it);
    }

    pb.increment();
  }

  return Rcpp::wrap(interval);
};

/* filter mosquitos */
bool filter_ovirate_fn(const Rcpp::NumericVector& x){
  if(x[0] > 0.0){
    return true;
  } else {
    return false;
  }
}

/*
 from: https://stackoverflow.com/questions/838384/reorder-vector-using-a-vector-of-indices/1267878#1267878
 to reorder a vector 'value_iterator' based on order vector 'order_iterator'
*/
template< typename order_iterator, typename value_iterator >
void reorder( order_iterator order_begin, order_iterator order_end, value_iterator v )  {
    typedef typename std::iterator_traits< value_iterator >::value_type value_t;
    typedef typename std::iterator_traits< order_iterator >::value_type index_t;
    typedef typename std::iterator_traits< order_iterator >::difference_type diff_t;

    diff_t remaining = order_end - 1 - order_begin;
    for ( index_t s = index_t(), d; remaining > 0; ++ s ) {
        for ( d = order_begin[s]; d > s; d = order_begin[d] ) ;
        if ( d == s ) {
            -- remaining;
            value_t temp = v[s];
            while ( d = order_begin[d], d != s ) {
              std::swap( temp, v[d] );
                -- remaining;
            }
            v[s] = temp;
        }
    }
}

// [[Rcpp::export]]
Rcpp::List Bionomics_ovipositionRateCpp(const Rcpp::DataFrame& mosquitos, bool verbose = true){

  /* filter out mosquitos that were still alive at the end of the simulation and who oviposited at least once */
  Rcpp::LogicalVector filter_bool = Rcpp::sapply(Rcpp::as<Rcpp::List>(mosquitos["behavior"]),filter_fn);
  Rcpp::LogicalVector filter_ovi_bool = Rcpp::sapply(Rcpp::as<Rcpp::List>(mosquitos["ovipositionTimes"]),filter_ovirate_fn);

  /* combine into single filter */
  Rcpp::LogicalVector filter_bool_both = filter_bool & filter_ovi_bool;
  Rcpp::IntegerVector filter = Rcpp::seq(0,filter_bool_both.size()-1);
  filter = filter[filter_bool_both];
  size_t n = filter.size();

  /* elements of dataframe */
  Rcpp::List time_all = mosquitos["time"];
  Rcpp::List ovipositionBatchSize_all = mosquitos["ovipositionBatchSize"];
  Rcpp::List ovipositionTimes_all = mosquitos["ovipositionTimes"];

  /* progress bar */
  Progress pb(n, verbose);

  /* iterate over mosquitos */
  std::vector<double> ages;
  std::vector<int> batches;
  for(size_t i=0; i<n; i++){

    double bday = Rcpp::as<Rcpp::NumericVector>(time_all[filter[i]])[0];
    Rcpp::IntegerVector batch_sizes = Rcpp::as<Rcpp::IntegerVector>(ovipositionBatchSize_all[filter[i]]);
    Rcpp::NumericVector batch_times = Rcpp::as<Rcpp::NumericVector>(ovipositionTimes_all[filter[i]]);
    batch_times = batch_times - bday;

    for(auto it = batch_sizes.begin(); it != batch_sizes.end(); it++){
      batches.emplace_back(*it);
    }
    for(auto it = batch_times.begin(); it != batch_times.end(); it++){
      ages.emplace_back(*it);
    }

    pb.increment();
  }

  /* sort before returning */
  std::vector<size_t> agesort(ages.size());
  std::size_t nn(0);
  std::generate(agesort.begin(), agesort.end(), [&]{ return nn++; });

  std::sort(std::begin(agesort),
            std::end(agesort),
            [&](int i1, int i2) { return ages[i1] < ages[i2];});

  reorder(agesort.begin(), agesort.end(), batches.begin());
  reorder(agesort.begin(), agesort.end(), ages.begin());

  return Rcpp::List::create(Rcpp::Named("ages")=Rcpp::wrap(ages),
                            Rcpp::Named("batches")=Rcpp::wrap(batches));

}
