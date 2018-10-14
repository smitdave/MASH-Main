#include <Rcpp.h>

#include <R.h>
#include <Rinternals.h>

/* RcppProgress */
#include <progress.hpp>
#include <progress_bar.hpp>

using namespace Rcpp;
// [[Rcpp::depends(RcppProgress)]]


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
  if(Rcpp::any(Rcpp::is_nan(out))){
    Rcpp::LogicalVector isnan = Rcpp::is_nan(out);
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