// // [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
using namespace Rcpp;


template <int RTYPE>
IntegerVector qFCppImpl(const Vector<RTYPE>& x, bool sort, bool ordered, bool na_exclude, bool keep_attr, int ret) {
    Vector<RTYPE> levs = (sort && na_exclude) ? na_omit(sort_unique(x)) :
                          sort ? sort_unique(x) :
                          na_exclude ? na_omit(unique(x)) : unique(x);
    IntegerVector out = (na_exclude || RTYPE != REALSXP) ? match(x, levs) : as<IntegerVector>(Rf_match(levs, x, NA_INTEGER));
    if(ret == 1) { // returning a factor
      if(keep_attr) SHALLOW_DUPLICATE_ATTRIB(out, x); // works for all atomic objects ?
      if(RTYPE == STRSXP) {
        Rf_setAttrib(out, R_LevelsSymbol, levs);
      } else {
        Rf_setAttrib(out, R_LevelsSymbol, Rf_coerceVector(levs, STRSXP)); // What about date objects...
      }
      Rf_classgets(out, (ordered && !na_exclude) ? CharacterVector::create("ordered","factor","na.included") :
                           ordered ? CharacterVector::create("ordered","factor") :
                          (!na_exclude) ? CharacterVector::create("factor","na.included") : CharacterVector::create("factor"));
    } else { // returnin a qG
      out.attr("N.groups") = levs.size();
      if(ret == 3) {
        DUPLICATE_ATTRIB(levs, x);
        out.attr("groups") = levs;
      }
      Rf_classgets(out, (ordered && !na_exclude) ? CharacterVector::create("ordered","qG","na.included") :
        ordered ? CharacterVector::create("ordered","qG") :
        (!na_exclude) ? CharacterVector::create("qG","na.included") : CharacterVector::create("qG"));
    }
    return out;
}


// [[Rcpp::export]]   // do Cpp 11 solution using return macro ?
SEXP qFCpp(SEXP x, bool sort = true, bool ordered = true, bool na_exclude = true, bool keep_attr = true, int ret = 1) {
  switch(TYPEOF(x)) {
  case INTSXP: return qFCppImpl<INTSXP>(x, sort, ordered, na_exclude, keep_attr, ret);
  case REALSXP: return qFCppImpl<REALSXP>(x, sort, ordered, na_exclude, keep_attr, ret);
  case STRSXP: return qFCppImpl<STRSXP>(x, sort, ordered, na_exclude, keep_attr, ret);
  case LGLSXP: {
    LogicalVector xl = x;
    int l = xl.size();
    LogicalVector nd(3);
    IntegerVector out = no_init_vector(l);
    if(na_exclude) {
      for(int i = 0; i != l; ++i) {
        if(xl[i] == NA_LOGICAL) {
          out[i] = NA_INTEGER;
        } else if(xl[i]) {
          out[i] = 2;
          nd[1] = true;
        } else {
          out[i] = 1;
          nd[0] = true;
        }
      }
      if(!nd[0]) for(int i = l; i--; ) if(out[i] == 2) out[i] = 1; // no FALSE // otherwise malformed factor.. only 2 level but not 1 level
    } else {
      for(int i = 0; i != l; ++i) {
        if(xl[i] == NA_LOGICAL) {
          out[i] = 3;
          nd[2] = true;
        } else if(xl[i]) {
          out[i] = 2;
          nd[1] = true;
        } else {
          out[i] = 1;
          nd[0] = true;
        }
      }
      if(!nd[0] || (nd[2] && !nd[1])) {
        if(!nd[0]) { // no FALSE
          if(nd[1]) { // has TRUE (and NA)
            out = out - 1;
          } else { // only has NA
            out = out - 2;
          }
        } else { // NA and no TRUE
          for(int i = l; i--; ) if(out[i] == 3) out[i] = 2;
        }
      }
    }
    if(ret == 1) { // return factor
      if(keep_attr) SHALLOW_DUPLICATE_ATTRIB(out, x);
      Rf_setAttrib(out, R_LevelsSymbol, CharacterVector::create("FALSE", "TRUE", NA_STRING)[nd]);
      Rf_classgets(out, (ordered && !na_exclude) ? CharacterVector::create("ordered","factor","na.included") :
                          ordered ? CharacterVector::create("ordered","factor") :
                          (!na_exclude) ? CharacterVector::create("factor","na.included") : CharacterVector::create("factor"));
    } else {
      out.attr("N.groups") = int(nd[0]+nd[1]+nd[2]);
      if(ret == 3) {
        LogicalVector groups = LogicalVector::create(false, true, NA_LOGICAL)[nd];
        DUPLICATE_ATTRIB(groups, x);
        out.attr("groups") = groups;
      }
      Rf_classgets(out, (ordered && !na_exclude) ? CharacterVector::create("ordered","qG","na.included") :
                            ordered ? CharacterVector::create("ordered","qG") :
                            (!na_exclude) ? CharacterVector::create("qG","na.included") : CharacterVector::create("qG"));
    }
    return out;
  }
  default: stop("Not Supported SEXP Type");
  }
  return R_NilValue;
}


// TODO: could still remove NA, and also for sort_unique
template<int RTYPE>
Vector<RTYPE> uniqueord(const Vector<RTYPE>& x) {
  sugar::IndexHash<RTYPE> hash(x);
  hash.fill();
  // int l = x.size(); // almost same speed as member fill.
  // for(int i = 0; i != l; ++i) {
  //   unsigned int addr = hash.get_addr(hash.src[i]);
  //   while(hash.data[addr] && hash.not_equal(hash.src[hash.data[addr] - 1], hash.src[i])) {
  //     ++addr;
  //     if(addr == static_cast<unsigned int>(hash.m)) addr = 0;
  //   }
  //   if(!hash.data[addr]) {
  //     hash.data[addr] = i+1;
  //     ++hash.size_;
  //   }
  // }

  int hs = hash.size_;
  IntegerVector ord = no_init_vector(hs);
  for(int i = 0, j = 0; j < hs; i++) if(hash.data[i]) ord[j++] = hash.data[i]-1;
  std::sort(ord.begin(), ord.end());
  Vector<RTYPE> res = no_init_vector(hs);
  for(int i = 0; i < hs; ++i) res[i] = hash.src[ord[i]];
  // inline Vector<RTYPE> keys() const{
  //   Vector<RTYPE> res = no_init(size_) ;
  //   for( int i=0, j=0; j<size_; i++){
  //     if( data[i] ) res[j++] = src[data[i]-1] ;
  //   }
  //   return res ;
  // }
  return res;
}


template <int RTYPE>
Vector<RTYPE> funiqueImpl(const Vector<RTYPE>& x, bool sort) {
  if(sort) {
    Vector<RTYPE> out = sort_unique(x);
    DUPLICATE_ATTRIB(out, x);
    Rf_setAttrib(out, R_NamesSymbol, R_NilValue);
    return out;
  } else {
    Vector<RTYPE> out = uniqueord<RTYPE>(x);
    DUPLICATE_ATTRIB(out, x);
    Rf_setAttrib(out, R_NamesSymbol, R_NilValue);
    return out;
  }
}


IntegerVector funiqueFACT(const IntegerVector& x, bool sort = true) {
  int nlevp = Rf_nlevels(x)+1, l = x.size(), k = 0;
  std::vector<bool> not_seen(nlevp, true);
  bool countNA = true;
  if(sort) {
    for(int i = 0; i != l; ++i) {
      if(x[i] == NA_INTEGER) {
        if(countNA) {
          ++k;
          countNA = false;
        }
        continue;
      }
      if(not_seen[x[i]]) {
        not_seen[x[i]] = false;
        if(++k == nlevp) break;
      }
    }
    IntegerVector out = no_init_vector(k);
    if(!countNA) out[k-1] = NA_INTEGER;
    k = 0;
    for(int i = 1; i != nlevp; ++i) if(!not_seen[i]) out[k++] = i;
    DUPLICATE_ATTRIB(out, x);
    return out;
  } else {
    IntegerVector uxp = no_init_vector(nlevp);
    for(int i = 0; i != l; ++i) {
      if(x[i] == NA_INTEGER) {
        if(countNA) {
          uxp[k++] = NA_INTEGER;
          countNA = false;
        }
        continue;
      }
      if(not_seen[x[i]]) {
        uxp[k++] = x[i];
        if(k == nlevp) {
          DUPLICATE_ATTRIB(uxp, x);
          return uxp;
        }
        not_seen[x[i]] = false;
      }
    }
    IntegerVector out = no_init_vector(k);
    for(int i = 0; i != k; ++i) out[i] = uxp[i];
    DUPLICATE_ATTRIB(out, x);
    return out;
  }
}


// [[Rcpp::export]]
SEXP funiqueCpp(SEXP x, bool sort = true) {
  switch(TYPEOF(x)) {
  case INTSXP:
    if(Rf_isFactor(x)) return funiqueFACT(x, sort);
    return funiqueImpl<INTSXP>(x, sort);
  case REALSXP: return funiqueImpl<REALSXP>(x, sort);
  case STRSXP: return funiqueImpl<STRSXP>(x, sort);
  case LGLSXP: {
    LogicalVector xl = x;
    LogicalVector nd(3);
    int ndc = 0;
    for(int i = xl.size(); i--; ) {
      if(!nd[2] && xl[i] == NA_LOGICAL) {
        nd[2] = true;
        ++ndc;
      } else if(!nd[1] && xl[i]) {
        nd[1] = true;
        ++ndc;
      } else if(!nd[0]) {
        nd[0] = true;
        ++ndc;
      }
      if(ndc == 3) break;
    }
    LogicalVector out = LogicalVector::create(false, true, NA_LOGICAL)[nd];
    DUPLICATE_ATTRIB(out, x);
    Rf_setAttrib(out, R_NamesSymbol, R_NilValue);
    return out;
  }
  default: stop("Not Supported SEXP Type");
  }
  return R_NilValue;
}



// [[Rcpp::export]]
IntegerVector fdroplevelsCpp(const IntegerVector& x, bool check_NA = true) {

  int nlevp = Rf_nlevels(x)+1, l = x.size(), n = 1;
  std::vector<int> uxp(nlevp, 1); // 1 is also true !
  bool anyNA = false;
  if(check_NA) {
    for(int i = 0; i != l; ++i) {
      if(x[i] == NA_INTEGER) {
        anyNA = true;
        continue;
      }
      if(uxp[x[i]]) {
        uxp[x[i]] = 0;
        if(++n == nlevp) return x;
      }
      // uxp[x[i]] = 1; // Runs through, slower than above on DHS Uganda (660 factors)
    }
  } else {
    for(int i = 0; i != l; ++i) {
      if(uxp[x[i]]) {
        uxp[x[i]] = 0;
        if(++n == nlevp) return x;
      }
      // uxp[x[i]] = 1; // Runs through, slower than above on DHS Uganda (660 factors)
    }
  }
  // n = std::accumulate(uxp.begin()+1, uxp.end(), 0);
  // if(n == nlevp-1) return x;
  CharacterVector levs = Rf_getAttrib(x, R_LevelsSymbol);
  CharacterVector newlevs = no_init_vector(n-1); // n
  n = 0;
  for(int i = 1; i != nlevp; ++i) {
    if(!uxp[i]) {
      newlevs[n] = levs[i-1];
      uxp[i] = ++n;
    }
  }
  IntegerVector out = no_init_vector(l); // fastest solution !
  // IntegerVector out = anyNA ? IntegerVector(l, NA_INTEGER) : no_init_vector(l); // Not faster !!
  if(anyNA) {
    // for(int i = 0; i != l; ++i) if(x[i] != NA_INTEGER) out[i] = uxp[x[i]];
    for(int i = 0; i != l; ++i) out[i] = (x[i] == NA_INTEGER) ? NA_INTEGER : uxp[x[i]];
  } else {
    for(int i = 0; i != l; ++i) out[i] = uxp[x[i]];
  }
  DUPLICATE_ATTRIB(out, x);
  Rf_setAttrib(out, R_LevelsSymbol, newlevs);
  return out;
}
