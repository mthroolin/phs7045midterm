// pre_filter.cpp

#include <Rcpp.h>
#include <unordered_set>
#include <unordered_map>
#include <algorithm>
#include <sstream>

using namespace Rcpp;

// Helper function to check if elements are in a set
inline LogicalVector in_set(CharacterVector x, const std::unordered_set<std::string>& s) {
  int n = x.size();
  LogicalVector result(n);
  for (int i = 0; i < n; ++i) {
    std::string xi = Rcpp::as<std::string>(x[i]);
    result[i] = s.count(xi) > 0;
  }
  return result;
}

// [[Rcpp::export]]
DataFrame pre_filter_cpp(DataFrame df,
                         DataFrame df_population,
                         double threshold,
                         double max_T,
                         CharacterVector var_type_override) {
  
  // Extract columns from df
  CharacterVector df_ID = df["ID"];
  NumericVector df_t = df["t"];
  CharacterVector df_var = df["var"];
  CharacterVector df_value = df["value"];
  
  
  // Extract IDs from df_population
  CharacterVector pop_ID = df_population["ID"];
  
  // Step 1: Filter rows based on population
  // Rcout << "Remove rows not in population" << std::endl;
  
  // Create unordered_set of std::string for pop_ID_set
  std::unordered_set<std::string> pop_ID_set;
  for (int i = 0; i < pop_ID.size(); ++i) {
    pop_ID_set.insert(Rcpp::as<std::string>(pop_ID[i]));
  }
  
  LogicalVector in_population = in_set(df_ID, pop_ID_set);
  
  // Subset the data
  df_ID = df_ID[in_population];
  df_t = df_t[in_population];
  df_var = df_var[in_population];
  df_value = df_value[in_population];
  
  // Step 2: Filter rows with time outside [0, max_T)
  // Rcout << "Remove rows with t outside of [0, " << max_T << ")" << std::endl;
  LogicalVector t_valid = is_na(df_t) | ((df_t >= 0) & (df_t < max_T));
  
  // Subset the data
  df_ID = df_ID[t_valid];
  df_t = df_t[t_valid];
  df_var = df_var[t_valid];
  df_value = df_value[t_valid];
  
  // Step 3: Identify categorical/hierarchical variables
  CharacterVector var_names = var_type_override.names();
  CharacterVector var_types = as<CharacterVector>(var_type_override);
  
  std::unordered_set<std::string> categorical_vars;
  
  for (int i = 0; i < var_types.size(); i++) {
    std::string type = Rcpp::as<std::string>(var_types[i]);
    std::string var_name = Rcpp::as<std::string>(var_names[i]);
    std::transform(type.begin(), type.end(), type.begin(), ::tolower);
    if (type.find("hierarchical") != std::string::npos ||
        type.find("categorical") != std::string::npos) {
      categorical_vars.insert(var_name);
    }
  }
  
  // Step 4: Check for duplicate numerical inconsistencies
  // Rcout << "Check for inconsistent numerical values" << std::endl;
  int n = df_var.size();
  std::unordered_set<std::string> id_t_var_set;
  bool duplicates_found = false;
  
  for (int i = 0; i < n; ++i) {
    std::string var = Rcpp::as<std::string>(df_var[i]);
    if (categorical_vars.find(var) == categorical_vars.end()) {
      std::string id = Rcpp::as<std::string>(df_ID[i]);
      double t = df_t[i];
      std::ostringstream key;
      key << id << "|" << t << "|" << var;
      std::string key_str = key.str();
      if (id_t_var_set.find(key_str) != id_t_var_set.end()) {
        duplicates_found = true;
        break;
      } else {
        id_t_var_set.insert(key_str);
      }
    }
  }
  
  if (duplicates_found) {
    stop("Inconsistent numerical values found");
  }
  
  // Step 5: Calculate variable frequencies
  // Rcout << "Calculate variable frequencies" << std::endl;
  std::unordered_map<std::string, std::unordered_set<std::string>> var_IDs_map;
  
  for (int i = 0; i < n; ++i) {
    std::string var = Rcpp::as<std::string>(df_var[i]);
    std::string id = Rcpp::as<std::string>(df_ID[i]);
    var_IDs_map[var].insert(id);
  }
  
  int total_IDs = pop_ID_set.size();
  
  // Step 6: Keep variables above the threshold
  std::vector<std::string> vars_keep;
  
  for (auto& it : var_IDs_map) {
    std::string var = it.first;
    int count = it.second.size();
    double freq = static_cast<double>(count) / total_IDs;
    if (freq > threshold) {
      vars_keep.push_back(var);
    }
  }
  
  // Step 7: Filter df to include only vars_keep
  // Rcout << "Filter variables based on threshold" << std::endl;
  std::unordered_set<std::string> vars_keep_set(vars_keep.begin(), vars_keep.end());
  LogicalVector vars_in_keep(n);
  
  for (int i = 0; i < n; ++i) {
    std::string var = Rcpp::as<std::string>(df_var[i]);
    vars_in_keep[i] = vars_keep_set.find(var) != vars_keep_set.end();
  }
  
  // Subset the data
  df_ID = df_ID[vars_in_keep];
  df_t = df_t[vars_in_keep];
  df_var = df_var[vars_in_keep];
  df_value = df_value[vars_in_keep];
  
  // Rcout << "Remaining variables: " << vars_keep.size() << std::endl;
  
  // Return the filtered DataFrame
  return DataFrame::create(
    Named("ID") = df_ID,
    Named("t") = df_t,
    Named("var") = df_var,
    Named("value") = df_value
  );
}
