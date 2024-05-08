

ar_range = 0:4
ma_range = 0:4
p_range = 0:4
q_range = 0:4
best_aic = Inf

for (p in p_range) {
  for (q in q_range) {
    for (ar in ar_range) {
      for (ma in ma_range) {
        spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
                           mean.model = list(armaOrder = c(ar, ma), include.mean = FALSE),
                           distribution.model = "norm")
        
        # Using tryCatch to handle errors and warnings
        fit_result <- tryCatch({
          ugarchfit(spec = spec, data = nintendo.in, solver.control = list(trace = 0))
        }, error = function(e) {
          NULL  # Returns NULL if an error occurs
        }, warning = function(w) {
          NULL  # Optionally handle warnings, here just ignore and return NULL
        })
        
        # Check if fit_result is not NULL (i.e., no error occurred)
        if (!is.null(fit_result)) {
          current_aic <- infocriteria(fit_result)[1]
          
          if (!is.na(current_aic) && current_aic < best_aic) {
            best_aic <- current_aic
            best_model <- list(ar = ar, ma = ma, p = p, q = q)
            best_spec <- spec
            best_fit <- fit_result
          }
        }
      }
    }
  }
}



