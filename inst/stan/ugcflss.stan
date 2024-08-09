data {
  int n_grp;
  int n_cuts;
  matrix[n_cuts + 1, n_grp] count_mat;
  // array[n_cuts + 1, n_grp] int count_arr;
  int n_gamma;
  matrix[n_cuts, n_gamma] spl_mat;
}
transformed data {
  int n_levs = n_cuts + 1;
  int n_levs_grp = n_levs * n_grp;
  vector[n_levs_grp] count_vec;

  {
    int pos = 0;
    for (j in 1:n_grp) {
      count_vec[(pos + 1):(pos + n_levs)] = count_mat[, j];
      pos += n_levs;
    }
  }
}
parameters {
  real intercept;
  real<lower = 0> sigma_0;
  real<lower = 0> sigma_1;
  vector<lower = 0>[n_gamma] gamma;
  matrix[n_gamma, n_grp] ln_lambda;
}
model {
  matrix[n_gamma, n_grp] phi;

  for (j in 1:n_grp) {
    phi[, j] = gamma .* exp(sigma_1 * ln_lambda[, j]);
  }

  intercept ~ normal(0, 5);
  sigma_0 ~ normal(0, 2.5);
  sigma_1 ~ normal(0, 2.5);
  gamma ~ normal(0, sigma_0);
  to_vector(ln_lambda) ~ std_normal();

  {
    vector[n_cuts] cuts;
    vector[n_levs] p_group;
    vector[n_levs_grp] p_group_all;
    int pos = 0;
    for (j in 1:n_grp) {
      cuts = inv_logit(spl_mat * phi[, j] - intercept);
      p_group[1] = cuts[1];
      for (i in 2:n_cuts) p_group[i] = cuts[i] - cuts[i - 1];
      p_group[n_levs] = 1.0 - cuts[n_cuts];
      // count_arr[, j] ~ multinomial(p_group);
      p_group_all[(pos + 1):(pos + n_levs)] = p_group;
      pos += n_levs;
    }
    target += dot_product(count_vec, log(p_group_all + machine_precision()));
  }
}
generated quantities {
  matrix[n_gamma, n_grp] phi;

  for (j in 1:n_grp) {
    phi[, j] = gamma .* exp(sigma_1 * ln_lambda[, j]);
  }
}
