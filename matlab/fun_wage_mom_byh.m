function [hwage_mom,mwage_mom] = fun_wage_mom_byh(g_e_sm,g_e_sf,w_grid,hours,n_kids,n_spinc,p_a)
%This function wage moments conditional on hours based on stationary distribution and wage grids.
% This function is called by compare_bench_minwage.m.
% Inputs
% g_e_sm,g_e_sf: Stationary distribution of men and women across jobs given hours. Dim
% (n_wage, 1, n_kids, n_spinc)
% w_grid: wage grid given hours. Dim (n_wage)
% hours: scalar
% n_kids, n_spinc: Scalars. Sizes of kids, marital status/spousal income,
% and hours grids. 
% p_a: female skill relative to male skill given hours. Dim (scalar).
% Outputs:
% hwage_mom: dim(3,1). Hourly wage moments: mean, p20, p50.
% mwage_mom: dim(3,1). Monthly wage moments: mean, p20, p50.


% Initialize outputs
hwage_mom = zeros(3,1);
mwage_mom = zeros(3,1);

% Compute hourly and monthly wage moments
w_pmf = zeros([size(g_e_sm) 2]); % last dim is gender
w_pmf(:,:,:,:,1) = g_e_sm; % male
w_pmf(:,:,:,:,2) = g_e_sf; % female
hw_array = zeros(size(w_pmf)); % hourly wage array
mw_array = zeros(size(w_pmf)); % monthly wage array

    % Hourly wage, male (4.3 weeks per month)
    hw_array(:,1,:,:,1) = repmat(w_grid,[1,1,n_kids,n_spinc]);
    % Hourly wage, female
    hw_array(:,1,:,:,2) = repmat(w_grid*p_a,[1,1,n_kids,n_spinc]);

    % Monthly wage, male (4.3 weeks per month)
    mw_array(:,1,:,:,1) = repmat(w_grid,[1,1,n_kids,n_spinc])*hours*4.3;
    % Monthly wage, female
    mw_array(:,1,:,:,2) = repmat(w_grid*p_a,[1,1,n_kids,n_spinc])*hours*4.3;


% Sort distribution according to the values in hw_array
w_vec   = hw_array(:);
pmf_vec = w_pmf(:)/sum(w_pmf,'all');
[w_sort,ind_sort] = sort(w_vec);
pmf_vec_sort = pmf_vec(ind_sort);
cdf_vec_sort = cumsum(pmf_vec_sort);
% Mean
hwage_mom(1) = dot(w_vec,pmf_vec);
% Percentiles
i_20 = find(cdf_vec_sort>=0.2,1);
hwage_mom(2)  = w_sort(i_20);
i_50 = find(cdf_vec_sort>=0.5,1);
hwage_mom(3) = w_sort(i_50);

% Sort distribution according to the values in mw_array
w_vec   = mw_array(:);
pmf_vec = w_pmf(:)/sum(w_pmf,'all');
[w_sort,ind_sort] = sort(w_vec);
pmf_vec_sort = pmf_vec(ind_sort);
cdf_vec_sort = cumsum(pmf_vec_sort);
% Mean
mwage_mom(1) = dot(w_vec,pmf_vec);
% Percentiles
i_20 = find(cdf_vec_sort>=0.2,1);
mwage_mom(2)  = w_sort(i_20);
i_50 = find(cdf_vec_sort>=0.5,1);
mwage_mom(3) = w_sort(i_50);

end