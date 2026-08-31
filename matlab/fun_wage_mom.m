function [hwage_mom,mwage_mom,hwage_gender] = fun_wage_mom(g_e_sm,g_e_sf,w_grid,hours_grid,n_kids,n_spinc,n_hours,p_a)
%This function wage moments based on stationary distribution and wage grids.
% This function is called by compare_bench_minwage.m.
% Inputs
% g_e_sm,g_e_sf: Stationary distribution of men and women across jobs. Dim
% (n_wage, n_hours, n_kids, n_spinc)
% w_grid: wage grid. Dim (n_wage, n_hours)
% hours_grid: hours grid. Dim (n_hours)
% n_kids, n_spinc, n_hours: Scalars. Sizes of kids, marital status/spousal income,
% and hours grids. 
% p_a: female skill relative to male skill. Dim (n_hours).
% Outputs:
% hwage_mom: dim(3,1). Hourly wage moments: mean, p20, p50.
% mwage_mom: dim(3,1). Monthly wage moments: mean, p20, p50.
% hwage_gender: dim(3,3). First dim: Hourly wage moments: mean, p10, p25.
%                         Second dim: gender male = 1, female=2,
%                         female/male=3

% Initialize outputs
hwage_mom = zeros(3,1);
mwage_mom = zeros(3,1);
hwage_gender = zeros(3,2);

% Compute hourly and monthly wage moments
w_pmf = zeros([size(g_e_sm) 2]); % last dim is gender
w_pmf(:,:,:,:,1) = g_e_sm; % male
w_pmf(:,:,:,:,2) = g_e_sf; % female
hw_array = zeros(size(w_pmf)); % hourly wage array
mw_array = zeros(size(w_pmf)); % monthly wage array

for h_c = 1:n_hours
    % Hourly wage, male (4.3 weeks per month)
    hw_array(:,h_c,:,:,1) = repmat(w_grid(:,h_c),[1,1,n_kids,n_spinc]);
    % Hourly wage, female
    hw_array(:,h_c,:,:,2) = repmat(w_grid(:,h_c)*p_a(h_c),[1,1,n_kids,n_spinc]);

    % Monthly wage, male (4.3 weeks per month)
    mw_array(:,h_c,:,:,1) = repmat(w_grid(:,h_c),[1,1,n_kids,n_spinc])*hours_grid(h_c)*4.3;
    % Monthly wage, female
    mw_array(:,h_c,:,:,2) = repmat(w_grid(:,h_c)*p_a(h_c),[1,1,n_kids,n_spinc])*hours_grid(h_c)*4.3;
end

% Sort distribution according to the values in hw_array (gender specific)
for g_c = 1:2
    w_vec   = hw_array(:,:,:,:,g_c);
    w_vec   = w_vec(:);
    pmf_vec = w_pmf(:,:,:,:,g_c)/sum(w_pmf(:,:,:,:,g_c),'all');
    pmf_vec = pmf_vec(:);
    [w_sort,ind_sort] = sort(w_vec);
    pmf_vec_sort = pmf_vec(ind_sort);
    cdf_vec_sort = cumsum(pmf_vec_sort);
    % Mean
    hwage_gender(1,g_c) = dot(w_vec,pmf_vec);
    % Percentiles
    i_10 = find(cdf_vec_sort>=0.1,1);
    hwage_gender(2,g_c)  = w_sort(i_10);
    i_25 = find(cdf_vec_sort>=0.25,1);
    hwage_gender(3,g_c) = w_sort(i_25);
end
hwage_gender(:,3) = 1-hwage_gender(:,2)./hwage_gender(:,1);

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