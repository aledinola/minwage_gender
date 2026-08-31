function [g_cf_sm,g_cf_sf] = fun_cf_dist(g_e_sm_1,g_e_sf_1,...
    g_e_sm_2,g_e_sf_2,n_kids,n_spinc)
%This function computes counterfactual wage distribution where the distribution 
% over productivity is based on model 1, but distribution of hours conditional 
% on productivity, and the total mass (of employed workers) is based on
% model 2.
% Inputs
% g_e_sm_1,g_e_sf_1,g_e_sm_2,g_e_sf_2: Stationary distribution of men and women across jobs
% in model 1 and 2. Dim(n_wage, n_hours, n_kids, n_spinc)
% n_kids, n_spinc, n_hours: Scalars. Sizes of kids, marital status/spousal income,
% and hours grids.
% Outputs:
% g_cf_sm,g_cf_sf: dim(n_w, n_hours,n_kids,n_spinc). Counterfactual
% distribution of employment over jobs. 

% Build counterfactual distribution
% Counterfactual distribution should have the same distribution over p as
% in model 1, but the same distribution over h conditional on p as in model
% 2. 
g_p_sm_1 = squeeze(sum(g_e_sm_1,2));
g_p_sf_1 = squeeze(sum(g_e_sf_1,2));

g_hcondp_sm_2 = zeros(size(g_e_sm_2));
g_hcondp_sf_2 = zeros(size(g_e_sm_2));
g_cf_sm = zeros(size(g_e_sm_2));
g_cf_sf = zeros(size(g_e_sm_2));
for sp_c = 1:n_spinc
    for k_c = 1:n_kids
        for p_c = 1:size(g_e_sm_2,1)
            % dist over h_c conditional on p_c
            g_hcondp_sm_2(p_c,:,k_c,sp_c) = g_e_sm_2(p_c,:,k_c,sp_c)/sum(squeeze(g_e_sm_2(p_c,:,k_c,sp_c)));
            g_hcondp_sf_2(p_c,:,k_c,sp_c) = g_e_sf_2(p_c,:,k_c,sp_c)/sum(squeeze(g_e_sf_2(p_c,:,k_c,sp_c)));
            % Counterfactual distribution over p_c and h_c
            g_cf_sm(p_c,:,k_c,sp_c) = g_hcondp_sm_2(p_c,:,k_c,sp_c) *g_p_sm_1(p_c,k_c,sp_c);
            g_cf_sf(p_c,:,k_c,sp_c) = g_hcondp_sf_2(p_c,:,k_c,sp_c) *g_p_sf_1(p_c,k_c,sp_c);
        end
    end
end


% Normalize by the mass of model 2
g_cf_sm = g_cf_sm/sum(g_cf_sm,"all")*sum(g_e_sm_2,"all");
g_cf_sf = g_cf_sf/sum(g_cf_sf,"all")*sum(g_e_sf_2,"all");

end