function [d_hours_smallfirms] = fun_hours_smallfirms(g_e_sm_1,g_e_sf_1,g_e_sm_2,g_e_sf_2,...
    hours_grid)
%This function computes hours in the 82.5% of small firms and its
%difference between model 2 and 1.
% Inputs
% g_e_sm_1,g_e_sf_1,g_e_sm_2,g_e_sf_2: Stationary distribution of men and 
%   women across jobs in model 1 and 2. Dim(n_wage, n_hours, n_kids, n_spinc)
% hours_grid: hours grid. Dim (n_hours)
% n_kids, n_spinc, n_hours: Scalars. Sizes of kids, marital status/spousal income,
% and hours grids. 
% Outputs:
% d_hours_smallfirms: scalar. rel difference in average hours in small
% firms model 2 vs 1. 

% Firm size distribution in model 1
firm_size_1 = sum(g_e_sm_1+g_e_sf_1,[2,3,4])/sum(g_e_sm_1+g_e_sf_1,'all');
firm_cum_1 = cumsum(firm_size_1);
% find small firm cut-off (all firms with index less than or equal to
% i_small are small firms.
i_small = find(firm_cum_1>=0.1,1);

% Compute average firm size in terms of hours of small firms
hours_smallfirms_1 = 0;
hours_smallfirms_2 = 0;
for i_firm = 1:i_small
    % Model 1
    hours_smallfirms_1 =  hours_smallfirms_1 ...
        +hours_grid(1)*sum(g_e_sm_1(i_firm,1,:,:)+g_e_sf_1(i_firm,1,:,:),'all') ...
        +hours_grid(2)*sum(g_e_sm_1(i_firm,2,:,:)+g_e_sf_1(i_firm,2,:,:),'all') ...
        +hours_grid(3)*sum(g_e_sm_1(i_firm,3,:,:)+g_e_sf_1(i_firm,3,:,:),'all');
    
    % Model 2
     hours_smallfirms_2 =  hours_smallfirms_2 ...
        +hours_grid(1)*sum(g_e_sm_2(i_firm,1,:,:)+g_e_sf_2(i_firm,1,:,:),'all') ...
        +hours_grid(2)*sum(g_e_sm_2(i_firm,2,:,:)+g_e_sf_2(i_firm,2,:,:),'all') ...
        +hours_grid(3)*sum(g_e_sm_2(i_firm,3,:,:)+g_e_sf_2(i_firm,3,:,:),'all');
   
end

% Difference
d_hours_smallfirms = (hours_smallfirms_2-hours_smallfirms_1)/hours_smallfirms_1;
end