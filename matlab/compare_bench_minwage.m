function [] = compare_bench_minwage(struct1,struct2,name1,name2,par)
%compare_bench_minwage makes plots to compare equil objects of benchmark
%and counterfactual min wage economies
% INPUTS
%  struct1, struct2: Structures with objects from two different economies
%  name1, name2:     Characters
%  par:              Structure with parameters

%% Extract parameters and set plotting/table options
FigDir = par.FigDir;
TabDir = par.TabDir;
[n_w,n_hours,n_kids,n_spinc,p_a,hours_names,hours_grid,...
    p_delta_ME,p_delta_PT,p_delta_FT] = unpack(par,'n_w','n_hours',...
    'n_kids','n_spinc','p_a','hours_names','hours_grid',...
    'p_delta_ME','p_delta_PT','p_delta_FT');
myfs = 20;
mylw = 3;

%% Compute employment distribution, hours and income,unconditional and by gender and demographic types
[emp_dist_1,avg_h1,tot_inc1,emprate_1,hwage_1] = fun_compute_inc_emp_hours(struct1,par);
[emp_dist_2,avg_h2,tot_inc2,emprate_2,hwage_2] = fun_compute_inc_emp_hours(struct2,par);

%emp_dist_diff = 100*(emp_dist_2(:,1:3) - emp_dist_1(:,1:3))./emp_dist_1(:,1:3);
emp_dist_diff = (emp_dist_2(:,1:3) - emp_dist_1(:,1:3));

%% Fraction of jobs in equilbrium below min wage
frac_below_1 = zeros(n_hours+1,1); % ME, PT, FT, all emp
frac_below_2 = zeros(n_hours+1,1);
mass_below_1 = zeros(n_hours,1);
mass_below_2 = zeros(n_hours,1);
mass_all_1 = zeros(n_hours,1);
mass_all_2 = zeros(n_hours,1);
p_min_wage = struct2.minWage;
for h_c = 1:n_hours
    % min wage economy
    ind_below_m = struct2.w_grid(:,h_c)<p_min_wage;
    ind_below_f = struct2.w_grid(:,h_c)*p_a(h_c)<p_min_wage;
    mass_below_2(h_c) = sum(struct2.g_e_sm(ind_below_m,h_c,:,:),'all') ...
        + sum(struct2.g_e_sf(ind_below_f,h_c,:,:),'all');
    mass_all_2(h_c) = sum(struct2.g_e_sm(:,h_c,:,:),'all') ...
        + sum(struct2.g_e_sf(:,h_c,:,:),'all');
    frac_below_2(h_c) = mass_below_2(h_c)/mass_all_2(h_c);
    % benchmark economy
    ind_below_m = struct1.w_grid(:,h_c)<p_min_wage;
    ind_below_f = struct1.w_grid(:,h_c)*p_a(h_c)<p_min_wage;
    mass_below_1(h_c) = sum(struct1.g_e_sm(ind_below_m,h_c,:,:),'all') ...
        + sum(struct1.g_e_sf(ind_below_f,h_c,:,:),'all');
    mass_all_1(h_c) = sum(struct1.g_e_sm(:,h_c,:,:),'all') ...
        + sum(struct1.g_e_sf(:,h_c,:,:),'all');
    frac_below_1(h_c) = mass_below_1(h_c)/mass_all_1(h_c);
end
% All employment
frac_below_1(n_hours+1) = sum(mass_below_1)/sum(mass_all_1);
frac_below_2(n_hours+1) = sum(mass_below_2)/sum(mass_all_2);


% Table on screen
disp('Fraction below min wage')
disp('Benchmark  -  Min. wage  -  Min.wage minus Bench')
disp([frac_below_1,frac_below_2,frac_below_2-frac_below_1])

% Tables in tex
table_name = ['frac_below_',name1,'_',name2,'.tex'];
FID = fopen(fullfile(par.TabDir,table_name),'w');
fprintf(FID,' \\begin{tabular}{llcc} \\hline \n');
fprintf(FID,'       &  %s  &  %s  &   \\\\ \n',name1,name2);
fprintf(FID,' \\hline \n');
for h_c = 1:par.n_hours
    fprintf(FID,' %s  & %f  &  %f  & %f \\\\ \n',hours_names{h_c},...
        frac_below_1(h_c),frac_below_2(h_c),frac_below_2(h_c)-frac_below_1(h_c));
end
fprintf(FID, '\\hline \n \\end{tabular} \n');
fclose(FID);


%% Wage effect by hours


% Wage effects by hours
hwage_byh_mom_1 = zeros(3,n_hours);
hwage_byh_mom_2 = zeros(3,n_hours);
for h_c = 1:n_hours
    % Benchmark
    hwage_byh_mom_1(:,h_c) = fun_wage_mom_byh(struct1.g_e_sm(:,h_c,:,:),struct1.g_e_sf(:,h_c,:,:), ...
        struct1.w_grid(:,h_c),hours_grid(h_c),n_kids,n_spinc,p_a(h_c));
    % Min wage economy
    hwage_byh_mom_2(:,h_c) = fun_wage_mom_byh(struct2.g_e_sm(:,h_c,:,:),struct2.g_e_sf(:,h_c,:,:), ...
        struct2.w_grid(:,h_c),hours_grid(h_c),n_kids,n_spinc,p_a(h_c));
end

% Difference in wage moments (mean, p20, p50)
d_hwage_byh_mom = 100*(hwage_byh_mom_2-hwage_byh_mom_1)./hwage_byh_mom_1;

disp('Wage effect (mean, p20, p50)')
disp('ME      PT   FT')
disp(d_hwage_byh_mom)

%% Model validation: Effect of min wage on employment, hourly wage, and allocation
% Wage effects:
% - Hourly wage (mean, p20, p50)
% - Hourly wage by gender at p20
% - Monthly wage (mean, p20, p50)
% Employment effexts:
% - Overall employment 
% - Regular employment
% - Marginal employment
% Rellocation effects:
% - Hours in the bottom 82.5\% of firms by size (<10 employees in the data)
% - Counterfactual earnings change due to productivity improvement by gender, 
%   child state, and demographics.  

% Wage effects
% Benchmark
[hwage_mom_1,mwage_mom_1,hwage_gender_1] = fun_wage_mom(struct1.g_e_sm,struct1.g_e_sf, ...
    struct1.w_grid,hours_grid,n_kids,n_spinc,n_hours,p_a);
% Min wage economy
[hwage_mom_2,mwage_mom_2,hwage_gender_2] = fun_wage_mom(struct2.g_e_sm,struct2.g_e_sf, ...
    struct2.w_grid,hours_grid,n_kids,n_spinc,n_hours,p_a);

% Difference in wage moments (mean, p20, p50)
d_hwage_mom = 100*(hwage_mom_2-hwage_mom_1)./hwage_mom_1;
d_mwage_mom = 100*(mwage_mom_2-mwage_mom_1)./mwage_mom_1;
% Reduction in gender hourly wage gap (mean, p10, p25)
pctd_hwage_gender = 1-(1-hwage_gender_2(:,3))./(1-hwage_gender_1(:,3));
d_hwage_gender = hwage_gender_2-hwage_gender_1;

disp('Wage effect (mean, p20, p50)')
disp('Hourly wage      Monthly wage')
display([d_hwage_mom,d_mwage_mom])

disp('Wage effect (mean, p20, p50)')
disp('Hourly wage      Monthly wage')
display([d_hwage_mom,d_mwage_mom])


% Counterfactual wage in model 2 if distribution were the same as in model
% 1. (This is also not consistent with Dustmann et al, who find that
% productivity accounts for about 17\% of wage increase of min wage
% workers. Though here we are looking at the overall wage change, not
% limiting to min wage workers, since we don't track transition dynamics. 

% Employment effects
d_emp_uncond = fun_d_emp(emp_dist_1,emp_dist_2);
disp('Employment effect (ME, Regular, overall)')
display(d_emp_uncond)

% Hours in small firms (contrary results to Bossler, Liang and Schank,
% which shows a negative hours for small plant). 
d_hours_smallfirms = fun_hours_smallfirms(struct1.g_e_sm,struct1.g_e_sf,...
    struct2.g_e_sm,struct2.g_e_sf,hours_grid);
disp('Effect on the average employment in small firms')
display(d_hours_smallfirms)

% Tables in tex
% * average bite in Bossler 2024 is 12.4%
% ** ave bite in Bossler 2026 is 11.8%
% *** ave gap measure in Dustmann is 0.017
table_name = ['d_emp_uncond_',name1,'_',name2,'.tex'];
FID = fopen(fullfile(par.TabDir,table_name),'w');
fprintf(FID,' \\begin{tabular}{lccl} \\hline \n');
fprintf(FID,'       & Model  &  Data  & Source   \\\\ \n');
fprintf(FID,' \\hline \n');
% Employment effect
fprintf(FID,' \\multicolumn{4}{c}{\\textit{Employment}}   \\\\ \n');
fprintf(FID,' Overall   &  %.2f  & -0.42  & Caliendo et al. (2018, Table 4, col. 3)  \\\\ \n', d_emp_uncond(3));
fprintf(FID,' Marginal  &  %.2f  & -2.97  & Caliendo et al. (2018, Table 4, col. 3)  \\\\ \n', d_emp_uncond(1));
fprintf(FID,' Regular (part-time and full-time)  &  %.2f  & -0.19  &  Caliendo et al. (2018, Table 4, col. 3) \\\\ \n', d_emp_uncond(2));
% Wage effect
fprintf(FID,' \\multicolumn{4}{c}{\\textit{Monthly Wage}}   \\\\ \n');
fprintf(FID,' Mean  &  %.2f  & 4.4  & Bossler et al. (2023, Table 2, col. 1)*  \\\\ \n', d_mwage_mom(1)); 
fprintf(FID,'       &        & 5.1  & Bossler et al. (2026, Table 2, col. 3)**  \\\\ \n');
fprintf(FID,' P20   &  %.2f  & 12.1  &  Bossler et al. (2026, Table 3.B, col. 5)** \\\\ \n', d_mwage_mom(2));
fprintf(FID,'       &        & 21.8  &  Bossler et al. (2023, Table 2, col. 4)* \\\\ \n');
fprintf(FID,' P50   &  %.2f  & 1.9 &  Bossler et al. (2023, Table 2, col. 7)* \\\\ \n', d_mwage_mom(3));
fprintf(FID,'       &        & 3.2  &  Bossler et al. (2026, Table 3.B, col. 7)** \\\\ \n');
fprintf(FID,' \\multicolumn{4}{c}{\\textit{Hourly Wage}}   \\\\ \n');
fprintf(FID,' Mean  &  %.2f  & 1.2 &  Dustmann et al. (2022, Table VII.A, col. 2)*** \\\\ \n', d_hwage_mom(1));
fprintf(FID,'       &        & 5.0  & Bossler et al. (2026, Table 2, col. 1)**  \\\\ \n');
fprintf(FID,' P20   &  %.2f  & 8.6  &  Bossler et al. (2026, Table 3.A, col. 5)** \\\\ \n', d_hwage_mom(2));
fprintf(FID,' P50   &  %.2f  & 2.1  &  Bossler et al. (2026, Table 3.A, col. 7)** \\\\ \n', d_hwage_mom(3));
fprintf(FID, '\\hline \n \\end{tabular} \n');

fclose(FID);

end %end function "compare_bench_minwage"

