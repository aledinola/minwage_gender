function [] = compare_minwage_cf(struct0,struct1,struct2,struct3,name1,name2,name3,par)
%compare_minwage_cf compares min wage and two counterfactuals.
% Inputs:
%   - struct0: structure with benchmark model results
%   - struct1: structure with min wage equilibrium results
%   - struct2: structure with min wage counterfactual with PE (fixed firm
%   response)
%   - struct3: structure with min wage counterfactual with higher
%   non-compliance penalty
% Note: struct1-3 should have the same min wage.

%% Extract parameters and set figure/table options
[n_w,n_hours,n_kids,n_spinc,p_a,hours_names,hours_grid,...
    p_delta_ME,p_delta_PT,p_delta_FT] = unpack(par,'n_w','n_hours',...
    'n_kids','n_spinc','p_a','hours_names','hours_grid',...
    'p_delta_ME','p_delta_PT','p_delta_FT');
p_min_wage = struct1.minWage;

myfs = 18;
mylw = 3;
FigDir = par.FigDir;
TabDir = par.TabDir;

%% Table comparing effects of min wage 
% Panel A: change in effective contact rate, job finding rate, and
% separation rate of men and women.
% Panel B: Change in \Delta^h, the measure of firms offering each type of
% hours.
% Panel C: change in the measure of jobs of each hours type in equilibrium.


%% % Panel A: 
% change in effective contact rate, job finding rate, and separation rate 
% of men and women.

% Effective job contact rate
struct1.f_effective(3,2) = struct1.f_effective(3,2) *0.99;
%struct2.f_effective(3,2) = struct2.f_effective(3,2) *0.99;

f_effective_diff1 = zeros(n_hours+1,2);
f_effective_diff2 = zeros(n_hours+1,2);
f_effective_diff3 = zeros(n_hours+1,2);
for g_c = 1:2
    for h_c = 1:n_hours
        f_effective_diff1(h_c,g_c) = 100*(struct1.f_effective(h_c,g_c) - struct0.f_effective(h_c,g_c))/(struct0.f_effective(h_c,g_c));
        f_effective_diff2(h_c,g_c) = 100*(struct2.f_effective(h_c,g_c) - struct0.f_effective(h_c,g_c))/(struct0.f_effective(h_c,g_c));
        f_effective_diff3(h_c,g_c) = 100*(struct3.f_effective(h_c,g_c) - struct0.f_effective(h_c,g_c))/(struct0.f_effective(h_c,g_c));

    end
end


f_effective_diff1(n_hours+1,:) = 100*sum(struct1.f_effective - struct0.f_effective,1)./sum(struct0.f_effective,1);
f_effective_diff2(n_hours+1,:) = 100*sum(struct2.f_effective - struct0.f_effective,1)./sum(struct0.f_effective,1);
f_effective_diff3(n_hours+1,:) = 100*sum(struct3.f_effective - struct0.f_effective,1)./sum(struct0.f_effective,1);

% Job finding rate
NE_E_diff1 = 100*squeeze(struct1.trans_emp_j(1,2,:) - struct0.trans_emp_j(1,2,:))./squeeze(struct0.trans_emp_j(1,2,:));
NE_E_diff2 = 100*squeeze(struct2.trans_emp_j(1,2,:) - struct0.trans_emp_j(1,2,:))./squeeze(struct0.trans_emp_j(1,2,:));
NE_E_diff3 = 100*squeeze(struct3.trans_emp_j(1,2,:) - struct0.trans_emp_j(1,2,:))./squeeze(struct0.trans_emp_j(1,2,:));

% Job Separation rate: \delta_h weighted by employment rates by hours
% Employment rate by hours and gender
emp_hours_j_0 = zeros(n_hours,2);
emp_hours_j_1 = zeros(n_hours,2);
emp_hours_j_2 = zeros(n_hours,2);
emp_hours_j_3 = zeros(n_hours,2);
for h_c = 1:n_hours
    % Male
    j_c = 1;
    emp_hours_j_0(h_c,j_c) = sum(struct0.g_e_sm(:,h_c,:,:),"all")/(sum(struct0.g_e_sm,"all"));
    emp_hours_j_1(h_c,j_c) = sum(struct1.g_e_sm(:,h_c,:,:),"all")/(sum(struct1.g_e_sm,"all"));
    emp_hours_j_2(h_c,j_c) = sum(struct2.g_e_sm(:,h_c,:,:),"all")/(sum(struct2.g_e_sm,"all"));
    emp_hours_j_3(h_c,j_c) = sum(struct3.g_e_sm(:,h_c,:,:),"all")/(sum(struct3.g_e_sm,"all"));
    % Female
    j_c = 2;
    emp_hours_j_0(h_c,j_c) = sum(struct0.g_e_sf(:,h_c,:,:),"all")/(sum(struct0.g_e_sf,"all"));
    emp_hours_j_1(h_c,j_c) = sum(struct1.g_e_sf(:,h_c,:,:),"all")/(sum(struct1.g_e_sf,"all"));
    emp_hours_j_2(h_c,j_c) = sum(struct2.g_e_sf(:,h_c,:,:),"all")/(sum(struct2.g_e_sf,"all"));
    emp_hours_j_3(h_c,j_c) = sum(struct3.g_e_sf(:,h_c,:,:),"all")/(sum(struct3.g_e_sf,"all"));

end
% Read separation rates: p_delta is an internally estimated parameter! Need to copy file
% "est_mom.txt"!
p_delta= [p_delta_ME,p_delta_PT,p_delta_FT]';
% delta_h_j: p_delta for each hours and gender (two genders have the same
% delta parameters)
delta_h_j = zeros(n_hours,2);
for j_c = 1:2
    delta_h_j(:,j_c) = p_delta;
end

% Compute weighted average
avesep_0 = sum(emp_hours_j_0.*delta_h_j,1);
avesep_diff_1 = 100*(sum(emp_hours_j_1.*delta_h_j,1) - avesep_0)./mean(avesep_0);
avesep_diff_2 = 100*(sum(emp_hours_j_2.*delta_h_j,1) - avesep_0)./mean(avesep_0);
avesep_diff_3 = 100*(sum(emp_hours_j_3.*delta_h_j,1) - avesep_0)./mean(avesep_0);

%% % Panel B: 
% Change in \Delta^h (hpol in code) and \Delta^act, the measure of firms 
% offering each type of hours and the share of active firms.
dhpol_1 = zeros(n_hours);
dhpol_2 = zeros(n_hours);
dhpol_3 = zeros(n_hours);
for h_c = 1:n_hours
    dhpol_1(h_c) = 100*(struct1.hpol(h_c)-struct0.hpol(h_c))/struct0.hpol(h_c);
    dhpol_2(h_c) = 100*(struct2.hpol(h_c)-struct0.hpol(h_c))/struct0.hpol(h_c);
    dhpol_3(h_c) = 100*(struct3.hpol(h_c)-struct0.hpol(h_c))/struct0.hpol(h_c);
end
dactive_1 = 100*(sum(struct1.hpol) - sum(struct0.hpol))/sum(struct0.hpol);
dactive_2 = 100*(sum(struct2.hpol) - sum(struct0.hpol))/sum(struct0.hpol);
dactive_3 = 100*(sum(struct3.hpol) - sum(struct0.hpol))/sum(struct0.hpol);

%% % Panel C: 
% change in the share equilibrium jobs with wage <8.5.
frac_below_0 = zeros(n_hours+1,1);
frac_below_1 = zeros(n_hours+1,1); % ME, PT, FT, all emp
frac_below_2 = zeros(n_hours+1,1);
frac_below_3 = zeros(n_hours+1,1);
mass_below_0 = zeros(n_hours,1);
mass_below_1 = zeros(n_hours,1);
mass_below_2 = zeros(n_hours,1);
mass_below_3 = zeros(n_hours,1);
mass_all_0 = zeros(n_hours,1);
mass_all_1 = zeros(n_hours,1);
mass_all_2 = zeros(n_hours,1);
mass_all_3 = zeros(n_hours,1);
for h_c = 1:n_hours
    % Benchmark economy
    ind_below_m = struct0.w_grid(:,h_c)<p_min_wage;
    ind_below_f = struct0.w_grid(:,h_c)*p_a(h_c)<p_min_wage;
    mass_below_0(h_c) = sum(struct0.g_e_sm(ind_below_m,h_c,:,:),'all') ...
        + sum(struct0.g_e_sf(ind_below_f,h_c,:,:),'all');
    mass_all_0(h_c) = sum(struct0.g_e_sm(:,h_c,:,:),'all') ...
        + sum(struct0.g_e_sf(:,h_c,:,:),'all');
    frac_below_0(h_c) = mass_below_0(h_c)/mass_all_0(h_c);
    % min wage economy
    ind_below_m = struct1.w_grid(:,h_c)<p_min_wage;
    ind_below_f = struct1.w_grid(:,h_c)*p_a(h_c)<p_min_wage;
    mass_below_1(h_c) = sum(struct1.g_e_sm(ind_below_m,h_c,:,:),'all') ...
        + sum(struct1.g_e_sf(ind_below_f,h_c,:,:),'all');
    mass_all_1(h_c) = sum(struct1.g_e_sm(:,h_c,:,:),'all') ...
        + sum(struct1.g_e_sf(:,h_c,:,:),'all');
    frac_below_1(h_c) = mass_below_1(h_c)/mass_all_1(h_c);
    % min wage CF economy: PE
    ind_below_m = struct2.w_grid(:,h_c)<p_min_wage;
    ind_below_f = struct2.w_grid(:,h_c)*p_a(h_c)<p_min_wage;
    mass_below_2(h_c) = sum(struct2.g_e_sm(ind_below_m,h_c,:,:),'all') ...
        + sum(struct2.g_e_sf(ind_below_f,h_c,:,:),'all');
    mass_all_2(h_c) = sum(struct2.g_e_sm(:,h_c,:,:),'all') ...
        + sum(struct2.g_e_sf(:,h_c,:,:),'all');
    frac_below_2(h_c) = mass_below_2(h_c)/mass_all_2(h_c);
    % min wage CF economy: higher penalty
    ind_below_m = struct3.w_grid(:,h_c)<p_min_wage;
    ind_below_f = struct3.w_grid(:,h_c)*p_a(h_c)<p_min_wage;
    mass_below_3(h_c) = sum(struct3.g_e_sm(ind_below_m,h_c,:,:),'all') ...
        + sum(struct3.g_e_sf(ind_below_f,h_c,:,:),'all');
    mass_all_3(h_c) = sum(struct3.g_e_sm(:,h_c,:,:),'all') ...
        + sum(struct3.g_e_sf(:,h_c,:,:),'all');
    frac_below_3(h_c) = mass_below_3(h_c)/mass_all_3(h_c);
end
% All employment
frac_below_0(n_hours+1) = sum(mass_below_0)/sum(mass_all_0);
frac_below_1(n_hours+1) = sum(mass_below_1)/sum(mass_all_1);
frac_below_2(n_hours+1) = sum(mass_below_2)/sum(mass_all_2);
frac_below_3(n_hours+1) = sum(mass_below_3)/sum(mass_all_3);

% difference
dfrac_below_1 = 100*(frac_below_1-frac_below_0); %./frac_below_0;
dfrac_below_2 = 100*(frac_below_2-frac_below_0); %./frac_below_0;
dfrac_below_3 = 100*(frac_below_3-frac_below_0); %./frac_below_0;

%% Make latex table
% compare benchmark vs min wage scenarios -
%- effective job contact rate significantly decreases with min wage
%- job finding rate changes little - because prob of accepting increases
%- calculate effective job separation rate (weight average of delta by emp shares) and see how it has changed: job separation rate likely decreased due to min wage
%- conclude that increase in female emp is due to decrease in job separation rate, rather than increase in job finding rate.
%}
table_name = ['compare_minwage_cf','.tex'];
FID = fopen(fullfile(TabDir,table_name),'w');
fprintf(FID,' \\begin{tabular}{lccc} \\hline \\hline \n');
fprintf(FID,'   & \\textbf{(1) %s}   & \\textbf{(2) %s} & \\textbf{(3) %s}  \\\\ \n', name1,name2,name3);
fprintf(FID,' \\hline \n');
fprintf(FID,' \\multicolumn{4}{l}{\\textbf{A: Effects on firm decisions}} \\\\ \n');
fprintf(FID,'Share of active firms $\\Delta^{act}$ (\\%% change)  & %.1f  &  %.1f  &  %.1f \\\\ \n',...
    dactive_1,0,dactive_3);
fprintf(FID,' \\multicolumn{4}{l}{\\textit{Firms posting hours requirement $h$, $\\Delta^h$ (\\%% change):}} \\\\ \n');
for h_c = 1:par.n_hours
    fprintf(FID,' %s  & %.1f  &  %.1f  & %.1f \\\\ \n',hours_names{h_c},...
        dhpol_1(h_c),0, dhpol_3(h_c));
end
fprintf(FID,' \\hline \n');
fprintf(FID,' \\multicolumn{4}{l}{\\textbf{B: Effects on employment transitions }} \\\\ \n');
fprintf(FID,' \\multicolumn{4}{l}{\\textit{Men:}} \\\\ \n');
fprintf(FID,' Effective contact rate (\\%% change)  & %.1f  &  %.1f  &  %.1f  \\\\ \n',...
    f_effective_diff1(n_hours+1,1),f_effective_diff2(n_hours+1,1),f_effective_diff3(n_hours+1,1));
% for h_c = 1:n_hours 
%     fprintf(FID,' Effective contact rate, %s (\\%% change)  & %.1f  &  %.1f  &  %.1f  \\\\ \n',...
%     hours_names{h_c}, f_effective_diff1(h_c,1),f_effective_diff2(h_c,1),f_effective_diff3(h_c,1));
% end
fprintf(FID,' Job-finding rate (\\%% change)  & %.1f  &  %.1f  &  %.1f  \\\\ \n',...
    NE_E_diff1(1),NE_E_diff2(1),NE_E_diff3(1));
fprintf(FID,' Ave. separation rate (\\%% change)  & %.1f  &  %.1f  &  %.1f \\\\ \n',...
    avesep_diff_1(1),avesep_diff_2(1),avesep_diff_3(1));
fprintf(FID,' \\multicolumn{4}{l}{\\textit{Women:}}\\\\ \n');
fprintf(FID,' Effective contact rate (\\%% change)  & %.1f  &  %.1f  &  %.1f  \\\\ \n',...
    f_effective_diff1(n_hours+1,2),f_effective_diff2(n_hours+1,2),f_effective_diff3(n_hours+1,2));
% for h_c = 1:n_hours 
%     fprintf(FID,' Effective contact rate, %s (\\%% change)  & %.1f  &  %.1f  &  %.1f  \\\\ \n',...
%     hours_names{h_c}, f_effective_diff1(h_c,2),f_effective_diff2(h_c,2),f_effective_diff3(h_c,2));
% end
fprintf(FID,' Job-finding rate (\\%% change)  & %.1f  &  %.1f  &  %.1f  \\\\ \n',...
    NE_E_diff1(2),NE_E_diff2(2),NE_E_diff3(2));
fprintf(FID,' Ave. separation rate (\\%% change)  & %.1f  &  %.1f  &  %.1f \\\\ \n',...
    avesep_diff_1(2),avesep_diff_2(2),avesep_diff_3(2));
fprintf(FID,' \\hline \n');
fprintf(FID,' \\multicolumn{4}{l}{\\textbf{C: Effects on wages }} \\\\ \n');
fprintf(FID,' \\multicolumn{4}{l}{\\textit{Share with wage below 8.5 \\texteuro\\ (p.p. change):}}\\\\ \n');
for h_c = 1:par.n_hours
    fprintf(FID,' %s  & %.1f  &  %.1f  & %.1f \\\\ \n',hours_names{h_c},...
        dfrac_below_1(h_c),dfrac_below_2(h_c), dfrac_below_3(h_c));
end
fprintf(FID, '\\hline \\hline \n \\end{tabular} \n');
fclose(FID);

end %end function

