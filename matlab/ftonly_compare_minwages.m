function [] = ftonly_compare_minwages(struct,n_struct,par)
%compare_emp_dist compares equilibria under different minimum wages. It
%generates figures, tables and other results.
% Inputs:
%   - struct: dim(n_struct,1) structure array, containing results from each
%   min wage economy. First entry is benchmark economy. Min wage levels are
%   stored in struct.minWage.
%   - n_struct: size of input struct.
%   - par: structure containing model parameters.

%% Unpack parameters, set figure and table options
[n_w,n_hours,n_kids,n_spinc,p_a,p_min_wage,hours_names,hours_grid] = unpack(par,'n_w',...
    'n_hours','n_kids','n_spinc','p_a','p_min_wage','hours_names','hours_grid');
myfs = 18;
mylw = 3;
FigDir = par.FigDir;
TabDir = par.TabDir;
barcolors = [0.16 0.32 0.75; 1 1 1]; % Navy blue for men, white for women

% minWage_names is a cell array of min wage levels, could be used as plot
% labels for bar plots (currently not used)
minWage_names = cell(n_struct -1,1);
for ii = 1:n_struct-1 
    minWage_names{ii} = compose("%2.1f",struct(ii+1).minWage);
end

%% Compute effective job finding rates, job finding rate

f_eff=zeros(n_struct,n_hours+1,2);
NE_E = zeros(n_struct,2);
for ii = 1:n_struct
    f_eff(ii,1:n_hours,:)  = (struct(ii).f_effective);
    % hours = 4: all types
    f_eff(ii,n_hours+1,:)  = sum(struct(ii).f_effective,1); 
    % Job finding rate 
    NE_E(ii,:) = struct(ii).trans_emp_j(1,2,:);
end

figure
plot([struct(2:end).minWage]',f_eff(2:end,n_hours+1,1),"-",'linewidth',mylw)
hold on
plot([struct(2:end).minWage]',f_eff(2:end,n_hours+1,2),"--",'linewidth',mylw)
hold off
grid on
ax = gca;
ax.FontSize = myfs;
%ax.YLim = [-1.5,2];
ax.XLim = [8,14];
legend('Men','Women','fontsize',myfs-2,'location','best')
xlabel('Minimum wage','fontsize',myfs)
ylabel("Effective Job Contact Rate",'fontsize',myfs)
fig_name = 'minwages_f_eff';
print(fullfile(FigDir,fig_name),'-dpng','-r300')
print(fullfile(FigDir,fig_name),'-depsc')

%% hpol: offer distribution over hours

hpol_vec=zeros(n_struct,n_hours);
for ii = 1:n_struct
    hpol_vec(ii,:)  = (struct(ii).hpol);
end
figure
plot([struct(2:end).minWage]',100*(sum(hpol_vec(2:end,1:3),2)-sum(hpol_vec(1,1:3)))/sum(hpol_vec(1,1:3)),"-",'linewidth',mylw)
hold on
plot([struct(2:end).minWage]',100*(hpol_vec(2:end,1)-hpol_vec(1,1))/hpol_vec(1,1),":",'linewidth',mylw)
hold on
plot([struct(2:end).minWage]',100*(hpol_vec(2:end,2)-hpol_vec(1,2))/hpol_vec(1,2),"-.",'linewidth',mylw)
hold on
plot([struct(2:end).minWage]',100*(hpol_vec(2:end,3)-hpol_vec(1,3))/hpol_vec(1,3),"--",'linewidth',mylw)
hold off
grid on
ax = gca;
ax.FontSize = myfs;
%ax.YLim = [-1.5,2];
%ax.XLim = [8,18];
legend('Active','ME','PT','FT','fontsize',myfs-2,'location','best')
xlabel('Minimum wage','fontsize',myfs)
ylabel("% change ",'fontsize',myfs)
fig_name = 'minwages_hpol';
print(fullfile(FigDir,fig_name),'-dpng','-r300')
print(fullfile(FigDir,fig_name),'-depsc')


%% Compute employment distribution, hours and income,unconditional and by gender 
% result is a structure array.
% The last dimension of each field of result has size 5:
% Last dimension:  1=all, 2=single-childless, 3=single-with
% children, 4 = married-childless, 5=married-with children.
for ii = 1:n_struct
    [result(ii).emp_dist,result(ii).avg_h,result(ii).tot_inc,result(ii).emprate,result(ii).hwage] ...
        = fun_compute_inc_emp_hours(struct(ii),par);
    % Change hours = 4 to emp rate rather than nonemp rate
    result(ii).emp_dist(4,:,:) = 1-result(ii).emp_dist(4,:,:); 
end

%%  Income  (unconditional on employment)
% result.tot_inc
% Dimension 1: 1=male, 2=female, 3=all
% Dimension 2: 1=all, 2=single-childless, 3=single-with children,
%              4 = married-childless, 5=married-with children.

demog_c = 1; % consider all demographic types
% Extract vectors of employment effects for demog_c 
% Ave income
result_inc = zeros(n_struct,3); % dim1: min wage; dim2: gender (male, female, all)
% Gender gap: 100* female/male
result_inc_gender = zeros(n_struct,1); % dim1: min wage;
result_inc_gender_gap = zeros(n_struct,1); % dim1: min wage;

% Gender gap: log(female)-log(male)
result_inc_gender_log = zeros(n_struct,1); % dim1: min wage;

for ii = 1:n_struct
        for g_c = 1:3
            % Change in emp relative to benchmark
            result_inc(ii,g_c) = 100*(result(ii).tot_inc(g_c,demog_c) ...
                - result(1).tot_inc(g_c,demog_c))./result(1).tot_inc(g_c,demog_c);
        end % g_c
        % Gender gap 
        result_inc_gender_gap(ii) = 100*(result(ii).tot_inc(2,demog_c)./result(ii).tot_inc(1,demog_c) );
        % Gender gap relative to benchmark
        result_inc_gender(ii) = 100*(result(ii).tot_inc(2,demog_c)./result(ii).tot_inc(1,demog_c) ) ...
                - 100*(result(1).tot_inc(2,demog_c)./result(1).tot_inc(1,demog_c) ); 
        % Decompose gender gap: % log(gender gap min wage) - log(gender gap benchmark)
        result_inc_gender_log(ii) = log(result(ii).tot_inc(2,demog_c)./result(ii).tot_inc(1,demog_c) ) ...
                - log(result(1).tot_inc(2,demog_c)./result(1).tot_inc(1,demog_c) );
end

% Change in hours and gender hours gap
figure
plot([struct(2:end).minWage]',result_inc(2:end,3),'x','linewidth',mylw)
hold on
plot([struct(2:end).minWage]',result_inc_gender(2:end),'o','linewidth',mylw)
hold off
grid on
ax = gca;
ax.FontSize = myfs;
%ax.YLim = [-1.5,2];
ax.XLim = [8,14];
legend('Income','Gender Income Gap','fontsize',myfs-2,'location','best')
xlabel('Minimum wage','fontsize',myfs)
ylabel("p.p. change ",'fontsize',myfs)
fig_name = 'minwages_inc';
print(fullfile(FigDir,fig_name),'-dpng','-r300')
print(fullfile(FigDir,fig_name),'-depsc')

%% Make Wage plots (conditional on employment)
% result.hwage
% Dimension 1: 1=male, 2=female, 3=all
% Dimension 2: 1=all, 2=single-childless, 3=single-with children,
%              4 = married-childless, 5=married-with children.

demog_c = 1; % consider all demographic types
% Extract vectors of employment effects for demog_c 
result_wage = zeros(n_struct,3); % dim1: min wage; dim2: gender (male, female, all)
result_wage_gender = zeros(n_struct,1); % dim1: min wage;
result_wage_gender_gap = zeros(n_struct,1); % dim1: min wage;

% Gender gap: log(female)-log(male)
result_wage_gender_log = zeros(n_struct,1); % dim1: min wage;
for ii = 1:n_struct
        for g_c = 1:3
            % Change in emp relative to benchmark
            result_wage(ii,g_c) = 100*(result(ii).hwage(g_c,demog_c) ...
                - result(1).hwage(g_c,demog_c))./result(1).hwage(g_c,demog_c);
        end % g_c
        % Gender gap 
        result_wage_gender_gap(ii) = 100*(result(ii).hwage(2,demog_c)./result(ii).hwage(1,demog_c) );
        % Gender gap relative to benchmark
        result_wage_gender(ii) = 100*(result(ii).hwage(2,demog_c)./result(ii).hwage(1,demog_c) ) ...
                - 100*(result(1).hwage(2,demog_c)./result(1).hwage(1,demog_c) ); 
        % Gender gap relative to benchmark, log(gender gap min wage) - log(gender gap benchmark)
        result_wage_gender_log(ii) = log(result(ii).hwage(2,demog_c)./result(ii).hwage(1,demog_c) ) ...
                - log(result(1).hwage(2,demog_c)./result(1).hwage(1,demog_c) ); 
end

% Change in hours and gender hours gap
figure
plot([struct(2:end).minWage]',result_wage(2:end,3),'x','linewidth',mylw)
hold on
plot([struct(2:end).minWage]',result_wage_gender(2:end),'o','linewidth',mylw)
hold off
grid on
ax = gca;
ax.FontSize = myfs;
%ax.YLim = [-1.5,2];
ax.XLim = [8,14];
legend('Avg. Wage','Gender Wage Gap','fontsize',myfs-2,'location','best')
xlabel('Minimum wage','fontsize',myfs)
ylabel("p.p. change ",'fontsize',myfs)
fig_name = 'minwages_wage';
print(fullfile(FigDir,fig_name),'-dpng','-r300')
print(fullfile(FigDir,fig_name),'-depsc')



%% Make Hours plots (conditional on employment)
% result.avg_h
% Dimension 1: 1=male, 2=female, 3=all, 4 = gender gap (women-men)
% Dimension 2: 1=all, 2=single-childless, 3=single-with children,
%              4 = married-childless, 5=married-with children.

demog_c = 1; % consider all demographic types
% Extract vectors of employment effects for demog_c 
result_hours = zeros(n_struct,3); % dim1: min wage; dim2: gender (male, female, all)
result_hours_gender = zeros(n_struct,1); % dim1: min wage;
% Gender gap: log(female)-log(male)
result_hours_gender_log = zeros(n_struct,1); % dim1: min wage;
for ii = 1:n_struct
        for g_c = 1:3
            % Change in emp relative to benchmark
            result_hours(ii,g_c) = 100*(result(ii).avg_h(g_c,demog_c) ...
                - result(1).avg_h(g_c,demog_c))./result(1).avg_h(g_c,demog_c);
        end % g_c
        % Gender gap relative to benchmark
        result_hours_gender(ii) = 100*(result(ii).avg_h(2,demog_c)./result(ii).avg_h(1,demog_c) ) ...
                - 100*(result(1).avg_h(2,demog_c)./result(1).avg_h(1,demog_c) ); 
        % Gender gap relative to benchmark, log(gender gap min wage) - log(gender gap benchmark)
        result_hours_gender_log(ii) = log(result(ii).avg_h(2,demog_c)./result(ii).avg_h(1,demog_c) ) ...
                - log(result(1).avg_h(2,demog_c)./result(1).avg_h(1,demog_c) ); 
end

% Change in hours and gender hours gap
figure
plot([struct(2:end).minWage]',result_hours(2:end,3),'x','linewidth',mylw)
hold on
plot([struct(2:end).minWage]',result_hours_gender(2:end),'o','linewidth',mylw)
hold off
grid on
ax = gca;
ax.FontSize = myfs;
%ax.YLim = [-1.5,2];
ax.XLim = [8,14];
legend('Avg. Hours','Gender Hours Gap','fontsize',myfs-2,'location','best')
xlabel('Minimum wage','fontsize',myfs)
ylabel("p.p. change ",'fontsize',myfs)
fig_name = 'minwages_hours';
print(fullfile(FigDir,fig_name),'-dpng','-r300')
print(fullfile(FigDir,fig_name),'-depsc')

%% Make Employment plots
demog_c = 1; % consider all demographic types
% Extract vectors of employment effects for demog_c 
result_emp = zeros(n_struct,3,4); % dim1: min wage; dim2: gender (male, female, all); dim3: hours (ME,PT,FT,EMP)
result_emp_gender = zeros(n_struct,4); % dim1: min wage; dim2: hours (ME,PT,FT,EMP)
% Gender gap: log(female)-log(male)
result_emp_gender_log = zeros(n_struct,4); % dim1: min wage; dim2: hours (ME,PT,FT,EMP)
for ii = 1:n_struct
    for h_c = 1:4
        for g_c = 1:3
            % Change in emp relative to benchmark
            result_emp(ii,g_c,h_c) = 100*(result(ii).emp_dist(h_c,g_c,demog_c) ...
                - result(1).emp_dist(h_c,g_c,demog_c));
        end % g_c
        % Gender gap relative to benchmark
        result_emp_gender(ii,h_c) = 100*(result(ii).emp_dist(h_c,2,demog_c) ...
                ./result(ii).emp_dist(h_c,1,demog_c)) ...
                - 100*(result(1).emp_dist(h_c,2,demog_c)./result(1).emp_dist(h_c,1,demog_c)); 
        % Gender gap relative to benchmark, log(gender gap min wage) - log(gender gap benchmark)
        result_emp_gender_log(ii,h_c) = log(result(ii).emp_dist(h_c,2,demog_c) ...
                ./result(ii).emp_dist(h_c,1,demog_c)) ...
                - log(result(1).emp_dist(h_c,2,demog_c) ...
                ./result(1).emp_dist(h_c,1,demog_c)); 
    end
end

% Change in employment by gender
figure
plot([struct(2:end).minWage],result_emp(2:end,1,4),'x','linewidth',mylw)
hold on
plot([struct(2:end).minWage],result_emp(2:end,2,4),'o','linewidth',mylw)
hold off
grid on
ax = gca;
ax.FontSize = myfs;
%ax.YLim = [-1.5,2];
ax.XLim = [8,14];
legend('Male Employment','Female Employment','fontsize',myfs-2,'location','best')
xlabel('Minimum wage','fontsize',myfs)
ylabel("p.p. change ",'fontsize',myfs)
fig_name = 'minwages_emp_g';
print(fullfile(FigDir,fig_name),'-dpng','-r300')
print(fullfile(FigDir,fig_name),'-depsc')

% Change in employment and gender employment gap
figure
plot([struct(2:end).minWage],result_emp(2:end,3,4),'x','linewidth',mylw)
hold on
plot([struct(2:end).minWage],result_emp_gender(2:end,4),'o','linewidth',mylw)
hold off
grid on
ax = gca;
ax.FontSize = myfs;
%ax.YLim = [-1.5,2];
ax.XLim = [8,14];
legend('Employment','Gender Employment Gap','fontsize',myfs-2,'location','south')
xlabel('Minimum wage','fontsize',myfs)
ylabel("p.p. change ",'fontsize',myfs)
fig_name = 'minwages_emp';
print(fullfile(FigDir,fig_name),'-dpng','-r300')
print(fullfile(FigDir,fig_name),'-depsc')

%% Make hours worked plots (unconditional on employment)

result_hworked = zeros(n_struct,3); % dim1: min wage; dim2: gender (male, female, all)
result_hworked_gender = zeros(n_struct,1); % dim1: min wage;
result_hworked_gender_gap = zeros(n_struct,1); % dim1: min wage;

% Gender gap: log(female)-log(male)
result_hworked_gender_log = zeros(n_struct,1); % dim1: min wage;
for ii = 1:n_struct
        for g_c = 1:3
            % Change in emp relative to benchmark
            result_hworked(ii,g_c) = 100*(result(ii).avg_h(g_c,demog_c)*result(ii).emp_dist(4,g_c,demog_c) ...
                - result(1).avg_h(g_c,demog_c)*result(1).emp_dist(4,g_c,demog_c))...
                /(result(1).avg_h(g_c,demog_c)*result(1).emp_dist(4,g_c,demog_c));
        end % g_c
        % Gender gap relative to benchmark
        result_hworked_gender_gap(ii) = 100*(result(ii).avg_h(2,demog_c)*result(ii).emp_dist(4,2,demog_c) ...
            /(result(ii).avg_h(1,demog_c)*result(ii).emp_dist(4,1,demog_c))); 
        result_hworked_gender(ii) = 100*(result(ii).avg_h(2,demog_c)*result(ii).emp_dist(4,2,demog_c) ...
            /(result(ii).avg_h(1,demog_c)*result(ii).emp_dist(4,1,demog_c))) ...
                -100*(result(1).avg_h(2,demog_c)*result(1).emp_dist(4,2,demog_c) ...
            /(result(1).avg_h(1,demog_c)*result(1).emp_dist(4,1,demog_c))); 
        % Gender gap relative to benchmark, log(gender gap min wage) - log(gender gap benchmark)
        result_hworked_gender_log(ii) = log(result(ii).avg_h(2,demog_c)*result(ii).emp_dist(4,2,demog_c) ...
            /(result(ii).avg_h(1,demog_c)*result(ii).emp_dist(4,1,demog_c))) ...
                -log(result(1).avg_h(2,demog_c)*result(1).emp_dist(4,2,demog_c) ...
            /(result(1).avg_h(1,demog_c)*result(1).emp_dist(4,1,demog_c))); 
end

% Change in hours worked by gender
figure
plot([struct(2:end).minWage]',result_hworked(2:end,1),'x','linewidth',mylw)
hold on
plot([struct(2:end).minWage]',result_hworked(2:end,2),'o','linewidth',mylw)
hold off
grid on
ax = gca;
ax.FontSize = myfs;
%ax.YLim = [-1.5,2];
ax.XLim = [8,14];
legend('Male Hours Worked','Female Hours Worked','fontsize',myfs-2,'location','best')
xlabel('Minimum wage','fontsize',myfs)
ylabel("p.p. change ",'fontsize',myfs)
fig_name = 'minwages_hworked_g';
print(fullfile(FigDir,fig_name),'-dpng','-r300')
print(fullfile(FigDir,fig_name),'-depsc')

% Change in hours worked and gender hours worked gap
figure
plot([struct(2:end).minWage]',result_hworked(2:end,3),'x','linewidth',mylw)
hold on
plot([struct(2:end).minWage]',result_hworked_gender(2:end),'o','linewidth',mylw)
hold off
grid on
ax = gca;
ax.FontSize = myfs;
%ax.YLim = [-1.5,2];
ax.XLim = [8,14];
legend('Avg. Hours Worked','Gender Hours-Worked Gap','fontsize',myfs-2,'location','best')
xlabel('Minimum wage','fontsize',myfs)
ylabel("p.p. change ",'fontsize',myfs)
fig_name = 'minwages_hworked';
print(fullfile(FigDir,fig_name),'-dpng','-r300')
print(fullfile(FigDir,fig_name),'-depsc')

%% Calculate gender gap decomposition 
% Accounted for by hours worked
result_hworked_gender_decom = result_hworked_gender_log./result_inc_gender_log;
result_wage_gender_decom = result_wage_gender_log./result_inc_gender_log;

close all

%% Plot All labor market outcomes
for g_c = 1:3
    figure
    plot([struct(2:end).minWage],result_wage(2:end,g_c),'x','linewidth',mylw)
    hold on
    plot([struct(2:end).minWage],result_hworked(2:end,g_c),'o','linewidth',mylw)
    hold on
    plot([struct(2:end).minWage],result_inc(2:end,g_c),'diamond','linewidth',mylw)
    hold off
    grid on
    ax = gca;
    ax.FontSize = myfs;
    ax.YLim = [0,25];
    ax.XLim = [8,14];
    if g_c == 1
        legend('Wage','Hours Worked','Income','fontsize',myfs-2,'location','best')
    end
    xlabel('Minimum wage','fontsize',myfs)
    ylabel("% change ",'fontsize',myfs)
    fig_name = ['minwages_outcomes_g',num2str(g_c)];
    print(fullfile(FigDir,fig_name),'-dpng','-r300')
    print(fullfile(FigDir,fig_name),'-depsc')
end

%% Plot gender gaps

% gender gaps (rel to bench)
figure
plot([struct(2:end).minWage],result_wage_gender(2:end),'x','linewidth',mylw)
hold on
plot([struct(2:end).minWage],result_hworked_gender(2:end),'o','linewidth',mylw)
hold on
plot([struct(2:end).minWage],result_inc_gender(2:end),'diamond','linewidth',mylw)
hold off
grid on
ax = gca;
ax.FontSize = myfs;
ax.YLim = [0,8];
ax.XLim = [8,14];
legend('Wage Gap','Hours Worked Gap','Income Gap','fontsize',myfs-2,'location','best')
xlabel('Minimum wage','fontsize',myfs)
ylabel("p.p. change ",'fontsize',myfs)
fig_name = 'minwages_gender';
print(fullfile(FigDir,fig_name),'-dpng','-r300')
print(fullfile(FigDir,fig_name),'-depsc')

% gender gap decomposition
figure
plot([struct(2:end).minWage],100*result_wage_gender_decom(2:end),'x','linewidth',mylw)
hold on
plot([struct(2:end).minWage],100*result_hworked_gender_decom(2:end),'o','linewidth',mylw)
hold off
grid on
ax = gca;
ax.FontSize = myfs;
%ax.YLim = [-1.5,2];
ax.XLim = [8,14];
%legend('Wage Gap','Hours Worked Gap','fontsize',myfs-2,'location','best')
xlabel('Minimum wage','fontsize',myfs)
ylabel("Decomposition (%)",'fontsize',myfs)
fig_name = 'minwages_gender_decom';
print(fullfile(FigDir,fig_name),'-dpng','-r300')
print(fullfile(FigDir,fig_name),'-depsc')

% gender gap decomposition as bar
hname = categorical();
for ii = 2:n_struct
    hname(ii) = num2str(struct(ii).minWage);
end
figure
h=bar(hname(2:end),[100*result_wage_gender_decom(2:end),100*result_hworked_gender_decom(2:end)],'stacked');
set(h(1), 'FaceColor', barcolors(1,:), 'EdgeColor', barcolors(1,:), 'LineWidth', 2); % 
set(h(2), 'FaceColor', barcolors(2,:), 'EdgeColor', 'r', 'LineWidth', 2); % 
grid on
ax = gca;
ax.FontSize = myfs;
legend('Wage Gap','Hours Worked Gap','fontsize',myfs-2,'location','best')
xlabel('Minimum wage','fontsize',myfs)
ylabel("Decomposition (%)",'fontsize',myfs)
fig_name = 'minwages_gender_decom_bar';
print(fullfile(FigDir,fig_name),'-dpng','-r300')
print(fullfile(FigDir,fig_name),'-depsc')

end %end function
