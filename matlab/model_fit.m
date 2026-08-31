function [] = model_fit(struct,dir_soep,par)
%compare_emp_dist compares employment distributions based on the g's

[n_w,n_hours,n_kids,n_spinc,p_a,p_min_wage,hours_names,hours_grid,...
    p_offer_a,p_offer_b,p_offer_lb,p_offer_ub] = unpack(par,'n_w',...
    'n_hours','n_kids','n_spinc','p_a','p_min_wage','hours_names','hours_grid',...
    'p_offer_a','p_offer_b','p_offer_lb','p_offer_ub');
w_grid = struct.w_grid;
FigDir = par.FigDir;
TabDir = par.TabDir;
myfs = 14;
mylw = 3;
mymarkersz = 120;

%% Plot distribution of marital status/spousal income
% Extract distribution from SOEP_spinc_dist.txt from dir_soep
fid   = fopen(fullfile(dir_soep,'SOEP_mar.txt'));
SOEP_mar = textscan(fid, '%s%f');
fclose(fid);
share_married = SOEP_mar{2};

SOEP_spinc_dist = load(fullfile(dir_soep,'SOEP_spinc_dist.txt'));
SOEP_spinc_dist = reshape(SOEP_spinc_dist,[2*(n_spinc-1),3]);

SOEP_spinc_vec = load(fullfile(dir_soep,'SOEP_spinc_vec.txt'));
spinc_vec = zeros(n_spinc,1);
spinc_vec(2:n_spinc) = SOEP_spinc_vec(:,2)/12;

spinc_dist = zeros(n_spinc,2);
spinc_dist(1,:) = 1-share_married;
irow = 1;
for g_c = 2:-1:1
    for s_c = 2:n_spinc
        spinc_dist(s_c,g_c) = share_married*SOEP_spinc_dist(irow,3);
        irow = irow + 1;
    end
end

% Spousal income distribution
share_married = 1-spinc_dist(1,1);
figure
scatter(spinc_vec(2:n_spinc),spinc_dist(2:n_spinc,1)/share_married,mymarkersz,'filled')
hold on
scatter(spinc_vec(2:n_spinc),spinc_dist(2:n_spinc,2)/share_married,mymarkersz,'filled','d')
hold off
ax = gca;
grid on
ax.FontSize = myfs;
xticks(round(spinc_vec(2:n_spinc)))
%xticklabels({'Single 1','Married, low sp. inc. 2', '3', 'Married, mid. sp. inc. 4','5', 'Married, high sp. inc. 6'})
ylabel('Probability','fontsize',myfs)
%xlabel('Demographic type, x','fontsize',myfs)
xlabel('Monthly income (2025 euro)','fontsize',myfs)
legend('Wives','Husbands','location','best','fontsize',myfs)
fig_name = ['spinc_dist'];
print(fullfile(FigDir,fig_name),'-dpng','-r300')
print(fullfile(FigDir,fig_name),'-depsc')



%% Extract wage distribution SOEP_hwage.txt from dir_soep
SOEP_hwage = load(fullfile(dir_soep,'SOEP_hwage.txt'));
SOEP_hwage_a = load(fullfile(dir_soep,'SOEP_hwage_a.txt'));

%SOEP_hwage = reshape(SOEP_hwage,[7*n_hours*2,5]);

cdf_data_hwage = SOEP_hwage(1:7,3);
data_hwage = zeros(2,n_hours,7);
sterr_hwage = zeros(2,n_hours,7);
cdf_data_hwage_a = SOEP_hwage_a(1:7,3);
data_hwage_a = zeros(2,n_hours,7);
sterr_hwage_a = zeros(2,n_hours,7);

irow = 1;
for g_c = 1:2
    for h_c = 1:n_hours
        for q_c = 1:7
            data_hwage(g_c,h_c,q_c) = SOEP_hwage(irow,4);
            sterr_hwage(g_c,h_c,q_c) = SOEP_hwage(irow,5);
            data_hwage_a(g_c,h_c,q_c) = SOEP_hwage_a(irow,4);
            sterr_hwage_a(g_c,h_c,q_c) = SOEP_hwage_a(irow,5);

            irow = irow+1;
        end
    end
end

%% Extract model wage distribution from struct


cdf_mod_hwage = zeros(2,n_hours,n_w);

for h_c = 1:n_hours
    for w_c = 1:n_w
        % Men
        cdf_mod_hwage(1,h_c,w_c) = sum(struct.g_e_sm(w_c,h_c,:,:),'all');
        % Women
        cdf_mod_hwage(2,h_c,w_c) = sum(struct.g_e_sf(w_c,h_c,:,:),'all');
    end

    % Get cumulative distribution
    cdf_mod_hwage(1,h_c,:) = 100*cumsum(cdf_mod_hwage(1,h_c,:)/sum(cdf_mod_hwage(1,h_c,:))) ;
    cdf_mod_hwage(2,h_c,:) = 100*cumsum(cdf_mod_hwage(2,h_c,:)/sum(cdf_mod_hwage(2,h_c,:))) ;
end

%% Make plots: equilibrium wage distribution
% men
for h_c = 1:n_hours
    figure
    plot(w_grid(:,h_c),squeeze(cdf_mod_hwage(1,h_c,:)),'linewidth',mylw)
    hold on
    scatter(squeeze(data_hwage(1,h_c,:)),cdf_data_hwage,mymarkersz,'filled')
    hold on
    errorbar(squeeze(data_hwage(1,h_c,:)),cdf_data_hwage, 2*squeeze(sterr_hwage(1,h_c,:)), ...
         'horizontal', 'LineStyle', 'none', 'Color', 'r', 'LineWidth', 1.2);    ylim([0,100])
    xlim([0,40])
    ax = gca;
    grid on
    ax.FontSize = myfs;
    fig_name = ['Men, ',hours_names{h_c}];
    title(fig_name,'fontsize',myfs)
    xlabel('Hourly wage','fontsize',myfs)
    ylabel('Percentile','fontsize',myfs)
    legend('Model','Data','location','best','fontsize',myfs)
    fig_name = ['modelfit_CDF_hwage_g1_h',num2str(h_c)];
    print(fullfile(FigDir,fig_name),'-dpng','-r300')
    print(fullfile(FigDir,fig_name),'-depsc')
end

% women
for h_c = 1:n_hours
    figure
    plot(p_a(h_c)*w_grid(:,h_c),squeeze(cdf_mod_hwage(2,h_c,:)),'linewidth',mylw)
    hold on
    scatter(squeeze(data_hwage(2,h_c,:)),cdf_data_hwage,mymarkersz,'filled')
    hold on
    errorbar(squeeze(data_hwage(2,h_c,:)),cdf_data_hwage, 2*squeeze(sterr_hwage(2,h_c,:)), ...
         'horizontal', 'LineStyle', 'none', 'Color', 'r', 'LineWidth', 1.2);    ylim([0,100])
    hold off
    ylim([0,100])
    xlim([0,40])
    ax = gca;
    grid on
    ax.FontSize = myfs;
    fig_name = ['Women, ',hours_names{h_c}];
    title(fig_name,'fontsize',myfs)
    xlabel('Hourly wage','fontsize',myfs)
    ylabel('Percentile','fontsize',myfs)
    legend('Model','Data','location','best','fontsize',myfs)
    fig_name = ['modelfit_CDF_hwage_g2_h',num2str(h_c)];
    print(fullfile(FigDir,fig_name),'-dpng','-r300')
    print(fullfile(FigDir,fig_name),'-depsc')
end

%% wage computed based on contractual vs actual hours
actualWageFontSize = 16.8;
% men
for h_c = 1:n_hours
    figure
    scatter(squeeze(data_hwage(1,h_c,:)),cdf_data_hwage,mymarkersz,'filled')
    hold on
    errorbar(squeeze(data_hwage(1,h_c,:)),cdf_data_hwage, 2*squeeze(sterr_hwage(1,h_c,:)), ...
         'horizontal', 'LineStyle', 'none', 'Color', 'b', 'LineWidth', 1.2);    ylim([0,100])
    hold on
    scatter(squeeze(data_hwage_a(1,h_c,:)),cdf_data_hwage_a,mymarkersz,'r*')
    hold on
    errorbar(squeeze(data_hwage_a(1,h_c,:)),cdf_data_hwage_a, 2*squeeze(sterr_hwage_a(1,h_c,:)), ...
         'horizontal', 'LineStyle', 'none', 'Color', 'r', 'LineWidth', 1.2);    
    ylim([0,100])
    xlim([0,40])
    ax = gca;
    grid on
    ax.FontSize = actualWageFontSize;
    %fig_name = ['Men, ',hours_names{h_c}];
    %title(fig_name,'fontsize',myfs)
    xlabel('Hourly wage','fontsize',actualWageFontSize)
    ylabel('Percentile','fontsize',actualWageFontSize)
    legend('Contractual hours','','Actual hours','location','best','fontsize',actualWageFontSize)
    fig_name = ['CDF_hwage_a_g1_h',num2str(h_c)];
    print(fullfile(FigDir,fig_name),'-dpng','-r300')
    print(fullfile(FigDir,fig_name),'-depsc')
end
% women
for h_c = 1:n_hours
    figure
    scatter(squeeze(data_hwage(2,h_c,:)),cdf_data_hwage,mymarkersz,'filled')
    hold on
    errorbar(squeeze(data_hwage(2,h_c,:)),cdf_data_hwage, 2*squeeze(sterr_hwage(2,h_c,:)), ...
         'horizontal', 'LineStyle', 'none', 'Color', 'b', 'LineWidth', 1.2);    ylim([0,100])
    hold on
    scatter(squeeze(data_hwage_a(2,h_c,:)),cdf_data_hwage_a,mymarkersz,'r*')
    hold on
    errorbar(squeeze(data_hwage_a(2,h_c,:)),cdf_data_hwage_a, 2*squeeze(sterr_hwage_a(2,h_c,:)), ...
         'horizontal', 'LineStyle', 'none', 'Color', 'r', 'LineWidth', 1.2);    
    ylim([0,100])
    xlim([0,40])
    ax = gca;
    grid on
    ax.FontSize = actualWageFontSize;
    %fig_name = ['Women, ',hours_names{h_c}];
    %title(fig_name,'fontsize',myfs)
    xlabel('Hourly wage','fontsize',actualWageFontSize)
    ylabel('Percentile','fontsize',actualWageFontSize)
    legend('Contractual hours','','Actual hours','location','best','fontsize',actualWageFontSize)
    fig_name = ['CDF_hwage_a_g2_h',num2str(h_c)];
    print(fullfile(FigDir,fig_name),'-dpng','-r300')
    print(fullfile(FigDir,fig_name),'-depsc')
end
%% Make plots: wage offer distribution distribution
% Generate offered wage distribution from the supply side estimation based
% on paramters.
F_prob_bench=zeros(n_w,n_hours);
for h_c = 1:n_hours
 
    for w_c = 1:n_w

        wval = (w_grid(w_c,h_c)-p_offer_lb(h_c))/(p_offer_ub(h_c)-p_offer_lb(h_c));
        F_prob_bench(w_c,h_c)=betapdf(wval,p_offer_a(h_c),p_offer_b(h_c));
    end
    % Normalize F_prob_bench so that sum over w is one
    F_prob_bench(:,h_c)=F_prob_bench(:,h_c)/sum(F_prob_bench(:,h_c));
end

%{
% Make plots: pairwise
for h_c = 1:n_hours
    figure
    plot(w_grid(:,h_c),struct.F_prob(:,h_c),'linewidth',mylw)
    hold on
    plot(w_grid(:,h_c),F_prob_bench(:,h_c),'linewidth',mylw)
    hold off
    ylim([0,0.02])
    %xlim([0,40])
    ax = gca;
    grid on
    ax.FontSize = myfs;
    %fig_name = ['Men, ',hours_names{h_c}];
    %title(fig_name,'fontsize',myfs)
    xlabel('Wage rate','fontsize',myfs)
    ylabel('PDF','fontsize',myfs)
    legend('Demand-Estimation','Supply-Estimation','location','best','fontsize',myfs)
    %fig_name = ['modelfit_CDF_hwage_g1_h',num2str(h_c)];
    %print(fullfile(FigDir,fig_name),ExtFig)
end

% All together
figure
plot(w_grid(:,1),F_prob_bench(:,1),'b-','linewidth',mylw-1)
hold on
plot(w_grid(:,2),F_prob_bench(:,2),'b--','linewidth',mylw-1)
hold on
plot(w_grid(:,3),F_prob_bench(:,3),'b-.','linewidth',mylw-1)
hold on
plot(w_grid(:,1),struct.F_prob(:,1),'r-','linewidth',mylw-1)
hold on
plot(w_grid(:,2),struct.F_prob(:,2),'r--','linewidth',mylw-1)
hold on
plot(w_grid(:,3),struct.F_prob(:,3),'r-.','linewidth',mylw-1)
hold on
hold off
ylim([0,0.02])
%xlim([0,40])
ax = gca;
grid on
ax.FontSize = myfs;
fig_name = ['Fit of the offer distribution'];
title(fig_name,'fontsize',myfs)
xlabel('Wage rate','fontsize',myfs)
ylabel('PDF','fontsize',myfs)
legend('Supply: ME','Supply: PT','Supply: FT','Demand: ME','Demand: PT','Demand: FT','location','best','fontsize',myfs)
%fig_name = ['modelfit_CDF_hwage_g1_h',num2str(h_c)];
%print(fullfile(FigDir,fig_name),ExtFig)
%}

% Supply side only
figure
plot(w_grid(:,1),F_prob_bench(:,1),'-','linewidth',mylw-1)
hold on
plot(w_grid(:,2),F_prob_bench(:,2),'--','linewidth',mylw-1)
hold on
plot(w_grid(:,3),F_prob_bench(:,3),'-.','linewidth',mylw-1)
hold off
ylim([0,0.02])
%xlim([0,40])
ax = gca;
grid on
ax.FontSize = myfs;
%fig_name = ['Fit of the offer distribution'];
%title(fig_name,'fontsize',myfs)
xlabel('Wage rate','fontsize',myfs)
ylabel('PDF','fontsize',myfs)
legend('ME','PT','FT','location','best','fontsize',myfs)
fig_name = ['modelfit_offerdist_supply'];
print(fullfile(FigDir,fig_name),'-dpng','-r300')
print(fullfile(FigDir,fig_name),'-depsc')

% Demand side only
figure
plot(w_grid(:,1),struct.F_prob(:,1),'-','linewidth',mylw-1)
hold on
plot(w_grid(:,2),struct.F_prob(:,2),'--','linewidth',mylw-1)
hold on
plot(w_grid(:,3),struct.F_prob(:,3),'-.','linewidth',mylw-1)
hold off
ylim([0,0.02])
%xlim([0,40])
ax = gca;
grid on
ax.FontSize = myfs;
%fig_name = ['Fit of the offer distribution'];
%title(fig_name,'fontsize',myfs)
xlabel('Wage rate','fontsize',myfs)
ylabel('PDF','fontsize',myfs)
legend('ME','PT','FT','location','best','fontsize',myfs)
fig_name = ['modelfit_offerdist_demand'];
print(fullfile(FigDir,fig_name),'-dpng','-r300')
print(fullfile(FigDir,fig_name),'-depsc')

%% Labor supply
figure
plot(w_grid(:,1),struct.labsup(:,1),'-','linewidth',mylw-1)
hold on
plot(w_grid(:,2),struct.labsup(:,2),'--','linewidth',mylw-1)
hold on
plot(w_grid(:,3),struct.labsup(:,3),'-.','linewidth',mylw-1)
hold off
%ylim([0,0.02])
%xlim([0,40])
ax = gca;
grid on
ax.FontSize = myfs;
%fig_name = ['Fit of the offer distribution'];
%title(fig_name,'fontsize',myfs)
xlabel('Wage rate','fontsize',myfs)
ylabel('Labor Supply','fontsize',myfs)
legend('ME','PT','FT','location','best','fontsize',myfs)
fig_name = ['labsup_bench'];
print(fullfile(FigDir,fig_name),'-dpng','-r300')
print(fullfile(FigDir,fig_name),'-depsc')
end %end function "model_fit"
