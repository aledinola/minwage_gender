function plot_fixedwage_bins(benchmark,minimumWage,par,outputDir)
% PLOT_FIXEDWAGE_BINS Plot fixed-wage earnings histograms by hours type.
%
% INPUTS:
% benchmark:   Fixed-wage equilibrium without a minimum wage
% minimumWage: Fixed-wage equilibrium with the 8.5-euro minimum wage
% par:         Structure with model dimensions and wage scaling
% outputDir:   Output-directory path
%
% OUTPUTS:
% None

[n_w,n_hours,p_a] = unpack(par,'n_w','n_hours','p_a');
prob1 = zeros(n_w,n_hours,2);
prob2 = zeros(n_w,n_hours,2);
for hh = 1:n_hours
    prob1(:,hh,1) = squeeze(sum(benchmark.g_e_sm(:,hh,:,:),[3,4]));
    prob1(:,hh,2) = squeeze(sum(benchmark.g_e_sf(:,hh,:,:),[3,4]));
    prob2(:,hh,1) = squeeze(sum(minimumWage.g_e_sm(:,hh,:,:),[3,4]));
    prob2(:,hh,2) = squeeze(sum(minimumWage.g_e_sf(:,hh,:,:),[3,4]));
    prob1(:,hh,:) = prob1(:,hh,:)./sum(prob1(:,hh,:),1);
    prob2(:,hh,:) = prob2(:,hh,:)./sum(prob2(:,hh,:),1);
end

edges = 0:40;
yMaximum = [0.5 0.3 0.1];
yStep = [0.1 0.1 0.02];
for hh = 1:n_hours
    weights1 = fun_G_bins(benchmark.w_grid(:,hh),squeeze(prob1(:,hh,:)),edges,p_a(hh),0);
    weights2 = fun_G_bins(minimumWage.w_grid(:,hh),squeeze(prob2(:,hh,:)),edges,p_a(hh),8.5);
    fig = figure;
    bar([weights1,weights2],'grouped','BarWidth',2)
    xticks(1:5:40)
    ylim([0 yMaximum(hh)])
    yticks(0:yStep(hh):yMaximum(hh))
    set(gca,'FontSize',20)
    if hh == 1
        legend('No Min. Wage','Min. Wage: 8.5 €','FontSize',18,'Location','northeast')
    end
    xlabel('Hourly wage bins','FontSize',20)
    ylabel('Probability','FontSize',20)
    saveFigurePair(fig,outputDir,sprintf('Gbins_h%d_benchmark_minWage',hh));
end
end %end function
