function plot_benchmark_minimum_wage(benchmark,minimumWage,par,outputDir)
% PLOT_BENCHMARK_MINIMUM_WAGE Reproduce benchmark 8.5-euro figure panels.
%
% INPUTS:
% benchmark:   Equilibrium without a minimum wage
% minimumWage: Equilibrium with the 8.5-euro minimum wage
% par:         Structure with model dimensions and labels
% outputDir:   Output-directory path
%
% OUTPUTS:
% None

[n_w,n_hours,n_kids,n_spinc,p_a,hours_names] = unpack(par, ...
    'n_w','n_hours','n_kids','n_spinc','p_a','hours_names');
myfs = 14;
mylw = 3;
cdfFontSize = 20;

for hh = 1:n_hours
    fig = figure;
    plot(benchmark.w_grid(:,hh),cumsum(benchmark.F_prob(:,hh)),'-','LineWidth',mylw)
    hold on
    plot(minimumWage.w_grid(:,hh),cumsum(minimumWage.F_prob(:,hh)),'--','LineWidth',mylw)
    hold off
    ylim([0 1])
    xlim([0 40])
    grid on
    set(gca,'FontSize',cdfFontSize)
    legend('Benchmark','Min. Wage','FontSize',cdfFontSize,'Location','best')
    xlabel('Wage rate, w','FontSize',cdfFontSize)
    ylabel('CDF','FontSize',cdfFontSize)
    title(hours_names{hh},'FontSize',cdfFontSize)
    saveFigurePair(fig,outputDir,sprintf('Fprob_cdf_h%d_benchmark_minWage',hh));
    close(fig)
end

fig = figure;
plot(benchmark.prod_grid,sum(benchmark.Delta,2),'-','LineWidth',mylw)
hold on
plot(minimumWage.prod_grid,sum(minimumWage.Delta,2),'--','LineWidth',mylw)
hold off
ylim([0.8 1])
set(gca,'FontSize',20)
legend('Benchmark','Min. Wage','FontSize',20,'Location','best')
xlabel('Firm productivity, p','FontSize',20)
ylabel('\Delta^{act}(p)','FontSize',20)
saveFigurePair(fig,outputDir,'Delta_act_benchmark_minWage');
close(fig)

for hh = 1:n_hours
    fig = figure;
    plot(benchmark.prod_grid,benchmark.Delta(:,hh)./sum(benchmark.Delta,2),'-','LineWidth',mylw)
    hold on
    plot(minimumWage.prod_grid,minimumWage.Delta(:,hh)./sum(minimumWage.Delta,2),'--','LineWidth',mylw)
    hold off
    if hh == 1
        ylim([0 0.1])
    elseif hh == 2
        ylim([0 0.3])
    else
        ylim([0.6 1])
    end
    xlim([0 250])
    set(gca,'FontSize',24)
    legend('Benchmark','Min. Wage','FontSize',24,'Location','best')
    xlabel('Firm productivity, p','FontSize',24)
    ylabel('\Delta(h;p)/\Delta^{act}(p)','FontSize',24)
    title(hours_names{hh},'FontSize',24)
    saveFigurePair(fig,outputDir,sprintf('Delta_cond_h%d_benchmark_minWage',hh));
    close(fig)
end

plot_employment_change(benchmark,minimumWage,par,outputDir, ...
    'emp_dist_benchmark_minWage',20,20);
close all

emp1 = zeros(n_hours+1,n_kids,2,n_spinc);
emp2 = zeros(n_hours+1,n_kids,2,n_spinc);
for kk = 1:n_kids
    for ss = 1:n_spinc
        mass1m = benchmark.g_u_sm(kk,ss) + sum(benchmark.g_e_sm(:,:,kk,ss),'all');
        mass1f = benchmark.g_u_sf(kk,ss) + sum(benchmark.g_e_sf(:,:,kk,ss),'all');
        mass2m = minimumWage.g_u_sm(kk,ss) + sum(minimumWage.g_e_sm(:,:,kk,ss),'all');
        mass2f = minimumWage.g_u_sf(kk,ss) + sum(minimumWage.g_e_sf(:,:,kk,ss),'all');
        for hh = 1:n_hours
            emp1(hh,kk,1,ss) = sum(benchmark.g_e_sm(:,hh,kk,ss))/mass1m;
            emp1(hh,kk,2,ss) = sum(benchmark.g_e_sf(:,hh,kk,ss))/mass1f;
            emp2(hh,kk,1,ss) = sum(minimumWage.g_e_sm(:,hh,kk,ss))/mass2m;
            emp2(hh,kk,2,ss) = sum(minimumWage.g_e_sf(:,hh,kk,ss))/mass2f;
        end
        emp1(4,kk,1,ss) = benchmark.g_u_sm(kk,ss)/mass1m;
        emp1(4,kk,2,ss) = benchmark.g_u_sf(kk,ss)/mass1f;
        emp2(4,kk,1,ss) = minimumWage.g_u_sm(kk,ss)/mass2m;
        emp2(4,kk,2,ss) = minimumWage.g_u_sf(kk,ss)/mass2f;
    end
end
empChange = emp2 - emp1;

for gg = 1:2
    for kk = 1:n_kids
        fig = figure;
        bar(1:n_spinc,squeeze(empChange(1:3,kk,gg,:)),'stack')
        hold on
        plot(1:n_spinc,-squeeze(empChange(4,kk,gg,:)),'sg', ...
            'MarkerSize',15,'MarkerFaceColor','g')
        hold off
        legend({'ME','PT','FT','Employment'},'Location','northoutside', ...
            'FontSize',myfs,'NumColumns',4)
        xticks(1:n_spinc)
        xticklabels({'Single','Married, low sp. inc.','', ...
            'Married, mid sp. inc.','','Married, high sp. inc.'})
        xtickangle(30)
        ylim([-0.02 0.02])
        ylabel('Effect of the minimum wage','FontSize',myfs)
        set(gca,'FontSize',myfs)
        saveFigurePair(fig,outputDir,sprintf( ...
            'bar_empdist_diff_g%d_k%d_benchmark_minWage',gg,kk));
        close(fig)
    end
end

prob1 = zeros(n_w,n_hours,2);
prob2 = zeros(n_w,n_hours,2);
for hh = 1:n_hours
    prob1(:,hh,1) = squeeze(sum(benchmark.g_e_sm(:,hh,:,:),[3,4]));
    prob1(:,hh,2) = squeeze(sum(benchmark.g_e_sf(:,hh,:,:),[3,4]));
    prob2(:,hh,1) = squeeze(sum(minimumWage.g_e_sm(:,hh,:,:),[3,4]));
    prob2(:,hh,2) = squeeze(sum(minimumWage.g_e_sf(:,hh,:,:),[3,4]));
    prob1(:,hh,:) = prob1(:,hh,:)./sum(prob1(:,hh,:),1);
    prob2(:,hh,:) = prob2(:,hh,:)./sum(prob2(:,hh,:),1);

    wage1 = [benchmark.w_grid(:,hh);p_a(hh)*benchmark.w_grid(:,hh)];
    wage2 = [minimumWage.w_grid(:,hh);p_a(hh)*minimumWage.w_grid(:,hh)];
    weight1 = [prob1(:,hh,1);prob1(:,hh,2)];
    weight2 = [prob2(:,hh,1);prob2(:,hh,2)];
    points = linspace(min(wage1),max(wage1),150);
    [density1,x1] = ksdensity(wage1,points,'Weights',weight1);
    [density2,x2] = ksdensity(wage2,points,'Weights',weight2);
    fig = figure;
    plot(x1,density1,'-','LineWidth',mylw)
    hold on
    plot(x2,density2,'--','LineWidth',mylw)
    xline(8.5)
    hold off
    set(gca,'FontSize',20)
    if hh == 1
        legend('Benchmark','Min. Wage','FontSize',20,'Location','best')
    end
    xlabel('Hourly wage','FontSize',20)
    ylabel('Probability density','FontSize',20)
    saveFigurePair(fig,outputDir,sprintf('Gpdf_h%d_benchmark_minWage',hh));
    close(fig)
end

prod1 = benchmark.prod_grid;
prod2 = benchmark.prod_grid*(minimumWage.prod_grid(1)/benchmark.prod_grid(1));
aveProd1 = zeros(n_kids,2,n_spinc);
aveProd2 = zeros(n_kids,2,n_spinc);
aveByHours1 = zeros(n_hours,n_kids,2,n_spinc);
aveByHours2 = zeros(n_hours,n_kids,2,n_spinc);
for kk = 1:n_kids
    for gg = 1:2
        for ss = 1:n_spinc
            if gg == 1
                dist1 = benchmark.g_e_sm(:,:,kk,ss);
                dist2 = minimumWage.g_e_sm(:,:,kk,ss);
            else
                dist1 = benchmark.g_e_sf(:,:,kk,ss);
                dist2 = minimumWage.g_e_sf(:,:,kk,ss);
            end
            aveProd1(kk,gg,ss) = sum(prod1.*sum(dist1,2))/sum(dist1,'all');
            aveProd2(kk,gg,ss) = sum(prod2.*sum(dist2,2))/sum(dist2,'all');
            for hh = 1:n_hours
                aveByHours1(hh,kk,gg,ss) = sum(prod1.*dist1(:,hh))/sum(dist1(:,hh));
                aveByHours2(hh,kk,gg,ss) = sum(prod2.*dist2(:,hh))/sum(dist2(:,hh));
            end
        end
    end
end
aveProdChange = aveProd2 - aveProd1;
decomposition = zeros(4,n_kids,2,n_spinc);
for kk = 1:n_kids
    for gg = 1:2
        for ss = 1:n_spinc
            employmentWeights = emp1(1:3,kk,gg,ss)/sum(emp1(1:3,kk,gg,ss));
            decomposition(1:3,kk,gg,ss) = employmentWeights.* ...
                (aveByHours2(:,kk,gg,ss)-aveByHours1(:,kk,gg,ss));
            decomposition(4,kk,gg,ss) = aveProdChange(kk,gg,ss) - ...
                sum(decomposition(1:3,kk,gg,ss));
        end
    end
end

for gg = 1:2
    for kk = 1:n_kids
        fig = figure;
        bar(1:n_spinc,squeeze(decomposition(:,kk,gg,:)),'stack')
        hold on
        plot(1:n_spinc,squeeze(aveProdChange(kk,gg,:)),'sg', ...
            'MarkerSize',15,'MarkerFaceColor','g')
        hold off
        legend({'ME','PT','FT','Change in h','Overall'},'Location','northoutside', ...
            'FontSize',myfs,'NumColumns',5)
        xticks(1:n_spinc)
        xticklabels({'Single','Married, low sp. inc.','', ...
            'Married, mid sp. inc.','','Married, high sp. inc.'})
        xtickangle(30)
        ylim([-4 4])
        ylabel('Effect of the minimum wage','FontSize',myfs)
        set(gca,'FontSize',myfs)
        saveFigurePair(fig,outputDir,sprintf( ...
            'bar_aveprod_diff_g%d_k%d_benchmark_minWage',gg,kk));
        close(fig)
    end
end
end %end function
