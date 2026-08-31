function plot_welfare_effects(benchmarkDir,minimumWageDir,par,outputDir)
% PLOT_WELFARE_EFFECTS Plot CEV effects for childless model entrants.
%
% INPUTS:
% benchmarkDir:   Directory containing benchmark value functions
% minimumWageDir: Directory containing minimum-wage value functions
% par:            Structure with model dimensions and discount rate
% outputDir:      Directory for generated figure files
%
% OUTPUTS:
% None

benchmarkMale = loadArray(fullfile(benchmarkDir,'V_u_sm.txt'), ...
    [par.n_kids,par.n_spinc]);
benchmarkFemale = loadArray(fullfile(benchmarkDir,'V_u_sf.txt'), ...
    [par.n_kids,par.n_spinc]);
minimumWageMale = loadArray(fullfile(minimumWageDir,'V_u_sm.txt'), ...
    [par.n_kids,par.n_spinc]);
minimumWageFemale = loadArray(fullfile(minimumWageDir,'V_u_sf.txt'), ...
    [par.n_kids,par.n_spinc]);

cevMale = 100*(exp(par.rho*(minimumWageMale-benchmarkMale))-1);
cevFemale = 100*(exp(par.rho*(minimumWageFemale-benchmarkFemale))-1);

myfs = 14;
mylw = 3;
fig = figure;
plot(1:par.n_spinc,cevMale(1,:),'o-','LineWidth',mylw)
hold on
plot(1:par.n_spinc,cevFemale(1,:),'x--','LineWidth',mylw)
yline(0)
hold off
ylabel('CEV, %','FontSize',myfs)
legend('Male','Female','FontSize',myfs)
xticks(1:par.n_spinc)
xticklabels({'Single','Married, low sp. inc.','', ...
    'Married, mid. sp. inc.','','Married, high sp. inc.'})
ylim([-2 4])
set(gca,'FontSize',myfs)
saveFigurePair(fig,outputDir,'cev_benchmark_minWage');
close(fig)
end %end function
