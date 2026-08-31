function plot_employment_change(benchmark,counterfactual,par,outputDir,name,fontSize,legendFontSize)
% PLOT_EMPLOYMENT_CHANGE Plot employment-state changes by gender.
%
% INPUTS:
% benchmark:    Benchmark equilibrium structure
% counterfactual: Counterfactual equilibrium structure
% par:          Structure with model parameters
% outputDir:    Output-directory path
% name:         Output filename without extension
% fontSize:     Axes font size in points
% legendFontSize: Legend font size in points
%
% OUTPUTS:
% None

emp1 = fun_compute_inc_emp_hours(benchmark,par);
emp2 = fun_compute_inc_emp_hours(counterfactual,par);
change = emp2(:,1:2,1) - emp1(:,1:2,1);
labelFontSize = 1.1*fontSize;
fig = figure;
bars = bar(change);
set(bars(1),'FaceColor',[0.16 0.32 0.75],'EdgeColor',[0.16 0.32 0.75], ...
    'LineWidth',2)
set(bars(2),'FaceColor',[1 1 1],'EdgeColor','r','LineWidth',2)
xticks(1:4)
xticklabels(par.hours_names)
ylim([-0.04 0.04])
yticks(-0.04:0.02:0.04)
grid on
set(gca,'FontSize',fontSize)
legend('Men','Women','Location','best','FontSize',legendFontSize)
xlabel('Employment state','FontSize',labelFontSize)
ylabel('Change in employment rate','FontSize',labelFontSize)
saveFigurePair(fig,outputDir,name);
end %end function
