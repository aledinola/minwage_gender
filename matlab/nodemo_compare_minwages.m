function [] = nodemo_compare_minwages(models,nModels,par)
% NODEMO_COMPARE_MINWAGES Plot gender gaps in the no-demographic model.
%
% INPUTS:
% models:  [1,nModels] structure array of saved equilibria
% nModels: Scalar number of equilibria, including the benchmark
% par:     Structure of model dimensions and plotting parameters
%
% OUTPUTS:
% None

fontSize = 18;
lineWidth = 3;
figureDir = par.FigDir;
barColors = [0.16 0.32 0.75; 1 1 1];

wageRatio = zeros(nModels,1);
hoursRatio = zeros(nModels,1);
incomeRatio = zeros(nModels,1);

for ii = 1:nModels
    [~,avgHours,totalIncome,employmentRate,hourlyWage] = ...
        fun_compute_inc_emp_hours(models(ii),par);
    wageRatio(ii) = hourlyWage(2,1)/hourlyWage(1,1);
    hoursRatio(ii) = avgHours(2,1)*employmentRate(2,1)/ ...
        (avgHours(1,1)*employmentRate(1,1));
    incomeRatio(ii) = totalIncome(2,1)/totalIncome(1,1);
end

wageGap = 100*(wageRatio-wageRatio(1));
hoursGap = 100*(hoursRatio-hoursRatio(1));
incomeGap = 100*(incomeRatio-incomeRatio(1));
wageShare = 100*log(wageRatio/wageRatio(1))./log(incomeRatio/incomeRatio(1));
hoursShare = 100*log(hoursRatio/hoursRatio(1))./log(incomeRatio/incomeRatio(1));
minimumWages = [models(2:end).minWage];

figure
plot(minimumWages,wageGap(2:end),'x','linewidth',lineWidth)
hold on
plot(minimumWages,hoursGap(2:end),'o','linewidth',lineWidth)
plot(minimumWages,incomeGap(2:end),'diamond','linewidth',lineWidth)
hold off
grid on
ax = gca;
ax.FontSize = fontSize;
ax.YLim = [0,8];
ax.XLim = [8,14];
legend('Wage Gap','Hours Worked Gap','Income Gap', ...
    'fontsize',fontSize-2,'location','best')
xlabel('Minimum wage','fontsize',fontSize)
ylabel("p.p. change ",'fontsize',fontSize)
print(fullfile(figureDir,'minwages_gender'),'-dpng','-r300')
print(fullfile(figureDir,'minwages_gender'),'-depsc')

labels = categorical();
for ii = 2:nModels
    labels(ii) = num2str(models(ii).minWage);
end
figure
bars = bar(labels(2:end),[wageShare(2:end),hoursShare(2:end)],'stacked');
set(bars(1),'FaceColor',barColors(1,:),'EdgeColor',barColors(1,:),'LineWidth',2)
set(bars(2),'FaceColor',barColors(2,:),'EdgeColor','r','LineWidth',2)
grid on
ax = gca;
ax.FontSize = fontSize;
legend('Wage Gap','Hours Worked Gap','fontsize',fontSize-2,'location','best')
xlabel('Minimum wage','fontsize',fontSize)
ylabel("Decomposition (%)",'fontsize',fontSize)
print(fullfile(figureDir,'minwages_gender_decom_bar'),'-dpng','-r300')
print(fullfile(figureDir,'minwages_gender_decom_bar'),'-depsc')

end %end function
