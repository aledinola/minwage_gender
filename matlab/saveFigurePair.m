function saveFigurePair(fig,outputDir,name)
% SAVEFIGUREPAIR Save one MATLAB figure as EPS and 300-DPI PNG.
%
% INPUTS:
% fig:       MATLAB figure handle
% outputDir: Output-directory path
% name:      Output filename without extension
%
% OUTPUTS:
% None

figure(fig)
set(findall(fig,'-property','FontName'),'FontName','Helvetica')
figureUnits = fig.Units;
fig.Units = 'inches';
figureSize = fig.Position(3:4);
fig.Units = figureUnits;
fig.PaperUnits = 'inches';
fig.PaperPosition = [0 0 figureSize];
fig.PaperSize = figureSize;
drawnow
print(fullfile(outputDir,name),'-dpng','-r300');
print(fullfile(outputDir,name),'-depsc');
end %end function
