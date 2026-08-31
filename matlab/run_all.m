function run_all
% RUN_ALL Reproduce all package tables and figures from saved data.
%
% INPUTS:
% None
%
% OUTPUTS:
% None

matlabDir = fileparts(mfilename('fullpath'));
packageDir = fileparts(matlabDir);
outputDir = fullfile(packageDir,'paper_outputs');
generatedDir = tempname;

mkdir(generatedDir)
if ~isfolder(outputDir)
    mkdir(outputDir)
end

oldVisibility = get(groot,'DefaultFigureVisible');
oldFigureUnits = get(groot,'DefaultFigureUnits');
oldFigurePosition = get(groot,'DefaultFigurePosition');
oldAxesFontName = get(groot,'DefaultAxesFontName');
oldTextFontName = get(groot,'DefaultTextFontName');
oldLegendFontName = get(groot,'DefaultLegendFontName');
oldPaperPositionMode = get(groot,'DefaultFigurePaperPositionMode');
set(groot,'DefaultFigureVisible','off', ...
    'DefaultFigureUnits','inches', ...
    'DefaultFigurePosition',[1 1 5.833 4.375], ...
    'DefaultAxesFontName','Helvetica', ...
    'DefaultTextFontName','Helvetica', ...
    'DefaultLegendFontName','Helvetica', ...
    'DefaultFigurePaperPositionMode','auto')
cleanup = onCleanup(@() set(groot, ...
    'DefaultFigureVisible',oldVisibility, ...
    'DefaultFigureUnits',oldFigureUnits, ...
    'DefaultFigurePosition',oldFigurePosition, ...
    'DefaultAxesFontName',oldAxesFontName, ...
    'DefaultTextFontName',oldTextFontName, ...
    'DefaultLegendFontName',oldLegendFontName, ...
    'DefaultFigurePaperPositionMode',oldPaperPositionMode));

make_benchmark_outputs(packageDir,generatedDir);
collect_benchmark_outputs(generatedDir,outputDir);
make_variant_outputs(packageDir,generatedDir,outputDir);
make_cross_economy_outputs(packageDir,generatedDir,outputDir);

[~,~] = rmdir(generatedDir,'s');
fprintf('Replication complete. Outputs are in %s\n',outputDir)
end %end function
