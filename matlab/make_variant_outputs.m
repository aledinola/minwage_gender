function make_variant_outputs(packageDir,generatedDir,outputDir)
% MAKE_VARIANT_OUTPUTS Reproduce fixed-wage, FT-only, and no-demo panels.
%
% INPUTS:
% packageDir:   Replication package root
% generatedDir: Temporary output directory
% outputDir:    Directory for numbered paper outputs
%
% OUTPUTS:
% None

fixDir = fullfile(packageDir,'fixwage');
fixPar = model_parameters(fixDir,'fixwage',generatedDir);
fixBenchmark = load_model_output(fullfile(fixDir,'output_benchmark'),fixPar,false);
fixBenchmark.minWage = 0;
fixMinimumWage = load_model_output(fullfile(fixDir,'output_minWage'),fixPar,false);
fixMinimumWage.minWage = 8.5;
plot_fixedwage_bins(fixBenchmark,fixMinimumWage,fixPar,generatedDir);
fixSeries = load_minwage_series(fixDir,fixPar,false);
fixwage_compare_minwages(fixSeries,numel(fixSeries),fixPar);
for hh = 1:3
    copyFigurePair(generatedDir,sprintf('Gbins_h%d_benchmark_minWage',hh), ...
        outputDir,['Figure19' char('a'+hh-1)]);
end
figure20 = {'minwages_outcomes_g1','minwages_outcomes_g2', ...
    'minwages_gender','minwages_gender_decom_bar'};
for ii = 1:numel(figure20)
    copyFigurePair(generatedDir,figure20{ii},outputDir,['Figure20' char('a'+ii-1)]);
end
copyFigurePair(generatedDir,'minwages_hpol_d',outputDir,'Figure21b');
close all

ftDir = fullfile(packageDir,'FTonly');
ftPar = model_parameters(ftDir,'FTonly',generatedDir);
ftSeries = load_minwage_series(ftDir,ftPar,false);
ftonly_compare_minwages(ftSeries,numel(ftSeries),ftPar);
copyFigurePair(generatedDir,'minwages_gender',outputDir,'Figure22a');
close all

nodemoDir = fullfile(packageDir,'noDemoHet');
nodemoPar = model_parameters(nodemoDir,'noDemoHet',generatedDir);
nodemoBenchmark = load_model_output(fullfile(nodemoDir,'output_benchmark'),nodemoPar,false);
nodemoBenchmark.minWage = 0;
nodemoMinimumWage = load_model_output(fullfile(nodemoDir,'output_minWage'),nodemoPar,false);
nodemoMinimumWage.minWage = 8.5;
plot_employment_change(nodemoBenchmark,nodemoMinimumWage,nodemoPar,generatedDir, ...
    'nodemo_emp_dist_benchmark_minWage',20,20);
nodemoSeries = load_minwage_series(nodemoDir,nodemoPar,false);
nodemo_compare_minwages(nodemoSeries,numel(nodemoSeries),nodemoPar);
copyFigurePair(generatedDir,'nodemo_emp_dist_benchmark_minWage',outputDir,'Figure22b');
copyFigurePair(generatedDir,'minwages_gender',outputDir,'Figure22c');
copyFigurePair(generatedDir,'minwages_gender_decom_bar',outputDir,'Figure22d');
close all
end %end function
