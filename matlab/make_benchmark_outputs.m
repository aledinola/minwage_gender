function make_benchmark_outputs(packageDir,generatedDir)
% MAKE_BENCHMARK_OUTPUTS Reproduce benchmark tables and figure panels.
%
% INPUTS:
% packageDir:   Replication package root
% generatedDir: Temporary output directory
%
% OUTPUTS:
% None

economyDir = fullfile(packageDir,'benchmark');
par = model_parameters(economyDir,'benchmark',generatedDir);

benchmark = load_model_output(fullfile(economyDir,'output_benchmark'),par,false);
benchmark.minWage = 0;
minimumWage = load_model_output(fullfile(economyDir,'output_minWage'),par,true);
minimumWage.minWage = 8.5;
partialEquilibrium = load_model_output(fullfile(economyDir,'output_cf_min_wage_1'),par,false);
partialEquilibrium.minWage = 8.5;
highPenalty = load_model_output(fullfile(economyDir,'output_minWage_penalty'),par,false);
highPenalty.minWage = 8.5;

model_fit(benchmark,fullfile(economyDir,'input'),par);
close all
compare_bench_minwage(benchmark,minimumWage,'benchmark','minWage',par);
plot_benchmark_minimum_wage(benchmark,minimumWage,par,generatedDir);
plot_welfare_effects(fullfile(economyDir,'output_benchmark'), ...
    fullfile(economyDir,'output_minWage'),par,generatedDir);
plot_employment_change(benchmark,partialEquilibrium,par,generatedDir, ...
    'emp_dist_benchmark_cfmw1',20,20);
plot_employment_change(benchmark,highPenalty,par,generatedDir, ...
    'emp_dist_benchmark_minWage_penalty',20,20);
compare_minwage_cf(benchmark,minimumWage,partialEquilibrium,highPenalty, ...
    'Baseline','CF 1 ','CF 2',par);

models = load_minwage_series(economyDir,par,true);
compare_minwages(models,numel(models),par);
plot_employment_demographics(benchmark,par,generatedDir);
close all
end %end function
