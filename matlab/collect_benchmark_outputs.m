function collect_benchmark_outputs(generatedDir,outputDir)
% COLLECT_BENCHMARK_OUTPUTS Assign paper numbers to benchmark outputs.
%
% INPUTS:
% generatedDir: Directory containing descriptive generated names
% outputDir:    Directory for numbered paper outputs
%
% OUTPUTS:
% None

copyFigurePair(generatedDir,'spinc_dist',outputDir,'Figure2');

for gg = 1:2
    for hh = 1:3
        panel = char('a' + (gg-1)*3 + hh-1);
        copyFigurePair(generatedDir,sprintf('modelfit_CDF_hwage_g%d_h%d',gg,hh), ...
            outputDir,['Figure3' panel]);
        copyFigurePair(generatedDir,sprintf('CDF_hwage_a_g%d_h%d',gg,hh), ...
            outputDir,['Figure14' panel]);
    end
end

copyFigurePair(generatedDir,'modelfit_offerdist_supply',outputDir,'Figure4a');
copyFigurePair(generatedDir,'modelfit_offerdist_demand',outputDir,'Figure4b');
for hh = 1:3
    panel = char('a' + hh-1);
    copyFigurePair(generatedDir,sprintf('Fprob_cdf_h%d_benchmark_minWage',hh), ...
        outputDir,['Figure5' panel]);
    copyFigurePair(generatedDir,sprintf('Gpdf_h%d_benchmark_minWage',hh), ...
        outputDir,['Figure15' panel]);
end

figure6 = {'Delta_act_benchmark_minWage','Delta_cond_h1_benchmark_minWage', ...
    'Delta_cond_h2_benchmark_minWage','Delta_cond_h3_benchmark_minWage'};
for ii = 1:numel(figure6)
    copyFigurePair(generatedDir,figure6{ii},outputDir,['Figure6' char('a'+ii-1)]);
end

figure7 = {'emp_dist_benchmark_minWage','emp_dist_benchmark_cfmw1', ...
    'emp_dist_benchmark_minWage_penalty'};
for ii = 1:numel(figure7)
    copyFigurePair(generatedDir,figure7{ii},outputDir,['Figure7' char('a'+ii-1)]);
end

for gg = 1:2
    for kk = 1:3
        panel = char('a' + (gg-1)*3 + kk-1);
        copyFigurePair(generatedDir,sprintf('bar_empdist_diff_g%d_k%d_benchmark_minWage',gg,kk), ...
            outputDir,['Figure8' panel]);
        copyFigurePair(generatedDir,sprintf('bar_aveprod_diff_g%d_k%d_benchmark_minWage',gg,kk), ...
            outputDir,['Figure10' panel]);
    end
end

copyFigurePair(generatedDir,'cev_benchmark_minWage',outputDir,'Figure9');

figure11 = {'minwages_outcomes_g1','minwages_outcomes_g2', ...
    'minwages_gender','minwages_gender_decom_bar'};
for ii = 1:numel(figure11)
    copyFigurePair(generatedDir,figure11{ii},outputDir,['Figure11' char('a'+ii-1)]);
end

copyFigurePair(generatedDir,'minwages_hpol_d',outputDir,'Figure21a');
copyFigurePair(generatedDir,'empdist_g1_benchmark',outputDir,'Figure17a');
copyFigurePair(generatedDir,'empdist_g2_benchmark',outputDir,'Figure17b');

copyfile(fullfile(generatedDir,'d_emp_uncond_benchmark_minWage.tex'), ...
    fullfile(outputDir,'Table6.tex'));
copyfile(fullfile(generatedDir,'compare_minwage_cf.tex'), ...
    fullfile(outputDir,'Table7.tex'));
end %end function
