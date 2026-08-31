function make_cross_economy_outputs(packageDir,generatedDir,outputDir)
% MAKE_CROSS_ECONOMY_OUTPUTS Reproduce Figure 18 across model variants.
%
% INPUTS:
% packageDir:   Replication package root
% generatedDir: Temporary output directory
% outputDir:    Directory for numbered paper outputs
%
% OUTPUTS:
% None

names = {'benchmark','fixwage','FTonly','noDemoHet'};
for ii = 1:numel(names)
    economyDir = fullfile(packageDir,names{ii});
    parNow = model_parameters(economyDir,names{ii},generatedDir);
    models{ii} = load_model_output(fullfile(economyDir,'output_benchmark'),parNow,false); %#ok<AGROW>
    models{ii}.p_a = parNow.p_a;
end
par = model_parameters(fullfile(packageDir,'benchmark'),'benchmark',generatedDir);
model_compare(models{1},models{2},models{3},models{4},par);
for gg = 1:2
    for hh = 1:3
        panel = char('a' + (gg-1)*3 + hh-1);
        copyFigurePair(generatedDir,sprintf('base_altmodels_CDF_hwage_g%d_h%d',gg,hh), ...
            outputDir,['Figure18' panel]);
    end
end
close all
end %end function
