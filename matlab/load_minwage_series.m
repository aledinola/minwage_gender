function models = load_minwage_series(economyDir,par,adjustInitialMW)
% LOAD_MINWAGE_SERIES Load the no-MW and 8.5--14 euro equilibria.
%
% INPUTS:
% economyDir:     Root directory for one model economy
% par:            Structure with model dimensions
% adjustInitialMW: Logical scalar for the benchmark 8.5-euro adjustment
%
% OUTPUTS:
% models:         [1,8] structure array of saved equilibria

folders = {'output_benchmark','output_minWage','output_minWage_9', ...
    'output_minWage_10','output_minWage_11','output_minWage_12', ...
    'output_minWage_13','output_minWage_14'};
minimumWages = [0,8.5,9,10,11,12,13,14];
for ii = 1:numel(folders)
    adjust = adjustInitialMW && ii == 2;
    model = load_model_output(fullfile(economyDir,folders{ii}),par,adjust);
    model.minWage = minimumWages(ii);
    models(ii) = model; %#ok<AGROW>
end
end %end function
