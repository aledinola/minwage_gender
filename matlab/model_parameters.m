function par = model_parameters(economyDir,economyName,generatedDir)
% MODEL_PARAMETERS Read saved dimensions and set plotting parameters.
%
% INPUTS:
% economyDir:  Root directory for one model economy
% economyName: Economy identifier
% generatedDir: Temporary output directory
%
% OUTPUTS:
% par:         Structure of dimensions and plotting parameters

benchmarkDir = fullfile(economyDir,'output_benchmark');
constants = load(fullfile(benchmarkDir,'constants.txt'));
par.n_spinc = constants(1);
par.n_prod = constants(2);
par.n_kids = constants(3);
par.n_hours = constants(4);
par.n_w = constants(5);
par.n_w_fine = constants(6);
par.p_min_wage = 8.5;
par.p_a = load(fullfile(benchmarkDir,'p_a.txt'));
par.hours_grid = loadArray(fullfile(benchmarkDir,'hours_grid.txt'),[par.n_hours,1])/52;
par.hours_names = {'ME','PT','FT','NE'};
par.rho = 0.073;
par.FigDir = generatedDir;
par.TabDir = generatedDir;
par.load_labsup = strcmp(economyName,'benchmark');
par.load_full_output = ~strcmp(economyName,'noDemoHet');

if strcmp(economyName,'fixwage')
    par.p_eps_bar_ME = 0.22;
    par.p_eps_bar_PT = 0.196846;
    par.p_delta_ME = 0.169561;
    par.p_delta_PT = 0.030044;
    par.p_delta_FT = 0.010161;
else
    par.p_eps_bar_ME = 1.238366;
    par.p_eps_bar_PT = 0.340932;
    par.p_delta_ME = 0.255524;
    par.p_delta_PT = 0.028581;
    par.p_delta_FT = 0.017679;
end
par.p_eps_bar_FT = 0;
par.p_offer_a = [1.440034;1.649604;2.128553];
par.p_offer_b = [3.812258;3.448740;3.298685];
par.p_offer_lb = [1.2,1.0,2.0];
par.p_offer_ub = [21.0,26.0,38.0];
end %end function
