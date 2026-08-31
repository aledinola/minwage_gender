function model = load_model_output(outputDir,par,adjustFemaleFT)
% LOAD_MODEL_OUTPUT Load saved equilibrium arrays from one model output.
%
% INPUTS:
% outputDir:     Directory containing saved TXT arrays
% par:           Structure with model dimensions
% adjustFemaleFT: Logical scalar for the paper's benchmark MW adjustment
%
% OUTPUTS:
% model:         Structure of saved equilibrium arrays

[n_w,n_hours,n_kids,n_spinc] = unpack(par,'n_w','n_hours','n_kids','n_spinc');

model.w_grid = loadArray(fullfile(outputDir,'w_grid_eq.txt'),[n_w,n_hours]);
model.g_e_sm = loadArray(fullfile(outputDir,'g_e_sm.txt'),[n_w,n_hours,n_kids,n_spinc]);
model.g_e_sf = loadArray(fullfile(outputDir,'g_e_sf.txt'),[n_w,n_hours,n_kids,n_spinc]);
model.g_u_sm = loadArray(fullfile(outputDir,'g_u_sm.txt'),[n_kids,n_spinc]);
model.g_u_sf = loadArray(fullfile(outputDir,'g_u_sf.txt'),[n_kids,n_spinc]);

if par.load_full_output
    model.F_prob = loadArray(fullfile(outputDir,'F_prob_eq.txt'),[n_w,n_hours]);
    model.hpol = loadArray(fullfile(outputDir,'hpol_eq.txt'),[n_hours,1]);
    model.Delta = loadArray(fullfile(outputDir,'Delta_new.txt'),[n_w,n_hours]);
    model.prod_grid = loadArray(fullfile(outputDir,'prod_grid.txt'),[n_w,1]);
    model.prod_PMF = loadArray(fullfile(outputDir,'prod_PMF.txt'),[n_w,1]);

    model.labsup = [];
    if par.load_labsup && isfile(fullfile(outputDir,'labsup_eq.txt'))
        model.labsup = loadArray(fullfile(outputDir,'labsup_eq.txt'),[n_w,n_hours]);
    end
    model.f_effective = [];
    if isfile(fullfile(outputDir,'f_effective.txt'))
        model.f_effective = loadArray(fullfile(outputDir,'f_effective.txt'),[n_hours,2]);
    end
    model.trans_emp_j = [];
    if isfile(fullfile(outputDir,'trans_emp_j.txt'))
        model.trans_emp_j = loadArray(fullfile(outputDir,'trans_emp_j.txt'),[2,2,2]);
    end
end

if adjustFemaleFT
    oldEmployed = sum(model.g_e_sf,'all');
    model.g_e_sf(:,3,:,:) = 0.99*model.g_e_sf(:,3,:,:);
    newUnemployed = sum(model.g_u_sf,'all') + oldEmployed - sum(model.g_e_sf,'all');
    model.g_u_sf = model.g_u_sf/sum(model.g_u_sf,'all')*newUnemployed;
end
end %end function
