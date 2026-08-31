function d_emp_uncond = fun_d_emp(emp_dist_1,emp_dist_2)
%This function computes the difference in employment, unconditional on
%gender and demographic variables, between emp_dist_1 and emp_dist_2.
% This function is called by compare_bench_minwage.m.
% Inputs
% emp_dist_1,emp_dist_2 are employment distribution in model 1 and model 2.
%   - First dimension: hours 1=ME, 2=PT, 3=FT, 4=NE
%   - Second dimension: 1=male, 2=female, 3=all, 4 = difference (women-men)
%   - Third dimension:  1=all, 2=single-childless, 3=single-with
%       children, 4 = married-childless, 5=married-with children.
% Outputs:
% d_emp_uncond: dim(3,1) ME, Regular emp (PT+FT), overall employment (ME,
% PT, FT)

% Initialize output
d_emp_uncond = zeros(3,1); % ME, Regular emp (PT+FT), overall employment (ME, PT, FT)

% Employment effects
emp_dist_1_uncond = squeeze(emp_dist_1(:,3,1));
emp_dist_2_uncond = squeeze(emp_dist_2(:,3,1));

d_emp_uncond(1) = 100*(emp_dist_2_uncond(1) - emp_dist_1_uncond(1))/emp_dist_1_uncond(1);
d_emp_uncond(2) = 100*(sum(emp_dist_2_uncond(2:3)) - sum(emp_dist_1_uncond(2:3)))/sum(emp_dist_1_uncond(2:3));
d_emp_uncond(3) = 100*(sum(emp_dist_2_uncond(1:3)) - sum(emp_dist_1_uncond(1:3)))/sum(emp_dist_1_uncond(1:3));




end