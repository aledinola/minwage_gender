function [cdf_out,w_out] = find_interp_Fprob(grida,gridb,Fa,Fb)
% find_interp_Fprob looks for the first intersection of the two CDFs
% defined on grida and gridb.


fun_Ia = @(w) interp1(grida,Fa,w,'linear','extrap');
fun_Ib = @(w) interp1(gridb,Fb,w,'linear','extrap');

% Find the smallest w such that Ib(w)>=Ia(w)

nnw = 10*length(grida);
min_w = min(grida(1),gridb(1));
max_w = max(grida(end),gridb(end));
grid_fine = linspace(min_w,max_w,nnw)';

for iw = 1:nnw
    if fun_Ib(grid_fine(iw))>=fun_Ia(grid_fine(iw))
        break
    end
end %end for
% we have found the index iw

w_out   = grid_fine(iw);
cdf_out = fun_Ia(grid_fine(iw));


end %end function