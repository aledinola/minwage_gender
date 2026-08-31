function [G_weights] = fun_G_bins(w_grid, G_prob, bin_edges, p_a,eff_min_wage)
% G_prob: dim(n_w, 2), second dim is gender
% w_grid: dim(n_w, 1)
% p_a: scalar
% eff_min_wage: scalar, effective minimum wage
% bin_edges: dim(nbins+1,1)
% G_weights: dim(nbins,1)

nbins = length(bin_edges)-1;
G_weights = zeros(nbins,1);
w_grid_m = max(w_grid,eff_min_wage);
w_grid_f = max(p_a*w_grid,eff_min_wage);
for bb=1:nbins
    ind_m = (w_grid_m>=bin_edges(bb) & w_grid_m<bin_edges(bb+1));
    ind_f = (w_grid_f>=bin_edges(bb) & w_grid_f<bin_edges(bb+1));
    G_weights(bb) = sum(G_prob(ind_m,1),"all") + sum(G_prob(ind_f,2),"all");
end
G_weights = G_weights/sum(G_weights);
end