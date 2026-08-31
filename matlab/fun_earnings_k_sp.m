function [earnings,wages] = fun_earnings_k_sp(g_e_sm,g_e_sf,g_u_sm,g_u_sf,w_grid,hours_grid,n_kids,n_spinc,n_hours,p_a)
%This function computes average earnings (including zeros for non-emp) by
%gender, children and spousal income types.
% Inputs
% g_e_sm,g_e_sf: Stationary distribution of men and women across jobs. Dim
% (n_wage, n_hours, n_kids, n_spinc)
% w_grid: wage grid. Dim (n_wage, n_hours)
% hours_grid: hours grid. Dim (n_hours)
% n_kids, n_spinc, n_hours: Scalars. Sizes of kids, marital status/spousal income,
% and hours grids. 
% p_a: female skill relative to male skill. Dim (n_hours).
% Outputs:
% earnings: dim(n_kids,2,n_spinc). Average earnings by kids state, gender,
% and marital/spousal income type.
% wages: dim(n_kids,2,n_spinc). Average hourly wage by kids state, gender,
% and marital/spousal income type.

earnings = zeros(n_kids,2,n_spinc);
wages = zeros(n_kids,2,n_spinc);

for k_c = 1:n_kids
    for s_c = 1:n_spinc

        % Males
        mass = g_u_sm(k_c,s_c) + sum(g_e_sm(:,:,k_c,s_c),'all'); % mass of all with k_c and s_c
        %mass = sum(g_e_sm(:,:,k_c,s_c),'all');
        mass_e =  sum(g_e_sm(:,:,k_c,s_c),'all'); % mass of the employed only
        for h_c = 1:n_hours
            earnings(k_c,1,s_c) = earnings(k_c,1,s_c) ...
                + dot(squeeze(g_e_sm(:,h_c,k_c,s_c)),squeeze(w_grid(:,h_c)))*hours_grid(h_c);
            wages(k_c,1,s_c) = wages(k_c,1,s_c) ...
                + dot(squeeze(g_e_sm(:,h_c,k_c,s_c)),squeeze(w_grid(:,h_c)));
        end
        earnings(k_c,1,s_c) = earnings(k_c,1,s_c)/mass;
    	wages(k_c,1,s_c) = wages(k_c,1,s_c)/mass_e;

        % Females
        mass = g_u_sf(k_c,s_c) + sum(g_e_sf(:,:,k_c,s_c),'all');
        %mass = sum(g_e_sf(:,:,k_c,s_c),'all');

        mass_e =  sum(g_e_sf(:,:,k_c,s_c),'all'); % mass of the employed only
        for h_c = 1:n_hours
            earnings(k_c,2,s_c) = earnings(k_c,2,s_c) ...
            + dot(squeeze(g_e_sf(:,h_c,k_c,s_c)),squeeze(w_grid(:,h_c)))*hours_grid(h_c)*p_a(h_c);
            wages(k_c,2,s_c) = wages(k_c,2,s_c) ...
                + dot(squeeze(g_e_sf(:,h_c,k_c,s_c)),squeeze(w_grid(:,h_c)));
        end
        earnings(k_c,2,s_c) = earnings(k_c,2,s_c)/mass;
        wages(k_c,2,s_c) = wages(k_c,2,s_c)/mass_e;

    end
end

end