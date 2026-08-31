function [emp_dist,avg_h,tot_inc,emprate,hwage] = fun_compute_inc_emp_hours(struct,par)
% Compute employment distribution, average hours, total income, employment
% rates, and hourly wages by gender and demographic groups

[n_w,n_hours,n_kids,n_spinc,p_a,p_min_wage,hours_names,hours_grid] = unpack(par,'n_w',...
    'n_hours','n_kids','n_spinc','p_a','p_min_wage','hours_names','hours_grid');


%  g_e_sm(w,h,kids,spinc)
%  g_u_sm(kids,spinc)

%% mass: mass of households
% First dimension: 1=male, 2=female, 3=all
% Third dimension:  1=all, 2=single-childless, 3=single-with
% children, 4 = married-childless, 5=married-with children.

mass = zeros(3,5);
% all demographic groups
mass(1,1) = sum(struct.g_e_sm,'all')+sum(struct.g_u_sm,'all');
mass(2,1) = sum(struct.g_e_sf,'all')+sum(struct.g_u_sf,'all');
mass(3,1) = mass(1,1) + mass(2,1);

% single childless
mass(1,2) = sum(struct.g_e_sm(:,:,1,1),'all')+sum(struct.g_u_sm(1,1),'all');
mass(2,2) = sum(struct.g_e_sf(:,:,1,1),'all')+sum(struct.g_u_sf(1,1),'all');
mass(3,2) = mass(1,2) + mass(2,2);

% single with children
mass(1,3) = sum(struct.g_e_sm(:,:,2:3,1),'all')+sum(struct.g_u_sm(2:3,1),'all');
mass(2,3) = sum(struct.g_e_sf(:,:,2:3,1),'all')+sum(struct.g_u_sf(2:3,1),'all');
mass(3,3) = mass(1,3) + mass(2,3);

% Married childless
mass(1,4) = sum(struct.g_e_sm(:,:,1,2:n_spinc),'all')+sum(struct.g_u_sm(1,2:n_spinc),'all');
mass(2,4) = sum(struct.g_e_sf(:,:,1,2:n_spinc),'all')+sum(struct.g_u_sf(1,2:n_spinc),'all');
mass(3,4) = mass(1,4) + mass(2,4);

% Married with children
mass(1,5) = sum(struct.g_e_sm(:,:,2:3,2:n_spinc),'all')+sum(struct.g_u_sm(2:3,2:n_spinc),'all');
mass(2,5) = sum(struct.g_e_sf(:,:,2:3,2:n_spinc),'all')+sum(struct.g_u_sf(2:3,2:n_spinc),'all');
mass(3,5) = mass(1,5) + mass(2,5);

%% emp_dist: employment distribution
% First dimension: hours 1=ME, 2=PT, 3=FT, 4=NE
% Second dimension: 1=male, 2=female, 3=all, 4 = difference (women-men)
% Third dimension:  1=all, 2=single-childless, 3=single-with
% children, 4 = married-childless, 5=married-with children.

emp_dist = zeros(n_hours+1,4,5); 
for h_c = 1:n_hours
% All demographic groups
    % Males
    emp_dist(h_c,1,1) = sum(struct.g_e_sm(:,h_c,:,:),'all')/mass(1,1);
    % Females
    emp_dist(h_c,2,1) = sum(struct.g_e_sf(:,h_c,:,:),'all')/mass(2,1);
    % All
    emp_dist(h_c,3,1) = sum(struct.g_e_sm(:,h_c,:,:)+struct.g_e_sf(:,h_c,:,:),'all')/mass(3,1);
% Single childless
    % Males
    emp_dist(h_c,1,2) = sum(struct.g_e_sm(:,h_c,1,1),'all')/mass(1,2);
    % Females
    emp_dist(h_c,2,2) = sum(struct.g_e_sf(:,h_c,1,1),'all')/mass(2,2);
    % All
    emp_dist(h_c,3,2) = sum(struct.g_e_sm(:,h_c,1,1)+struct.g_e_sf(:,h_c,1,1),'all')/mass(3,2);
% Single with children
    % Males
    emp_dist(h_c,1,3) = sum(struct.g_e_sm(:,h_c,2:3,1),'all')/mass(1,3);
    % Females
    emp_dist(h_c,2,3) = sum(struct.g_e_sf(:,h_c,2:3,1),'all')/mass(2,3);
    % All
    emp_dist(h_c,3,3) = sum(struct.g_e_sm(:,h_c,2:3,1)+struct.g_e_sf(:,h_c,2:3,1),'all')/mass(3,3);
% Married childless
    % Males
    emp_dist(h_c,1,4) = sum(struct.g_e_sm(:,h_c,1,2:n_spinc),'all')/mass(1,4);
    % Females
    emp_dist(h_c,2,4) = sum(struct.g_e_sf(:,h_c,1,2:n_spinc),'all')/mass(2,4);
    % All
    emp_dist(h_c,3,4) = sum(struct.g_e_sm(:,h_c,1,2:n_spinc)+struct.g_e_sf(:,h_c,1,2:n_spinc),'all')/mass(3,4);
% Married with children
    % Males
    emp_dist(h_c,1,5) = sum(struct.g_e_sm(:,h_c,2:3,2:n_spinc),'all')/mass(1,5);
    % Females
    emp_dist(h_c,2,5) = sum(struct.g_e_sf(:,h_c,2:3,2:n_spinc),'all')/mass(2,5);
    % All
    emp_dist(h_c,3,5) = sum(struct.g_e_sm(:,h_c,2:3,2:n_spinc)+struct.g_e_sf(:,h_c,2:3,2:n_spinc),'all')/mass(3,5);
end
% Non-Employed
h_c = n_hours+1;
for demog_c = 1:5
    for g_c = 1:3
        emp_dist(h_c,g_c,demog_c) = 1 - sum(emp_dist(1:n_hours,g_c,demog_c));
    end
end
% Gender gap(Women vs men)
emp_dist(:,4,:) = emp_dist(:,2,:) - emp_dist(:,1,:);


%% Average hours conditional on employment
% Dimension 1: 1=male, 2=female, 3=all, 4 = gender gap (women-men)
% Dimension 2: 1=all, 2=single-childless, 3=single-with children,
%              4 = married-childless, 5=married-with children.
avg_h = zeros(4,5);
for g_c=1:3
    for demog_c = 1:5
        avg_h(g_c,demog_c) = sum(emp_dist(1:n_hours,g_c,demog_c).*hours_grid(1:n_hours))/sum(emp_dist(1:n_hours,g_c,demog_c));
    end
end
avg_h(4,:) = avg_h(2,:)-avg_h(1,:);

%% Total income
% Dimension 1: 1=male, 2=female, 3=all
% Dimension 2: 1=all, 2=single-childless, 3=single-with children,
%              4 = married-childless, 5=married-with children.
tot_inc = zeros(3,5);
% All demographic types
% Men
tot_inc(1,1) = sum(struct.w_grid.*hours_grid'.*struct.g_e_sm,'all')/...
    (sum(struct.g_e_sm,'all')+sum(struct.g_u_sm,'all'));
% Women
tot_inc(2,1) = sum(p_a'.*struct.w_grid.*hours_grid'.*struct.g_e_sf,'all')/...
    (sum(struct.g_e_sf,'all')+sum(struct.g_u_sf,'all'));
% All
num = sum(struct.w_grid.*hours_grid'.*struct.g_e_sm,'all')+sum(p_a'.*struct.w_grid.*hours_grid'.*struct.g_e_sf,'all');
den = sum(struct.g_e_sm,'all')+sum(struct.g_u_sm,'all')+sum(struct.g_e_sf,'all')+sum(struct.g_u_sf,'all');
tot_inc(3,1) = num/den;

% Single childless
% Men
tot_inc(1,2) = sum(struct.w_grid.*hours_grid'.*struct.g_e_sm(:,:,1,1),'all')/...
    (sum(struct.g_e_sm(:,:,1,1),'all')+sum(struct.g_u_sm(1,1),'all'));
% Women
tot_inc(2,2) = sum(p_a'.*struct.w_grid.*hours_grid'.*struct.g_e_sf(:,:,1,1),'all')/...
    (sum(struct.g_e_sf(:,:,1,1),'all')+sum(struct.g_u_sf(1,1),'all'));
% All
num = sum(struct.w_grid.*hours_grid'.*struct.g_e_sm(:,:,1,1),'all')+sum(p_a'.*struct.w_grid.*hours_grid'.*struct.g_e_sf(:,:,1,1),'all');
den = sum(struct.g_e_sm(:,:,1,1),'all')+sum(struct.g_u_sm(1,1),'all')+sum(struct.g_e_sf(:,:,1,1),'all')+sum(struct.g_u_sf(1,1),'all');
tot_inc(3,2) = num/den;

% Single with child
% Men
tot_inc(1,3) = sum(struct.w_grid.*hours_grid'.*struct.g_e_sm(:,:,2:3,1),'all')/...
    (sum(struct.g_e_sm(:,:,2:3,1),'all')+sum(struct.g_u_sm(2:3,1),'all'));
% Women
tot_inc(2,3) = sum(p_a'.*struct.w_grid.*hours_grid'.*struct.g_e_sf(:,:,2:3,1),'all')/...
    (sum(struct.g_e_sf(:,:,2:3,1),'all')+sum(struct.g_u_sf(2:3,1),'all'));
% All
num = sum(struct.w_grid.*hours_grid'.*struct.g_e_sm(:,:,2:3,1),'all')+sum(p_a'.*struct.w_grid.*hours_grid'.*struct.g_e_sf(:,:,2:3,1),'all');
den = sum(struct.g_e_sm(:,:,2:3,1),'all')+sum(struct.g_u_sm(2:3,1),'all')+sum(struct.g_e_sf(:,:,2:3,1),'all')+sum(struct.g_u_sf(2:3,1),'all');
tot_inc(3,3) = num/den;

% Married childless
% Men
tot_inc(1,4) = sum(struct.w_grid.*hours_grid'.*struct.g_e_sm(:,:,1,2:n_spinc),'all')/...
    (sum(struct.g_e_sm(:,:,1,2:n_spinc),'all')+sum(struct.g_u_sm(1,2:n_spinc),'all'));
% Women
tot_inc(2,4) = sum(p_a'.*struct.w_grid.*hours_grid'.*struct.g_e_sf(:,:,1,2:n_spinc),'all')/...
    (sum(struct.g_e_sf(:,:,1,2:n_spinc),'all')+sum(struct.g_u_sf(1,2:n_spinc),'all'));
% All
num = sum(struct.w_grid.*hours_grid'.*struct.g_e_sm(:,:,1,2:n_spinc),'all')+sum(p_a'.*struct.w_grid.*hours_grid'.*struct.g_e_sf(:,:,1,2:n_spinc),'all');
den = sum(struct.g_e_sm(:,:,1,2:n_spinc),'all')+sum(struct.g_u_sm(1,2:n_spinc),'all')+sum(struct.g_e_sf(:,:,1,2:n_spinc),'all')+sum(struct.g_u_sf(1,2:n_spinc),'all');
tot_inc(3,4) = num/den;

% Married with child
% Men
tot_inc(1,5) = sum(struct.w_grid.*hours_grid'.*struct.g_e_sm(:,:,2:3,2:n_spinc),'all')/...
    (sum(struct.g_e_sm(:,:,2:3,2:n_spinc),'all')+sum(struct.g_u_sm(2:3,2:n_spinc),'all'));
% Women
tot_inc(2,5) = sum(p_a'.*struct.w_grid.*hours_grid'.*struct.g_e_sf(:,:,2:3,2:n_spinc),'all')/...
    (sum(struct.g_e_sf(:,:,2:3,2:n_spinc),'all')+sum(struct.g_u_sf(2:3,2:n_spinc),'all'));
% All
num = sum(struct.w_grid.*hours_grid'.*struct.g_e_sm(:,:,2:3,2:n_spinc),'all')+sum(p_a'.*struct.w_grid.*hours_grid'.*struct.g_e_sf(:,:,2:3,2:n_spinc),'all');
den = sum(struct.g_e_sm(:,:,2:3,2:n_spinc),'all')+sum(struct.g_u_sm(2:3,2:n_spinc),'all')+sum(struct.g_e_sf(:,:,2:3,2:n_spinc),'all')+sum(struct.g_u_sf(2:3,2:n_spinc),'all');
tot_inc(3,5) = num/den;

%% emprate: employment rate
% Dimension 1: 1=male, 2=female, 3=all
% Dimension 2: 1=all, 2=single-childless, 3=single-with children,
%              4 = married-childless, 5=married-with children.
emprate = squeeze([(1-emp_dist(n_hours+1,1,:)),(1-emp_dist(n_hours+1,2,:)),(1-emp_dist(n_hours+1,3,:)),]); % male and female

%% hwage: average hourly wage (conditional on employment)
% Dimension 1: 1=male, 2=female,3=all
% Dimension 2: 1=all, 2=single-childless, 3=single-with children,
%              4 = married-childless, 5=married-with children.
hwage = [tot_inc(1,:)./(emprate(1,:).*avg_h(1,:)); ...
        tot_inc(2,:)./(emprate(2,:).*avg_h(2,:)); ...
        tot_inc(3,:)./(emprate(3,:).*avg_h(3,:))];

end % function  fun_compute_inc_emp_hours