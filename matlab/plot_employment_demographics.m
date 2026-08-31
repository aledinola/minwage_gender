function plot_employment_demographics(model,par,outputDir)
% PLOT_EMPLOYMENT_DEMOGRAPHICS Plot benchmark employment by demographic type.
%
% INPUTS:
% model:     Structure containing saved employment distributions
% par:       Structure with model dimensions and labels
% outputDir: Output-directory path
%
% OUTPUTS:
% None

[n_hours,n_kids,n_spinc,hours_names] = unpack(par, ...
    'n_hours','n_kids','n_spinc','hours_names');
emp = zeros(n_hours+1,n_kids,2,n_spinc);
for kk = 1:n_kids
    for ss = 1:n_spinc
        massM = model.g_u_sm(kk,ss) + sum(model.g_e_sm(:,:,kk,ss),'all');
        massF = model.g_u_sf(kk,ss) + sum(model.g_e_sf(:,:,kk,ss),'all');
        for hh = 1:n_hours
            emp(hh,kk,1,ss) = sum(model.g_e_sm(:,hh,kk,ss))/massM;
            emp(hh,kk,2,ss) = sum(model.g_e_sf(:,hh,kk,ss))/massF;
        end
        emp(n_hours+1,kk,1,ss) = model.g_u_sm(kk,ss)/massM;
        emp(n_hours+1,kk,2,ss) = model.g_u_sf(kk,ss)/massF;
    end
end

for gg = 1:2
    fig = figure('Units','inches','Position',[0 0 5.833 7.4]);
    tiledlayout(n_hours+1,1);
    for hh = 1:n_hours+1
        values = squeeze(emp(hh,:,gg,:));
        nexttile
        imagesc(values)
        colormap(summer)
        cb = colorbar;
        cb.FontSize = 9.6;
        if gg == 1
            clim([0,0.9])
        else
            clim([0,0.65])
        end
        [xx,yy] = meshgrid(1:n_spinc,1:n_kids);
        labels = compose('%.3f',values);
        text(xx(:),yy(:),labels(:),'HorizontalAlignment','center','FontSize',10.7)
        set(gca,'FontSize',10.7)
        yticks(1:n_kids)
        yticklabels({'No Children','Young Children','Grown Children'})
        if hh == n_hours+1
            xticks([1 2 4 6])
            xticklabels({'Single','Married, low sp. inc.','Married, mid. sp. inc.', ...
                'Married, high sp. inc.'})
        else
            xticks([])
        end
        title(hours_names{hh},'FontSize',10.7)
    end
    saveFigurePair(fig,outputDir,sprintf('empdist_g%d_benchmark',gg));
end
end %end function
