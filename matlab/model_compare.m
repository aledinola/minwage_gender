function [] = model_compare(struct_base,struct_fixwage,struct_ftonly,struct_nodemog,par)
% model_compare compares hourly wage CDFs across benchmark and alternative models

[n_w,n_hours,hours_names] = unpack(par,'n_w','n_hours','hours_names');
FigDir = par.FigDir;
myfs = 14;
mylw = 3;

models(1).label = 'Benchmark';
models(1).struct = struct_base;
models(1).line = '-';

models(2).label = 'Fixed Wage';
models(2).struct = struct_fixwage;
models(2).line = '--';

models(3).label = 'No Demog. Het.';
models(3).struct = struct_nodemog;
models(3).line = '-.';

models(4).label = 'FT Only';
models(4).struct = struct_ftonly;
models(4).line = ':';

for i_model = 1:numel(models)
    models(i_model).cdf_hwage = get_hwage_cdf(models(i_model).struct,n_w,n_hours);
end

for g_c = 1:2
    for h_c = 1:n_hours
        figure
        hold on
        legend_entries = {};

        for i_model = 1:numel(models)
            cdf_series = squeeze(models(i_model).cdf_hwage(g_c,h_c,:));
            mass = sum(cdf_series,'omitnan');
            if mass <= 0 || any(~isfinite(cdf_series))
                continue
            end

            wage_grid = models(i_model).struct.w_grid(:,h_c);
            if g_c == 2
                wage_grid = wage_grid * models(i_model).struct.p_a(h_c);
            end

            plot(wage_grid,cdf_series,models(i_model).line,'linewidth',mylw)
            legend_entries{end+1} = models(i_model).label; %#ok<AGROW>
        end

        hold off
        xlim([0,40])
        ylim([0,101])
        ax = gca;
        grid on
        ax.FontSize = myfs;
        fig_name = sprintf('%s, %s', ternary(g_c==1,'Men','Women'), hours_names{h_c});
        title(fig_name,'fontsize',myfs)
        xlabel('Hourly wage','fontsize',myfs)
        ylabel('Percentile','fontsize',myfs)
        if ~isempty(legend_entries)
            legend(legend_entries,'location','best','fontsize',myfs)
        end
        fig_name = sprintf('base_altmodels_CDF_hwage_g%d_h%d',g_c,h_c);
        print(fullfile(FigDir,fig_name),'-dpng','-r300')
        print(fullfile(FigDir,fig_name),'-depsc')
    end
end

end %end function "model_compare"

function cdf_hwage = get_hwage_cdf(struct_in,n_w,n_hours)

cdf_hwage = nan(2,n_hours,n_w);

for g_c = 1:2
    if g_c == 1
        g_e = struct_in.g_e_sm;
    else
        g_e = struct_in.g_e_sf;
    end

    for h_c = 1:n_hours
        mass_by_wage = squeeze(sum(g_e(:,h_c,:,:),[3,4]));
        total_mass = sum(mass_by_wage,'omitnan');
        if total_mass > 0
            cdf_hwage(g_c,h_c,:) = 100*cumsum(mass_by_wage/total_mass);
        end
    end
end

end

function out = ternary(cond,val_true,val_false)
if cond
    out = val_true;
else
    out = val_false;
end
end
