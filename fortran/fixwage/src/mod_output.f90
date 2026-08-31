module mod_output
  
    !=======================================================================
    !        Module output - 
    ! Here we write outputs
    !=======================================================================
	!=======================================================================
    
use mod_globals
use mod_utilities
use mod_numerical,only: myerror

implicit none

contains

!===============================================================================!
    
    subroutine tex_export_params(fname_int_params,fname_ext_params)
    ! Purpose: WRITES PARAMETERS to tex table
	implicit none
    !Declare inputs:
    character(LEN=*), intent(in)    :: fname_int_params,fname_ext_params
    !Declare locals:
    integer :: uno,ierr
    
    ! Write internal (estimated) parameters to latex file
	open(newunit=uno, file=fname_int_params, status='replace',iostat=ierr)
	if (ierr/=0) then
		write(*,*) "ierr = ",ierr
		call myerror("Error in tex_export_params: cannot open file fname_int_params")
	endif

	write(uno, '(a)')' \begin{tabular}{llc} \hline \hline'
	write(uno,'(a)') '  Parameter  &     &  value \\ '
	write(uno, '(a)')'  \hline  '
	write(uno,'(a)') '  \textit{Supply side:}  &     &   \\ '
    write(uno, '(a,a,f12.3,a)') 'Offer distrib. parameter, $\alpha_{F_{h}}$ &','ME &',p_offer_a(1),' \\'
    write(uno, '(a,a,f12.3,a)') '							 &','PT &',p_offer_a(2) ,' \\'
    write(uno, '(a,a,f12.3,a)') '							 &','FT &',p_offer_a(3) ,' \\'
    write(uno, '(a,a,f12.3,a)') 'Offer distrib. parameter, $\beta_{F_{h}}$ &','ME &',p_offer_b(1) ,' \\'
    write(uno, '(a,a,f12.3,a)') '							 &','PT &',p_offer_b(2) ,' \\'
    write(uno, '(a,a,f12.3,a)') '							 &','FT &',p_offer_b(3) ,' \\'
    write(uno, '(a,a,f12.3,a)') 'Offer distrib. parameter, $\Delta^h$	 &','ME &',p_hpol_arr(1)/sum(p_hpol_arr),' \\' 
    write(uno, '(a,a,f12.3,a)') '							 &','PT &',p_hpol_arr(2)/sum(p_hpol_arr) ,' \\'
    write(uno, '(a,a,f12.3,a)') '							 &','FT &',p_hpol_arr(3)/sum(p_hpol_arr) ,' \\'
    write(uno, '(a,a,f12.3,a)') 'Contact rate, $\lambda$  &','&',p_lambda,' \\'
    write(uno, '(a,a,f12.3,a)') 'Exog. separation rate, $\delta(h)$ &','ME &',p_delta(1),' \\'
    write(uno, '(a,a,f12.3,a)') '						 &','PT &',p_delta(2),' \\'
    write(uno, '(a,a,f12.3,a)') '						 &','FT &',p_delta(3),' \\'
    write(uno, '(a,a,f12.3,a)') 'Preference parameters,$\psi_k^j$ &','male, young children &',p_psi_m(2),' \\'
    write(uno, '(a,a,f12.3,a)') '									 &','male, grown children &',p_psi_m(3),' \\'
    write(uno, '(a,a,f12.3,a)') '									 &','female, no children &',p_psi_f(1),' \\'
    write(uno, '(a,a,f12.3,a)') '									 &','female, young children &',p_psi_f(2),' \\'
    write(uno, '(a,a,f12.3,a)') '									 &','female, grown children &',p_psi_f(3),' \\'
 	write(uno, '(a,a,f12.3,a)') 'Preference parameters,$\gamma_k^j$ &','male, young children &',p_gamma_m(2),' \\'
    write(uno, '(a,a,f12.3,a)') '									 &','male, grown children &',p_gamma_m(3),' \\'
    write(uno, '(a,a,f12.3,a)') '									 &','female, no children &',p_gamma_f(1),' \\'
    write(uno, '(a,a,f12.3,a)') '									 &','female, young children &',p_gamma_f(2),' \\'
    write(uno, '(a,a,f12.3,a)') '									 &','female, grown children &',p_gamma_f(3),' \\'
 	write(uno, '(a,a,f12.3,a)') 'Distrib. of disutility $\mu_\alpha(j,k,FT)$ &','female, young children &',p_mu_alpha(2,2),' \\'
 	write(uno, '(a,a,f12.3,a)') '								 &','female, grown children &',p_mu_alpha(3,2),' \\'
 	write(uno, '(a,a,f12.3,a)') 'Labor efficiency, $a_j(h)$ &','female, ME &',p_a(1),' \\'
 	write(uno, '(a,a,f12.3,a)') '						   &','female, PT &',p_a(2),' \\'
 	write(uno, '(a,a,f12.3,a)') '						   &','female, FT &',p_a(3),' \\'
	write(uno,'(a)') '   '
	write(uno,'(a)') '  \textit{Demand side:}  &     &   \\ '
 	write(uno, '(a,a,f12.3,a)') 'Production function param., $\theta_h$  &','ME &',p_theta(1),' \\'
	write(uno, '(a,a,f12.3,a)') '										 &','PT &',p_theta(2),' \\'
 	write(uno, '(a,a,f12.1,a)') 'Job-posting cost  (monthly), $\bar{\epsilon}_h  $ &','ME &',p_eps_bar(1)*52d0*365d0/12d0,' \\'
	write(uno, '(a,a,f12.1,a)') '										 &','PT &',p_eps_bar(2)*52d0*365d0/12d0,' \\'
 	write(uno, '(a,a,f12.1,a)') 'Job-posting cost (monthly), $\sigma_\varepsilon  $   &',' &',p_sigma_eps*52d0*365d0/12d0 ,' \\'
	write(uno,'(a)') '  \textit{Minimum wage implementation:}  &     &   \\ '
 	write(uno, '(a,a,f12.3,a)') 'Non-compliance penalty, $\kappa_0$  &',' &',p_kappa0(1),' \\'
    write(uno,'(a)') '\hline\hline '
    write(uno,'(a)') '\end{tabular} '

	close(uno)
	
	 ! Write external (fixed) parameters to latex file
	
	open(newunit=uno, file=fname_ext_params, status='replace',iostat=ierr)
	if (ierr/=0) then
		write(*,*) "ierr = ",ierr
		call myerror("Error in tex_export_params: cannot open file fname_ext_params")
	endif

	write(uno, '(a)')' \begin{tabular}{lc} \hline \hline'
	write(uno,'(a)') '  Parameter  &     Value  \\ '
	write(uno, '(a)')'  \hline  '
	
	write(uno,'(a,f12.3,a)') 'Discount rate, $\rho$ & ',p_rho,' \\'
	write(uno,'(a,f12.3,a)') '``Death'' rate, $\rho_d$ & ',p_rho_d,' \\'
	write(uno,'(a)') ' & \\  '
	write(uno,'(a)') '\multicolumn{2}{l}{\textit{Child transition rate, $\phi_{j,x}(k)$:}} \\'
	write(uno,'(a,f12.3,a)') 'No children to young children, single men & ',p_phi(1,1,1),' \\'
	write(uno,'(a,f12.3,a)') 'No children to young children, single women & ',p_phi(1,2,1),'  \\'
	write(uno,'(a,f12.3,a)') 'No children to young children, married & ',p_phi(1,1,2),'  \\'
	write(uno,'(a,f12.3,a)') 'Young children to grown children, single men & ',p_phi(2,1,1),'   \\'
	write(uno,'(a,f12.3,a)') 'Young children to grown children, single men & ',p_phi(2,2,1),' \\'
	write(uno,'(a,f12.3,a)') 'Young children to grown children, married & ',p_phi(2,1,2),' \\'
	write(uno,'(a)') ' & \\  '
    write(uno,'(a,f12.3,a)') '\textit{Share single} & ', spinc_dist(1,1),'   \\'
    write(uno,'(a)') ' & \\  '
	write(uno,'(a)') '\multicolumn{2}{l}{\textit{Hours grid, $\mathcal{H}$:}} \\'
	write(uno,'(a,i,a)') 'Weekly hours of ME & ',int(fun_hours_wk(hours_grid(1))),'  \\'
	write(uno,'(a,i,a)') 'Weekly hours of PT & ',int(fun_hours_wk(hours_grid(2))),'   \\'
	write(uno,'(a,i,a)') 'Weekly hours of FT & ',int(fun_hours_wk(hours_grid(3))),'   \\'
	write(uno,'(a)') ' & \\  '
	write(uno,'(a)') '\multicolumn{2}{l}{\textit{Spousal income grid (monthly), $\mathcal{X}$:}}  \\'
	write(uno,'(a,f12.1,a)') '$x=2$ (married, lowest spousal income) & ',spinc_vec(2)/12.0d0,'  \\'
	write(uno,'(a,f12.1,a)') '$x=3$ & ',spinc_vec(3)/12.0d0,' \\'
	write(uno,'(a,f12.1,a)') '$x=4$ & ',spinc_vec(4)/12.0d0,' \\'
	write(uno,'(a,f12.1,a)') '$x=5$ & ',spinc_vec(5)/12.0d0,'  \\'
	write(uno,'(a,f12.1,a)') '$x=6$ (married, highest spousal income) & ',spinc_vec(6)/12.0d0,' \\'
	write(uno,'(a)') ' & \\  '
	write(uno,'(a,f12.3,a)') '\textit{Social security tax rate, $\tau_{s}$} & ', p_tax_ss,'   \\'
	write(uno,'(a)') ' & \\  '
	write(uno,'(a)') '\multicolumn{2}{l}{\textit{Tax function parameters, $\tau_0,\tau_1$ :}}  \\'
	write(uno,'(a,f12.1,a,f12.2,a)') 'Single, no children  & ',p_tax0_single(1),', ',p_tax1_single(1),'   \\'
	write(uno,'(a,f12.1,a,f12.2,a)') 'Single, young children  & ',p_tax0_single(2),', ',p_tax1_single(2),'  \\'
	write(uno,'(a,f12.1,a,f12.2,a)') 'Single, grown children  & ',p_tax0_single(3),', ',p_tax1_single(3),'  \\'
	write(uno,'(a,f12.1,a,f12.2,a)') 'Married, no children  & ',p_tax0_couple(1),', ',p_tax1_couple(1),'   \\'
	write(uno,'(a,f12.1,a,f12.2,a)') 'Married, young children  & ',p_tax0_couple(2),', ',p_tax1_couple(2),' \\'
	write(uno,'(a,f12.1,a,f12.2,a)') 'Married, grown children  & ',p_tax0_couple(3),', ',p_tax1_couple(3),'   \\'
	write(uno,'(a)') ' & \\  '
	write(uno,'(a)') '\multicolumn{2}{l}{\textit{Non-employment benefit (monthly), $b_{j,x}$ :}}  \\'
	write(uno,'(a,f12.1,a)') 'Men, single  & ',p_b_sm(1)/12.0d0,'   \\'
	write(uno,'(a,f12.1,a)') 'Women, single  & ',p_b_sf(1)/12.0d0,'   \\'
	write(uno,'(a,f12.1,a)') 'Men, married  & ',p_b_mm(1)/12.0d0,'   \\'
	write(uno,'(a,f12.1,a)') 'Women, married  & ',p_b_mf(1)/12.0d0,'   \\'
	write(uno,'(a)') ' & \\  '
	write(uno,'(a)') '\multicolumn{2}{l}{\textit{Parental benefit (monthly), $b^k_{j}$ :}}  \\'
	write(uno,'(a,f12.1,a)') 'Men  & ',(p_b_sm(2)-p_b_sm(1))/12.0d0,'   \\'
	write(uno,'(a,f12.1,a)') 'Women  & ',(p_b_sf(2)-p_b_sf(1))/12.0d0,'   \\'
		
	
	write(uno,'(a)') '\hline\hline '
    write(uno,'(a)') '\end{tabular} '
	close(uno)
    end subroutine tex_export_params
!===============================================================================!
    
    subroutine tex_export_fit(fname,mom_model)
    ! Purpose: WRITES model fit to tex table
	implicit none
    !Declare inputs:
    character(LEN=*), intent(in)    :: fname
    type(mom),intent(in) :: mom_model
    !Declare locals:
    integer:: uno,ierr,k_c,q_c,h_c
    character(len = 40) :: myformat,myformat_short

    ! Write model fit to latex file
	open(newunit=uno, file=fname, status='replace',iostat=ierr)
	if (ierr/=0) then
		write(*,*) "ierr = ",ierr
		call myerror("Error in tex_export_fit: cannot open file fname")
	endif
	
	myformat = '(a,f12.3,a,f12.3,a,f12.3,a,f12.3,a)'
	myformat_short = '(a,f12.3,a,f12.3,a)'

    
	write(uno, '(a)')' \begin{tabular}{lcccc} \hline \hline'
	write(uno,'(a)') '  \textbf{Moment}   & \multicolumn{2}{c}{\textbf{Men}} & \multicolumn{2}{c}{\textbf{Women}} \\ '

	write(uno,'(a)') '     &  Data & Model &  Data & Model \\ '
	write(uno, '(a)')'  \hline  '
	write(uno,'(a)') '  \multicolumn{5}{l}{\textit{Employment distribution:}}   \\ '
	write(uno,'(a)') '  \multicolumn{5}{l}{No children} \\ '
	k_c = 1
	h_c = 1
    write(uno, myformat) 'ME & ',mom_data%mom_empdist(h_c,k_c,1), ' & ',mom_model%mom_empdist(h_c,k_c,1), &
    	 ' & ',mom_data%mom_empdist(h_c,k_c,2), ' & ',mom_model%mom_empdist(h_c,k_c,2),' \\'
    h_c = 2
    write(uno, myformat) 'PT & ',mom_data%mom_empdist(h_c,k_c,1), ' & ',mom_model%mom_empdist(h_c,k_c,1), &
    	 ' & ',mom_data%mom_empdist(h_c,k_c,2), ' & ',mom_model%mom_empdist(h_c,k_c,2),' \\'
	h_c = 3
    write(uno, myformat) 'FT & ',mom_data%mom_empdist(h_c,k_c,1), ' & ',mom_model%mom_empdist(h_c,k_c,1), &
    	 ' & ',mom_data%mom_empdist(h_c,k_c,2), ' & ',mom_model%mom_empdist(h_c,k_c,2),' \\'
	write(uno,'(a)') '  \multicolumn{5}{l}{Young children} \\ '
	k_c = 2
	h_c = 1
    write(uno, myformat) 'ME & ',mom_data%mom_empdist(h_c,k_c,1), ' & ',mom_model%mom_empdist(h_c,k_c,1), &
    	 ' & ',mom_data%mom_empdist(h_c,k_c,2), ' & ',mom_model%mom_empdist(h_c,k_c,2),' \\'
    h_c = 2
    write(uno, myformat) 'PT & ',mom_data%mom_empdist(h_c,k_c,1), ' & ',mom_model%mom_empdist(h_c,k_c,1), &
    	 ' & ',mom_data%mom_empdist(h_c,k_c,2), ' & ',mom_model%mom_empdist(h_c,k_c,2),' \\'
	h_c = 3
    write(uno, myformat) 'FT & ',mom_data%mom_empdist(h_c,k_c,1), ' & ',mom_model%mom_empdist(h_c,k_c,1), &
    	 ' & ',mom_data%mom_empdist(h_c,k_c,2), ' & ',mom_model%mom_empdist(h_c,k_c,2),' \\'
	write(uno,'(a)') '  \multicolumn{5}{l}{Grown children} \\ '
	k_c = 3
	h_c = 1
    write(uno, myformat) 'ME & ',mom_data%mom_empdist(h_c,k_c,1), ' & ',mom_model%mom_empdist(h_c,k_c,1), &
    	 ' & ',mom_data%mom_empdist(h_c,k_c,2), ' & ',mom_model%mom_empdist(h_c,k_c,2),' \\'
    h_c = 2
    write(uno,myformat) 'PT & ',mom_data%mom_empdist(h_c,k_c,1), ' & ',mom_model%mom_empdist(h_c,k_c,1), &
    	 ' & ',mom_data%mom_empdist(h_c,k_c,2), ' & ',mom_model%mom_empdist(h_c,k_c,2),' \\'
	h_c = 3
    write(uno,myformat) 'FT & ',mom_data%mom_empdist(h_c,k_c,1), ' & ',mom_model%mom_empdist(h_c,k_c,1), &
    	 ' & ',mom_data%mom_empdist(h_c,k_c,2), ' & ',mom_model%mom_empdist(h_c,k_c,2),' \\'
	
	
	write(uno,'(a)') '  \multicolumn{5}{l}{\textit{Hourly wage:}}   \\ '
	write(uno,'(a)') '  \multicolumn{5}{l}{ME}   \\ '
	h_c = 1
	q_c = 3
    write(uno, myformat) 'P25 & ',mom_data%mom_wageqtl(q_c,h_c,1), ' & ',mom_model%mom_wageqtl(q_c,h_c,1), &
    	' & ',mom_data%mom_wageqtl(q_c,h_c,2), ' & ',mom_model%mom_wageqtl(q_c,h_c,2),' \\'
   	q_c = 4
    write(uno, myformat) 'P50 & ',mom_data%mom_wageqtl(q_c,h_c,1), ' & ',mom_model%mom_wageqtl(q_c,h_c,1), &
    	' & ',mom_data%mom_wageqtl(q_c,h_c,2), ' & ',mom_model%mom_wageqtl(q_c,h_c,2),' \\'
   	q_c = 5
    write(uno, myformat) 'P75 & ',mom_data%mom_wageqtl(q_c,h_c,1), ' & ',mom_model%mom_wageqtl(q_c,h_c,1), &
    	' & ',mom_data%mom_wageqtl(q_c,h_c,2), ' & ',mom_model%mom_wageqtl(q_c,h_c,2),' \\'	

	write(uno,'(a)') '  \multicolumn{5}{l}{PT}   \\ '
	h_c = 2
	q_c = 3
    write(uno, myformat) 'P25 & ',mom_data%mom_wageqtl(q_c,h_c,1), ' & ',mom_model%mom_wageqtl(q_c,h_c,1), &
    	' & ',mom_data%mom_wageqtl(q_c,h_c,2), ' & ',mom_model%mom_wageqtl(q_c,h_c,2),' \\'
   	q_c = 4
    write(uno, myformat) 'P50 & ',mom_data%mom_wageqtl(q_c,h_c,1), ' & ',mom_model%mom_wageqtl(q_c,h_c,1), &
    	' & ',mom_data%mom_wageqtl(q_c,h_c,2), ' & ',mom_model%mom_wageqtl(q_c,h_c,2),' \\'
   	q_c = 5
    write(uno, myformat) 'P75 & ',mom_data%mom_wageqtl(q_c,h_c,1), ' & ',mom_model%mom_wageqtl(q_c,h_c,1), &
    	' & ',mom_data%mom_wageqtl(q_c,h_c,2), ' & ',mom_model%mom_wageqtl(q_c,h_c,2),' \\'	
    write(uno,'(a)') '  \multicolumn{5}{l}{FT}   \\ '
	h_c = 3
	q_c = 3
    write(uno, myformat) 'P25 & ',mom_data%mom_wageqtl(q_c,h_c,1), ' & ',mom_model%mom_wageqtl(q_c,h_c,1), &
    	' & ',mom_data%mom_wageqtl(q_c,h_c,2), ' & ',mom_model%mom_wageqtl(q_c,h_c,2),' \\'
   	q_c = 4
    write(uno, myformat) 'P50 & ',mom_data%mom_wageqtl(q_c,h_c,1), ' & ',mom_model%mom_wageqtl(q_c,h_c,1), &
    	' & ',mom_data%mom_wageqtl(q_c,h_c,2), ' & ',mom_model%mom_wageqtl(q_c,h_c,2),' \\'
   	q_c = 5
    write(uno, myformat) 'P75 & ',mom_data%mom_wageqtl(q_c,h_c,1), ' & ',mom_model%mom_wageqtl(q_c,h_c,1), &
    	' & ',mom_data%mom_wageqtl(q_c,h_c,2), ' & ',mom_model%mom_wageqtl(q_c,h_c,2),' \\'	
	
    
    write(uno,'(a)') '  \multicolumn{5}{l}{\textit{Job separation prob. (annual, both genders):}}   \\ '
    write(uno, myformat_short) 'ME-to-NE & ',mom_data%trans_mat(1,0), ' & ',mom_model%trans_mat(1,0),' & & \\'
    write(uno, myformat_short) 'PT-to-NE & ',mom_data%trans_mat(2,0), ' & ',mom_model%trans_mat(2,0),' & & \\'
    write(uno, myformat_short) 'FT-to-NE & ',mom_data%trans_mat(3,0), ' & ',mom_model%trans_mat(3,0),' & & \\'
 	
 	write(uno,'(a)') '\hline\hline '
    write(uno,'(a)') '\end{tabular} '
	close(uno)
	
	
	
	
    end subroutine tex_export_fit
!===============================================================================!
    
    subroutine txt_export(filename,par_vec,mom_model)
    ! Purpose: WRITES PARAMETERS AND TARGETS TO A FILE
    ! It also writes input parameters and grids to files
    ! to be read into Matlab
	implicit none
    !Declare inputs:
    character(LEN=*), intent(in)    :: filename
    real(8),intent(in) :: par_vec(:)
    type(mom), intent(in) :: mom_model
    !Declare locals:
    character(len=128) :: pwd
    integer :: uno,append,ierr,g_c,h_c,k_c,l0_c,l1_c,q_c
	
	!Export parameters
	call getcwd(pwd)
	if (verbose==2) print *, "The current working directory is: ",trim(pwd)
	if (verbose==2) write(*,*) 'Saving txt files in the following directory:'
	if (verbose==2) write(*,*) savedir
	if (debug) pause
	open(15,file=trim(savedir)//'constants.txt',iostat=ierr,status='replace')
	if (ierr/=0) call myerror("initialize: cannot open file")
	if (debug) pause
	write(15,*) n_spinc
	write(15,*) n_prod
	write(15,*) n_kids
	write(15,*) n_hours
	write(15,*) n_w
	write(15,*) n_w_fine
	
	close(15)
	
	! Export other parameters
	call writedim(p_a,trim(savedir)//'p_a.txt')
	
	!Export grids
	call writedim(hours_grid,trim(savedir)//'hours_grid.txt')
    call writedim(F_prob,trim(savedir)//'F_prob.txt')
    call writedim(hpol,trim(savedir)//'hpol.txt')
    call writedim(w_grid,trim(savedir)//'w_grid.txt')
    
    ! NOTE:
    ! prod_grid and prod_PMF are generated in the demand side
    ! see subroutine txt_export_demand
	
	
	! Write external parameters and moments to filename file
	open(newunit=uno, file=filename, status='replace',iostat=ierr)
	if (ierr/=0) then
		call myerror("Error in txt_export: cannot open file")
	endif
	
	!Two blank lines (align with excel)
    write(uno,'(a)')' '

    write(uno, '(a)')'MIN WAGE'
    write(uno, '(a,I)')'do_min_wage			: ',do_eq_min_wage 
    write(uno, '(a,f12.6)')'p_min_wage		: ',p_min_wage    
    write(uno, '(a,f12.6)')'p_kappa0		: ',p_kappa0(1)    

    write(uno, '(a)')' '
    write(uno, '(a)')'GRIDS'
    write(uno, '(a,I4)')'n_prod				: ',n_prod
    write(uno, '(a,I4)')'n_hours			: ',n_hours
    write(uno, '(a,I4)')'n_kids				: ',n_kids
    
    write(uno, '(a)')' '
    write(uno, '(a)')'SIMULATION PARAMS'
    write(uno, '(a,I4)')'T_sim				: ',T_sim
    write(uno, '(a,I6)')'N_sim_couple		: ',N_sim_couple 
    write(uno, '(a,I6)')'N_sim_single		: ',N_sim_single
    write(uno, '(a,I6)')'max_counter		: ',max_counter
    
    write(uno, '(a)')' '
    write(uno, '(a)')'EXTERNAL PARAMETERS'
    write(uno, '(a,f12.6)')'hours_grid(1)	: ',hours_grid(1)      
    write(uno, '(a,f12.6)')'hours_grid(2)	: ',hours_grid(2)      
    write(uno, '(a,f12.6)')'hours_grid(3)	: ',hours_grid(3)      
    write(uno, '(a,f12.6)')'p_N_couple		: ',p_N_couple
    write(uno, '(a,f12.6)')'p_N_single		: ',p_N_single
    write(uno, '(a,f12.6)')'p_rho			: ',p_rho
    write(uno, '(a,f12.6)')'p_rho_d			: ',p_rho_d
    write(uno, '(a,f12.6)')'p_phi(1,male,single)	: ',p_phi(1,1,1)
    write(uno, '(a,f12.6)')'p_phi(1,female,single)	: ',p_phi(1,2,1)
    write(uno, '(a,f12.6)')'p_phi(1,male,married)	: ',p_phi(1,1,2)
   write(uno, '(a,f12.6)')'p_phi(2,male,single)	: ',p_phi(2,1,1)
    write(uno, '(a,f12.6)')'p_phi(2,female,single)	: ',p_phi(2,2,1)
    write(uno, '(a,f12.6)')'p_phi(2,male,married)	: ',p_phi(2,1,2)
    write(uno, '(a,f12.6)')'p_offer_lb(1)	: ',p_offer_lb(1)
    write(uno, '(a,f12.6)')'p_offer_lb(2)	: ',p_offer_lb(2)
    write(uno, '(a,f12.6)')'p_offer_lb(3)	: ',p_offer_lb(3)
    write(uno, '(a,f12.6)')'p_offer_ub(1)	: ',p_offer_ub(1)
    write(uno, '(a,f12.6)')'p_offer_ub(2)	: ',p_offer_ub(2)
    write(uno, '(a,f12.6)')'p_offer_ub(3)	: ',p_offer_ub(3)
!    
!   
  	write(uno, '(a)')' '
    write(uno, '(a)')'MOMENTS'
    ! mom_empdist: dim(n_hours,2)
    write(uno, '(a)')'Emp. Distrib.: No kids'
    k_c = 1
    do g_c = 1,2
    do h_c = 1,n_hours
    	write(uno, *)'Gender',g_c,'h_c',h_c, mom_data%mom_empdist(h_c,k_c,g_c),mom_model%mom_empdist(h_c,k_c,g_c)
    enddo
    enddo
    write(uno, '(a)')'Emp. Distrib.: Young kids'
    k_c = 2
    do g_c = 1,2
    do h_c = 1,n_hours
    	write(uno, *)'Gender',g_c,'h_c',h_c, mom_data%mom_empdist(h_c,k_c,g_c),mom_model%mom_empdist(h_c,k_c,g_c)
    enddo
    enddo
    write(uno, '(a)')'Emp. Distrib.: Grown kids'
    k_c = 3
    do g_c = 1,2
    do h_c = 1,n_hours
    	write(uno, *)'Gender',g_c,'h_c',h_c, mom_data%mom_empdist(h_c,k_c,g_c),mom_model%mom_empdist(h_c,k_c,g_c)
    enddo
    enddo
    ! mom_wageqtl: dim(1:9,2:3,1:2)
    write(uno, '(a)')'Men, ME'
    g_c = 1
    h_c = 1
    do q_c = 1,7
    	write(uno, *)'q_c',q_c, mom_data%mom_wageqtl(q_c,h_c,g_c),mom_model%mom_wageqtl(q_c,h_c,g_c)
    enddo
    write(uno, '(a)')'Men, PT'
    g_c = 1
    h_c = 2
    do q_c = 1,7
    	write(uno, *)'q_c',q_c, mom_data%mom_wageqtl(q_c,h_c,g_c),mom_model%mom_wageqtl(q_c,h_c,g_c)
    enddo
    write(uno, '(a)')'Men, FT'
    g_c = 1
    h_c = 3
    do q_c = 1,7
    	write(uno, *)'q_c',q_c, mom_data%mom_wageqtl(q_c,h_c,g_c),mom_model%mom_wageqtl(q_c,h_c,g_c)
    enddo

    write(uno, '(a)')'Women, ME'
    g_c = 2
    h_c = 1
    do q_c = 1,7
    	write(uno, *)'q_c',q_c, mom_data%mom_wageqtl(q_c,h_c,g_c),mom_model%mom_wageqtl(q_c,h_c,g_c)
    enddo

    write(uno, '(a)')'Women, PT'
    g_c = 2
    h_c = 2
    do q_c = 1,7
    	write(uno, *)'q_c',q_c, mom_data%mom_wageqtl(q_c,h_c,g_c),mom_model%mom_wageqtl(q_c,h_c,g_c)
    enddo

    write(uno, '(a)')'Women, FT'
    g_c = 2
    h_c = 3
    do q_c = 1,7
    	write(uno, *)'q_c',q_c, mom_data%mom_wageqtl(q_c,h_c,g_c),mom_model%mom_wageqtl(q_c,h_c,g_c)
    enddo

	write(uno, '(a)')'Labor force transition'
! 	! trans_mat: dim(0:3,0:3) 
	do l0_c = 0,3
	do l1_c = 0,3
		write(uno, *) 'From',l0_c,'to',l1_c, mom_data%trans_mat(l0_c,l1_c),mom_model%trans_mat(l0_c,l1_c)
	enddo
	enddo

		
	! close file
    close(uno)
	! Write Parameters to file
	append = 1
	call write_params_eq(par_vec,filename,append)

    ! Write untargeted moments to external files
    call writedim(mom_model%trans_mat_j,trim(savedir)//'trans_mat_j.txt')
    call writedim(mom_model%trans_emp_j,trim(savedir)//'trans_emp_j.txt')
	
    end subroutine txt_export
    !===============================================================================!
    
    subroutine txt_export_demand(filename,Delta_ob,Delta_th,hpol,hpol_est)
    ! Purpose: WRITES Demand side PARAMETERS AND TARGETS TO A FILE
    ! It also writes objects to files that will be read into Matlab
	implicit none
    !Declare inputs:
    character(LEN=*), intent(in)    :: filename
    real(8),intent(in) :: Delta_ob(:,:),Delta_th(:,:),hpol(:),hpol_est(:)
    
    !Declare locals:
    integer:: uno,ierr,p_c,h_c
	
	! prod_grid and prod_PMF are generated in the demand side
    call writedim(prod_grid,trim(savedir)//'prod_grid.txt')
    call writedim(prod_PMF,trim(savedir)//'prod_PMF.txt')
	call writedim(p_theta,trim(savedir)//'p_theta.txt')
	! Write external parameters and moments to filename file
	open(newunit=uno, file=filename, status='replace',iostat=ierr)
	if (ierr/=0) then
		write(*,*) "ierr = ",ierr 
		call myerror("Error in txt_export_demand: cannot open file")
	endif
	
	!Two blank lines (align with excel)
    write(uno,'(a)')' '       
    write(uno, '(a)')' '
    write(uno, '(a)')'PARAMETERS'
    write(uno, '(a,f12.6)')'p_theta(1)	: ',p_theta(1) 
    write(uno, '(a,f12.6)')'p_theta(2)	: ',p_theta(2)   
     write(uno, '(a,f12.6)')'p_theta(3)	: ',p_theta(3)     
    write(uno, '(a,f12.6)')'p_sigma_eps	: ',p_sigma_eps
    write(uno, '(a,f12.6)')'p_eps_bar(1)		: ',p_eps_bar(1)
    write(uno, '(a,f12.6)')'p_eps_bar(2)		: ',p_eps_bar(2)
    write(uno, '(a,f12.6)')'p_eps_bar(3)		: ',p_eps_bar(3)
   
  
    write(uno, '(a)')' '
    write(uno, '(3a)')'h_c  ','hpol  ','hpol_est  '
    do h_c = 1,n_hours
			write(uno, '(I,2f12.6)') h_c, hpol(h_c),hpol_est(h_c)
    enddo
  
 	write(uno, '(a)')' '
    write(uno, '(4a)')'h_c  ','prod  ','Delta_ob  ','Delta_th  '
    ! 
    do h_c = 1,n_hours
		do p_c = 1,n_prod
			write(uno, '(I,3f12.6)') h_c, prod_grid(p_c), Delta_ob(p_c,h_c),Delta_th(p_c,h_c)
		enddo
    enddo

	close(uno)
	
    end subroutine txt_export_demand
    
!====================================================! 

    
    subroutine write_params(par_vec,file_name,append)
    !! write_params writes parameter values par_vec for supply side to file file_name
    implicit none
    ! Declare variables
    real(8), intent(in) :: par_vec(:)
    character(len=*), intent(in) :: file_name
    integer, intent(in) :: append
    ! Declare local variables
    integer :: unitno,ierr,i
    character (len = 20), allocatable :: pnames(:)
    
    
    ! Open file
    if (append==1) then
    	open(newunit=unitno,file=file_name,form='formatted',status='old',position='append',iostat=ierr)
    	if (ierr/=0) then
			call myerror("Error in write_params: cannot open file")
		endif
    elseif (append==0) then
	    open(newunit=unitno, file=file_name, status='replace',iostat=ierr)
	    if (ierr/=0) then
			call myerror("Error in write_params: cannot open file")
		endif
	else
		call myerror("Invalid append in write_params.")
    endif 
    
    ! Allocate pnames
    allocate(pnames(n_params))
    
    ! Construct vector of parameter names
    ! todo: update these!!!
    ! Note that these have to be in the exact same order as in pack_params and unpack_params!!!
    i=1
    pnames(i) = "p_offer_a(1)"
    i=i+1
    pnames(i) = "p_offer_a(2)"
    i=i+1
    pnames(i) = "p_offer_a(3)"
    i=i+1
    pnames(i) = "p_offer_b(1)"
    i=i+1
    pnames(i) = "p_offer_b(2)"
    i=i+1
    pnames(i) = "p_offer_b(3)"
    i=i+1
    pnames(i) = "p_hpol_arr(1)"
    i=i+1
    pnames(i) = "p_hpol_arr(2)"
    i=i+1
    pnames(i) = "p_lambda" 
    i=i+1
    pnames(i) = "p_delta(1)" 
    i=i+1
    pnames(i) = "p_delta(2)" 
    i=i+1
    pnames(i) = "p_delta(3)" 
    i=i+1
    pnames(i) = "p_psi_m(2)" 
    i=i+1
    pnames(i) = "p_psi_m(3)" 
    i=i+1
    pnames(i) = "p_psi_f(1)" 
    i=i+1
    pnames(i) = "p_psi_f(2)" 
    i=i+1
    pnames(i) = "p_psi_f(3)" 
    i=i+1
    pnames(i) = "p_gamma_m(2)" 
    i=i+1
    pnames(i) = "p_gamma_m(3)"
     i=i+1
    pnames(i) = "p_gamma_f(1)" 
    i=i+1
    pnames(i) = "p_gamma_f(2)" 
    i=i+1
    pnames(i) = "p_gamma_f(3)"
     i=i+1
    pnames(i) = "p_mu_alpha(2,2)" 
    i=i+1
    pnames(i) = "p_mu_alpha(3,2)" 
    i=i+1
    pnames(i) = "p_a(1)" 
     i=i+1
    pnames(i) = "p_a(2)" 
     i=i+1
    pnames(i) = "p_a(3)" 
    if (n_params/=i) then
    	call myerror("Error in write_params: n_params mismatch")
    endif 
    
    ! Write 
    write(unitno,'(a)')' '

    write(unitno, '(a)') 'ESTIMATED PARAMETERS'
    do i=1,n_params
    	write(unitno,'(a,a,f12.6)') pnames(i)," : ",par_vec(i)   
    enddo
    
    ! close output file
    close(unitno)
    
    end subroutine write_params
!====================================================! 

    
    subroutine write_params_eq(par_vec,file_name,append)
    !! write_params writes parameter values par_vec for supply side to file file_name
    implicit none
    ! Declare variables
    real(8), intent(in) :: par_vec(:)
    character(len=*), intent(in) :: file_name
    integer, intent(in) :: append
    ! Declare local variables
    integer :: unitno,ierr,i
    character (len = 20), allocatable :: pnames(:)
    
    
    ! Open file
    if (append==1) then
    	open(newunit=unitno,file=file_name,form='formatted',status='old',position='append',iostat=ierr)
    	if (ierr/=0) then
			call myerror("Error in write_params: cannot open file")
		endif
    elseif (append==0) then
	    open(newunit=unitno, file=file_name, status='replace',iostat=ierr)
	    if (ierr/=0) then
			call myerror("Error in write_params: cannot open file")
		endif
	else
		call myerror("Invalid append in write_params.")
    endif 
    
    ! Allocate pnames
    allocate(pnames(n_params_eq))
    
    ! Construct vector of parameter names
    ! todo: update these!!!
    ! Note that these have to be in the exact same order as in pack_params and unpack_params!!!
    i=1
    pnames(i) = "p_wage_ratio"
    i=i+1
    pnames(i) = "p_eps_bar(1)"
    i=i+1
    pnames(i) = "p_eps_bar(2)"
    i=i+1
    pnames(i) = "p_theta(1)"
    i=i+1
    pnames(i) = "p_theta(2)"
    i=i+1
    pnames(i) = "p_eps_sigma"
    i=i+1
    pnames(i) = "p_lambda" 
    i=i+1
    pnames(i) = "p_delta(1)"
    i=i+1
    pnames(i) = "p_delta(2)"
    i=i+1
    pnames(i) = "p_delta(3)" 
    i=i+1
    pnames(i) = "p_psi_m(2)" 
    i=i+1
    pnames(i) = "p_psi_m(3)" 
    i=i+1
    pnames(i) = "p_psi_f(1)" 
    i=i+1
    pnames(i) = "p_psi_f(2)" 
    i=i+1
    pnames(i) = "p_psi_f(3)" 
    i=i+1
    pnames(i) = "p_gamma_m(2)" 
    i=i+1
    pnames(i) = "p_gamma_m(3)" 
    i=i+1
    pnames(i) = "p_gamma_f(1)" 
    i=i+1
    pnames(i) = "p_gamma_f(2)" 
    i=i+1
    pnames(i) = "p_gamma_f(3)" 
    i=i+1
    pnames(i) = "p_mu_alpha(2,2)" 
    i=i+1
    pnames(i) = "p_mu_alpha(3,2)" 
    i=i+1
    pnames(i) = "p_a(1)" 
     i=i+1
    pnames(i) = "p_a(2)" 
     i=i+1
    pnames(i) = "p_a(3)"
    
    if (n_params_eq/=i) then
    	call myerror("Error in write_params_eq: n_params_eq mismatch")
    endif 
    
    ! Write 
    write(unitno,'(a)')' '

    write(unitno, '(a)') 'ESTIMATED PARAMETERS'
    do i=1,n_params_eq
    	write(unitno,'(a,a,f12.6)') pnames(i)," : ",par_vec(i)   
    enddo
    
    ! close output file
    close(unitno)
    
    end subroutine write_params_eq
!===============================================================================!

subroutine write_params_demand(par_vec,file_name)
    !! write_params writes parameter values par_vec for demand side to file file_name
    implicit none
    ! Declare variables
    real(8), intent(in) :: par_vec(:)
    character(len=*), intent(in) :: file_name
    ! Declare local variables
    integer :: unitno,ierr,nparams,i
    character (len = 20), allocatable :: pnames(:)
    
    ! Open file
	open(newunit=unitno, file=file_name, status='replace',iostat=ierr)
	if (ierr/=0) then
		call myerror("Error in write_params_demand: cannot open file")
	endif
	
    ! Allocate pnames
    nparams = size(par_vec)
    allocate(pnames(nparams))
    
    ! Construct vector of parameter names
    ! Note that these have to be in the exact same order as in pack_params_demand and 
    ! unpack_params_demand !!!
    i=1
    pnames(i) = "p_sigma_eps"
    i=i+1
    pnames(i) = "p_eps_bar(1)"
    i=i+1
    pnames(i) = "p_eps_bar(2)" 
    

    ! Write 
    do i=1,nparams
    	write(unitno,'(f12.6,a,a)') par_vec(i)," ! ",pnames(i)   
    enddo
    
    ! close output file
    close(unitno)
    
    end subroutine write_params_demand
!===============================================================================!

	subroutine read_params_demand(par_vec,file_name)
    !! read_params_demand reads parameter values par_vec for demand side to file file_name
    implicit none
    ! Declare input/output variables
    real(8), intent(out) :: par_vec(:)
    character(len=*), intent(in) :: file_name
    ! Locals
    integer :: unitno,ierr,nparams,ii
    character(len=40) :: s1,s2
    
    open(newunit=unitno, file=file_name, status='old', iostat=ierr)
    if (ierr/=0) then
        call myerror("Error in read_params_demand: cannot open file")
    endif

	nparams = size(par_vec)

	! Read 
    do ii=1,nparams
    	read(unitno,'(f12.6,a,a)') par_vec(ii),s1,s2   
    enddo

	! close file
    close(unitno)
    
	end subroutine read_params_demand
!===============================================================================!

    subroutine write_compstat(par_vary_vec,mom_model_vec,varname)
    ! Purpose: WRITES PARAMETERS AND TARGETS TO A FILE for comparative static experiments
    !	This subroutine is called from subroutine compstat in main.
	implicit none
    !Declare inputs:
    character(LEN=*), intent(in)    :: varname
    real(8),intent(in) :: par_vary_vec(:)
    type(mom), intent(in) :: mom_model_vec(:)
    !Declare locals:
    integer:: uno,ierr
	character(LEN=40) :: filename
	
	! Checks
	if (size(mom_model_vec) /= size(par_vary_vec)) then
		call myerror("Error in write_compstat: Dimension mismatch")
	endif
	
	! Write comparative statics results to output file
	filename = trim(savedir)//"compstat_"//trim(varname)//".txt"
	open(newunit=uno, file=filename, status='replace',iostat=ierr)
	if (ierr/=0) then
		call myerror("Error in write_compstat: cannot open file")
	endif
	
	! The first column is the name of the variable/moment
	! The first row is the values of the parameter of interest
! 	! Each subsequent row is a moment

	
	! close output file
    close(uno)
	
	end subroutine write_compstat
end module mod_output
