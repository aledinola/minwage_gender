module mod_firm
    
    !=======================================================================
    ! Here we collect programs to solve firm problem
    ! Main program: solve_firm
    ! Notation: Member 1 => m, Member 2 => f
	!=======================================================================
    
    use mod_globals
    use mod_numerical, only: myerror,sub_logsum,linspace
    use mod_utilities
    use solve_household, only: vfi_singles, acceptance_rules_s
    use mod_distribution, only: get_distribution_s,labor_supply_gender

    implicit none
	
	private
	public solve_firm
        
    contains
       
    
    !========================================================================!
    
    subroutine solve_firm(V_inout,V_inout_s,g_inout,g_inout_s,wage_pol, wage_pol_new,Delta_new,p_mw_idx,p_hat_idx)
    
    implicit none
    
    !-----------------------------------------------------------------------------!
    !Declare inputs/outputs:
    type(valfun),intent(inout)   :: V_inout
    type(valfun_s),intent(inout) :: V_inout_s
    type(gdist), intent(inout) :: g_inout !Fields: uu, states: (k,alpha1,alpha2)
                                 !        eu, states: (w1,h1,k,alpha1,alpha2)
                                 !        ue, states: (w2,h2,k,alpha1,alpha2)
                                 !        ee, states: (w1,h1,w2,h2,k,alpha1,alpha2)
    type(gdist_s), intent(inout) :: g_inout_s
    real(8), intent(in)  :: wage_pol(:,:)     ! states: (p,h)
    real(8), intent(out) :: wage_pol_new(:,:) ! states: (p,h)
    real(8), intent(out) :: Delta_new(:,:)    ! states: (p,h)
    integer, intent(in) :: p_mw_idx(n_hours)
    integer, intent(inout) :: p_hat_idx(n_hours)

    !Declare locals:
    integer:: p_c,h_c,istat
    real(8), allocatable :: profit_max(:,:),profit_daily(:,:),labsup(:,:,:),labsup_p_hat(:,:,:,:)
    real(8) :: y_val,hours
    real(8) :: log_sum
    real(8) :: profit_p_hat_c(n_hours), profit_p_hat_nc(n_hours)
    real(8) :: delta_job(n_hours),profit_vec(n_hours),delta_post(2),profit_post(2)

    !-----------------------------------------------------------------------------!
    ! Wage Policy
    
	!Allocate 
    allocate(profit_max(n_prod,n_hours),profit_daily(n_prod,n_hours),&
    	labsup(n_prod,n_hours,2),labsup_p_hat(n_prod,n_hours,2,n_hours+1),stat=istat)
    if (istat/=0) then
        call myerror("solve_firm: Allocation failed!")
    endif
	! Initialize labsup to zero
	labsup=0.0d0
    labsup_p_hat=0.0d0
    
	! Get labor supply given initial w_grid (p_hat is fully compliant)
    call sub_labor_supply_gender(labsup_p_hat(:,:,:,1),V_inout_s,g_inout_s)
    ! Get labor supply assuming firm p_hat_idx choose to be non-compliant
    
 ! Now wage_pol is endogenous if do_eq_min_wage = .true. because firms can choose either to pay the minimum wage or the non-compliant fixed wage. 
    ! We should also keep track of the two thresholds: the threshold p_mw_idx - above which firms' fixed wage is always compliant (for both genders), and p_hat_idx(<p_mw_idx) - below which firms' fixed wage gives a higher profit than the minimum wage. For firms in between p_hat_idx and p_mw_idx, the wage policy is the minimum wage. Note that these two thresholds are hours-specific. 
    do h_c = 1,n_hours 
        p_c = p_hat_idx(h_c)
        if (p_c>=1) then 
            ! Update wage grid
            w_grid = wage_pol ! reset, assuming only non-compliant in hours h_c
            w_grid(p_c,h_c) = p_min_wage/min(1.0d0,p_a(h_c)) ! full compliance wage
            ! Get labor supply
            call sub_labor_supply_gender(labsup_p_hat(:,:,:,h_c+1),V_inout_s,g_inout_s)
        else 
            ! set labsup_p_hat to be the same as the one with the unchanged w_grid
            labsup_p_hat(:,:,:,h_c+1) = labsup_p_hat(:,:,:,1)
        endif 
    enddo ! h_c 


    ! Compare profits
    ! Initialize wage_pol_new 
    wage_pol_new  = wage_pol
    write(*,*) "p_hat_idx before update:",p_hat_idx

    do h_c = 1,n_hours 
        p_c = p_hat_idx(h_c)
        hours = hours_grid(h_c)
		y_val = y_arr(p_c,h_c)
        ! Profit from the unchanged w_grid (wage_pol(p_c,h_c) should be the non-compliant minimum wage. )
        profit_p_hat_nc(h_c) = firm_profit(y_val,hours,wage_pol(p_c,h_c), labsup_p_hat(p_c,h_c,1,1)) + firm_profit(y_val,hours,wage_pol(p_c,h_c), labsup_p_hat(p_c,h_c,2,1))
        ! Profit from compliant wage 
        profit_p_hat_c(h_c) = firm_profit(y_val,hours,p_min_wage/min(1.0d0,p_a(h_c)), labsup_p_hat(p_c,h_c,1,h_c+1)) + firm_profit(y_val,hours,fun_fixwage(prod_grid(p_c),h_c), labsup_p_hat(p_c,h_c,2,h_c+1)) 
        write(*,*) "h_c = ",h_c 
        write(*,*) "Non-compliant wage and labsup (m,f):",wage_pol(p_c,h_c), labsup_p_hat(p_c,h_c,1,1), labsup_p_hat(p_c,h_c,2,1)
        write(*,*) "Compliant wage and labsup (m,f):",p_min_wage/min(1.0d0,p_a(h_c)), labsup_p_hat(p_c,h_c,1,h_c+1), labsup_p_hat(p_c,h_c,2,h_c+1)
        write(*,*) "Profit with compliant wage:",profit_p_hat_c(h_c)
        write(*,*) "Profit with non-compliant wage:",profit_p_hat_nc(h_c)
        ! Compare profits
        if (p_hat_idx(h_c)>=1 .and. profit_p_hat_nc(h_c)<profit_p_hat_c(h_c)) then 
            ! Update p_hat_idx 
            p_hat_idx(h_c)= p_hat_idx(h_c) - 1
            ! Update wage_pol 
            wage_pol_new(p_c,h_c) = p_min_wage/min(1.0d0,p_a(h_c))
        endif 
    enddo 
	write(*,*) "p_hat_idx after update:",p_hat_idx

    ! Update w_grid
    w_grid = wage_pol_new 
    ! Update labsup
    call sub_labor_supply_gender(labsup,V_inout_s,g_inout_s)
    
    if (verbose==2) write(*,*) "Writing labor supply to file.."
    call writedim(labsup,trim(savedir)//'labsup_eq.txt')
    !Write current wage to file:
    call writedim(w_grid,trim(savedir)//'w_grid_eq.txt')

	!---Update profit 
	! We do this because the labor supply (labsup) has been updated given the optimal
	!	wages of the firm firm p_c = p_hat_idx.
	do h_c = 1,n_hours
        do p_c = 1,n_prod
		    hours = hours_grid(h_c)
		    y_val = y_arr(p_c,h_c)
		    profit_max(p_c,h_c) = firm_profit(y_val,hours,w_grid(p_c,h_c), labsup(p_c,h_c,1) )  + firm_profit(y_val,hours,w_grid(p_c,h_c), labsup(p_c,h_c,2) ) 
        enddo
	enddo !h_c
    
    !Write maximized profits to file:
    call writedim(profit_max,trim(savedir)//'profit_max.txt')
    
	!-----------------------------------------------------------------------------!
    !Solve for optimal choice of hours and gender (eqns 10) 
    ! Daily profit
    profit_daily = profit_max/365.0d0
    
    !update !\Delta(p,h,gender) and hpol
    Delta_new = 0.0d0

    do p_c = 1,n_prod
    	! Compute the probability of posting a particular job conditional 
    	!	on posting a job.
    	do h_c = 1,n_hours
			profit_vec(h_c) = (profit_daily(p_c,h_c)-p_eps_bar(h_c))
        enddo ! h_c
        ! outputs are log_sum (scalar) and delta_job (dim: 2*n_hours)
    	call sub_logsum(log_sum,delta_job, profit_vec,p_sigma_eps)

		if (verbose>=2) write(*,*) p_c,"log_sum",log_sum
		
        !Compute probability of posting a job (active)
        profit_post = [log_sum,0.0d0]
        ! output form sub_logsum: delta_post = [prob. active, prob. inactive]
        call sub_logsum(log_sum,delta_post, profit_post,p_sigma_eps)

        ! compute Delta
        ! (Prob of not posting a job is 1-sum(Delta(p_c,:)))
		do h_c = 1,n_hours   
			Delta_new(p_c,h_c) =  delta_post(1)*delta_job(h_c)
		enddo

!         !check if sum(Delta(p_c,:))<1
        
    enddo ! p_c

    if (verbose>=2) then
	    write(*,*) "End of solve_firm"
	    pause
	endif
     
    end subroutine solve_firm
    !========================================================================!

    subroutine sub_labor_supply_gender(labsup,V_inout_s,g_inout_s)
    ! sub_labor_supply_gender(labsup_p_hat(:,:,:,1))
    implicit none
    type(valfun_s), intent(inout) :: V_inout_s
    type(gdist_s), intent(inout) :: g_inout_s
    real(8),intent(out) :: labsup(:,:,:)
    ! Local variables
    integer :: p_c,h_c,g_c
    logical :: flag_vfi_s, flag_dist_s

    !---Value Function Iteration, Acceptance Rules and Distribution---!		
        ! Allocate and initialize value functions
        ! Allocate and initialize distribution g_inout
        ! VFI for single households
        call vfi_singles(V_inout_s,flag_vfi_s)
        if (flag_vfi_s == .false. ) then
            call myerror("VFI for singles did not converge!")
        endif
        if (verbose>=2) then	
            write(*,*) "VFI for singles done!" 
            pause
        endif
        ! Distributions of singles	
        call acceptance_rules_s(V_inout_s)
        call get_distribution_s(g_inout_s,flag_dist_s)
        if (flag_dist_s==.false.) then
            call myerror("Distribution G of singles did not converge!")
        endif

        if (allocated(omega_u_sf)) deallocate(omega_u_sf)
        if (allocated(omega_u_sm)) deallocate(omega_u_sm)
        if (allocated(omega_e_sf)) deallocate(omega_e_sf)
        if (allocated(omega_e_sm)) deallocate(omega_e_sm)
        if (allocated(Oh_e_sm)) deallocate(Oh_e_sm)
        if (allocated(Oh_e_sf)) deallocate(Oh_e_sf)

        if (verbose>=2) then	
            write(*,*) "Acceptance rules for singles done!" 
            pause
        endif
        
        !---Solve firm problem for p_c > 1: update wage policy and Delta---!
        
        !---Compute labor supply for all firms---!
        do p_c = 1,n_prod
        do h_c = 1,n_hours
        do g_c = 1,2
            ! labsup is output
            call labor_supply_gender(g_inout,g_inout_s,p_c,h_c,g_c,labsup(p_c,h_c,g_c))
        enddo
        enddo
        enddo

    end subroutine sub_labor_supply_gender

end module mod_firm
    
