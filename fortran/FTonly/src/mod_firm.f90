module mod_firm
    
    !=======================================================================
    ! Here we collect programs to solve firm problem
    ! Main program: solve_firm
    ! Notation: Member 1 => m, Member 2 => f
	!=======================================================================
    
    use mod_globals
    use mod_numerical, only: myerror,sub_logsum,linspace
    use mod_pchip
    use toolbox, only: plot, execplot

    implicit none
	
	private
	public solve_firm
        
    contains
       
    
    !========================================================================!
    
    subroutine solve_firm(V_inout,V_inout_s,g_inout,g_inout_s,wage_pol, wage_pol_new,Delta_new)
    
    use mod_utilities
    use mod_numerical, only: linint,linspace
    use solve_household, only: vfi, vfi_singles, acceptance_rules, acceptance_rules_s
    use mod_distribution, only: get_distribution, get_distribution_s, labor_supply
    
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
    
    !Declare locals:
    real(8), allocatable :: profit_1(:,:),profit_max(:,:),profit_daily(:,:),labsup(:,:)
    real(8) :: y_val,hours,wage_val,ls_val
    real(8) :: base,trapezoid,integral,log_sum
    integer :: p_c,h_c,w_fine_c,wopt,istat
    real(8) :: delta_job(n_hours),profit_vec(n_hours),delta_post(2),profit_post(2)
    logical :: flag_vfi_s, flag_dist_s

    ! labsup_1 is a global variable
    !-----------------------------------------------------------------------------!
    ! Wage Policy
    
	!Allocate 
    allocate(profit_1(n_w_fine,n_hours),profit_max(n_prod,n_hours),profit_daily(n_prod,n_hours),&
    	labsup(n_prod,n_hours),stat=istat)
    if (istat/=0) then
        call myerror("solve_firm: Allocation failed!")
    endif
	! Initialize labsup to zero
	labsup=0.0d0
	
	! Initialize wage_pol_new to be wage_pol
	wage_pol_new  = wage_pol
	
	!---Find the optimal wage for p_c=1 by searching over w_fine_grid---!
    p_c = 1
	do h_c = n_hours,n_hours ! Counterfactual only for FT
		hours = hours_grid(h_c)
		do w_fine_c = 1,n_w_fine
			! We vary wage of the least prod firm on the finer
			! grid to get profit
			y_val = y_arr(p_c,h_c)	    
			wage_val = w_fine_grid(w_fine_c,h_c)
			ls_val = labsup_1(w_fine_c,h_c)
			
			! profit (considering both genders)
			profit_1(w_fine_c,h_c) = firm_profit(y_val,hours,wage_val,ls_val)
		enddo ! w_fine_c
		
		! solve prof maximization w.r.t. wage
		!Max wrt wage:
		wopt = maxloc(profit_1(:,h_c),dim=1)
		profit_max(p_c,h_c) = profit_1(wopt,h_c)
		
		!Update wage policy function, only for p_c=1 (set before loop):
		wage_pol_new(p_c,h_c) = w_fine_grid(wopt,h_c)
		! Update labor supply
		labsup(p_c,h_c) = labsup_1(wopt,h_c)
	enddo ! h_c
    
	if (verbose>=2) write(*,*) "Writing profit of p=1 to file.."
    if (debug) pause
    call writedim(profit_1,trim(savedir)//'profit_1.txt')
	
    !---Solve VFI and Distribution with the updated wage grid---!
    if (verbose>=2) then	
    	write(*,*) "Solve VFI and Distribution Again at the updated wage grid"
    	pause
    endif
    !---Update wage grid---!
    w_grid = wage_pol_new
	
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
	deallocate(omega_u_sf,omega_u_sm,omega_e_sf,omega_e_sm,Oh_e_sm,Oh_e_sf)
	if (verbose>=2) then	
    	write(*,*) "Acceptance rules for singles done!" 
		pause
	endif
	
		
    if (write_v_g) then
    	
        !Export value function results for singles
        if (verbose==2) write(*,*) "Writing valfun singles to file.."
        call writedim(V_inout_s%V_u_sm,trim(savedir)//'V_u_sm.txt')
        call writedim(V_inout_s%V_u_sf,trim(savedir)//'V_u_sf.txt')
        call writedim(V_inout_s%V_e_sm,trim(savedir)//'V_e_sm.txt')
        call writedim(V_inout_s%V_e_sf,trim(savedir)//'V_e_sf.txt')

        !Export distributions for singles
 	    if (verbose==2) write(*,*) "Writing distrib. singles to file.."
        call writedim(g_inout_s%u_sm,trim(savedir)//'g_u_sm.txt')
        call writedim(g_inout_s%u_sf,trim(savedir)//'g_u_sf.txt')
        call writedim(g_inout_s%e_sm,trim(savedir)//'g_e_sm.txt')
        call writedim(g_inout_s%e_sf,trim(savedir)//'g_e_sf.txt')
    endif
     
    !---Solve firm problem for p_c > 1: update wage policy and Delta---!
    if (verbose>=2) then	
		write(*,*) "Start Firm's problem for p_c > 1.."
		pause
	endif
    
    !---Compute labor supply for all firms---!
    do p_c = 1,n_prod
    do h_c = n_hours,n_hours ! Counterfactual only for FT
    	! labsup is output
	    call labor_supply(g_inout,g_inout_s,p_c,h_c,labsup(p_c,h_c))
	enddo
    enddo
    
    if (verbose==2) write(*,*) "Writing labor supply to file.."
    if (debug) pause
    call writedim(labsup,trim(savedir)//'labsup_eq.txt')
    !Write current wage to file:
    call writedim(w_grid,trim(savedir)//'w_grid_eq.txt')

	!---Update profit for firm pc = 1---!
	! We do this because the labor supply (labsup) has been updated given the optimal
	!	wages of the firm firm p_c = 1.
	do h_c = n_hours,n_hours ! Counterfactual only for FT
		hours = hours_grid(h_c)
		p_c = 1
		y_val = y_arr(p_c,h_c)
		profit_max(p_c,h_c) = firm_profit(y_val,hours,wage_pol_new(p_c,h_c), labsup(p_c,h_c) ) 
	enddo !h_c
	
    !---Update wage policy for p_c>1---!
    ! Here we use envelope method (as in Aizawa and Fang JPE)
    do h_c = n_hours,n_hours ! Counterfactual only for FT
        integral = 0.0d0
    	do p_c = 2,n_prod
    		! First, obtain profit_max(p,h_c) for p_c>1 by trapezoid integration

			 base = prod_grid(p_c) - prod_grid(p_c-1)
			 trapezoid = base * (labsup(p_c-1,h_c)+labsup(p_c,h_c)) / 2.0d0
			 integral = integral+firm_prod_dp(h_c)* trapezoid
			 
             ! Equation (15), based on Envelope Thm:
			 profit_max(p_c,h_c) = profit_max(1,h_c) + integral
		   	 
			 ! Then, update the wage function for males using Envelope condition:
			 if (labsup(p_c,h_c)>0.0d0) then
                 call wage_update(wage_pol_new(p_c,h_c))          
			 else
				 wage_pol_new(p_c,h_c) = wage_pol_new(p_c-1,h_c)
			 endif
			 
			 ! make sure that the wage policy is not decreasing!
			 if (wage_pol_new(p_c,h_c)<=wage_pol_new(p_c-1,h_c) ) then
				 write(*,*) "p_c,h_c,wage(p-1)>=wage(p)",p_c,h_c,wage_pol_new(p_c-1,h_c), &
					 wage_pol_new(p_c,h_c)
				 ! Trick: we need strictly increasing wage policy
				 wage_pol_new(p_c,h_c) =wage_pol_new(p_c-1,h_c) +small
			 endif

        enddo !p_c 
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
    	do h_c = n_hours,n_hours ! Counterfactual only for FT
			profit_vec(h_c) = (profit_daily(p_c,h_c)-p_eps_bar(h_c))
        enddo ! h_c
        ! Counterfactual only for FT: no hours choice - must post FT job if active
        ! outputs are log_sum (scalar) and delta_job (dim: n_hours)
        log_sum = profit_vec(n_hours) ! Counterfactual: profit is only for FT
        delta_job = [0.0d0,0.0d0,1.0d0] ! Counterfactual:  post FT job if active
		if (verbose>=2) write(*,*) p_c,"log_sum",log_sum
		
		! Always consider activity choice.
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

    
    contains
    
        subroutine wage_update(wage_plus)
        implicit none
        
        ! PURPOSE: Update the wage policy. 
        ! Declare outputs:
        real(8), intent(out) :: wage_plus
        
        ! Declare local variables:
        real(8) :: rhs,h
        ! Note: we already know that labsup(p_c,h_c)>0
        h = fun_hours_wk(hours_grid(h_c)) 
        rhs = y_arr(p_c,h_c)/h-( profit_max(p_c,h_c)/(h*labsup(p_c,h_c)))
        wage_plus = rhs
        
    
        end subroutine wage_update
        
    end subroutine solve_firm
    !========================================================================!

end module mod_firm
    
