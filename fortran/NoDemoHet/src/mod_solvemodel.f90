module mod_solvemodel
    
    !=======================================================================
    !        Module Solve Model
    ! Here we solve the model finding fixed point over wage and sector choice 
    ! functions in bigloop. We use a subgrid to solve profit maximization 
    ! for the first grid point and then we exploit the envelope condition.
    !=======================================================================
	!=======================================================================
    
    use mod_globals
    use mod_utilities 
    use omp_lib
	implicit none
    
    private
    public solvemodel
    
    contains 
    !-----------------------------------------------------------------------
    subroutine solvemodel()
    
    use solve_household, only: vfi, vfi_singles, acceptance_rules, acceptance_rules_s
    use mod_distribution, only: get_distribution, get_distribution_s, labor_supply
    use mod_firm, only: solve_firm
    use mod_numerical, only: myerror, linspace
    implicit none
    
    ! Local variables
    logical :: flag_vfi, flag_vfi_s, flag_dist, flag_dist_s
    integer :: istat, ierr, unitno, ii,t2,t1,t2_fine,t1_fine
    integer :: h_c, p_c, iter
    real(8) :: err_tot, err_wage, err_delta, y_max, y_min, check_tol,temp_real,wmin,Delta_act_bench
    character(len=80) :: err_msg
    type(valfun)   :: V_inout,V0_inout
    type(valfun_s) :: V_inout_s,V0_inout_s
    type(gdist)    :: g0_inout
    type(gdist_s)  :: g0_inout_s
	! - fine grid for p_c = 1
	integer :: h_fine_c, w_fine_c
    ! - Update for w(p,h,j), j is gender - wage function
    real(8), allocatable :: wage_pol_new(:,:) 
    real(8), allocatable :: wage_pol_new_vec(:)
    ! - Update for Delta(p,h) choice probability function
    real(8), allocatable :: Delta_new(:,:)   
    
    !=============== Initialize V, g, and allocate policy arrays ===============!
    ! Allocate and initialize value functions
    ! V_inout,V_inout_s are declared in mod_solvemodel (see above)
	call initialize_V(V_inout,V_inout_s)
	
	! Allocate and initialize distribution g_inout
    ! g_inout and g_inout_s are defined as globals in mod_globals
	call initialize_g()
    
    ! Initialize V0's and g0's (also defined in mod_solvemodel
	V0_inout_s = V_inout_s
	g0_inout_s = g_inout_s
    
    if (verbose>=2) then
    	write(*,*) "V0's and g0's initialized"
    	pause
    endif
    
    !Allocate arrays
	allocate(wage_pol_new(n_prod,n_hours),Delta_new(n_prod,n_hours),&
		 stat=istat,errmsg=err_msg)
	if (istat/=0) call myerror("Allocation of <wage_pol_new,...> failed")
    
    if (verbose>=2) then	
    	write(*,*) "solvemodel: Finished initialization of V and g!"
   	 	pause
    endif
	
    !================ GUESS WAGE FUNCTION AND DELTA ================!

   	! Initial guess for wage policy wage_pol and sector choice Delta
	if (do_eq_min_wage == .false.) then ! benchmark economy
		wage_pol = w_grid
	else
		! If an equilibrium minimum wage is imposed, redefine initial guess
		do h_c = 1,n_hours
			wmin = find_wmin(h_c) ! find_min is a function in mod_globals
			if (w_grid(1,h_c)< wmin) then
				temp_real = 1.0d0 ! (wmin/w_grid(1,h_c))**0.05d0
				wage_pol(:,h_c) = linspace(wmin,temp_real*w_grid(n_w,h_c),n_w)
			else
				wage_pol(:,h_c) = linspace(w_grid(1,h_c),w_grid(n_w,h_c),n_w)
			endif
		enddo
	endif
	
	! Sector choice must be consistent with hpol, F_prob, and prod_PMF
	do h_c = 1,n_hours
	do p_c = 1,n_prod
		Delta(p_c,h_c) = hpol(h_c)*F_prob(p_c,h_c)/prod_PMF(p_c)
	enddo
	enddo
    if (verbose>=2) then	
    	write(*,*) "solvemodel: Finished initial guess of wage and Delta!"
    	pause
    endif
	
    ! File 'share_act.txt' should have been created when estimating demand-side of benchmark economy. 
    !   See mod_estimation.f90.
    if (do_eq_min_wage == .true.) then ! min wage counterfactual 
        if (verbose>=2) write(*,*) "Reading benchmark share of active firms from file   ",trim(savedir_bench)//'share_act.txt'
        call readscalar(Delta_act_bench,trim(savedir_bench)//'Delta_act_bench.txt')
        if (verbose>=2) then 
                write(*,*) "Delta_act_bench = ", Delta_act_bench
                pause 
        endif
    endif ! (do_eq_min_wage == .true.) 

    !================ START BIG LOOP ================!
    
    ! Create file to store w(p) for each iteration of big loop
    open(newunit=unitno,file=trim(savedir)//"iter_wage.txt", form='formatted',status='replace',iostat=ierr)
    if (ierr/=0) then 
        call myerror("Before big loop: Cannot open file")
    endif
	close(unitno)
	
	open(newunit=unitno,file=trim(savedir)//"iter_err.txt", form='formatted',status='replace',iostat=ierr)
    if (ierr/=0) then 
        call myerror("Before big loop: Cannot open file")
    endif
	close(unitno)
    
    err_tot = 10.0d0
    
    ! Begin bigloop
    bigloop: do iter = 1,maxiter
    
    t1 = omp_get_wtime()   
    write(*,"(a)") "!===================================================!"
    write(*,"(a)") " "
	do h_c = 1,n_hours
		! For each h, integrate over p
		hpol(h_c) = sum(Delta(:,h_c)*prod_PMF(:))
	enddo
	call writedim(hpol,trim(savedir)//'hpol_eq.txt')
	
	!Update wage grid and offer distribution
	do h_c = 1,n_hours
		! Offer distribution for each hours-sector h_c
		F_prob(:,h_c)=prod_PMF*Delta(:,h_c)/hpol(h_c)
	enddo 
	call writedim(F_prob,trim(savedir)//'F_prob_eq.txt')
    	
	if (verbose>=2) then	
   		write(*,*) "solvemodel: Finished updating F and Delta!"
    	pause
    endif 

	! Normalize hpol so that sum_h hpol(h) = 1 in the benchmark
     if (do_eq_min_wage == .true.) then ! min wage counterfactual 
         hpol = hpol/Delta_act_bench
     else ! Benchmark economy
         call writescalar(sum(hpol),trim(savedir)//'Delta_act_bench.txt')
         hpol = hpol/sum(hpol)
    endif 

    if (do_subgrid) then
    ! Define wage subgrids for each h_c: each subgrid should run from a very small number to somewhere  
    !	between wage_pol(1,h) and wage_pol(2,h)
    do h_c = 1,n_hours 
    	y_min = 0.1d0 * wage_pol(1,h_c)
    	
    	if (do_eq_min_wage) then
    		wmin = find_wmin(h_c)
    		! If eq. min wage is imposed, lower bound of the find grid cannot go below min wage.
    		y_min = max(y_min,wmin)
    	endif
    	y_max = wage_pol(1,h_c)+0.95d0*(wage_pol(2,h_c) - wage_pol(1,h_c))
		write(*,*) "h_c",h_c,"y_max",y_max,"y_min",y_min
    	w_fine_grid(:,h_c) = linspace(y_min,y_max,n_w_fine)
    enddo !h_c
    call writedim(w_fine_grid,trim(savedir)//'w_fine_grid.txt')
    if (verbose>=2) write(*,*) "Entering loop over wage subgrid"
    ! Loop over the wage subgrid (one change at a time)
    do h_fine_c = 1,n_hours
        t1_fine = omp_get_wtime()
		
        do w_fine_c = 1,n_w_fine
            if (verbose>=0 .and. w_fine_c==1) then	
			    write(*,*) "subgrid h_fine_c",h_fine_c
		    endif
		
			! Reset value functions and distributions to those from the 
			!	previous iteration or the initial guess (if iter = 1)
 			V_inout   = V0_inout
 			V_inout_s = V0_inout_s
 			g_inout   = g0_inout
 			g_inout_s = g0_inout_s
			
			! Allocate and initialize value functions
			! Allocate and initialize distribution g_inout
			
			!---Update wage grid---!
			w_grid = wage_pol

			 ! adjust the appropriate wage for the lowest-p
			 w_grid(1,h_fine_c) = w_fine_grid(w_fine_c,h_fine_c)
			
			!---Compute Value functions, Acceptance Rules and Distribution---!	
			! VFI singles	
			 call vfi_singles(V_inout_s,flag_vfi_s)
			 if (flag_vfi_s == .false. ) then
				 call myerror("VFI for singles did not converge!")
			 endif
			 ! Distributions of singles	
			 call acceptance_rules_s(V_inout_s)
			 call get_distribution_s(g_inout_s,flag_dist_s)
			 if (flag_dist_s==.false.) then
				 call myerror("Distribution G of singles did not converge!")
			 endif
			 deallocate(omega_u_sf,omega_u_sm,omega_e_sf,omega_e_sm,Oh_e_sm,Oh_e_sf)
			
			if (do_nocouple == .false.) then
				! VFI couples
				 call vfi(V_inout,flag_vfi)
				 if (flag_vfi==.false.) then
					 call myerror("VFI for married couples did not converge!")
				 endif

				 ! Distributions of couples	
				 !Outputs are the omega sets, but we define them as globals		 
				 ! Note that omega's and Oh's are globals and allocated in acceptance_rules.
				 call acceptance_rules(V_inout)
				 call get_distribution(g_inout,flag_dist)
				 if (flag_dist==.false.) then
					 call myerror("Distribution G of married couples did not converge!")
				 endif
				 ! deallocate Omega's and Oh's 
				 deallocate(omega_uu1,omega_uu2, omega_eu1,omega_eu2, &
					 omega_ue2,omega_ue1,omega_ee1,omega_ee2,Oh_eu1,Oh_ue2, &
					 Oh_ee12,Oh_ee1,Oh_ee2,Oh_ee_minus,Oh_eu2_1stays,Oh_ue1_2stays, &
					 Oh_ee1_2stays,Oh_ee2_1stays)
			endif
			
			 !---Compute Labor Supply for p_c=1 (lapsup_1) ---!	
			 ! males
			 call labor_supply(g_inout,g_inout_s,1,h_fine_c, &
				labsup_1(w_fine_c,h_fine_c))
		
				
        enddo ! w_fine_c
        
        t2_fine = omp_get_wtime()
        if (verbose>=1 .and. w_fine_c==1) then	
        	write(*,"(a,I9)") " Time (seconds) = ", t2_fine-t1_fine
        endif
        
    enddo ! h_fine_c
    endif !endif flag for subgrid
    
    ! Write labor supply to file
    call writedim(labsup_1,trim(savedir)//'labsup_1.txt')
    
	!Solving firm problem gives new wage and choice prob functions
	! Reset value functions and distributions
 	V_inout   = V0_inout
    V_inout_s = V0_inout_s
    g_inout   = g0_inout
    g_inout_s = g0_inout_s
    
    ! Allocate and initialize value functions
	! Allocate and initialize distribution g_inout
	call solve_firm(V_inout,V_inout_s,g_inout,g_inout_s,wage_pol, wage_pol_new,Delta_new)

	!Write new wage function to file:
    call writedim(wage_pol_new,trim(savedir)//'wage_pol_new.txt')
    !Write new Delta function (optimal sector choice to file:
    call writedim(Delta_new,trim(savedir)//'Delta_new.txt')

    !Update w(p) and \Delta(h,gender;p)
	wage_pol_new = relax*wage_pol + (1.0d0-relax)*wage_pol_new
	Delta_new    = relax*Delta + (1.0d0-relax)*Delta_new
    
	!Compute error:
    check_tol = (sum(wage_pol(:,:))/dble(n_prod*n_hours))
    err_wage  = maxval(abs(wage_pol_new(:,:)-wage_pol(:,:)))/check_tol
	err_delta = maxval(abs(Delta_new-Delta))
	err_tot   = max(err_wage,err_delta)
	t2 = omp_get_wtime()
	
	! Write to screen
	write(*,*) "-----------------------------------"
	write(*,"(a,I3)") " Fixed-point iter num = ", iter
	write(*,"(a,I9)") " Time (seconds) = ", t2-t1
	write(*,'(a,f0.7)') " err_wage  = ", err_wage
	write(*,'(a,f0.7)') " err_delta = ", err_delta
	write(*,'(a,f0.7)') " err_tot   = ", err_tot
	write(*,*)          " check_tol = ", check_tol
	write(*,*) "-----------------------------------"
	
	open(newunit=unitno,file=trim(savedir)//"iter_err.txt",form='formatted',status='old',position='append',IOSTAT=ierr)
    if (ierr/=0) then 
        call myerror("Inside big loop: Cannot open file")
    endif
	write(unitno,*) "-----------------------------------"
	write(unitno,"(a,I3)") " Fixed-point iter num = ", iter
	write(unitno,"(a,I9)") " Time (seconds) = ", t2-t1
	write(unitno,'(a,f0.7)') " err_wage  = ", err_wage
	write(unitno,'(a,f0.7)') " err_delta = ", err_delta
	write(unitno,'(a,f0.7)') " err_tot   = ", err_tot
	write(unitno,*)          " check_tol = ", check_tol
	write(unitno,*) "-----------------------------------"
	close(unitno)
	if (verbose>=2) pause
	
    ! Save w(p) to a file for each iteration iter
    ! Reshape wage_pol_new as a long column vector
    wage_pol_new_vec = reshape(wage_pol_new,[size(wage_pol_new)]) 
    open(6155,file=trim(savedir)//"iter_wage.txt",form='formatted',status='old',position='append',IOSTAT=ierr)
    if (ierr/=0) then 
        call myerror("Inside big loop: Cannot open file")
    endif
    do ii=1,size(wage_pol_new_vec) 
    	write(6155,*) wage_pol_new_vec(ii)
    enddo	 
    close(6155)
    
    if (err_tot<tol_ge) then
		write(*,"(a,I3,a)") " Converged after ", iter," iterations."
		exit bigloop
    endif
    
    ! Update iteration objects
    wage_pol   = wage_pol_new
    Delta      = Delta_new
	V0_inout   = V_inout
	V0_inout_s = V_inout_s
	g0_inout   = g_inout
	g0_inout_s = g_inout_s
	
    enddo bigloop
    !==================END BIG LOOP==================================  
    
    !Write results on txt files
   
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
        
    end subroutine solvemodel
    
    
end module mod_solvemodel
