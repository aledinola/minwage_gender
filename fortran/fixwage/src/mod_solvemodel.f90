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
    public solvemodel_fixwage
    
    contains 

	!-----------------------------------------------------------------------
    subroutine solvemodel_fixwage()
    
    use solve_household, only: vfi, vfi_singles, acceptance_rules, acceptance_rules_s
    use mod_distribution, only: get_distribution, get_distribution_s, labor_supply
    use mod_firm, only: solve_firm
    use mod_numerical, only: myerror, linspace
    implicit none
    
    ! Local variables
    integer :: istat, ierr, unitno, ii,t2,t1
    integer :: h_c, p_c, iter
    real(8) :: err_tot, err_wage, err_delta, check_tol,Delta_act_bench,wage_full_compliance,a_min
    character(len=80) :: err_msg
    type(valfun)   :: V_inout,V0_inout
    type(valfun_s) :: V_inout_s,V0_inout_s
    type(gdist)    :: g0_inout
    type(gdist_s)  :: g0_inout_s
    ! - Update for w(p,h,j), j is gender - wage function
    real(8), allocatable :: wage_pol_new(:,:) 
    real(8), allocatable :: wage_pol_new_vec(:)
    integer :: p_mw_idx(n_hours), p_hat_idx(n_hours)
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
    	write(*,*) "solvemodel_fixwage: Finished initialization of V and g!"
   	 	pause
    endif
	
    !================ GUESS WAGE FUNCTION AND DELTA ================!
    ! Find the index of the first firm that pays a fully compliant wage given the fixed wage rule. 
    call fixed_wage_mw_idx(p_mw_idx)
    ! Initialize p_hat_idx (first firm that pays the minimum wage rather than the non-compliant fixed wage) to be 1 for all h_c. We will update it in the big loop.
    p_hat_idx = max(p_mw_idx-1,1)
	
	! Sector choice must be consistent with hpol, F_prob, and prod_PMF
	do h_c = 1,n_hours
        ! Firms paying non-compliant fixed wage
        do p_c = 1,p_hat_idx(h_c)-1
            wage_pol(p_c,h_c) = fun_fixwage(prod_grid(p_c),h_c)
            Delta(p_c,h_c) = hpol(h_c)*F_prob(p_c,h_c)/prod_PMF(p_c)
        enddo
        a_min = min(1.0d0,p_a(h_c))
        wage_full_compliance = p_min_wage/a_min
        ! Firms paying minimum wage
        do p_c = p_hat_idx(h_c),p_mw_idx(h_c)-1
            wage_pol(p_c,h_c) = wage_full_compliance
            Delta(p_c,h_c) = hpol(h_c)*F_prob(p_c,h_c)/prod_PMF(p_c)
        enddo 
        ! Firms paying compliant fixed wage
        do p_c = p_mw_idx(h_c),n_prod
            wage_pol(p_c,h_c) = fun_fixwage(prod_grid(p_c),h_c)
            Delta(p_c,h_c) = hpol(h_c)*F_prob(p_c,h_c)/prod_PMF(p_c)
        enddo
	enddo
    !---Update wage grid---!
	w_grid = wage_pol
    if (verbose>=2) then	
    	write(*,*) "solvemodel_fixwage: Finished initial guess of wage and Delta!"
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
   		write(*,*) "solvemodel_fixwage: Finished updating F and Delta!"
    	pause
    endif 

	! Normalize hpol so that sum_h hpol(h) = 1 in the benchmark
     if (do_eq_min_wage == .true.) then ! min wage counterfactual 
         hpol = hpol/Delta_act_bench
     else ! Benchmark economy
         call writescalar(sum(hpol),trim(savedir)//'Delta_act_bench.txt')
         hpol = hpol/sum(hpol)
    endif 
    
	!Solving firm problem gives new wage and choice prob functions
	! Reset value functions and distributions
 	V_inout   = V0_inout
    V_inout_s = V0_inout_s
    g_inout   = g0_inout
    g_inout_s = g0_inout_s
    
    ! Allocate and initialize value functions
	! Allocate and initialize distribution g_inout
	call solve_firm(V_inout,V_inout_s,g_inout,g_inout_s,wage_pol, wage_pol_new,Delta_new, p_mw_idx, p_hat_idx)

	!Write new wage function to file:
    call writedim(wage_pol_new,trim(savedir)//'wage_pol_new.txt')
    !Write new Delta function (optimal sector choice to file:
    call writedim(Delta_new,trim(savedir)//'Delta_new.txt')

    !Update w(p) and \Delta(h,gender;p)
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
    write(*,*) "p_mw_idx = ",p_mw_idx
    write(*,*) "p_hat_idx = ",p_hat_idx
    write(*,*) "p_kappa0/(0.57d0/(8.5d0**p_kappa1)) = ",p_kappa0(1)/(0.57d0/(8.5d0**p_kappa1))
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
        
    end subroutine solvemodel_fixwage
    !========================================================================!
    
    subroutine fixed_wage_mw_idx(p_mw_idx)
    ! This subroutine finds the index of the first firm that pays a fully compliant wage given the fixed wage rule. 
    implicit none
    integer, intent(out) :: p_mw_idx(n_hours)
    integer :: h_c, p_c
    real(8) :: a_min, wage_full_compliance
    
    do h_c = 1,n_hours
        a_min = min(1.0d0,p_a(h_c))
        wage_full_compliance = p_min_wage/a_min
        p_mw_idx(h_c) = n_prod + 1
        do p_c = 1,n_prod
            if (fun_fixwage(prod_grid(p_c),h_c)>=wage_full_compliance) then
                p_mw_idx(h_c) = p_c
                exit
            endif
        enddo
    enddo
    
    end subroutine fixed_wage_mw_idx
    !========================================================================!
    
end module mod_solvemodel
