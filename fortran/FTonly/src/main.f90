!##############################################################################
!
! copyright: Alessandro Di Nola and Haomin Wang
!##############################################################################


program main

    use omp_lib
    use mod_globals 
    use mod_output
    use mod_numerical, only: grid, linspace, discretize_lognormal_AF,discretize_beta,&
    		discretize_norm, myerror, discretize_grid_beta
	use mod_solvemodel, only: solvemodel
    use solve_household
    use mod_distribution
    use mod_estimation
    use mod_utilities
	use simulated_annealing
	use mod_nelder_meade, only: nelder_meade_bnd, inf
	use toolbox, only: plot, execplot
    implicit none
    
    !===============================================================
    ! DECLARATION
    !===============================================================
    
    integer:: istat,p_c
    integer :: ierr, unitno, flag, randseed
    real(8) :: par_vec(n_params),par_opt(n_params),Delta_act_bench
    real(8),allocatable :: LB(:),UB(:)
    real(8):: x,sa_factor
    character(len=80) :: err_msg
    character(len=128) :: savedir_cf_min_wage_1,fname_params_tex,fname_mom_tex,fname_ext_params_tex
	type(valfun)   :: V_inout
    type(valfun_s) :: V_inout_s
    logical :: flag_vfi_s, flag_dist_s
    real(8) :: t1,t2
	
	 namelist /params/ p_lambda_e,p_lambda_u,p_psi_m,p_psi_f,p_gamma_m,p_gamma_f,p_delta_m,p_delta_f, &
        	p_a,p_sigma_alpha,p_mu_alpha
	namelist /params_offer/ p_offer_a,p_offer_b,p_hpol_arr,p_offer_lb, p_offer_ub
	namelist /params_demand/ p_theta,p_eps_bar,p_sigma_eps

    !===============================================================
    ! EXECUTION
    !===============================================================
    ! Set up some flags
    do_estim     =2! 0 = solve for equilibrium
					 ! 1=estimate supply-side, 
    				 ! 2=solve supply-side and produce moments, 
    				 ! 3 = comparative statics of the supply side
    				 ! 4 = get productivity distribution given supply-side estimation
    				 ! 10 = export parameters and moments latex files
    method_estim = 1 ! (matters if do_estim == 1) 1 = nelder-mead; 2 = simulated annealing
	
	do_read_offer = .true. ! If true, offer distribution is read from external file and  
						   ! 	is not updated given offer distribution parameters.
						   ! In most cases, this should be false. 
						   ! This should only be set to true if do_estim = 0 or 2.
	if (do_estim /= 0 .and. do_estim /= 2) do_read_offer = .false.
	
	do_resetg   = .false.  ! true = reset g_inout each time fun_obj is called.
    do_genRand  = .false. ! true = generate rand numbers, false = read rand numbers from file
    
    verbose     = 0 !0/1/2 no/low/high verbosity in the code
    par_fortran = 1  !0/1 Use openMP or not
    par_fortran_g = 1 !0/1 Use openMP or not in distribution
    debug       = .false.  ! T/F Set it to false when running it on the server
    ! Options related to do_estim = 0 (solve for equilibrium)
    do_subgrid  = .true. ! T/F to do subgrid
    write_v_g   = .true. ! T/F to write value and distrib to txt file (slow)
	if (do_estim == 0 .or. do_estim == 1 ) write_v_g = .false.
    
    ! Counterfactuals
    do_eq_min_wage = .true. ! t/f equilibrium minimum wage
    cf_min_wage = 0       !0 = default - no counterfactual. 
    					  !1 = benchmark offer distribution, with min wage and I_mw
    cf_penalty = 0 		  ! 0 = benchmark penalty, 1 = counterfactual penalty
	! cf_penalty is only relevant for do_eq_min_wage == .true. and min wage = 8.5
	if (do_eq_min_wage == .false.) cf_penalty = 0 
	! only one of the flags can be true: cf_min_wage and cf_penalty 
	if (cf_min_wage == 1 .and. cf_penalty == 1) then
		call myerror("Error in main: cf_min_wage and cf_penalty cannot be both true!")
	endif
    ! Debug: 
    do_nocouple = .true. ! Assume only single households
        
    ! Identify OS
    ! "#ifdef" is preprocessor directive. Must use "-fpp" in compilation. 
    ! No indention for preprocessor directive.
#ifdef __APPLE__
	myOS = 1
#endif
#ifdef _WIN64
	myOS = 2
#endif
     ! Define output directory
    ! Folder structure: 
    ! ROOT
    ! - exe: where exec is stored
    ! - output: where results should be stored
    ! - src: where fortran source codes are stored
    if (myOS==1) then
        !MAC
	     savedir='output/'
         inputdir='input/'
		 savedir_bench = 'output_benchmark/'
		 savedir_cf_min_wage_1 = 'output_cf_min_wage_1/'
    elseif (myOS==2) then
        !WINDOWS
		savedir  = 'output\'
        inputdir = 'input\'
        savedir_bench = 'output_benchmark\'
        savedir_cf_min_wage_1 = 'output_cf_min_wage_1\'
    else
        savedir='output/'
        inputdir='input/'
		savedir_bench = 'output_benchmark/'
		savedir_cf_min_wage_1 = 'output_cf_min_wage_1/'
	endif
    
    if (verbose==2) write(*,*) "Hello World!"
    if (verbose==2) write(*,*) myOS
    
    if (verbose==2) write(*,*) "Before openMP.."
    
    !$omp parallel if (par_fortran==1)
    if (verbose>=1) write(*,*) "Parallel hello to you!"
    !$omp end parallel

    if (debug) pause
    
    if (verbose>=1) then
        write(*,*) "Grid sizes:"
        write(*,'(X,a,I3)') "n_prod   = ", n_prod
        write(*,'(X,a,I3)') "n_hours  = ", n_hours
        write(*,'(X,a,I3)') "n_kids   = ", n_kids
        write(*,'(X,a,I3)') "n_w_fine = ", n_w_fine
        write(*,*)          "do_subgrid = ", do_subgrid
        if (debug) pause
    endif

	
    ! Estimation vs solution
    if (do_estim==0) then
    	! Initialize parameters
    	call init_params()
    	! Read parameters from external file
		open(newunit = unitno,file = trim(savedir)//'estim_params.txt',iostat=ierr,action='read')
		read (unit=unitno,nml=params,iostat=ierr)
		read (unit=unitno,nml=params_demand,iostat=ierr)
   	   	close(unitno)
    	! Read and unpack demand-side parameters 
    	!	Note that read_params_demand is a subroutine in mod_output.
    	!	The vector par_vec_demand stores transformed parameter values.
    	! Read productivity distribution (prod_grid and prod_PMF)
    	call read_prod_dist()
    	! Inialize wage grids and offer distribution (w_grid, F_prob, hpol)
    	call init_wage_dist()
    	if (do_read_offer) then
   	   		call read_wage_dist(trim(savedir)//'F_prob_eq.txt', &
   	   			 trim(savedir)//'w_grid_eq.txt',trim(savedir)//'hpol_eq.txt' )
			
   	   	endif
    	! Initialization of V and g are done inside solvemodel
        ! solve for equilibrium
        call solvemodel()
    elseif (do_estim==1 .or. do_estim==2 .or. do_estim==3) then
        ! estimate supply-side
        write(*,*) "Solving supply-side only."
        ! Number of simulated households
    	! Based on SOEP 2009-2018, among those age 25-54 with 12 years or less education, 
    	! 	55.4% are single, and 44.6% are married
    	! If there are 5540 single households, there should be 2230 couple households
    	if (do_nocouple) then
    		N_sim_couple = 0
    		N_sim_single = 10000
    	else
	    	N_sim_couple = 2230
	    	N_sim_single = 5540
	    endif
		
        ! Initialize data moments and weights 
        call init_mom_data()
        call init_weight()
        !===============  Generate Random Numbers for Simulation  ===============!
		allocate(randmat0(N_sim_couple+N_sim_single,2))
		allocate(randmat1(N_sim_couple+N_sim_single,max_counter*6),stat=ierr)
		if (ierr/=0) then
			call myerror("Allocation of randmat1 failed!")
		endif

		if (verbose>=1) write(*,*) "do_genRand is ",do_genRand
		if (do_genRand) then
			randseed=3085113 
			call initSeed_dhw(randseed) ! this should not be called more than one time
		
			call randMat_dhw(randmat0) ! Generate random numbers for initialization
			call randMat_dhw(randmat1) ! Generate random numbers for simulation
			!! Save random matrices in external files
			call writedim(randmat0,trim(inputdir)//'randmat0.txt')
			call writedim(randmat1,trim(inputdir)//'randmat1.txt')
		else ! read random numbers from file

			!! Save random matrices in external files
			call readdim(randmat0,trim(inputdir)//'randmat0.txt')
			call readdim(randmat1,trim(inputdir)//'randmat1.txt')
		endif 
        
       
        ! initialize variables 
    	call init_params()
    	! Read parameters from external file
		open(newunit = unitno,file = trim(savedir)//'estim_params.txt',iostat=ierr,action='read')
		read (unit=unitno,nml=params,iostat=ierr)
   	   	close(unitno)
    	! initialize distributions
    	call initialize_g()
		! Initialize offer distribution
		
		call init_offer()
		! Read parameters from external file
		open(newunit = unitno,file = trim(savedir)//'estim_params.txt',iostat=ierr,action='read')
   		read (unit=unitno,nml=params_offer,iostat=ierr)
   	   	close(unitno)
   
   		if (do_read_offer) then
			if ( cf_min_wage == 0) then
				call read_wage_dist(trim(savedir)//'F_prob_eq.txt', &
					 trim(savedir)//'w_grid_eq.txt',trim(savedir)//'hpol_eq.txt' )
			elseif ( cf_min_wage == 1) then
				call read_wage_dist(trim(savedir_bench)//'F_prob_eq.txt', &
					 trim(savedir_bench)//'w_grid_eq.txt',trim(savedir_bench)//'hpol_eq.txt' )
				savedir = savedir_cf_min_wage_1
			endif
			! Normalize hpol such that in the benchmark, sum_h hpol(h) = 1
			if (do_eq_min_wage == .false.) then ! benchmark
				hpol = hpol/sum(hpol)
			else ! min wage CF
				call readscalar(Delta_act_bench,trim(savedir_bench)//'Delta_act_bench.txt')
				hpol = hpol/Delta_act_bench
			endif
   	   	endif
        
         ! Define output file names
        file_est_distance = trim(savedir)//'est_distance.txt'
        open(newunit=unitno, file=file_est_distance, status='replace')
        close(unitno)
        file_est_moments  = trim(savedir)//'est_mom.txt'
        file_data_moments = trim(savedir)//'data_mom.txt'
        file_fit_tex      = trim(savedir)//'model_fit.tex'
        
        ! Pack parameters into a vector (to be used as initial guess)
        call pack_params(par_vec)
        
        ! Pack bounds into an array (to be used in fun_obj)
        call pack_bounds(bounds_vec)
        ! write data moments to external file (txt_export is a subroutine in mod_estimation)
        call txt_export(file_data_moments,par_vec,mom_data)
		estim_distance = 1.0d10
        if (do_estim==1) then
        	! estim_distance is a global var storing the best distance
        	
        	if (method_estim == 1) then
				write(*,*) "Estimating supply-side parameters using nelder_meade_bnd"
				! Nelder Mead parameters
				! Start estimation using Nelder-Mead
				LB = bounds_vec(:,1)
				UB = bounds_vec(:,2)
				call nelder_meade_bnd(par_opt,x,flag,fun_obj,par_vec,LB,UB)
				par_vec = par_opt
				if (flag==-1) then
					write(*,*) "nelder_meade_bnd flag: Maximum iteration reached."
				elseif (flag==-2) then
					write(*,*) "nelder_meade_bnd flag: Error!!!"
				else
					write(*,*) "nelder_meade_bnd completed successfully!"
				endif 
			elseif (method_estim == 2) then
				write(*,*) "Estimating supply-side parameters using Simulated Annealing"
				sa_factor = 0.95d0
				! Set bounds
				do p_c = 1,n_params
					if (par_vec(p_c)>0.0d0) then
						bounds_vec(p_c,1) = 0.25d0*par_vec(p_c)
						bounds_vec(p_c,2) = 1.75d0*par_vec(p_c)
					elseif (par_vec(p_c)<0.0d0) then
						bounds_vec(p_c,1) = 1.75d0*par_vec(p_c)
						bounds_vec(p_c,2) = 0.25d0*par_vec(p_c)
					else ! parameter is exactly 0.0d0
						bounds_vec(p_c,1) = -1.0d-1
						bounds_vec(p_c,2) = 1.0d-1
					endif
				enddo
				
				! Start estimation using Simulated annealing
				call SA_minsearch(par_vec, x, bounds_vec(:,1), bounds_vec(:,2), fun_obj, sa_factor, file_est_distance)
			else 
				call myerror("main: method_estim out of bounds!")
			endif ! method_estim
			! solve supply-side and produce moments
			x = fun_obj(par_vec)
		elseif (do_estim==2) then
			write(*,*) "Calculating supply side moments given par_vec."

			! solve supply-side and produce moments
			t1 = omp_get_wtime()
			x = fun_obj(par_vec)
			t2 = omp_get_wtime()
			write(*,*) "Solve supply-side took",t2-t1,"seconds."
			
			
		elseif (do_estim == 3) then
			write(*,*) "Comparative statics of the supply side...under construction!"
			! Compute model around the parameter vector par_vec
			
		endif ! do_estim given supply side only (do_estim = 1,2,or 3)
	elseif (do_estim == 4) then
		write(*,*) "Demand side only: get productivity grid"
		! Define output file names
        file_demand_distance = trim(savedir)//'demand_distance.txt'
        open(newunit=unitno, file=file_demand_distance, status='replace')
        close(unitno)
		file_demand_moments = trim(savedir)//'demand_mom.txt'
		
		! initialize variables, grids 
    	call init_params()
    	! Read parameters from external file
		open(newunit = unitno,file = trim(savedir)//'estim_params.txt',iostat=ierr,action='read')
		read (unit=unitno,nml=params,iostat=ierr)
		read (unit=unitno,nml=params_demand,iostat=ierr)
   	   	close(unitno)

		

		!---Step (0): Initializations
			! Initialize F_prob,hpol, w_grid
		call read_wage_dist(trim(savedir)//'F_prob.txt', trim(savedir)//'w_grid.txt',trim(savedir)//'hpol.txt' )
	
		
		! Allocate V_inout, V_inout_s
		call initialize_V(V_inout,V_inout_s)
		! Allocate g
		call initialize_g()
        ! Compute value functions and distributions
			!---Value Function Iteration---!		
			! VFI for single households
			call vfi_singles(V_inout_s,flag_vfi_s)
			if (flag_vfi_s == .false. ) then
				call myerror('VFI for singles did not converge!')
			endif
			! Distributions of singles	
			call acceptance_rules_s(V_inout_s)
			call get_distribution_s(g_inout_s,flag_dist_s)

			if (flag_dist_s==.false.) then
				call myerror('Distribution G (singles) did not converge!')
			endif
		call get_prod_grid()
    	
    	
    	! Write productivity distribution to file
    	call writedim(prod_grid,trim(savedir)//'prod_grid.txt')
    	
	elseif (do_estim == 10) then
		! Write parameters and moments to latex files

    	! Initialize parameters
    	call init_params()
    	! Read parameters from external file
		open(newunit = unitno,file = trim(savedir)//'estim_params.txt',iostat=ierr,action='read')
		read (unit=unitno,nml=params,iostat=ierr)
		read (unit=unitno,nml=params_demand,iostat=ierr)
   	   	close(unitno)
   	   	
   	   	call init_offer()
		! Read parameters from external file
		open(newunit = unitno,file = trim(savedir)//'estim_params.txt',iostat=ierr,action='read')
   		read (unit=unitno,nml=params_offer,iostat=ierr)
   	   	close(unitno)
   	   	
   	   	fname_params_tex = trim(savedir)//'est_params.tex'
   	   	fname_params_tex = trim(fname_params_tex)
   	   	fname_ext_params_tex = trim(savedir)//'external_params.tex'
   	   	fname_ext_params_tex = trim(fname_ext_params_tex)
   	   	fname_mom_tex = trim(savedir)//'est_mom.tex'
   	   	fname_mom_tex = trim(fname_mom_tex)
   
   	   	call tex_export_params(fname_params_tex,fname_ext_params_tex)

    else
        call myerror("Flag <do_estim> not specified correctly")
    endif ! end if <do_estim>

	if (verbose==2) write(*,*) "Program completed successfully!"




    contains
    
    !===============================================================  
    
  subroutine init_mom_data()
   implicit none 
    ! Declare local variables
    integer :: unitno,ierr,temp_i,temp_i2,temp_i3,i,q_c
    real(8) :: temp_r1,temp_r2

    ! This subroutine updates mom_data, which is a global object
    ! Note: child state 1 = no kids, 2 = have kids <=6 yo, 3 = kids >6yo only

	! Read hours distribution
    open(newunit=unitno, file=trim(inputdir)//'SOEP_empdist_gc.txt', status='old', iostat=ierr)
    if (ierr/=0) then
        call myerror("Error in init_mom_data: cannot open SOEP_empdist_gc.txt")
    endif
    do i = 1,(1+n_hours)*n_kids*2
        read(unitno,*) temp_i,temp_i2,temp_i3,temp_r1,temp_r2
		if (temp_i3>0) then
        	mom_data%mom_empdist(temp_i3,temp_i2,temp_i) = temp_r1
		endif
    enddo
	
    close(unitno)
    
    ! Read wage percentiles
    open(newunit=unitno, file=trim(inputdir)//'SOEP_hwage.txt', status='old', iostat=ierr)
    if (ierr/=0) then
        call myerror("Error in init_mom_data: cannot open SOEP_hwage.txt")
    endif
    do i = 1,7*n_hours*2
        read(unitno,*) temp_i,temp_i2,temp_i3,temp_r1,temp_r2
        select case (temp_i3)
        	case (5)
        		q_c = 1
        	case (10)
        		q_c = 2
        	case (25)
        		q_c = 3
        	case (50)
        		q_c = 4
        	case (75)
        		q_c = 5
        	case (90)
        		q_c = 6
        	case (95)
        		q_c = 7
        	case default
        		call myerror("Error reading SOEP_hwage: percentile unrecognized!")
        end select

        mom_data%mom_wageqtl(q_c,temp_i2,temp_i) = temp_r1
    enddo
    close(unitno)
    
    ! Read lfs transitions
    open(newunit=unitno, file=trim(inputdir)//'SOEP_lfstran.txt', status='old', iostat=ierr)
    if (ierr/=0) then
        call myerror("Error in init_mom_data: cannot open SOEP_lfstran.txt")
    endif
    do i = 1,(n_hours+1)*(n_hours+1)
        read(unitno,*) temp_i,temp_i2,temp_r1,temp_r2
        mom_data%trans_mat(temp_i,temp_i2) = temp_r1
    enddo
    close(unitno)

    ! Check
    end subroutine init_mom_data
    !===============================================================  

    subroutine init_weight()
    implicit none 
    ! Declare local variables
    ! This subroutine updates weight, which is a global object
    
    	
    weight%mom_empdist = 2d0 
    weight%mom_wageqtl = 1d-1 
    weight%mom_wageqtl(2,:,:) = 0.5d0
    weight%mom_wageqtl(6,:,:) = 0.5d0
    weight%mom_wageqtl(3:5,:,:) = 2d0 
    
    ! Only job finding rates and separation rates get positive weights
    weight%trans_mat = 0d0 
    weight%trans_mat(1:3,0) = 2d1 
    
    
    
    end subroutine init_weight
    
    !===============================================================  
    subroutine init_params()
    	! This subroutine initializes model parameters and objects

        implicit none
        ! Declare input/output
        ! NONE
		! This subroutine uses global flags do_eq_min_wage 
        ! Declare local variables
        integer:: i,ierr,unitno,temp_i,temp_i2
        real(8) :: temp_r,frac_mar,temp_r2
        character(len=256) :: temp_ch

       
        !-------EXOGENOUS OBJECTS-------!
        
        ! Set minimum wage
        if (do_eq_min_wage) then
            p_min_wage = 13.0d0
        else
            p_min_wage = 0.0d0
        endif
		
		p_kappa1 = 2.0d0
		if (cf_penalty == 0) then ! benchmark penalty
			p_kappa0 =  0.57d0/(8.5d0**p_kappa1)  !0.5d0/(8.5d0**p_kappa1) 
		elseif (cf_penalty == 1) then ! counterfactual: twice as high as benchmark
			p_kappa0 = 2.0d0*0.57d0/(8.5d0**p_kappa1)
		else 
			call myerror("Error in init_params: cf_penalty out of bounds!")
		endif
    	! Spousal income vector
    	spinc_vec = 0.0d0
    	! Read spinc_vec
		open(newunit=unitno, file=trim(inputdir)//'SOEP_spinc_vec.txt', status='old', iostat=ierr)
		if (ierr/=0) then
			call myerror("Error in init_params: cannot o pen SOEP_spinc_vec.txt") 
		endif
		do i = 1,n_spinc-1
			read(unitno,*) temp_i,temp_r
			spinc_vec(temp_i) = temp_r
		enddo	
		close(unitno)
		
		spinc_dist = 0.0d0
    	! Read fraction of married individuals
		open(newunit=unitno, file=trim(inputdir)//'SOEP_mar.txt', status='old', iostat=ierr)
		if (ierr/=0) then
			call myerror("Error in init_params: cannot open SOEP_mar.txt") 
		endif
		read(unitno,*) temp_ch,frac_mar
		close(unitno)
		spinc_dist(1,:) = 1.0d0-frac_mar ! share of unmarried individuals
		
		! Read spinc_dist
		open(newunit=unitno, file=trim(inputdir)//'SOEP_spinc_dist.txt', status='old', iostat=ierr)
		if (ierr/=0) then
			call myerror("Error in init_params: cannot open SOEP_spinc_dist.txt") 
		endif
		do i = 1,2*(n_spinc-1)
			read(unitno,*) temp_i,temp_i2,temp_r
			spinc_dist(temp_i2,temp_i) = temp_r*frac_mar
		enddo	
		close(unitno)
		
		! Check results
    	! Hours: median observed weekly hours from SOEP	
        hours_grid = [11.0d0,25.0d0,40.0d0] 
        ! Tranform to annual hours
        hours_grid = hours_grid/7.0d0*365.0d0

        ! 	Other parameters - fixed
    	if (do_nocouple) then 
    		p_N_couple = 0.0d0
    	else
	    	p_N_couple = 1.0d0
	    endif
    	p_N_single = 1.0d0
    	p_rho = 0.04d0
    	p_rho_d = 0.033d0! Annual "death" rate
    	
    	! Child state transition rate
    	p_phi = 0.0d0
    	! Read child transition (annual prob in data, need to convert to rate in cont. time)
		open(newunit=unitno, file=trim(inputdir)//'SOEP_child.txt', status='old', iostat=ierr)
		if (ierr/=0) then
			call myerror("Error in init_params: cannot open SOEP_child.txt") 
		endif
		! Row 1: married
		read(unitno,*) temp_ch,temp_r,temp_r2
		p_phi(1,:,2) = -log(1.0d0-temp_r) ! from state 1 to state 2
		p_phi(2,:,2) = -log(1.0d0-temp_r2) ! from state 2 to state 3
		! Row 2: single, male
		read(unitno,*) temp_ch,temp_r,temp_r2
		p_phi(1,1,1) = -log(1.0d0-temp_r) ! from state 1 to state 2
		p_phi(2,1,1) = -log(1.0d0-temp_r2) ! from state 2 to state 3
		! Row 3: single, female
		read(unitno,*) temp_ch,temp_r,temp_r2
		p_phi(1,2,1) = -log(1.0d0-temp_r) ! from state 1 to state 2
		p_phi(2,2,1) = -log(1.0d0-temp_r2) ! from state 2 to state 3
		close(unitno)
		
		! Check results
    	
    	!-----INITIALIZE SUPPLY-SIDE PARAMETERS-----!
    	!	Parameters to be estimated
    	p_lambda = 2.2d0
    	! Unpack parameters
    	p_lambda_u = p_lambda ! only p_lambda is estimated
		p_lambda_e = p_lambda ! only p_lambda is estimated
		p_psi_m   = [1.0d0,1.0d0,1.0d0] ! 
		p_psi_f   = [4.0d0,10.0d0,6.0d0]
		p_gamma_m  = [2.0d0,2.0d0,2.00d0]
		p_gamma_f  = [3.0d0,4.0d0,3.5d0]
		p_delta = [0.15d0, 0.03d0, 0.01d0]   ! Mini-jobs have higher separation rates
		p_delta_m = p_delta
		p_delta_f = p_delta
    	! Preference shock parameter
    	p_sigma_alpha = 1.0d0 !1.0d0
		p_mu_alpha = 0.0d0 ! mean disutility of working fulltime 
		p_a = [1.1d0,1.0d0,0.9d0] 

    	! Demand side parameters
	
    	p_sigma_eps  = 0.01d0
    	sigma_eps_ub = 500d0
	    p_eps_bar   = [0.0d0,-10.0d0,0.0d0]
        
        ! If p_theta is large, profit per firm will be extremely large for high prod firms. 
        ! This will cause the program to fail due to infinity values in solving for Delta_new
        p_theta = [0.1d0,0.15d0, 1.0d0]

		
		! - Initialize bounds for supply side parameters to be estimated
	
		! scalars
		lb_lambda = 0.5d0
		ub_lambda = 10.0d0
		! dim(1:3)
		lb_delta = 0.001d0
		ub_delta = 0.5d0	
		! dim(1:3)
		lb_psi_m = 0.01d0
		ub_psi_m = 1.0d4
		lb_psi_f = 0.01d0
		ub_psi_f = 1.0d4
		lb_gamma_m = 1.1d0
		ub_gamma_m = 10.0d0
		lb_gamma_f = 1.1d0
		ub_gamma_f = 10.0d0
		lb_mu_alpha = 0.0d0
		ub_mu_alpha = 50.0d0
		lb_a = 0.1d0
		ub_a = 10.0d0
		
    	!Allocate arrays
		allocate(prod_grid(n_prod),prod_PMF(n_prod),&
            F_prob(n_w,n_hours),hpol(n_hours),&
            w_grid(n_w,n_hours), y_arr(n_prod,n_hours),&
            wage_pol(n_prod,n_hours),Delta(n_prod,n_hours),&
            w_fine_grid(n_w_fine,n_hours),labsup_1(n_w_fine,n_hours),&
            stat=istat,errmsg=err_msg)
		if (istat/=0) then
			print *, err_msg
            pause
			stop
        endif
               
       
        ! Initialize wage fine grids and labor supply for firm 1 on the fine grids
        w_fine_grid = 0.0d0
        labsup_1    = 0.0d0
        
                
    end subroutine init_params

    
    !===============================================================   
	
    subroutine init_offer()
    	! This subroutine initilizes the parametric offer distribution for supply-side estimation.
    	!	Note that when do_estim=0 (solve equilibrium option), the offer distribution
    	!	will be initialized inside the solvemodel module. 
    	integer:: h_c
    	
    	
    	! Check if do_estim is set for supply-side estimation or calibration
     	if (do_estim/=1 .and. do_estim/=2 .and. do_estim/=3 .and. do_estim/=10) then
     		call myerror("Error in init_offer: do_estim is invalid.")
     	endif
    	
    	! Initialize parameters in the parametric hourly wage distributions
		p_offer_a = [1.0d0, 1.0d0, 1.0d0]
		p_offer_b = [2.0d0, 3.5d0, 3.0d0]

		
    	! Initialize p_hpol
    	!  Normalize p_hpol_arr(3) to 1 !!!
    	p_hpol_arr = [0.15d0,0.2d0,1.0d0]
 
       	! Initialize hpol_m and hpol_f according to the parameters p_hpol
       	do h_c = 1,n_hours
       		hpol(h_c) = p_hpol_arr(h_c)/sum(p_hpol_arr)
		enddo
		
		if (do_eq_min_wage .and. cf_min_wage==2) then
			! minimum wage is imposed and counterfactual: no ME!
			hpol(1) = 0.0d0
		endif
		
		
		! Initialize bounds of the parametric hourly wage distributions.
		! Set the bounds based on the wage distribution in data - must call init_mom_data first!!!

		! Note that bounds of the offer distribution are fixed: 
		!	They are not part of the supply-side estimation. 
		
		! lower bound
		p_offer_lb = [1.0d0,2.0d0,2.5d0]
		! upper bound
		p_offer_ub = [21.0d0,25.0d0,38.0d0]
 		! Check if upper bound is greater than the lower bound
		if (minval(p_offer_ub-p_offer_lb)<0.0d0) then
			call myerror("Error in initialize: p_offer_ub ill-defined!")
		endif
		
		! Also initialize the bounds for the offer parameters - used in fun_obj
		lb_offer_a = 0.01d0
		ub_offer_a = 10.0d0
		lb_offer_b = 0.5d0
		ub_offer_b = 20.0d0
		lb_hpol_arr = small
		ub_hpol_arr = 5.0d0
		
 		 
    end subroutine init_offer
    !===============================================================   
	
    subroutine read_wage_dist(fname_F_prob,fname_w_grid,fname_hpol)
    	! read_wage_dist reads F_prob, hpol,w_grid from external files
    	! The files containing F_prob, w_grid should be generated from the supply side estimation.
    	! This subroutine is used for demand side estimation, or model solution 
    	! (wage distributions are initial guesses).
    	! Inputs/outputs
    	character(len=*),intent(in) ::  fname_F_prob,fname_w_grid,fname_hpol

    	! Read offer distribution (from the supply side)
    	call readdim(F_prob,fname_F_prob)
    	call readdim(w_grid,fname_w_grid)
  		call readdim(hpol,fname_hpol)
		p_hpol_arr = hpol
    end subroutine read_wage_dist
    !===============================================================  
     
	subroutine init_wage_dist()
    	! init_wage_dist initializes the offer distribution based on results from demand
    	! side estimation. The initial offer distribution is used for model solution
    	! (do_estim=0).
    	! Local variables
    	real(8) :: prod_PMF(n_prod)
  		
  		! Read offer distribution (from the demand side)
  		call readdim(prod_PMF,trim(savedir)//'prod_PMF.txt')
    	call readdim(w_grid,trim(savedir)//'w_grid.txt')
		hpol = [0.0d0,0.0d0,1.0d0]
		F_prob=0.0d0
		F_prob(:,n_hours) = prod_PMF
		
		 
    end subroutine init_wage_dist
    !===============================================================  

    subroutine read_prod_dist()
    	! This subroutine read prod_grid and prod_PMF from external files, and pre-computes array for firm output y_arr.
    	! The files containing prod_grid and prod_PMF should be generated from the demand side estimation.
    	! This subroutine is used for model solution.
    	! Local variables
    	integer :: h_c,p_c
    	
    	! Read
    	call readdim(prod_grid,trim(savedir)//'prod_grid.txt')
    	call readdim(prod_PMF,trim(savedir)//'prod_PMF.txt')
		        
        !Pre-compute array for firm output
		do h_c = 1,n_hours
			do p_c = 1,n_prod
				y_arr(p_c,h_c) = firm_prod(prod_grid(p_c),h_c)
			enddo
		enddo
        
 		 
    end subroutine read_prod_dist
    
       !===============================================================   
!     
    
    
    
    
    
    
    
    
end program main
