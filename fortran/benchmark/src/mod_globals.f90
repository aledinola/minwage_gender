module mod_globals
    use mod_numerical,only: myerror
    use mod_utilities 

    implicit none
    !=======================================================================
    !                     GLOBAL MODULE
    ! Here define type of parameters and arrays (as allocatable)
    ! Arrays are then assigned a numerical value (i.e. they are initialized)
    ! in the subroutine initialize()
	!=======================================================================
    
    ! OS
    integer :: myOS
    ! OS dependent output directory
    character(len=40) :: file_est_distance, file_est_moments, file_data_moments, &
    	file_demand_moments,file_demand_distance,file_fit_tex
    character(len=40) :: inputdir, savedir,savedir_bench
    !Global to control openMP
    integer :: par_fortran,par_fortran_g
    integer :: verbose
    logical :: do_nocouple,do_read_offer, do_eq_min_wage

    !=======================================================================
    !                   	SIMULATION				                        !	
    !=======================================================================
    ! Number of simulated households
    ! Based on SOEP 2009-2018, among those age 25-54 with 12 years or less education, 
    ! 	55.4% are single, and 44.6% are married
    ! If there are 5540 single households, there should be 2230 couple households
    integer :: N_sim_couple
	integer :: N_sim_single
	integer,parameter :: max_counter = 20 ! max number of spells to be simulated
	! number of simulated duration
	integer,parameter :: T_sim=3  !(in years)
	integer,parameter :: Tm_sim = 3*12  !(in months)
	! random matrices
	real(8),allocatable :: randmat0(:,:),randmat1(:,:)
    
    !=======================================================================
    !                     MODEL PARAMETERS 
    !=======================================================================
    
    ! - Number of parameters to be estimated
    integer, parameter :: n_params = 27
    integer, parameter :: n_params_demand = 3
    
    ! - Population measure
    real(8) :: p_N_couple,p_N_single
    
    ! - Hours vector (H-script in paper)
    !Notation: 1 = ME, 2 = PT, 3 = FT
    integer,parameter :: n_hours =3
    real(8) :: hours_grid(n_hours)
    ! monthly hours endowment
	real(8),parameter :: hbar = 80.0d0/7d0*365.0d0 ! maximum annual hours
	real(8),parameter :: weekhours = 52.0d0
	
	! - Spousal income and marital state
	! Notation: 1 = unmarried, 2 = married, spouse unemployed, 3-7 = married, spouse employed
	integer,parameter :: n_spinc = 6
	real(8) :: spinc_vec(n_spinc)
	real(8) :: spinc_dist(n_spinc,2) ! dim(n_spinc, genders)
	
    ! - TRANSITION RATES
    ! job offer arrival rate non-emp (n) / employed (e) male (m) / female (f)
    real(8) :: p_lambda_u,p_lambda_e,p_lambda  
    ! job separation rate male (m) / female (f)
    real(8) :: p_delta_m(n_hours), p_delta_f(n_hours)  , p_delta(n_hours)
    !discount rate (contin. time)
    real(8) :: p_rho  
    ! death rate 
    real(8) :: p_rho_d 
    ! kids state transition rates (couple, single male, single female)
    ! p_phi(1,g_c,m_c): transition from k=1 to k=2
    ! p_phi(2,g_c,m_c): transition from k=2 to k=3
    ! p_phi(3,g_c,m_c): transition from k=3, impossible so it is zero
    integer, parameter :: n_kids = 3
    real(8) :: p_phi(n_kids,2,2) ! dim: child state, gender, marital status
    
    
    ! - UTILITY FUNCTION
    ! scale parameters on preference for hours
    real(8) :: p_psi_m(n_kids),p_psi_f(n_kids)
    ! exponent parameters on preference for hours
	real(8) :: p_gamma_m(n_kids),p_gamma_f(n_kids)
	
    ! - PREFERENCE SHOCK
    real(8) :: p_sigma_alpha(n_kids,2),p_mu_alpha(n_kids,2)
 
    ! - FIRM PRODUCTION FUNCTION
    real(8) :: p_a(n_hours),p_theta(n_hours)
     
    ! - FIXED COSTS DISTRIBUTION
    real(8) :: p_sigma_eps  ! scale parameter of the job-posting cost distribution
    real(8) :: p_eps_bar(n_hours) ! mean cost for each type of jobs
    real(8) :: sigma_eps_ub  ! Upper bound for p_sigma_eps
    
   	! - Statutory minimum wage (hourly)
    real(8) :: p_min_wage  ! min wage
	real(8) :: p_kappa0(n_hours),p_kappa1 ! contact success rate parameter
	
	! - Offer distribution parameters (for estimation)
	real(8),allocatable :: p_offer_a(:),p_offer_b(:) 
    real(8) :: p_offer_lb(n_hours),p_offer_ub(n_hours) 
    
    ! Note: we normalize p_hpol_arr(3) to 1, and we have
    real(8),allocatable :: p_hpol_arr(:)
    
    ! - Tax functions parameters, annual basis
    !   Net income = p_tax0*(gross income)**p_tax1
    !   The parameters p_tax0,p_tax1 are conditional on marital status and kids status (kids=1,2,3)
    ! Based on GSOEP estimates.
    real(8), parameter :: p_tax0_couple(n_kids) = [exp(1.56d0), exp(4.62d0), exp(4.08d0)]
    real(8), parameter :: p_tax1_couple(n_kids) = [0.84d0, 0.56d0, 0.61d0]  
    real(8), parameter :: p_tax0_single(n_kids) = [exp(1.33d0), exp(5.05d0), exp(4.89d0)]
    real(8), parameter :: p_tax1_single(n_kids) = [0.86d0, 0.50d0, 0.52d0]  
    
    ! - NON-LABOR INCOME male (m) / female (f), annual basis
    !   Conditional on gender, marital status and kids status
    ! Based on GSOEP estimates.
    real(8), parameter :: p_b_mm(n_kids) = [697.1374d0, 697.1374d0+13.90d0, 697.1374d0] *12.0d0	! married man
    real(8), parameter :: p_b_mf(n_kids) =  [209.8875d0, 209.8875d0+101.9798d0, 209.8875d0]*12.0d0	! married female
    real(8), parameter :: p_b_sm(n_kids) =  [ 849.4531d0, 849.4531d0+13.90d0, 849.4531d0] *12.0d0	! single male
    real(8), parameter :: p_b_sf(n_kids) =  [772.131d0, 772.131d0+101.9798d0, 772.131d0]*12.0d0! single female
    
    ! - SOCIAL SECURITY tax rate
    !   Recall that minijob holders (hours = 1) do not pay social security tax
    real(8), parameter :: p_tax_ss = 0.0935d0!0.09d0 !0.15d0
    
    !=======================================================================
    !                     BOUNDS OF MODEL PARAMETERS 
    !=======================================================================
    real(8),dimension(1:n_hours) :: lb_offer_a,ub_offer_a,lb_offer_b,ub_offer_b, &
    	lb_delta, ub_delta,lb_a,ub_a,lb_hpol_arr,ub_hpol_arr
    real(8),dimension(1:n_kids) :: lb_psi_f,ub_psi_f,lb_psi_m,ub_psi_m, &
    	lb_gamma_f,ub_gamma_f,lb_gamma_m,ub_gamma_m
    real(8),dimension(n_kids,2) :: lb_mu_alpha,ub_mu_alpha
    real(8) :: lb_lambda,ub_lambda
    real(8) :: bounds_vec(n_params,2)

    !=======================================================================
    !                   GRIDS, COMPUTATION OBJECTS                         !	
    !=======================================================================

    
    type mom
        ! Targeted moments:
        real(8),dimension(n_hours,n_kids,2) :: mom_empdist ! hours by genders
        real(8),dimension(7,n_hours,2) :: mom_wageqtl ! 7 percentiles, hours,  genders
	    real(8),dimension(0:3,0:3) :: trans_mat
        ! Untargeted moments:
        real(8),dimension(0:3,0:3,2) :: trans_mat_j ! transition matrix by gender: 0=NE, 1=ME, 2=PT, 3=FT
        real(8),dimension(0:1,0:1,2) :: trans_emp_j ! employment transition matrix by gender: 0=NE, 1=E
    end type mom
    
     ! data moment and weight
    type(mom) :: mom_data,weight
    ! productivity grid specifications
    integer, parameter :: n_prod = 150
    integer, parameter :: n_w = 150
    ! wage subgrid
    integer,parameter :: n_w_fine = 25
    
    ! productivity grid and vector of productivity PMF:
    real(8), allocatable :: prod_grid(:), prod_PMF(:)
    
    ! Wage function w_{h}(p)
    real(8), allocatable :: wage_pol(:,:)
    ! Sector choice function. Dim: n_prod,n_hours
    real(8), allocatable :: Delta(:,:)
    
    !Offer distributions: F(w,h)
    real(8), allocatable :: F_prob(:,:)
    real(8) :: I_mw(n_w,n_hours,2)
    
    !Grids for wages
    ! wage grids: n_w by n_hours
    real(8), allocatable :: w_grid(:,:)
    ! wage subgrids: n_w_fine by n_hours
    real(8), allocatable :: w_fine_grid(:,:)
    
    
    !Preallocation of firm output by wage,hours 
    real(8),allocatable :: y_arr(:,:)
    
    !Hours choice prob (in paper: \Delta_h, conditional on hours)
    real(8), allocatable :: hpol(:)
    
   
    
    ! - Derived data for value functions MARRIED COUPLES
    type valfun
        real(8),allocatable :: V_uu(:) !states: (k)
        real(8),allocatable :: V_eu(:,:,:) !states: (w1,h1,k)
        real(8),allocatable :: V_ue(:,:,:) !states: (w2,h2,k)
        real(8),allocatable :: V_ee(:,:,:,:,:) !states: (w1,h1,w2,h2,k)
    end type valfun
    
    ! - Derived data for value functions SINGLES
    !   Four cases to consider: {sm,sf}*{u,e}
    type valfun_s
        real(8),allocatable :: V_u_sm(:,:) !states: (k,spinc)
        real(8),allocatable :: V_u_sf(:,:) !states: (k,spinc)
        real(8),allocatable :: V_e_sm(:,:,:,:) !states: (w,h,k,spinc)
        real(8),allocatable :: V_e_sf(:,:,:,:) !states: (w,h,k,spinc)
    end type valfun_s
    
    ! - Derived data for accepted offer distribution g
    type gdist
        real(8),allocatable :: uu(:) !states: (k)
        real(8),allocatable :: eu(:,:,:) !states: (w1,h1,k)
        real(8),allocatable :: ue(:,:,:) !states: (w2,h2,k)
        real(8),allocatable :: ee(:,:,:,:,:) !states: (w1,h1,w2,h2,k)
    end type gdist
    
    ! - Derived data for accepted offer distribution g of singles
    type gdist_s
        real(8),allocatable :: u_sm(:,:) !states: (k,spinc)
        real(8),allocatable :: u_sf(:,:) !states: (k,spinc)
        real(8),allocatable :: e_sm(:,:,:,:) !states: (w,h,k,spinc)
        real(8),allocatable :: e_sf(:,:,:,:) !states: (w,h,k,spinc)
    end type gdist_s
    ! Define distributions as globals
    type(gdist)    :: g_inout
    type(gdist_s)  :: g_inout_s
    
    ! - Acceptance probability DUAL SEARCHER (uu)
    real(8), allocatable :: omega_uu1(:,:,:) !(w',h',k)
    real(8), allocatable :: omega_uu2(:,:,:) !(w',h',k)
    
    ! - Acceptance probability SINGLE EARNERS (eu)
    real(8), allocatable :: omega_eu1(:,:,:,:,:) !(w',h',w1,h1,k)
    real(8), allocatable :: omega_eu2(:,:,:,:,:)  !(w',h',w1,h1,k)
    
    ! - Acceptance probability SINGLE EARNERS (ue)
    real(8), allocatable :: omega_ue2(:,:,:,:,:) !(w',h',w2,h2,k)
    real(8), allocatable :: omega_ue1(:,:,:,:,:)  !(w',h',w2,h2,k)
     
    ! - Acceptance probability DUAL EARNERS (ee)
    real(8), allocatable :: omega_ee1(:,:,:,:,:,:,:)  !(w',h',w1,h1,w2,h2,k)
    real(8), allocatable :: omega_ee2(:,:,:,:,:,:,:)  !(w',h',w1,h1,w2,h2,k)
        
    ! - Viability of job situations 
    logical, allocatable :: Oh_ee12(:,:,:,:,:) !(w1,h1,w2,h2,k)
    logical, allocatable :: Oh_ee1(:,:,:,:,:) !(w1,h1,w2,h2,k)
    logical, allocatable :: Oh_ee2(:,:,:,:,:) !(w1,h1,w2,h2,k)
    logical, allocatable :: Oh_ee_minus(:,:,:,:,:) !(w1,h1,w2,h2,k)
    logical, allocatable :: Oh_eu1(:,:,:)  !(w1,h1,k)
    logical, allocatable :: Oh_ue2(:,:,:) !(w2,h2,k)
    
    ! - Whether one stays when the partner accepts a job
    logical, allocatable :: Oh_ee2_1stays(:,:,:,:,:) !(w1,h1,w2,h2,k)
    logical, allocatable :: Oh_ee1_2stays(:,:,:,:,:) !(w1,h1,w2,h2,k)
    logical, allocatable :: Oh_eu2_1stays(:,:,:,:,:) !(w1,h1,w2,h2,k)
    logical, allocatable :: Oh_ue1_2stays(:,:,:,:,:) !(w1,h1,w2,h2,k)

    
    ! - Acceptance probability Singles
    real(8), allocatable :: omega_u_sf(:,:,:,:) !(w',h',k,spinc)
	real(8), allocatable :: omega_u_sm(:,:,:,:) !(w',h',k,spinc)    
    real(8), allocatable :: omega_e_sf(:,:,:,:,:,:) !(w',h',w,h,k,spinc)
	real(8), allocatable :: omega_e_sm(:,:,:,:,:,:) !(w',h',w,h,k,spinc)  
	! - Viability of job situations
	logical, allocatable :: Oh_e_sm(:,:,:,:) !(w1,h1,k,spinc)
	logical, allocatable :: Oh_e_sf(:,:,:,:) !(w2,h2,k,spinc)
	
    ! - Labor Supply on wage subgrids
	real(8), allocatable :: labsup_1(:,:) ! (w,h)
	
   !=======================================================================
   !                   HELPER PARAMETERS                                 !	
   !======================================================================
    real(8), parameter :: large_negative = -1.0d100 !-10.0d0**25
    real(8), parameter :: large_positive = 1.0d100 !10.0d0**25
    real(8), parameter :: small   = 1.0d-8
    
    integer :: do_estim,method_estim,cf_min_wage,cf_penalty
    logical :: debug,do_subgrid,do_genRand,do_resetg
    logical :: write_v_g

    real(8), parameter :: tolV         = 1.0d-4  ! Tolerance for value function (it can be looser than tol_dist)
    real(8), parameter :: tol_dist     = 1.0d-10  ! Tolerance for the distribution (please set it lower than 1e-5)
    integer, parameter :: maxitV       = 5000  ! Maximum number of iterations for the value function
    integer, parameter :: minitV       = 50    ! Minimum number of iterations for the value function 
    integer, parameter :: maxit_dist   = 5000 ! Maximum number of iterations for the distribution G
    integer, parameter :: minit_dist   = 50   ! Minimum number of iterations for the distribution G
    real(8), parameter :: tol_ge       = 0.002d0  !Tolerance for general equilibrium loop over wage and choice functions
    real(8), parameter :: relax        = 0.9d0    ! Relaxation or dampening parameter (if =0, no dampening)
    integer, parameter :: maxiter      = 100   !max number of iterations for big loop
    real(8), parameter :: adjust_g     = 1.0d0/6.0d0   ! Adjustment factor for couple distribution update
    real(8), parameter :: adjust_gs     = 1.0d0/6.0d0  ! Adjustment factor for single distribution update

    !Communication variables
    real(8) :: estim_distance
    !=================================================
    ! OMP DIRECTIVES (leftover from another code)
    !=================================================
    !stuff to be available to (sub)procedures and which may need to be varied across OpenMP threads
    !!!!$omp threadprivate(z_today,a_today,EVz)
    !!!$omp threadprivate(EVz)
    
    contains
    
!---------------------------------------------------
    subroutine initialize_V(V_inout,V_inout_s)
    implicit none
    ! input/output
    type(valfun),intent(out)   :: V_inout
    type(valfun_s),intent(out) :: V_inout_s
    
    ! For married couples:
	allocate(V_inout%V_uu(n_kids)) !states: (k)
	allocate(V_inout%V_eu(n_w,n_hours,n_kids)) !states: (w1,h1,k)
	allocate(V_inout%V_ue(n_w,n_hours,n_kids)) !states: (w2,h2,k)
	allocate(V_inout%V_ee(n_w,n_hours,n_w,n_hours,n_kids)) !states: (w1,h1,w2,h2,k)
	! For singles:
	allocate(V_inout_s%V_u_sm(n_kids,n_spinc)) !states: (k)
	allocate(V_inout_s%V_u_sf(n_kids,n_spinc)) !states: (k)
	allocate(V_inout_s%V_e_sm(n_w,n_hours,n_kids,n_spinc)) !states: (w,h,k)
	allocate(V_inout_s%V_e_sf(n_w,n_hours,n_kids,n_spinc)) !states: (w,h,k)
	
	V_inout%V_uu = 0.0d0
    V_inout%V_eu = 0.0d0
    V_inout%V_ue = 0.0d0
    V_inout%V_ee = 0.0d0
    
    V_inout_s%V_u_sm = 0.0d0
    V_inout_s%V_u_sf = 0.0d0
    V_inout_s%V_e_sm = 0.0d0
    V_inout_s%V_e_sf = 0.0d0
    
    end subroutine initialize_V
    
!---------------------------------------------------
    subroutine initialize_g()
    implicit none
    ! Declare local variables:
    real(8):: sum_tot,rescale
    real(8) :: sum_u_sf,sum_u_sm,sum_e_sf,sum_e_sm
    integer :: ierr,sp_c
    
    if (allocated(g_inout%uu)) deallocate(g_inout%uu)
    if (allocated(g_inout%ue)) deallocate(g_inout%ue)
    if (allocated(g_inout%eu)) deallocate(g_inout%eu)
    if (allocated(g_inout%ee)) deallocate(g_inout%ee)
    if (allocated(g_inout_s%u_sf)) deallocate(g_inout_s%u_sf)
    if (allocated(g_inout_s%u_sm)) deallocate(g_inout_s%u_sm)
    if (allocated(g_inout_s%e_sf)) deallocate(g_inout_s%e_sf)
    if (allocated(g_inout_s%e_sm)) deallocate(g_inout_s%e_sm)
    
    allocate(g_inout%uu(n_kids),& !states: (k)
	g_inout%eu(n_w,n_hours,n_kids),& !states: (w1,h1,k)
	g_inout%ue(n_w,n_hours,n_kids),& !states: (w2,h2,k)
	g_inout%ee(n_w,n_hours,n_w,n_hours,n_kids),& !states: (w1,h1,w2,h2,k)
	stat = ierr)
	if (ierr/=0) call myerror("initialize_g: Allocation of g failed")
	! allocate g_inout_s for singles
	allocate(g_inout_s%u_sf(n_kids,n_spinc),& !states: (k)
	g_inout_s%u_sm(n_kids,n_spinc),& !states: (k)
	g_inout_s%e_sf(n_w,n_hours,n_kids,n_spinc),& !states: (w,h,k)
	g_inout_s%e_sm(n_w,n_hours,n_kids,n_spinc),& !states: (w,h,k)
	stat = ierr)
	if (ierr/=0) call myerror("initialize_g: Allocation of g_s failed")
    
    ! initialize g for couples and singles
    g_inout%uu = 0.0d0
    g_inout%eu = 0.0d0
    g_inout%ue = 0.0d0
    g_inout%ee = 0.0d0 
    
    do sp_c = 1,n_spinc
	    g_inout_s%u_sf(:,sp_c) = spinc_dist(sp_c,2)
    	g_inout_s%u_sm(:,sp_c) = spinc_dist(sp_c,1)
    	g_inout_s%e_sf(:,:,:,sp_c) = spinc_dist(sp_c,2)
    	g_inout_s%e_sm(:,:,:,sp_c) = spinc_dist(sp_c,1)
   enddo
        
        
    ! Rescale g's so that
	! (couples:) total measure of households is p_N_couple
	! (singles:) total measure of households is p_N_single
    
 
    ! singles
	! single males
	sum_u_sm = sum(g_inout_s%u_sm)
	sum_e_sm = sum(g_inout_s%e_sm)
	sum_tot = (sum_u_sm+sum_e_sm)
	rescale = 0.5d0*p_N_single / sum_tot
	g_inout_s%u_sm = g_inout_s%u_sm * rescale
	g_inout_s%e_sm = g_inout_s%e_sm * rescale
	! single females
	sum_u_sf = sum(g_inout_s%u_sf)
	sum_e_sf = sum(g_inout_s%e_sf)
	sum_tot = (sum_u_sf+sum_e_sf)
	rescale = 0.5d0*p_N_single / sum_tot
	g_inout_s%u_sf = g_inout_s%u_sf * rescale
	g_inout_s%e_sf = g_inout_s%e_sf * rescale
    
    
    
    end subroutine initialize_g
!---------------------------------------------------

    subroutine initialize_F()
    use mod_numerical, only: discretize_beta,grid
	! This subroutine uses productivity parameters (p_offer...) 
	!  to initialize the offer distribution F_prob and the wage grids w_grid.
    implicit none
	
	integer:: h_c
	
	! Note:  F_prob, w_grid should be allocated already.

    
	! Hourly wage distribution for h_c>1
    !	Note: <discretize_beta> discretizes a beta pdf
    if (verbose>=2) write(*,*) "Setting up wage grids"
    do h_c = 1,n_hours
    	! males
    	call grid(w_grid(:,h_c),0.01d0,0.99d0,1.0d0)
    	call discretize_beta(p_offer_a(h_c),p_offer_b(h_c), w_grid(:,h_c),F_prob(:,h_c))
    	! rescale wage grid
    	w_grid(:,h_c)=w_grid(:,h_c)*(p_offer_ub(h_c)-p_offer_lb(h_c))+p_offer_lb(h_c)
    enddo
  

    
    
    end subroutine initialize_F
!---------------------------------------------------

    function contact_success_rate(wage,min_wage,h_c) result(F)
	! This function gives the contact success rate for job with hourly wage
	!	wage, given minimum wage min_wage
	! Inputs
	real(8),intent(in) :: wage,min_wage
	integer,intent(in) :: h_c
	! Output
	real(8) :: F
	
	if (wage<min_wage) then
		F = max(0.0d0, 1.0d0-p_kappa0(h_c)*(min_wage - wage)**p_kappa1)
	elseif (wage>=min_wage) then
		F = 1.0d0
	endif
	
	end function contact_success_rate
!---------------------------------------------------

    subroutine initialize_I_mw(I_mw,w_grid)
	! This subroutine sets I_mw: indicator if each point on the wage grid is above the hourly min wage.
    ! This is called in module solve_household
    implicit none
	! Inputs and outputs
	real(8),dimension(:,:,:), intent(out) :: I_mw
	real(8),dimension(:,:), intent(in) :: w_grid
    ! Local variables
    real(8),allocatable :: f_effective(:,:)
	integer :: h_c,w_c,g_c
	
    if (verbose>=2) write(*,*) "Setting up I_mw"
    if (do_eq_min_wage == .true.) then
		do h_c = 1,n_hours
			do w_c = 1,n_w
			! males
				I_mw(w_c,h_c,1) = contact_success_rate(w_grid(w_c,h_c),p_min_wage,h_c)
		
			! females
                    I_mw(w_c,h_c,2) = contact_success_rate(w_grid(w_c,h_c)*p_a(h_c),p_min_wage,h_c)
			enddo
		enddo
	else ! no min wage imposed
		I_mw = 1.0d0
	endif
  	
  	! I_mw has to be between zero and one
  	I_mw = min(max(I_mw,0.0d0),1.0d0)
    
    
    ! Output
    call writedim(I_mw,trim(savedir)//'I_mw.txt')
    
    ! Calculate the effective job contact rate by gender and hours
    ! F_prob dim(n_w,n_hours), I_mw dim(n_w,n_hours,2)
    ! effective contact rate: f_effective dim(n_hours,2)
    allocate(f_effective(n_hours,2))
    do h_c = 1,n_hours
    	do g_c = 1,2	
            f_effective(h_c,g_c) = hpol(h_c)*sum(I_mw(:,h_c,g_c) *F_prob(:,h_c))
        enddo
    enddo
    ! Output
    call writedim(f_effective,trim(savedir)//'f_effective.txt')
    

    end subroutine initialize_I_mw
!---------------------------------------------------

    function find_wmin(h_c) result(wmin)
	! This function finds the wage rate wmin such that the contact success rate 
	!	 is zero for both genders. This function must be consistent with the function
	!	"contact_success_rate"
    implicit none
	! Inputs
	integer,intent(in) :: h_c
	! Output
	real(8) :: wmin
	! Local variables
	real(8) :: wmin_m,wmin_f
	
	
	wmin_m = p_min_wage*(1.0d0-(1.0d0/p_kappa0(h_c))**p_kappa1)
	wmin_f = p_min_wage/p_a(h_c)*(1.0d0-(1.0d0/p_kappa0(h_c))**p_kappa1)
	
	wmin = max(0.0d0,min(wmin_m,wmin_f))
	
    if (verbose>=2) then
    	write(*,*) "wmin in h_c = ",h_c," is ",wmin
    	pause
    endif

    end function find_wmin
!---------------------------------------------------   
    function util_couple(c,h,k) result(F)
    ! flow utility of couples
    implicit none
    
    !Declare inputs
    real(8), intent(in) :: c  ! household consumption
    real(8), intent(in) :: h(2) ! hours of work
    integer, intent(in) :: k ! kids state
    
    !Declare output
    real(8) :: F,L(2)
    
    ! Check if input values are within bounds
    if (minval(h)<0.0d0 .or. maxval(h)>hbar .or. k<1 .or. k>n_kids ) then
    	write(*,*) "Error in util_couple: bad inputs"
    	write(*,*) "h = ",h
    	write(*,*) "k = ",k
    	stop
    endif
    
	if (c>0.0d0) then 
		L(1) = 	(hbar-h(1))/(hbar)
		L(2) = 	(hbar-h(2))/(hbar)
	    F = 2.0d0*log(c/2.0d0)+p_psi_m(k)*L(1)**(1.0d0-p_gamma_m(k))/(1.0d0-p_gamma_m(k)) &
	    	+p_psi_f(k)*L(2)**(1.0d0-p_gamma_f(k))/(1.0d0-p_gamma_f(k)) 
    else	
    	F = large_negative
    endif
    
    end function util_couple
!---------------------------------------------------   
    
    function util_sm(c,h,k) result(F)
    ! flow utility of single males
    implicit none
    
    !Declare inputs
    real(8), intent(in) :: c ! own income
    real(8), intent(in) :: h ! hours type
    integer, intent(in) :: k ! kids state
    !Declare output
    real(8) :: F,L
    
    ! Check if input values are within bounds
    if (h<0.0d0 .or. h>hbar .or. k<1 .or. k>n_kids) then
    	write(*,*) "Error in util_sm: bad inputs"
    	write(*,*) "h = ",h
    	write(*,*) "k = ",k
    	stop
    endif
    
	if (c>0.0d0) then 
		L = (hbar-h)/(hbar)
	    F = log(c)+p_psi_m(k)*L**(1.0d0-p_gamma_m(k))/(1.0d0-p_gamma_m(k))    	
    else	
    	F = large_negative
    endif
    
    end function util_sm
!---------------------------------------------------   
    function util_sf(c,h,k) result(F)
    ! flow utility of single females
    implicit none
    
    !Declare inputs
    real(8), intent(in) :: c  ! household consumption
    real(8), intent(in) :: h ! hours type
    integer, intent(in) :: k ! kids state
   
    !Declare output
    real(8) :: F,L
    
    ! Check if input values are within bounds
    if (h<0.0d0 .or. h>hbar .or. k<1 .or. k>n_kids ) then
    	write(*,*) "Error in util_sf: bad inputs"
    	write(*,*) "h = ",h
    	write(*,*) "k = ",k
    	stop
    endif
    
	if (c>0.0d0) then 
		L = (hbar-h)/(hbar)
	    F = log(c)+p_psi_f(k)*L**(1.0d0-p_gamma_f(k))/(1.0d0-p_gamma_f(k))   	
    else	
    	F = large_negative
    endif
    
    end function util_sf
!---------------------------------------------------  

    function netinc_couple(inc,emp,k) result(F)
    ! computes net income for couples given gross incomes
    implicit none
    
    !Declare inputs
    real(8), intent(in) :: inc(2)  ! gross labor incomes
    integer, intent(in) :: emp(2)  ! employment status (0=no work, 1 = ME, 2 = PT, 3 = FT)
    integer, intent(in) :: k ! kids status
	
	!Declare output
    real(8) :: F

    !Declare locals
    real(8) :: netinc,inc_me
        
    ! Checks
    if (k<1 .or. k>3) then
    	call myerror("netinc_couple: k out of bounds.")
    endif
    if (any(emp<0) .or. any(emp>3)) then
    	call myerror("netinc_couple: input emp out of bounds.")
    endif
    
    ! Steps to compute net income
    !	1. remove ME income from the income-tax base
    !	2. remove social security tax for employed members with FT or PT jobs
    !	3. remove income tax
    !	4. add tax-exempt ME income
    !	5. add unemp benefits
    
    inc_me = sum(inc,mask=emp==1)
    netinc = sum(inc)-inc_me
    
    !	2. remove social security tax for employed members with FT or PT jobs
    if (emp(1)==2 .or. emp(1)==3) then
        ! male pays soc sec tax
    	netinc = netinc-p_tax_ss*inc(1)
    endif
	if (emp(2)==2 .or. emp(2)==3) then
        ! female pays soc sec tax
    	netinc = netinc-p_tax_ss*inc(2)
    endif
    
    !	3. remove income tax
    netinc = p_tax0_couple(k)*netinc**p_tax1_couple(k)
    
    !	4. add tax-exempt ME income
    netinc = netinc+inc_me
    
    !	5. add unemp benefits
    if (emp(1)==0) then
    	 netinc = netinc+p_b_mm(k)
    endif
    if (emp(2)==0) then
    	 netinc = netinc+p_b_mf(k)
    endif
    
    
    F = max(netinc, 1d-8)
    
    end function netinc_couple
!--------------------------------------------------- 
   
    function netinc(gender,inc,emp,k,sp) result(F)
    ! computes net income for single males given gross income
    implicit none
    
    !Declare inputs
    integer, intent(in) :: gender ! 1 = male, 2 = female
    real(8), intent(in) :: inc  ! gross income
    integer, intent(in) :: emp  ! employment status(0=no work, 1 = ME, 2 = PT, 3 = FT)
    integer, intent(in) :: k ! kids state
	integer, intent(in) :: sp ! marital state and spousal income state 
				! (1 = unmarried, 2 = low spousal income, 3 = medium spousal income, 4 = high spousal income)
	!Declare output
    real(8) :: F
	! Declare local variables
	real(8) :: hhinc,inc_me
        
    ! Checks
    if (gender<1 .or. gender>2) then
    	call myerror("netinc: gender out of bounds.")
    endif
    if (k<1 .or. k>n_kids) then
    	call myerror("netinc: k out of bounds.")
    endif
    if (emp<0 .or. emp>3) then
    	call myerror("netinc: input emp out of bounds.")
    endif
    if (sp<1 .or. sp>n_spinc) then
    	call myerror("netinc: sp out of bounds.")
    endif

    	
    ! Steps to compute net income
    !	1. remove ME income from the income-tax base
    !	2. remove social security tax if employed in PT or FT jobs
    !	3. add spousal income
    !	4. remove income tax
    !	5. add tax-exempt ME income
    !	6. add unemp benefits
    
   
    inc_me = 0.0d0
    if (emp==1) inc_me = inc
    hhinc = inc-inc_me
    
    !	2. remove social security tax for employed members with FT or PT jobs
!         ! pays soc sec tax
    
    !	3. add spousal income
     hhinc = hhinc + spinc_vec(sp)
    
    !	4. remove income tax
    if (sp == 1) then ! single
    	hhinc = p_tax0_single(k)*hhinc**p_tax1_single(k)
    else ! married
    	hhinc = p_tax0_couple(k)*hhinc**p_tax1_couple(k)
    endif
    
    !	5. add tax-exempt ME income
    hhinc = hhinc+inc_me
    
    !	6. add unemp benefits
    if (emp==0) then
    	if (gender == 1) then ! male
    		if (sp == 1) then ! single
    	 		hhinc = hhinc + p_b_sm(k)
    	 	else ! married
    	 		hhinc = hhinc + p_b_mm(k)
    	 	endif ! marital status
    	 else ! female
    	 	if (sp == 1) then ! single
    	 		hhinc = hhinc + p_b_sf(k)
    	 	else ! married
    	 		hhinc = hhinc + p_b_mf(k)
    	 	endif ! marital status
    	 endif ! gender
    endif ! emp == 0
   
    ! 7. convert from household income to individual income
    if (sp == 1) then ! single
    	F = max(hhinc, small)
    else ! married
    	F = max(hhinc/2.0d0, small)
    endif
        
    end function netinc

    !====================================================!
    function fun_hours_wk(hval) result(h_wk)
     implicit none
    !Declare inputs:
    real(8), intent(in) :: hval !hours
    real(8) :: h_wk 
    
    h_wk = hval/weekhours
    
    end function fun_hours_wk
    
    
    !====================================================!
    
    function firm_prod(p,h_c) result(y)
    
    implicit none
    !Declare inputs:
    real(8), intent(in) :: p !productivity
    integer, intent(in) :: h_c !hours index

    
    !Declare function:
    real(8) :: y
    ! Declare locals:
    real(8) :: hours_wk
    
    ! Convert hours to weekly hours
    hours_wk =  fun_hours_wk(hours_grid(h_c))
    
    y = p*hours_wk*p_theta(h_c)
    
    
    end function firm_prod
    
    
    
    !====================================================!
    
    function firm_prod_dp(h_c) result(y)
    ! Derivative of the firm productivity with respect to p
    implicit none
    !Declare inputs:
    integer, intent(in) :: h_c !hours index

    
    !Declare function:
    real(8) :: y
    ! Declare locals:
    real(8) :: hours_wk
    
    ! Convert hours to weekly hours
    hours_wk =  fun_hours_wk(hours_grid(h_c))
    y = hours_wk*p_theta(h_c)
    
    
    end function firm_prod_dp

    !========================================================================!
    
    
    
    function firm_profit(y,hours,wage,l) result(pi)
    !DESCRIPTION: This subroutine computes the expected flow profit of a firm.
    !It is profit_{h,j}(w;p), eq. (7), where j=1,2 is gender
    
    implicit none
    
    !Declare inputs:
    real(8), intent(in) :: y, hours,wage,l
    
    !Declare function:
    real(8) :: pi,hours_wk
    
    hours_wk = fun_hours_wk(hours)

    
    pi = (y - wage*hours_wk)*l

    end function firm_profit
    !====================================================!
    
    
	SUBROUTINE initSeed_dhw(seed)
	IMPLICIT NONE
	integer, intent(IN) :: seed
	! ZBQLINI is a subroutine in the library cdflib90

	call ZBQLINI(seed)
	end SUBROUTINE initSeed_dhw
	SUBROUTINE randMat_dhw(res)
	IMPLICIT NONE
	double precision, intent(INOUT) :: res(:,:)
	! ZBQLU01 is a uniform random number generator in the library cdflib90
	real(8) ZBQLU01
	integer n,m,i,j
	
	n = size(res,1)
	m = size(res,2)
	
	do i=1,n
		do j=1,m
			res(i,j) = ZBQLU01(0.0d0)
		end do
	end do

    end SUBROUTINE randMat_dhw
!====================================================! 
    subroutine unpack_params_demand(par_vec)
    !! unpack_params_demand converts parameter vector par_vec
    ! of step 2 (demand side) into global variables
    implicit none
    ! Declare variables
    real(8), intent(in) :: par_vec(:)
    integer :: i
    
    ! Execution
    i=1
     p_sigma_eps = par_vec(i)
    i=i+1
    p_eps_bar(1) = par_vec(i)
    i=i+1
    p_eps_bar(2) = par_vec(i)
    
    if (size(par_vec)/=i) then
    	call myerror("Error in unpack_params_demand")
    endif 

    end subroutine unpack_params_demand
!====================================================!

    subroutine pack_params_demand(par_vec)
    !! pack_params_demand converts individual parameters (global variables)
    ! into a parameter vector "par_vec", to be used as input
    ! into fun_obj_demand
    implicit none
    ! Declare variables
    real(8), intent(out) :: par_vec(:)
    integer :: i
    
    ! Execution
    i=1
     par_vec(i) = p_sigma_eps
    i=i+1
    par_vec(i) = p_eps_bar(1)
    i=i+1
    par_vec(i) = p_eps_bar(2)
    
    if (size(par_vec)/=i) then
    	call myerror("Error in pack_params_demand")
    endif 
    
    end subroutine pack_params_demand
!====================================================!
    
    subroutine unpack_params(par_vec)
    !! unpack_params converts parameter vector par_vec
    ! into global variables
    implicit none
    ! Declare variables
    real(8), intent(in) :: par_vec(:)
    integer :: i
    
    if (.not. allocated(hpol)) allocate(hpol(n_hours)) 
    
    ! Order of parameters has to be the same as in pack_params
    
    ! Execution
    i=1
    p_offer_a(1)=par_vec(i)
    i=i+1
    p_offer_a(2)=par_vec(i)
    i=i+1
    p_offer_a(3)=par_vec(i)
    i=i+1
    p_offer_b(1)=par_vec(i)
    i=i+1
    p_offer_b(2)=par_vec(i)
    i=i+1
    p_offer_b(3)=par_vec(i)
    i=i+1
    p_hpol_arr(1) =par_vec(i)
    i=i+1
    p_hpol_arr(2)=par_vec(i)
    i=i+1
    p_lambda = par_vec(i)
    i=i+1
    p_delta(1) = par_vec(i)
    i=i+1
    p_delta(2) = par_vec(i)
    i=i+1
    p_delta(3) = par_vec(i)
    i=i+1
    p_psi_m(2) = par_vec(i)
    i=i+1
    p_psi_m(3) = par_vec(i)
    i=i+1
    p_psi_f(1) = par_vec(i)
    i=i+1
    p_psi_f(2) = par_vec(i)
    i=i+1
    p_psi_f(3) = par_vec(i)
    i=i+1
    p_gamma_m(2) = par_vec(i)
    i=i+1
    p_gamma_m(3) = par_vec(i)
    i=i+1
    p_gamma_f(1) = par_vec(i)
    i=i+1
    p_gamma_f(2) = par_vec(i)
    i=i+1
    p_gamma_f(3) = par_vec(i)
    i=i+1
    p_mu_alpha(2,2) = par_vec(i)
    i=i+1
    p_mu_alpha(3,2) = par_vec(i)
    i=i+1
    p_a(1) = par_vec(i)
     i=i+1
    p_a(2) = par_vec(i)
     i=i+1
    p_a(3) = par_vec(i)
	if (size(par_vec)/=i) then
    	call myerror("Error in unpack_params")
    endif  
    
    ! Update additional parameters
   
	p_lambda_u = p_lambda
	p_lambda_e = p_lambda
	p_delta_m = p_delta
	p_delta_f = p_delta
       
    end subroutine unpack_params
!====================================================! 
    
    subroutine pack_params(par_vec)
    !! pack_params converts individual parameters (global variables)
    ! into a parameter vector "par_vec", to be used as input
    ! into fun_obj
    implicit none
    ! Declare variables
    real(8), intent(out) :: par_vec(:)
    integer :: i
    
    
    ! 
    p_delta = p_delta_m
    p_lambda = p_lambda_u
    ! Order of parameters has to be the same as in unpack_params
    i=1
    par_vec(i) = p_offer_a(1)
    i=i+1
    par_vec(i) = p_offer_a(2)
    i=i+1
    par_vec(i) = p_offer_a(3)
    i=i+1
    par_vec(i) = p_offer_b(1)
    i=i+1
    par_vec(i) = p_offer_b(2)
    i=i+1
    par_vec(i) = p_offer_b(3)
    i=i+1
    par_vec(i) = p_hpol_arr(1)
    i=i+1
    par_vec(i) = p_hpol_arr(2)
    i=i+1
    par_vec(i) = p_lambda
    i=i+1
    par_vec(i) = p_delta(1)
    i=i+1
    par_vec(i) = p_delta(2)
    i=i+1
    par_vec(i) = p_delta(3) 
    i=i+1
    par_vec(i) = p_psi_m(2)
    i=i+1
    par_vec(i) = p_psi_m(3)
    i=i+1
    par_vec(i) = p_psi_f(1) 
    i=i+1
    par_vec(i) = p_psi_f(2)
    i=i+1
    par_vec(i) = p_psi_f(3)
    i=i+1
    par_vec(i) = p_gamma_m(2)
     i=i+1
    par_vec(i) = p_gamma_m(3)
    i=i+1
    par_vec(i) = p_gamma_f(1)
    i=i+1
    par_vec(i) = p_gamma_f(2)
     i=i+1
    par_vec(i) = p_gamma_f(3)
    i=i+1
    par_vec(i) = p_mu_alpha(2,2)
     i=i+1
    par_vec(i) = p_mu_alpha(3,2)
     i=i+1
    par_vec(i) = p_a(1)
     i=i+1
    par_vec(i) = p_a(2)
     i=i+1
    par_vec(i) = p_a(3)
    
    if (size(par_vec)/=i) then
    	write(*,*) "Size of par_vec = ",size(par_vec)
    	write(*,*) "i = ",i
    	call myerror("Error in pack_params")
    endif 
    
    end subroutine pack_params
    
!====================================================! 

subroutine pack_bounds(bounds_vec)
    !! pack_bounds converts individual bounds (global variables)
    ! into a parameter (N,2) vector "bounds_vec", to be used into fun_obj
    implicit none
    ! Declare variables
    real(8), intent(out) :: bounds_vec(:,:)
    integer :: i
    
    ! Order of bounds has to be the same as in unpack_bounds
    ! and MUST correspond to the order of parameters
    
    ! Execution
    i=1
    bounds_vec(i,1) = lb_offer_a(1)
    bounds_vec(i,2) = ub_offer_a(1)
    i=i+1
    bounds_vec(i,1) = lb_offer_a(2)
    bounds_vec(i,2) = ub_offer_a(2)
    i=i+1
    bounds_vec(i,1) = lb_offer_a(3)
    bounds_vec(i,2) = ub_offer_a(3)
    i=i+1
    bounds_vec(i,1) = lb_offer_b(1)
    bounds_vec(i,2) = ub_offer_b(1)
    i=i+1
    bounds_vec(i,1) = lb_offer_b(2)
    bounds_vec(i,2) = ub_offer_b(2)
    i=i+1
    bounds_vec(i,1) = lb_offer_b(3)
    bounds_vec(i,2) = ub_offer_b(3)
    i=i+1
    bounds_vec(i,1) = lb_hpol_arr(1)
    bounds_vec(i,2) = ub_hpol_arr(1)
    i=i+1
    bounds_vec(i,1) = lb_hpol_arr(2)
    bounds_vec(i,2) = ub_hpol_arr(2)
    i=i+1
    bounds_vec(i,1) = lb_lambda
    bounds_vec(i,2) = ub_lambda
    i=i+1
    bounds_vec(i,1) = lb_delta(1)
    bounds_vec(i,2) = ub_delta(1)
    i=i+1
    bounds_vec(i,1) = lb_delta(2)
    bounds_vec(i,2) = ub_delta(2)
    i=i+1
    bounds_vec(i,1) = lb_delta(3)
    bounds_vec(i,2) = ub_delta(3)
    i=i+1
    bounds_vec(i,1) = lb_psi_m(2)
    bounds_vec(i,2) = ub_psi_m(2)
    i=i+1
    bounds_vec(i,1) = lb_psi_m(3)
    bounds_vec(i,2) = ub_psi_m(3)
    i=i+1
    bounds_vec(i,1) = lb_psi_f(1)
    bounds_vec(i,2) = ub_psi_f(1)
    i=i+1
    bounds_vec(i,1) = lb_psi_f(2)
    bounds_vec(i,2) = ub_psi_f(2)
    i=i+1
    bounds_vec(i,1) = lb_psi_f(3)
    bounds_vec(i,2) = ub_psi_f(3)
    i=i+1
    bounds_vec(i,1) = lb_gamma_m(2)
    bounds_vec(i,2) = ub_gamma_m(2)
    i=i+1
    bounds_vec(i,1) = lb_gamma_m(3)
    bounds_vec(i,2) = ub_gamma_m(3)
	i=i+1
    bounds_vec(i,1) = lb_gamma_f(1)
    bounds_vec(i,2) = ub_gamma_f(1)
    i=i+1
    bounds_vec(i,1) = lb_gamma_f(2)
    bounds_vec(i,2) = ub_gamma_f(2)
    i=i+1
    bounds_vec(i,1) = lb_gamma_f(3)
    bounds_vec(i,2) = ub_gamma_f(3)
     i=i+1
    bounds_vec(i,1) = lb_mu_alpha(2,2)
    bounds_vec(i,2) = ub_mu_alpha(2,2)
    i=i+1
    bounds_vec(i,1) = lb_mu_alpha(3,2)
    bounds_vec(i,2) = ub_mu_alpha(3,2)
    i=i+1
    bounds_vec(i,1) = lb_a(1)
    bounds_vec(i,2) = ub_a(1)
    i=i+1
    bounds_vec(i,1) = lb_a(2)
    bounds_vec(i,2) = ub_a(2)
    i=i+1
    bounds_vec(i,1) = lb_a(3)
    bounds_vec(i,2) = ub_a(3)
    if (size(bounds_vec,dim=1)/=i) then
    	write(*,*) "Size of bounds_vec = ",size(bounds_vec,dim=1)
    	write(*,*) "i = ",i
    	call myerror("Error in pack_bounds")
    endif 
    
    end subroutine pack_bounds
    

    
!====================================================! 
    
end module mod_globals
