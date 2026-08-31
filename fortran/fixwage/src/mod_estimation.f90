module mod_estimation
    
    !=======================================================================
    !        Module Estimation
    ! Here we estimate the model following the two-steps procedure 
    ! No subgrid is needed for this demand-side step.
    !=======================================================================
	!=======================================================================
    
    use omp_lib
    use mod_globals
    use mod_output
    use mod_utilities 
    use mod_numerical, only: myerror, linspace, discretize_beta, quantili, linint, &
    						grid,jacob,is_monotone,sub_logsum
    use mod_pchip, only: pchip, pchip_extrap
	use linalg, only: lu_solve
    use mod_fitting, only: powerfit, powerval, powerint
	use mod_solvemodel, only: solvemodel_fixwage
    
	implicit none
    
    private
    public estimation, fun_obj,sub_moment, estimation_demand,fun_obj_demand,fun_obj_eq
    contains 
    
!=======================================================================
    function fun_obj_eq(par_vec) result(distance)
    use mod_numerical,only: myerror
    implicit none
    ! Declare inputs:
    real(8), intent(in) :: par_vec(:)
    ! Declare function value
    real(8) :: distance,dist_empdist,dist_wageqtl,dist_trans_mat
    ! Declare locals
    type(mom) :: mom_model
    integer:: append,unitno,ierr,flag_est
    
    ! Unpack par_vec and assign its elements to global parameters
    call unpack_params_eq(par_vec) ! par_vec is input here
	! Call solvemodel to get equilibrium offer distributions
	call solvemodel_fixwage()
	! Read in the equilibrium offer distributions
	! F_prob, w_grid, and hpol should already be in memory after solvemodel 


    ! Solve supply-side and generate moments
    call estimation(mom_model,flag_est)
    dist_empdist = 0.0d0
	dist_wageqtl = 0.0d0
	dist_trans_mat = 0.0d0

    if (flag_est==0) then
		! Calculate the distance between mom_data and mom_model: sum of squared percentage deviation
		! - calculate distance for each set of moments
		!
		dist_empdist = sum((mom_model%mom_empdist-mom_data%mom_empdist)/0.25d0 &
			*(mom_model%mom_empdist-mom_data%mom_empdist)/0.25d0*weight%mom_empdist)
		dist_wageqtl = 2.0d0*sum((mom_model%mom_wageqtl-mom_data%mom_wageqtl)/10.0d0 &
			*(mom_model%mom_wageqtl-mom_data%mom_wageqtl)/10.0d0*weight%mom_wageqtl)
		dist_trans_mat = sum((mom_model%trans_mat-mom_data%trans_mat)/0.05d0 &
		 	*(mom_model%trans_mat-mom_data%trans_mat)/0.05d0*weight%trans_mat)
		! - total distance is the sum of all distance components
		distance = dist_empdist + dist_wageqtl + dist_trans_mat 
		
		! Write current parameters and distance to file file_est_distance 
     	if (distance<= estim_distance) then
			append = 1
			call write_params_eq(par_vec,file_est_distance,append)
			open(newunit=unitno,file=file_est_distance,form='formatted',status='old',position='append',iostat=ierr)
			if (ierr/=0) then
				call myerror("Error in fun_obj: cannot open file")
			endif
			write(unitno,*) "Total Distance: ",distance
			write(unitno,*) "dist_empdist: ",dist_empdist
			write(unitno,*) "dist_wageqtl: ",dist_wageqtl
			write(unitno,*) "dist_trans_mat: ",dist_trans_mat
			write(unitno,*) "==========================="
			close(unitno)
			estim_distance = distance
		endif
	
		! Write current parameters and moments to file file_est_moments
		call txt_export(file_est_moments,par_vec,mom_model)
		call tex_export_fit(file_fit_tex,mom_model)
		
		
   	
   	else ! estimation returns flag
   		distance = large_positive
    	
    	return
   	endif ! flag_est==0
    
    
    end function fun_obj_eq

    !=======================================================================
    function fun_obj(par_vec) result(distance)
    use mod_numerical,only: myerror
    implicit none
    ! Declare inputs:
    real(8), intent(in) :: par_vec(:)
    ! Declare function value
    real(8) :: distance,dist_empdist,dist_wageqtl,dist_trans_mat
    ! Declare locals
    type(mom) :: mom_model
    integer:: append,unitno,ierr,h_c,flag_est
    
    ! Unpack par_vec and assign its elements to global parameters
    call unpack_params(par_vec) ! par_vec is input here
    ! Initialize hpol_m and hpol_f according to the parameters p_hpol
	do h_c = 1,n_hours
		if (sum(p_hpol_arr)>1.0d0) then
			hpol(h_c) = p_hpol_arr(h_c)/sum(p_hpol_arr)
		else
			hpol = p_hpol_arr
		endif
	enddo

    ! Solve supply-side and generate moments
    call estimation(mom_model,flag_est)
    dist_empdist = 0.0d0
	dist_wageqtl = 0.0d0
	dist_trans_mat = 0.0d0

    if (flag_est==0) then
		! Calculate the distance between mom_data and mom_model: sum of squared percentage deviation
		! - calculate distance for each set of moments
		!
		dist_empdist = sum((mom_model%mom_empdist-mom_data%mom_empdist)/0.25d0 &
			*(mom_model%mom_empdist-mom_data%mom_empdist)/0.25d0*weight%mom_empdist)
		dist_wageqtl = sum((mom_model%mom_wageqtl-mom_data%mom_wageqtl)/abs(mom_data%mom_wageqtl) &
			*(mom_model%mom_wageqtl-mom_data%mom_wageqtl)/abs(mom_data%mom_wageqtl)*weight%mom_wageqtl)
		dist_trans_mat = sum((mom_model%trans_mat-mom_data%trans_mat)/0.05d0 &
		 	*(mom_model%trans_mat-mom_data%trans_mat)/0.05d0*weight%trans_mat)
		! - total distance is the sum of all distance components
		distance = dist_empdist + dist_wageqtl + dist_trans_mat 
		
		! Write current parameters and distance to file file_est_distance 
     	if (distance<= estim_distance) then
			append = 1
			call write_params(par_vec,file_est_distance,append)
			open(newunit=unitno,file=file_est_distance,form='formatted',status='old',position='append',iostat=ierr)
			if (ierr/=0) then
				call myerror("Error in fun_obj: cannot open file")
			endif
			write(unitno,*) "Total Distance: ",distance
			write(unitno,*) "dist_empdist: ",dist_empdist
			write(unitno,*) "dist_wageqtl: ",dist_wageqtl
			write(unitno,*) "dist_trans_mat: ",dist_trans_mat
			write(unitno,*) "==========================="
			close(unitno)
			estim_distance = distance
		endif
	
		! Write current parameters and moments to file file_est_moments
		call txt_export(file_est_moments,par_vec,mom_model)
		call tex_export_fit(file_fit_tex,mom_model)
		
		
   	
   	else ! estimation returns flag
   		distance = large_positive
    	
    	return
   	endif ! flag_est==0
    
    
    end function fun_obj
    
    !=======================================================================
    subroutine sub_moment(par_vec,mom_model) 
    ! This subroutine computes supply side moments using using subroutine estimation and 
    !	returns the moments mom_model
    implicit none
    ! Declare inputs-outputs:
    real(8), intent(in) :: par_vec(:) ! supply side parameters
    type(mom), intent(out) :: mom_model ! supply side moments generated by the model
    ! Declare locals
    integer :: h_c,flag_est
    
    ! Unpack par_vec and assign its elements to global parameters
    call unpack_params(par_vec) ! par_vec is input here
    ! Initialize hpol_m and hpol_f according to the parameters p_hpol
	do h_c = 1,n_hours
		hpol(h_c) = p_hpol_arr(h_c)/sum(p_hpol_arr)
	enddo
	
	

    ! Solve supply-side and generate moments
    call estimation(mom_model,flag_est)
    
    end subroutine sub_moment
    
    

    !=======================================================================
    subroutine estimation(mom_model,flag_est)
    
    use solve_household,  only: vfi, vfi_singles, acceptance_rules, acceptance_rules_s
    use mod_distribution, only: get_distribution, get_distribution_s, labor_supply
    use mod_simulation,   only: sim_transition
    
    implicit none
    
    ! Declare outputs
    type(mom), intent(out) :: mom_model
    integer, intent(out) :: flag_est
    
    ! Declare Local variables
    logical :: flag_vfi, flag_vfi_s, flag_dist, flag_dist_s
    integer:: h_c,w_c
    ! See mod_globals for declaration of derived data types
    type(valfun)   :: V_inout
    type(valfun_s) :: V_inout_s
    
	real(8) :: mom_empdist(n_hours,n_kids,2)
    real(8) :: mom_wageqtl(7,n_hours,2) ! 9 percentiles, 3 hours type and 2 genders
    real(8) :: trans_mat(0:3,0:3),trans_mat_j(0:3,0:3,1:2),trans_emp_j(0:1,0:1,1:2)
    real(8) :: t1,t2
	
	! Labor supply
	real(8) :: labsup(n_w,1:n_hours)
		
	
	flag_est = 0    

    !===============  ALLOCATE AND INITIALIZE VALUE FUNCTIONS AND DISTRIBUTIONS  ===============!

    ! Allocate and initialize value functions
	call initialize_V(V_inout,V_inout_s)
    
    ! Optional: reset g
    ! Note: g should be initialized before calling fun_obj
    if (do_resetg == .true.) then
    	call initialize_g()
    endif

    !================ INITIALIZE WAGE GRID AND OFFER DIST ==========================================!

	! Initialize F_m_prob, F_f_prob, w_grid	
	if (do_read_offer == .false.) then
		call initialize_F()
	endif
    if (verbose>=2) write(*,*) "Finished setting up wage grids"


    !================ SOLVE HOUSEHOLD PROBLEM ===============================!
    !---Value Function Iteration---!		
	! VFI for single households
	t1 = omp_get_wtime()
	
    call vfi_singles(V_inout_s,flag_vfi_s)
	if (flag_vfi_s == .false. ) then
		write(*,*) "VFI for singles did not converge!"
		flag_est = -1
		return
    endif
	! Distributions of singles	
	call acceptance_rules_s(V_inout_s)
	call get_distribution_s(g_inout_s,flag_dist_s)
   if (flag_dist_s==.false.) then
		write(*,*) "Distribution G (singles) did not converge!"
		flag_est = -1
		deallocate(omega_u_sf,omega_u_sm,omega_e_sf,omega_e_sm,Oh_e_sm,Oh_e_sf)
		return
	endif

    if (do_nocouple == .false.) then ! If do_nocouple is true, skip couples problem
		!VFI for married households
		call vfi(V_inout,flag_vfi)
		if (flag_vfi==.false.) then
			write(*,*) "VFI for married couples did not converge!"
			flag_est = -1
			return
		endif
	
		   
		!---Compute Acceptance Rules and Distribution---!		
		!Outputs are the omega sets, but we define them as globals
		call acceptance_rules(V_inout)
		call get_distribution(g_inout,flag_dist)
		if (flag_dist==.false.) then
			write(*,*) "Distribution G (married) did not converge!"
			flag_est = -1
			! deallocate Omega's and Oh's 
			deallocate(omega_uu1,omega_uu2, omega_eu1,omega_eu2, &
				omega_ue2,omega_ue1,omega_ee1,omega_ee2,Oh_eu1,Oh_ue2, &
				Oh_ee12,Oh_ee1,Oh_ee2,Oh_ee_minus,Oh_eu2_1stays,Oh_ue1_2stays, &
				Oh_ee1_2stays,Oh_ee2_1stays)
	
			return
		endif
    endif !do_nocouple
  
	t2 = omp_get_wtime()
	if (verbose>=1) write(*,*) "Solving the model run for ", real(t2-t1)," seconds"

	
    if (write_v_g) then
    	if (do_nocouple == .false.) then 
			!Export value function results for couples
			if (verbose==2) write(*,*) "Writing valfun to file.."
			call writedim(V_inout%V_uu,trim(savedir)//'V_uu.txt')
			call writedim(V_inout%V_eu,trim(savedir)//'V_eu.txt')
			call writedim(V_inout%V_ue,trim(savedir)//'V_ue.txt')
			call writedim(V_inout%V_ee,trim(savedir)//'V_ee.txt')
        endif
        !Export value function results for singles
        if (verbose==2) write(*,*) "Writing valfun singles to file.."
        call writedim(V_inout_s%V_u_sm,trim(savedir)//'V_u_sm.txt')
        call writedim(V_inout_s%V_u_sf,trim(savedir)//'V_u_sf.txt')
        call writedim(V_inout_s%V_e_sm,trim(savedir)//'V_e_sm.txt')
        call writedim(V_inout_s%V_e_sf,trim(savedir)//'V_e_sf.txt')
        
 	    !Export distributions
 	    if (do_nocouple == .false.) then 
			if (verbose==2) write(*,*) "Writing distrib. to file.."
			call writedim(g_inout%uu,trim(savedir)//'g_uu.txt')
			call writedim(g_inout%eu,trim(savedir)//'g_eu.txt')
			call writedim(g_inout%ue,trim(savedir)//'g_ue.txt')
			call writedim(g_inout%ee,trim(savedir)//'g_ee.txt')
        endif
        !Export distributions for singles
 	    if (verbose==2) write(*,*) "Writing distrib. singles to file.."
        call writedim(g_inout_s%u_sm,trim(savedir)//'g_u_sm.txt')
        call writedim(g_inout_s%u_sf,trim(savedir)//'g_u_sf.txt')
        call writedim(g_inout_s%e_sm,trim(savedir)//'g_e_sm.txt')
        call writedim(g_inout_s%e_sf,trim(savedir)//'g_e_sf.txt')
    endif
    !================ COMPUTE MOMENTS ===============================!
    ! With the help of some internal subroutines (no need to pass 
    ! explicitely inputs and outputs).
    t1 = omp_get_wtime()
    ! Compute mom_empdist: Fraction of jobs by hours and gender
    !	Output is mom_empdist(h,childstate,gender)
    call empdist()
    
    ! Compute mom_wageqtl: Wage moments (e.g. deciles) of hourly wage distributions by hours and gender
    !	Output is mom_wageqtl(1:9,2:n_hours,2)
    call wagemoments()

    if (verbose>=2) write(*,*) "Done empdist and wagemoments..."
    call sim_transition(g_inout,g_inout_s,trans_mat,trans_mat_j,trans_emp_j)
	if (verbose>=2) write(*,*) "Done sim_transition..."
	! deallocate Omega's and Oh's 
	if (do_nocouple== .false.) then
		deallocate(omega_uu1,omega_uu2, omega_eu1,omega_eu2, &
			omega_ue2,omega_ue1,omega_ee1,omega_ee2,Oh_eu1,Oh_ue2, &
			Oh_ee12,Oh_ee1,Oh_ee2,Oh_ee_minus,Oh_eu2_1stays,Oh_ue1_2stays, &
			Oh_ee1_2stays,Oh_ee2_1stays)
	endif
	deallocate(omega_u_sf,omega_u_sm,omega_e_sf,omega_e_sm,Oh_e_sm,Oh_e_sf)
	
    
    t2 = omp_get_wtime()
    if (verbose>=1) write(*,*) "Simulation and moments run for ", real(t2-t1)," seconds"
    
    ! Pack all moments into single output structure
    mom_model%mom_empdist = mom_empdist
    mom_model%mom_wageqtl = mom_wageqtl
    mom_model%trans_mat = trans_mat
	mom_model%trans_mat_j= trans_mat_j
	mom_model%trans_emp_j= trans_emp_j
    
	!---Additional step: For each productivity, hours, gende,
	!	compute labor supply labsup (dim:n_w,n_hours,2)
	! Note: labor_supply is a subroutine in mod_distribution
	labsup=0.0d0    
	do w_c = 1,n_w
		do h_c = 1,n_hours
			! Output: labsup(w_c,h_c,g_c)
			call labor_supply(g_inout,g_inout_s,w_c,h_c,labsup(w_c,h_c))
		enddo
	enddo
    
    call writedim(labsup,trim(savedir)//'labsup.txt')
    
    
    contains
    !=======================================================================
    subroutine empdist()
   		! This subroutine calculates mom_empdist(h,child,gender) with h=1,2,3; c=1,2,3; g=1,2
   		!	which contains the fractions of jobs by hours for each child state and gender
		implicit none 

		!Declare locals
		real(8) :: l_m,l_f,mass
		integer :: gender_c,h_c,k_c
   		
		do gender_c = 1,2
		do k_c  = 1,n_kids
		
			! mass of nonemployed
			if (gender_c == 1) then
				mass = sum(g_inout_s%u_sm(k_c,:))
			else
				mass = sum(g_inout_s%u_sf(k_c,:))
			endif 
			do h_c = 1,n_hours 
				if (gender_c == 1) then
					! Males
					! Measure of single men in h_c
					l_m = sum(g_inout_s%e_sm(:,h_c,k_c,:))
					mass = mass + l_m
		   
					mom_empdist(h_c,k_c,gender_c) = l_m

				elseif (gender_c == 2) then
					! Females
					! Measure of single women in h_c
					l_f = sum(g_inout_s%e_sf(:,h_c,k_c,:))
					mass = mass + l_f
		   
					mom_empdist(h_c,k_c,gender_c) = l_f
				endif ! gender
			enddo !h_c
		
			! Rescale
			mom_empdist(:,k_c,gender_c) = mom_empdist(:,k_c,gender_c)/mass
			
		enddo !k_c
		enddo !gender_c  
		
		
		
    end subroutine empdist
    !=======================================================================
    
    subroutine wagemoments()
		! This subroutine calculates moments of the hourly wage distribution by hours and gender 
        !	Output: mom_wageqtl(1:7,h,gender) with h=1,3 
		implicit none 
		
        ! Local variables
		integer :: gender_c,h_c,w_c,k_c,sp_c
		real(8) :: q(7),wage_dist(n_w),l_m,l_f
		
		q=[0.05d0,0.1d0,0.25d0,0.5d0,0.75d0,0.9d0,0.95d0]
        
        ! Check dimensions are compatible
        if ( size(q)/=size(mom_wageqtl,dim=1) ) then
            call myerror("Dimensions of q and mom_wageqtl are not compatible")
        endif
		
		do gender_c = 1,2
		do h_c = 1,n_hours
			! Male
			if (gender_c==1) then
				! reset wage_dist
				wage_dist = 0.0d0 ! dim (n_w)
				do w_c = 1,n_w

					if (do_nocouple == .false.) then
						!married men: Integral over alpha and sum over k
						do k_c  = 1,n_kids
							! Measure of married men in w_c,h_c
							l_m = g_inout%eu(w_c,h_c,k_c) + sum(g_inout%ee(w_c,h_c,:,:,k_c)) 					
							wage_dist(w_c) = wage_dist(w_c) + l_m
						enddo
					endif
				
					!single men: integral over alpha and sum over k
					do sp_c = 1,n_spinc
					do k_c  = 1,n_kids
							! Measure of single men in w_c,h_c
							l_m = g_inout_s%e_sm(w_c,h_c,k_c,sp_c)
							wage_dist(w_c)  = wage_dist(w_c) +  l_m
					enddo
					enddo
		   		enddo !w_c
		   		
		   		! rescale wage_dist so that the sum is equal to 1
		   		wage_dist = wage_dist/sum(wage_dist)
  
				! calculate quintiles	
				mom_wageqtl(:,h_c,gender_c) = quantili(w_grid(:,h_c),wage_dist,q)
               
			! Female
			elseif (gender_c==2) then
				! reset wage_dist
				wage_dist = 0.0d0 ! dim: (n_w) 
				do w_c = 1,n_w
					if (do_nocouple == .false.) then
						!married women: Integral over alpha and sum over k
						do k_c  = 1,n_kids
							! Measure of married women in w_c,h_c
							l_f = g_inout%ue(w_c,h_c,k_c) + sum(g_inout%ee(:,:,w_c,h_c,k_c)) 					
							wage_dist(w_c) = wage_dist(w_c) + l_f
						enddo
		   			endif
					!single women: integral over alpha and sum over k
					do sp_c = 1,n_spinc
					do k_c  = 1,n_kids
						! Measure of single women in w_c,h_c
						l_f = g_inout_s%e_sf(w_c,h_c,k_c,sp_c)
						wage_dist(w_c)  = wage_dist(w_c)  + l_f
					enddo
					enddo
		   		enddo !w_c
		   		! rescale wage_dist so that the sum is equal to 1
		   		wage_dist = wage_dist/sum(wage_dist)
		   		
				! calculate deciles			
				! For females, hourly wage is p_a*w
				mom_wageqtl(:,h_c,gender_c) = quantili(p_a(h_c)*w_grid(:,h_c),wage_dist,q)
			endif
		enddo !h_c
		enddo !gender_c

    end subroutine wagemoments

   		
	end subroutine estimation
    
    !=======================================================================   	
    function fun_obj_demand(parvec) result(dist_demand)
    ! Calculate the criterion function for the demand estimation
    implicit none
    ! Input/Output
    real(8), intent(in) :: parvec(:)
    real(8) :: dist_demand
    ! Local variables
    real(8) :: dist_Delta, dist_hpol
    real(8) :: Delta_th(n_prod,1:n_hours), Delta_ob(n_prod,1:n_hours)
    real(8) :: prod_PMF_est(n_prod),hpol_est(n_hours),prod_PMF_est_rep(n_prod,n_hours)
    integer:: p_c,h_c,unitno,ierr
    real(8) :: weight_hpol 
    
	
	! First, unpack parameter vector and update global parameter values
    call unpack_params_demand(parvec)
    if (verbose>=2) write(*,*) "fun_obj_demand: parvec unpacked, calling estimation_demand..."
    
	   
    ! Call routine to calculate theoretical and observed Deltas
    ! See Section "Step 2 - Estimation of demand-side parameters"
    call estimation_demand(Delta_th,Delta_ob,prod_PMF_est)
    if (verbose>=2) then
		write(*,*) "p_c	h_c	Delta_th Delta_ob!"
		do p_c = 1,n_prod
		do h_c = 1,n_hours
			write(*,*) p_c,h_c,Delta_th(p_c,h_c),Delta_ob(p_c,h_c)
		enddo
		enddo
    endif
    ! Delta_th is (n_prod,n_hours), prod_PMF_est is (n_prod)
    ! hpol_est must be (n_hours)
    prod_PMF_est_rep = spread(prod_PMF_est,dim=2,ncopies=n_hours) !(n_prod,n_hours)
    
    
    hpol_est = sum(Delta_th*prod_PMF_est_rep,dim=1)
	hpol_est = hpol_est/sum(hpol_est) 
	
    dist_Delta  = 1.0d0*sum(abs(Delta_th(:,1) - Delta_ob(:,1))**2d0) &
    				+ sum(abs(Delta_th(:,2) - Delta_ob(:,2))**2d0) &
					+ sum(abs(Delta_th(:,3) - Delta_ob(:,3))**2d0)

    dist_hpol   = sum((abs(hpol-hpol_est))**2d0)
    weight_hpol = 0.5d0*n_hours*n_w/1d0
    dist_demand = dist_Delta + weight_hpol*dist_hpol
    
    
    if (verbose>=2) then 
    	write(*,*) "dist_demand = ", dist_demand
    endif
    
    ! Write distance to file file_est_distance 
	open(newunit=unitno,file=file_demand_distance,form='formatted',status='old',position='append',iostat=ierr)
	if (ierr/=0) then
		call myerror("Error in fun_obj_demand: cannot open file")
	endif
	write(unitno,*) "Total Distance: ",dist_demand
	write(unitno,*) "==========================="
	close(unitno)
    
    call txt_export_demand(file_demand_moments,Delta_ob,Delta_th,hpol,hpol_est)
    end function fun_obj_demand
!     !=======================================================================   	
! 
    subroutine estimation_demand(Delta_th,Delta_ob,prod_PMF_out)
    ! This subroutine produces prod_grid and prod_PMF
    use solve_household,  only: vfi, vfi_singles, acceptance_rules, acceptance_rules_s
    use mod_distribution, only: get_distribution, get_distribution_s, labor_supply
    implicit none
    ! Input/Output variables
    ! Delta_th (theoretical delta, no hat)
    ! Delta_ob (observed delta, with hat)
    real(8), intent(out) :: Delta_th(:,:), Delta_ob(:,:)  !(n_prod,n_hours)
    real(8), intent(out) :: prod_PMF_out(:) !n_prod
    ! Local variables
	real(8) :: Delta_th_act(n_prod,n_hours),Delta_act(n_prod),share_act
    real(8) :: prod_lb,prod_ub, prod_lb_vec(n_hours), prod_ub_vec(n_hours),prod_val,w_val
    real(8) :: labsup(n_w,n_hours),labsup_dw(n_w,n_hours), labsup_old(n_w,n_hours), &
    	prod_arr(n_w,n_hours),wage_arr(n_w,n_hours),labsup_arr(n_w,n_hours)
    real(8):: profit_arr(n_prod,n_hours),wage_pdf(n_prod,n_hours)
    real(8):: wage,hours,ls_val,wage_val,y_val
    integer:: h_c,p_c,w_c,pp_c
	real(8):: wage_grid(n_w),coef_power(3)
	real(8) :: profit_daily(n_prod,n_hours),profit_vec(n_hours),delta_job(n_hours),log_sum,profit_vec_act(2),delta_vec_act(2)
	real(8), allocatable :: A_prod_PMF(:,:),prod_PMF_act(:)
	integer:: istat
    
    !================Table of Contents 
    !---Step (1): For each productivity and hours, compute profit
	!   (1.a) Compute labor supply labsup
	!	(1.b) Compute derivative of labsup wrt w (i.e. labsup_dw) for each h
    !	(1.c) Compute productivity for each w,h (see eq. 18)
    !	(1.d) Compute profit for each productivity, hours, gender (use function firm_profit in mod_globals)
    !---Step (2): Construct the theoretical sector choice Delta_th(h,p) using eq. 10
    !---Step (3): Construct the observed sector choice function
    !	(3.a) Calculate the derivative of wage w.r.t. p 
    !	(3.b) Calculate Delta-hat using eq. on the top of page 34
     !================ 
  	
  	
    !---Step (1): For each productivity (p) and hours, compute profit
    !   (1.a) Compute labor supply labsup (dim:n_w,n_hours)
	! Note: labor_supply is a subroutine in mod_distribution
	labsup=0.0d0    
	do w_c = 1,n_w
		do h_c = 1,n_hours
			!Compute labor supply for h_c
			call labor_supply(g_inout,g_inout_s,w_c,h_c,labsup(w_c,h_c))
		enddo
	enddo
    
!     print *, "The current working directory is: ",trim(pwd)
    call writedim(labsup,trim(savedir)//'labsup.txt')
    if (verbose>=2) then
	write(*,*) "estimation_demand: Step 1.a done!"
	pause
	endif
	!	(1.b) Compute derivative of labsup wrt w 
  	labsup_old = labsup
  	labsup_dw = 0.0d0
	do h_c = 1,n_hours
		wage_grid = w_grid(:,h_c)
		
		do w_c = 1,n_w
			wage = wage_grid(w_c)
			!jacob: numerical derivative
			if (w_c == 1) then
				labsup_dw(w_c,h_c) = jacob(labsup_interp,wage,1) 
			elseif (w_c > 1 .and. w_c < n_w) then
				labsup_dw(w_c,h_c) = jacob(labsup_interp,wage,2) 
			elseif (w_c == n_w) then
				labsup_dw(w_c,h_c) = jacob(labsup_interp,wage,3)
			endif
			! labsup_dw must be monotonically decreasing
			if (w_c>1) then
				if (labsup_dw(w_c,h_c)>=labsup_dw(w_c-1,h_c)) then
				write(*,*) "labsup_dw is not monotonically decreasing"
				write(*,*) "h_c,w_c",h_c,w_c
				write(*,*) "labsup_dw(w_c-1,h_c,g_c)",labsup_dw(w_c-1,h_c)
				write(*,*) "labsup_dw(w_c,h_c,g_c)",labsup_dw(w_c,h_c)
				endif
			endif
		enddo ! w_c
		
		
		! Smooth the derivative of labsup by fitting a power function
		
		call powerfit(wage_grid,labsup_dw(:,h_c),coef_power)
		
		do w_c = 1,n_w
			labsup_dw(w_c,h_c) = powerval(coef_power,wage_grid(w_c)) 
			if (w_c>1) then
				labsup(w_c,h_c) = labsup(1,h_c)+powerint(coef_power,wage_grid(w_c)) &
					-powerint(coef_power,wage_grid(1))
			endif
		enddo
	enddo ! h_c
	
	call writedim(labsup_dw,trim(savedir)//'labsup_dw.txt')
	call writedim(labsup_old,trim(savedir)//'labsup_old.txt')
	call writedim(labsup,trim(savedir)//'labsup_new.txt')
	
	if (verbose>=2) then
		write(*,*) "estimation_demand: Step 1.b done!"
		pause
	endif
  	!	(1.c) Compute productivity grid using the first order condition
    ! Use the FOC of firm maximization for part-time and full-time to obtain productivity
    ! corresponding to the lowest and highest wage. 
    
    ! We want a common productivity grid that is the union of the h-specific
    !  productivity grids.
    prod_lb_vec = [wage_pol_inv(1,1),wage_pol_inv(1,2), wage_pol_inv(1,3)]
    prod_ub_vec = [wage_pol_inv(n_w,1),wage_pol_inv(n_w,2), wage_pol_inv(n_w,3)]
    
    ! Set bounds of the common prod. grid to the average of the h-specific bounds
    prod_lb = sum(prod_lb_vec)/real(n_hours,8)
    prod_ub = sum(prod_ub_vec)/real(n_hours,8)
    
    ! Set bounds of the common prod. grid such that the common grid is the union of the h-specific grids
    ! The average (above) is probably a better choice

    
    if (verbose>=1) then
		write(*,*) "---------------------------------------------------------"
		write(*,*) "p_theta = ", p_theta
		write(*,*) " "
		write(*,*) "Lower bounds of h-specific productivity grids, h=ME,PT,FT"
		write(*,*) wage_pol_inv(1,1),wage_pol_inv(1,2), wage_pol_inv(1,3)
		write(*,*) " "
		write(*,*) "Upper bounds of h-specific productivity grids, h=ME,PT,FT"
		write(*,*) wage_pol_inv(n_w,1),wage_pol_inv(n_w,2), wage_pol_inv(n_w,3)
		write(*,*) " "
		write(*,*) "prod_lb = ",prod_lb
		write(*,*) "prod_ub = ",prod_ub
		pause
    endif
    
    if (prod_lb>=prod_ub) then
    	write(*,*) "estimation_demand: prod_lb>=prod_ub"
    	Delta_th = 1.0d0
    	Delta_ob = 0.0d0
    	return
    endif	
    call grid(prod_grid,prod_lb,prod_ub,1.0d0)
    

    call writedim(prod_grid,trim(savedir)//'prod_grid.txt')
    
    if (verbose>=2) then
    	write(*,*) "estimation_demand: Step 1.c done!"
		pause
	endif
     !	(1.d) Compute profit for each productivity and hours 
!    	firm_profit(y,hours,wage,l) 

	! 1.d.1 Compute the productivity corresponding to each point on the wage grid, conditional on hours
	prod_arr = 0.0d0
	do h_c = 1,n_hours
	do w_c = 1,n_w
		prod_arr(w_c,h_c)  = wage_pol_inv(w_c,h_c)
	enddo
	enddo
	
	call writedim(prod_arr,trim(savedir)//'prod_arr.txt')
	
	
	! 1.d.2 Interpolate wage_pol at each point on the common productivity grid (prod_grid), conditional on hours
	wage_arr = 0.0d0
	do h_c = 1,n_hours
	do p_c = 1,n_prod
		! Avoid extrapolation. When out of bounds, wage is not defined
			wage_arr(p_c,h_c) = pchip_extrap(prod_arr(:,h_c),w_grid(:,h_c),prod_grid(p_c))
	enddo
	enddo
	call writedim(wage_arr,trim(savedir)//'wage_arr.txt')

	! 1.d.3 Interpolate labsup at each interpolated wage (wage_arr), conditional on hours
	labsup_arr = 0.0d0
	do h_c = 1,n_hours
	do p_c = 1,n_w
	! Avoid extrapolation.
			! males
			labsup_arr(p_c,h_c) = pchip_extrap(w_grid(:,h_c),labsup(:,h_c),wage_arr(p_c,h_c))
	enddo
	enddo
	call writedim(labsup_arr,trim(savedir)//'labsup_arr.txt')

	! 1.d.4 Compute the profit for each firm p on the common productivity grid.
	profit_arr = 0.0d0
	do p_c = 1,n_prod
		prod_val = prod_grid(p_c)
		do h_c = 1,n_hours ! PT, FT
				hours = hours_grid(h_c)
				y_val   = firm_prod(prod_val,h_c)
				wage_val  = wage_arr(p_c,h_c)
				ls_val    = labsup_arr(p_c,h_c) 


				profit_arr(p_c,h_c) = firm_profit(y_val,hours,wage_val,ls_val) 
		enddo ! h_c
    enddo ! p_c
    
    call writedim(profit_arr,trim(savedir)//'profit_arr.txt')
    if (verbose>=2) then
    	write(*,*) "estimation_demand: Step 1.d done!"
		pause
	endif
	

    !---Step (2): Construct the theoretical sector choice function
    !	(2.a) Calculate Delta(h,p) using eq. 10
    ! Delta_th (dim: n_prod,n_hours): theoretical Delta
    Delta_th = 0.0d0
	Delta_th_act = 0.0d0
	Delta_act = 0.0d0
    profit_daily = profit_arr/365.0d0
	do p_c = 1,n_prod
		do h_c = 1,n_hours
			profit_vec(h_c) = (profit_daily(p_c,h_c)-p_eps_bar(h_c))
        enddo ! h_c
		! Conditional on activity, calculate prob of choose each h
		! outputs are log_sum (scalar) and delta_job (dim: 2*n_hours)
    	call sub_logsum(log_sum,delta_job, profit_vec,p_sigma_eps)
    	Delta_th_act(p_c,:) = delta_job
		! Calculate prob active firms
		profit_vec_act = [log_sum,0.0d0]
		call sub_logsum(log_sum,delta_vec_act, profit_vec_act,p_sigma_eps)
		Delta_act(p_c) = delta_vec_act(1) 
		! Calculate Delta_th(p,h) (theoretical sector choice)
		Delta_th(p_c,:) = Delta_th_act(p_c,:)*Delta_act(p_c)  	
	enddo
	call writedim(Delta_th_act,trim(savedir)//'Delta_th_act.txt')
	call writedim(Delta_act,trim(savedir)//'Delta_act.txt')
	call writedim(Delta_th,trim(savedir)//'Delta_th.txt')
	if (verbose>=2) then    
    	write(*,*) "estimation_demand: Step 2 done!"
 		pause
 	endif
 	 	
     !---Step (3): Construct the observed sector choice function (Delta hat in paper)
	 ! For the derivative of the wage function, we take the derivative of the 
	 !	wage function derived from step (1.c) projected on the common productivity grid. 	 
	 
     !	(3.a) Calculate the derivative of wage w.r.t. p, i.e. w'(p)
     ! wage_dp (dim:p_c,h_c)
! 				!jacob: numerical derivative
 	
	
     !	(3.b) Calculate Delta_ob using eq. on the top of page 34

	! 3.b.1 Interpolate F_prob at the interpolated wages (wage_arr)
	wage_pdf = 0d0
	do h_c = 1,n_hours
		do p_c = 1,n_prod
				w_val = wage_arr(p_c,h_c) ! interpolated wage (posted by firms on the common productivity grid)
				wage_pdf(p_c,h_c) = pchip_extrap(w_grid(:,h_c), F_prob(:,h_c) , w_val)	
				wage_pdf(p_c,h_c) = max(0.0d0,wage_pdf(p_c,h_c))
		enddo
		wage_pdf(:,h_c)  = wage_pdf(:,h_c)  / sum(wage_pdf(:,h_c) )
	enddo
	call writedim(wage_pdf,trim(savedir)//'wage_pdf.txt')
	

	! 3.b.2 Calculate Delta_ob
	allocate(prod_PMF_act(n_prod),stat=istat)
	if (istat/=0) call myerror("Allocation of prod_PMF_act failed")
	Delta_ob = 0.0d0  
	prod_PMF_act = 0.0d0
    do p_c = 1,n_prod
    	do h_c = 1,n_hours
    			! For now, Delta_ob stores the numerator value only. Will be normalized by prod_PMF later.
    			Delta_ob(p_c,h_c) = hpol(h_c)/sum(hpol)*wage_pdf(p_c,h_c)  !hpol(h_c)*wage_pdf(p_c,h_c)
    			prod_PMF_act(p_c) = prod_PMF_act(p_c) + Delta_ob(p_c,h_c)
   		enddo ! h_c
    enddo ! p_c

    ! Calculate the productivity PMF for all firms (prod_PMF) using linear solver
	allocate(A_prod_PMF(n_prod,n_prod),stat=istat)
	if (istat/=0) call myerror("Allocation of A_prod_PMF failed")

	! First store b to prod_PMF
	prod_PMF = 0.0d0 
	prod_PMF(n_prod) = 1.0d0 
	! Then, construct the matrix A_prod_PMF
	A_prod_PMF = 0.0d0 
	do p_c = 1, n_prod 
		do pp_c = 1, n_prod - 1
			A_prod_PMF(pp_c,p_c) = prod_PMF_act(pp_c)*Delta_act(p_c)
			if (p_c == pp_c) then
				A_prod_PMF(pp_c,p_c) = (prod_PMF_act(pp_c)-1.0d0)*Delta_act(p_c)
			endif
		enddo 
	enddo 
	A_prod_PMF(n_prod,:) = 1.0d0 ! last row is all ones
	! Solve the linear system for prod_PMF
	call lu_solve(A_prod_PMF, prod_PMF)
	! Share of active firms 
	share_act = sum(Delta_act*prod_PMF)
	if (verbose>=2) then 
		write(*,*) "p_c	prod_PMF_act	prod_PMF Delta_act"
		do p_c = 1,n_prod 
			write(*,*) p_c,prod_PMF_act(p_c),prod_PMF(p_c),Delta_act(p_c)
		enddo 
				 

		write(*,*) "(1) sum of prod PMF conditional on act.  = ",sum(prod_PMF_act)
		write(*,*) "(2) sum of prod PMF = ",sum(prod_PMF)
		write(*,*) "(3) share_act = ", share_act
		write(*,*) "(1) and (2) should be 1, (3) should be < 1"
		pause
	endif
    ! Normalize prod_PMF such that it sums up to 1
    prod_PMF = prod_PMF/sum(prod_PMF)
    ! Normalize Delta_ob by gamma(p)
	do p_c = 1,n_prod
    	Delta_ob(p_c,:) = share_act*Delta_ob(p_c,:)/prod_PMF(p_c)
	enddo ! p_c
    ! assign output prod_PMF_out
    prod_PMF_out = prod_PMF

    call writedim(prod_PMF,trim(savedir)//'prod_PMF.txt')
    call writedim(Delta_ob,trim(savedir)//'Delta_ob.txt')
	
	if (verbose>=2) then
    	write(*,*) "estimation_demand: Step 3.b (last step) done!"
		pause
    endif
    

    !================ 
    contains
    
    function labsup_interp(wage) result(F)
    implicit none
    ! Declare inputs-outputs
    real(8),intent(in) :: wage
    real(8) :: F
    
    F = pchip_extrap(wage_grid,labsup(:,h_c),wage)
    
    end function labsup_interp
!--------------------------------------------------
!--------------------------------------------------
function wage_pol_inv(w_c,h_c) result(p)
    implicit none
    ! This function computes the inverse of the wage policy function
    !	using the first order condition of firm profit maximization. 
    ! Inputs:
    integer, intent(in) :: w_c,h_c
    ! Output: productivity
    real(8) :: p
    ! As inputs, the function uses labsup, labsup_der, w_c,h_c,g_c,w_grid

    ! Local variables
    real(8) :: numerator, denominator,h_wk
    
    numerator = labsup(w_c,h_c) + w_grid(w_c,h_c)*labsup_dw(w_c,h_c) 
    	
    h_wk = fun_hours_wk(hours_grid(h_c))
    denominator = p_theta(h_c)*labsup_dw(w_c,h_c)

    p = numerator / denominator
    
end function wage_pol_inv   
!--------------------------------------------------
    !--------------------------------------------------

    
    end subroutine estimation_demand
    

    !======================================================================= 
    !======================================================================= 
end module mod_estimation
