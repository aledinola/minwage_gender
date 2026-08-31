module mod_simulation
    
    !=======================================================================
    !        Module Simulation
    ! Here we simulate data from model to compute transition rates
    !=======================================================================
	!=======================================================================
    
    use mod_globals
    use mod_utilities 
    use mod_numerical
    use omp_lib
	implicit none
    
    private
    public sim_transition

    
    contains 
    !=======================================================================
    subroutine sim_transition(g,g_s,trans_mat,trans_mat_j,trans_emp_j)
    implicit none
    !Declare inputs/outputs:
    type(gdist),intent(in) :: g
    type(gdist_s),intent(in) :: g_s
    real(8),intent(out) :: trans_mat(0:,0:)
	real(8),optional,intent(out) :: trans_mat_j(0:,0:,:),trans_emp_j(0:,0:,:)
    !Declare local variables:
    integer :: hh_c,i_c,h_c,w_c,k_c,emp1_c,emp2_c,l0_c,l1_c,g_c,dur_c
    integer :: h1_c,h2_c,w1_c,w2_c,sp_c
    integer :: n_state_couple, n_state_single,n_state_emp
    integer,allocatable ::  sim_mat_couple0(:),sim_mat_single0(:)
    integer,allocatable :: state_single(:,:),state_couple(:,:),state_emp(:,:)
    integer :: counter,x0,x1,monthlydata_couple(Tm_sim,3),monthlydata_single(Tm_sim,2)
    integer :: birth_flag,istat
    real(8) :: sum_dur,max_dur,dur 
    integer,allocatable :: x_hh(:),birth_hh(:),sim_panel(:,:,:,:)
    real(8),allocatable :: dur_hh(:), rand_vec(:)
    logical :: couple_allocated,single_allocated
	    
	if (do_nocouple) then ! if we don't consider couples, set the flag couple_allocated to true
		couple_allocated = .true.
	else    
		couple_allocated =  (allocated(omega_uu1) .and. allocated(omega_uu2)  .and. allocated(omega_eu1) &
			 .and. allocated(omega_eu2) .and. allocated(omega_ue2) .and. allocated(omega_ue1) &
			 .and. allocated(omega_ee1) .and. allocated(omega_ee2) &
			 .and. allocated(Oh_eu1) .and. allocated(Oh_ue2) &
			 .and. allocated(Oh_ee12) .and. allocated(Oh_ee1) .and. allocated(Oh_ee2) &
			 .and. allocated(Oh_ee_minus) .and. allocated(Oh_eu2_1stays) .and. allocated(Oh_ue1_2stays) & 
			 .and. allocated(Oh_ee1_2stays) .and. allocated(Oh_ee2_1stays))
    endif
    
    single_allocated = (allocated(omega_u_sf) .and. allocated(omega_u_sm) &
    	 .and. allocated(omega_e_sf) .and. allocated(omega_e_sm) &
    	 .and. allocated(Oh_e_sm) .and. allocated(Oh_e_sf) )
    

    ! Check if decision rules Omegas and Oh's are present
    if ( .not. (couple_allocated .and. single_allocated) ) then
    	 call myerror("Error in sim_transition: decision rules not allocated")
    endif
        
    
    ! Condense state space
    n_state_emp = n_hours*n_w+1
    allocate(state_emp(n_state_emp,2))
    
    i_c = 1
    do h_c = 1,n_hours
    do w_c = 1,n_w
    	state_emp(i_c,:) = [w_c,h_c]
    	i_c = i_c+1
    enddo
    enddo
    state_emp(i_c,:) = [0,0]
    
    if (do_nocouple == .false.) then
		n_state_couple = n_state_emp * n_state_emp * n_kids
		allocate(state_couple(n_state_couple,3))
		! state_couple maps single index i_c into (emp1_c,emp2_c,k_c)
		i_c=1
		do k_c=1,n_kids
			do emp2_c=1,n_state_emp
				do emp1_c=1,n_state_emp
					state_couple(i_c,:)=[emp1_c,emp2_c,k_c]
					i_c=i_c+1
				enddo
			enddo
		enddo
   	endif ! do_nocouple == .false.
   	
   	n_state_single = n_state_emp * n_kids * n_spinc
    allocate(state_single(n_state_single,3))
	! state_single maps single index i_c into (emp1_c,k_c,sp_c)
	i_c=1
	do sp_c = 1,n_spinc
	do k_c=1,n_kids
		do emp1_c=1,n_state_emp
			state_single(i_c,:)=[emp1_c,k_c,sp_c]
			i_c=i_c+1
		enddo
	enddo
   	enddo

	! Simulate initial distributions
    ! sim_mat_couple0 and sim_mat_single0 are vectors of initial states for simulated households
    if (do_nocouple == .false.) then
    	allocate(sim_mat_couple0(N_sim_couple),STAT = istat)
    	if (istat /= 0) call myerror("Failed to allocation sim_mat_couple0 for initial distribution.")
    endif
    allocate(sim_mat_single0(N_sim_single),STAT = istat)
    if (istat /= 0) call myerror("Failed to allocation sim_mat_single0 for initial distribution.")
 
    if (do_nocouple == .false.) then
    	call sim_init(sim_mat_couple0,1)
    endif
    call sim_init(sim_mat_single0,0)
    if (verbose==2) then
    	write(*,*) "Finished simulating initial distribution"
    endif 
    
    ! Generate simulated panel data
    
    max_dur = real(T_sim,8)
    
    ! sim_panel holds the simulated data. For each individual, gender, and time period, 
    !	sim_panel contains: employment index, birth flag    
    allocate(x_hh(max_counter),dur_hh(max_counter),birth_hh(max_counter), &
    	sim_panel(N_sim_couple+N_sim_single/2,2,Tm_sim,2), STAT = istat)
    if (istat /= 0) call myerror("Failed to allocation simulation arrays.")

	sim_panel(:,:,:,:) = -1

     ! Generate simulated data for singles    
    do hh_c = 1,N_sim_single
    	! reset x_hh and dur_hh
    	x_hh = 0
    	dur_hh = 0.0d0
    	birth_hh = 0
    	! reset counter and sum_dur
    	counter = 1
    	sum_dur = 0.0d0
    	x0 = sim_mat_single0(hh_c)
    	x_hh(counter) = x0
    	! Gender: the first half of single hhs are male, the second female
    	if (hh_c<=N_sim_single/2) then
    		g_c = 1
    	else
    		g_c = 2
    	endif 	
    	do while (counter<max_counter .and. sum_dur<max_dur) ! the sum of simulated duration is less than a preset minimum. 
    		! sim_hh: given current state x0, and single-gender indicator (g_c for singles)
    		!	sim_hh returns duration (dur) to the next state (x1) and an indicator for whether
    		!	there has been a death/birth event (birth_flag)
			
    		rand_vec = randmat1(N_sim_couple+hh_c,(counter-1)*6+1:counter*6)

    		call sim_hh(x0,rand_vec,g_c,x1,dur,birth_flag)
    		dur_hh(counter) = dur
    		x_hh(counter+1) = x1
    		birth_hh(counter+1) = birth_flag
    		! update x0
    		x0 = x1
    		! update counter and sum_dur
    		counter = counter + 1
    		sum_dur = sum_dur + dur
    	enddo
    	! spell2panel_single: given simulated spell data (x_hh, dur_hh, birth_hh), 
    	!	spell2panel_single returns monthly lfs (monthlydata_hh(:,1)) and 
    	!	whether a death/birth event has happened (monthlydata_hh(:,2))
    	
    	call spell2panel_single(x_hh(1:counter),dur_hh(1:counter-1),birth_hh(1:counter),monthlydata_single)
    	! Update simulated panel
    	sim_panel(N_sim_couple + (hh_c - (g_c-1)*N_sim_single/2) ,g_c,:,:) = monthlydata_single ! dim: (1:Tm_sim,2)
    enddo ! hh_c for singles

    ! Output the simulated data

	! Compute monthly transition moments
	! - lfs transition matrix by gender
	call lfs_trans(trans_mat)

	! Compute untargeted transition moments 
	if (present(trans_mat_j)) then
		call lfs_trans_j(trans_mat_j,trans_emp_j)
    endif 

    contains
    
    !=======================================================================
		function state2index_couple(w1_c,h1_c,w2_c,h2_c,k_c) result(ind)
		implicit none
		!Purpose: map multiple subscripts for the states into a single index.
		!Declare inputs/outputs:
		integer,intent(in) :: w1_c,h1_c,w2_c,h2_c,k_c
		integer :: ind
		!Declare local variables:
		integer :: emp1_c,emp2_c
		
		if (h1_c==0) then
			emp1_c = n_state_emp
		else
			emp1_c = w1_c+(h1_c-1)*n_w
		endif 
		if (h2_c==0) then
			emp2_c = n_state_emp
		else
			emp2_c = w2_c+(h2_c-1)*n_w
		endif 
		
		ind = emp1_c+(emp2_c-1)*n_state_emp+(k_c-1)*n_state_emp*n_state_emp
		
		end function state2index_couple
    !=======================================================================
		function state2index_single(w1_c,h1_c,k_c,sp_c) result(ind)
		implicit none
		!Declare inputs/outputs:
		integer,intent(in) :: w1_c,h1_c,k_c,sp_c
		integer :: ind
		!Declare local variables:
		integer :: emp1_c
		
		if (h1_c==0) then
			emp1_c = n_state_emp
		else
			emp1_c = w1_c+(h1_c-1)*n_w
		endif 
		
		ind = emp1_c + (k_c-1)*n_state_emp + (sp_c-1)*n_kids*n_state_emp
		
		end function state2index_single
        !=======================================================================
        
		subroutine sim_init(sim_mat0,iscouple)
		! This subroutine samples from the steady state distribution over emp states,kids for couples
		implicit none
		!Declare inputs/outputs:
		!	- iscouple is 1 if drawing initial states for couples, 0 if for singles
		! 	- sim_mat0 is a vector of size N_sim_couple or N_sim_single containing state indices for each simulated hh.
		!This subroutine uses g (ss distribution) as inputs
		integer,intent(in) :: iscouple
		integer,intent(out) :: sim_mat0(:) 
		!Declare local variables:
		integer:: N
		integer :: i_c,jointemp_c,ind,h_c,w_c,h1_c,w1_c,h2_c,w2_c,k_c,istat,sp_c
		integer :: subvec4(4),subvec2(2)
		real(8) :: empdist_alpha_single(2),rand_vec(2)
        real(8), allocatable :: g_temp(:)
		
		! Check if the size of the output matrix is consistent with the pre-specified number of simulated households
		if (iscouple==1) then
			N = N_sim_couple
		else
			N = N_sim_single
		endif 
		if (size(sim_mat0,dim=1) /= N) then
			write(*,*) "iscouple = ",iscouple
			call myerror("Size of sim_mat0 is incorrect!")
		endif 
	
		
		! Begin simulation
		
        if (iscouple==0) then ! Simulate single households
			! The first half (N_sim_single/2 households) of sim_mat0 are male, 
			!	the second half female.
            
            
			! First, single males
			! Get prob dist over (e_sm,u_sm) 
	
		    empdist_alpha_single(1) = sum(g_s%e_sm)
			empdist_alpha_single(2) = sum(g_s%u_sm)
            
			! Begin simulating single males
            !$omp parallel if (par_fortran==1) default(shared) &
            !$ private(i_c,rand_vec,jointemp_c,g_temp,ind,subvec4,subvec2,w1_c,h1_c,k_c,sp_c)
            !$omp do
			do i_c = 1,N/2	
				! draw random number
				rand_vec = randmat0((1-iscouple)*N_sim_couple+i_c,:)

				! First, determine which of the two states: e,u
				! jointemp_c = 1 (e_sm), 2 (u_sm)
				jointemp_c = sample_pmf_fast(empdist_alpha_single,rand_vec(1))
				if (jointemp_c == 1) then ! e
					! need to determine w1_c,h1_c,k_c
                    g_temp = reshape(g_s%e_sm,[n_w*n_hours*n_kids*n_spinc])
					ind = sample_pmf_fast(g_temp,rand_vec(2))
					subvec4 = ind2sub([n_w,n_hours,n_kids,n_spinc],ind)
					w1_c =  subvec4(1)
					h1_c =  subvec4(2)
					k_c  =  subvec4(3)
					sp_c =  subvec4(4)
				elseif (jointemp_c ==2 ) then !u
					w1_c = 0
					h1_c = 0
					! need to determine k_c
                    g_temp = reshape(g_s%u_sm,[n_kids*n_spinc])
					ind = sample_pmf_fast(g_temp,rand_vec(2))
					subvec2 = ind2sub([n_kids,n_spinc],ind)
					k_c = subvec2(1)
					sp_c = subvec2(2)
				else
					call myerror("Single males: jointemp_c out of bounds!")
				endif 
				
				! Fill the simulation matrix
				sim_mat0(i_c) = state2index_single(w1_c,h1_c,k_c,sp_c)
            enddo !i_c index for simulated single males
			!$omp enddo
            !$omp end parallel
            
			! Then, single females
			! Get prob dist over (e_sf,u_sf)
			
			empdist_alpha_single(1) = sum(g_s%e_sf)
			empdist_alpha_single(2) = sum(g_s%u_sf)
            
			! Begin simulating single females
            !$omp parallel if (par_fortran==1) default(shared) &
            !$ private(i_c,rand_vec,jointemp_c,g_temp,ind,subvec4,subvec2,w1_c,h1_c,k_c,sp_c)
            !$omp do
			do i_c = N/2+1,N	
						
				! draw random number
				rand_vec = randmat0((1-iscouple)*N_sim_couple+i_c,:)

				! First, determine which of the four states: e_sf,u_sf
				jointemp_c = sample_pmf_fast(empdist_alpha_single,rand_vec(1))
				if (jointemp_c == 1) then ! e
					! need to determine w1_c,h1_c,k_c
					ind = sample_pmf_fast(reshape(g_s%e_sf, &
						[n_w*n_hours*n_kids*n_spinc]),rand_vec(2))
					subvec4 = ind2sub([n_w,n_hours,n_kids,n_spinc],ind)
					w1_c = subvec4(1)
					h1_c = subvec4(2)
					k_c = subvec4(3)
					sp_c = subvec4(4)
				elseif (jointemp_c ==2 ) then !u
					w1_c = 0
					h1_c = 0
					! need to determine k_c
					ind = sample_pmf_fast(reshape(g_s%u_sf, [n_kids*n_spinc]),rand_vec(2))
					subvec2 = ind2sub([n_kids,n_spinc],ind)
					k_c = subvec2(1)
					sp_c = subvec2(2)
				else
					call myerror("Single females: jointemp_c out of bounds!")
				endif 
				! Fill the simulation matrix
				sim_mat0(i_c) = state2index_single(w1_c,h1_c,k_c,sp_c)
            enddo !i_c index for simulated single females
            !$omp enddo
            !$omp end parallel
            
            
		endif !iscouple
	
		end subroutine sim_init
    !=======================================================================
		subroutine sim_hh(x0,rand_vec,sg_c,x1,dur,birth_flag)
		use mod_numerical, only: rand_exponential
		implicit none
		!Declare Input/Output variables
		integer,intent(in) :: x0, sg_c
		real(8),intent(in) :: rand_vec(:)
		integer,intent(out) :: x1, birth_flag
		real(8),intent(out) :: dur
		! This subroutine also uses acceptance rules Omegas and Ohs
		!Declare local variables
		integer :: emp1_c,emp2_c,k_c,w1_c,h1_c,w2_c,h2_c,sp_c
		integer :: kp_c,w1p_c,h1p_c,w2p_c,h2p_c
		integer, allocatable :: subvec(:)
		integer :: hp_c,wp_c,dur_c,ind
	real(8) :: dur1,dur2,dur3,dur4,dur5,dur6,rate1,rate2,rate3,rate4,rate5,rate6,temp
		real(8) :: prob_job(n_w,n_hours)
		! Unpack initial state x0
		if (sg_c == 0) then ! couple
			emp1_c = state_couple(x0,1)
			emp2_c = state_couple(x0,2)
			k_c = state_couple(x0,3)
			w1_c = state_emp(emp1_c,1)
			h1_c = state_emp(emp1_c,2)
			w2_c = state_emp(emp2_c,1)
			h2_c = state_emp(emp2_c,2)
		elseif (sg_c > 0) then ! single
			emp1_c = state_single(x0,1)
			w1_c = state_emp(emp1_c,1)
			h1_c = state_emp(emp1_c,2)
			k_c = state_single(x0,2)
			sp_c = state_single(x0,3)
		endif 
		
		! Draw random number
		
		! Initialize outputs
		birth_flag=0
		
		if (sg_c==0) then ! Couples
			! Which of the four cases: ee,eu,ue,uu
		elseif (sg_c ==1 ) then ! Single Males
			! Which of the two cases: e or u
			if (w1_c==0) then ! u_sm
				! (1) death, 
				rate1 = p_rho_d
				dur1 = rand_exponential(rand_vec(1),rate1)
				! (2) kids change, 
				kp_c = min(n_kids,k_c+1)
				rate2 = max(p_phi(kp_c,1,1),tiny(1.0d0))
				dur2 = rand_exponential(rand_vec(2),rate2)
				! (3) job-finding of m
				rate3 = 0.0d0
				do hp_c = 1,n_hours
				do wp_c = 1,n_w
					! job finding of m
					temp = omega_u_sm(wp_c,hp_c,k_c,sp_c)
					rate3 = rate3 + hpol(hp_c)*F_prob(wp_c,hp_c)*I_mw(wp_c,hp_c,1)*p_lambda_u*temp
				enddo !wp_c
				enddo !hp_c
				! calculate durations to job findings
				rate3 = max(rate3,tiny(1.0d0))
				dur3 = rand_exponential(rand_vec(3),rate3)
				! Record duration of spell x0
				dur = minval([dur1,dur2,dur3])
				! Find the destination state x1
				dur_c = minloc([dur1,dur2,dur3],dim=1)
				select case (dur_c)
					case (1) ! death/birth
						kp_c = 1
						w1p_c=0
						h1p_c=0
						birth_flag=1
					case (2) !kids change, 
						kp_c = min(n_kids,k_c+1)
						w1p_c=w1_c
						h1p_c=h1_c
					case (3) ! m finds a job
						kp_c = k_c
						do hp_c = 1,n_hours
						do wp_c = 1,n_w
							temp = omega_u_sm(wp_c,hp_c,k_c,sp_c)
							prob_job(wp_c,hp_c) = hpol(hp_c)*F_prob(wp_c,hp_c)*I_mw(wp_c,hp_c,1)*p_lambda_u*temp					
						enddo !wp_c
						enddo !hp_c
						ind = sample_pmf_fast(reshape(prob_job,[n_w*n_hours]),rand_vec(4))
						subvec = ind2sub([n_w,n_hours],ind)
						w1p_c = subvec(1)
						h1p_c = subvec(2)
					case default
						call myerror("sim_hh: sm dur_c out of bounds")
				end select
			else if (w1_c>0) then ! e_sm
				! (1) death, 
				rate1 = p_rho_d
				dur1 = rand_exponential(rand_vec(1),rate1)
				! (2) kids change, 
				kp_c = min(n_kids,k_c+1)
				rate2 = max(p_phi(kp_c,1,1),tiny(1.0d0))
				dur2 = rand_exponential(rand_vec(2),rate2)
				! (3) job-finding of m
				rate3 = 0.0d0
				do hp_c = 1,n_hours
				do wp_c = 1,n_w
					! job finding of m
					temp = omega_e_sm(wp_c,hp_c,w1_c,h1_c,k_c,sp_c)
					rate3 = rate3 + hpol(hp_c)*F_prob(wp_c,hp_c)*I_mw(wp_c,hp_c,1)*p_lambda_e*temp			
				enddo !wp_c
				enddo !hp_c
				! calculate durations to job findings
				rate3 = max(rate3,tiny(1.0d0))
				dur3 = rand_exponential(rand_vec(3),rate3)
				! (4) job separation of m
				rate4 = p_delta_m(h1_c)
				dur4 = rand_exponential(rand_vec(4),rate4)
			
				! Record duration of spell x0
				dur = minval([dur1,dur2,dur3,dur4])
				! Find the destination state x1
				dur_c = minloc([dur1,dur2,dur3,dur4],dim=1)
				select case (dur_c)
					case (1) ! death/birth
						kp_c = 1
						w1p_c=0
						h1p_c=0
						birth_flag=1
					case (2) !kids change,  consider quits
						kp_c = min(n_kids,k_c+1)
						if (Oh_e_sm(w1_c,h1_c,kp_c,sp_c)) then ! m does not quit
							w1p_c=w1_c
							h1p_c=h1_c
						else ! m quits
							w1p_c=0
							h1p_c=0
						endif
					case (3) ! m finds a job
						kp_c = k_c
						do hp_c = 1,n_hours
						do wp_c = 1,n_w
							temp = omega_e_sm(wp_c,hp_c,w1_c,h1_c,k_c,sp_c)
							prob_job(wp_c,hp_c) = hpol(hp_c)*F_prob(wp_c,hp_c)*I_mw(wp_c,hp_c,1)*p_lambda_e*temp					
						enddo !wp_c
						enddo !hp_c
						ind = sample_pmf_fast(reshape(prob_job,[n_w*n_hours]),rand_vec(5))
						subvec = ind2sub([n_w,n_hours],ind)
						w1p_c = subvec(1)
						h1p_c = subvec(2)

					case (4) ! m separates from job		
						kp_c = k_c
						w1p_c=0
						h1p_c=0	
					case default
						call myerror("sim_couple: sm dur_c out of bounds")
				end select
			else
				call myerror("sim_hh: single male state not well defined")
			endif 
			! Pack new state vars to index x1
			x1 = state2index_single(w1p_c,h1p_c,kp_c,sp_c) 
		elseif (sg_c == 2) then !Single Females
			! Which of the two cases: e or u
			if (w1_c==0) then ! u_sf
				! (1) death, 
				rate1 = p_rho_d
				dur1 = rand_exponential(rand_vec(1),rate1)
				! (2) kids change, 
				kp_c = min(n_kids,k_c+1)
				rate2 = max(p_phi(kp_c,2,1),tiny(1.0d0))
				dur2 = rand_exponential(rand_vec(2),rate2)
				! (3) job-finding of f
				rate3 = 0.0d0
				do hp_c = 1,n_hours
				do wp_c = 1,n_w
					! job finding of f
					temp = omega_u_sf(wp_c,hp_c,k_c,sp_c)
					rate3 = rate3 + hpol(hp_c)*F_prob(wp_c,hp_c)*I_mw(wp_c,hp_c,2)*p_lambda_u*temp
				enddo !wp_c
				enddo !hp_c
				! calculate durations to job findings
				rate3 = max(rate3,tiny(1.0d0))
				dur3 = rand_exponential(rand_vec(3),rate3)
				! Record duration of spell x0
				dur = minval([dur1,dur2,dur3])
				! Find the destination state x1
				dur_c = minloc([dur1,dur2,dur3],dim=1)
				select case (dur_c)
					case (1) ! death/birth
						kp_c = 1
						w1p_c=0
						h1p_c=0
						birth_flag=1
					case (2) !kids change, 
						kp_c = min(n_kids,k_c+1)
						w1p_c=w1_c
						h1p_c=h1_c
					case (3) ! f finds a job
						kp_c = k_c
						do hp_c = 1,n_hours
						do wp_c = 1,n_w
							temp = omega_u_sf(wp_c,hp_c,k_c,sp_c)
							prob_job(wp_c,hp_c) = hpol(hp_c)*F_prob(wp_c,hp_c)*I_mw(wp_c,hp_c,2)*p_lambda_u*temp					
						enddo !wp_c
						enddo !hp_c
						ind = sample_pmf_fast(reshape(prob_job,[n_w*n_hours]),rand_vec(4))
						subvec = ind2sub([n_w,n_hours],ind)
						w1p_c = subvec(1)
						h1p_c = subvec(2)
					case default
						call myerror("sim_hh: sf dur_c out of bounds")
				end select
			else if (w1_c>0) then ! e_sf
				! (1) death, 
				rate1 = p_rho_d
				dur1 = rand_exponential(rand_vec(1),rate1)
				! (2) kids change, 
				kp_c = min(n_kids,k_c+1)
				rate2 = max(p_phi(kp_c,2,1),tiny(1.0d0))
				dur2 = rand_exponential(rand_vec(2),rate2)
				! (3) job-finding of f
				rate3 = 0.0d0
				do hp_c = 1,n_hours
				do wp_c = 1,n_w
					! job finding of f
					temp = omega_e_sf(wp_c,hp_c,w1_c,h1_c,k_c,sp_c)
					rate3 = rate3 + hpol(hp_c)*F_prob(wp_c,hp_c)*I_mw(wp_c,hp_c,2)*p_lambda_e*temp			
				enddo !wp_c
				enddo !hp_c
				! calculate durations to job findings
				rate3 = max(rate3,tiny(1.0d0))
				dur3 = rand_exponential(rand_vec(3),rate3)
				! (4) job separation of f
				rate4 = p_delta_f(h1_c)
				dur4 = rand_exponential(rand_vec(4),rate4)
			
				! Record duration of spell x0
				dur = minval([dur1,dur2,dur3,dur4])
				! Find the destination state x1
				dur_c = minloc([dur1,dur2,dur3,dur4],dim=1)
				select case (dur_c)
					case (1) ! death/birth
						kp_c = 1
						w1p_c=0
						h1p_c=0
						birth_flag=1
					case (2) !kids change,  consider quits
						kp_c = min(n_kids,k_c+1)
						if (Oh_e_sf(w1_c,h1_c,kp_c,sp_c)) then ! f does not quit
							w1p_c=w1_c
							h1p_c=h1_c
						else ! f quits
							w1p_c=0
							h1p_c=0
						endif
					case (3) ! f finds a job
						kp_c = k_c
						do hp_c = 1,n_hours
						do wp_c = 1,n_w
							temp = omega_e_sf(wp_c,hp_c,w1_c,h1_c,k_c,sp_c)
							prob_job(wp_c,hp_c) = hpol(hp_c)*F_prob(wp_c,hp_c)*I_mw(wp_c,hp_c,2)*p_lambda_e*temp					
						enddo !wp_c
						enddo !hp_c
						ind = sample_pmf_fast(reshape(prob_job,[n_w*n_hours]),rand_vec(5))
						subvec = ind2sub([n_w,n_hours],ind)
						w1p_c = subvec(1)
						h1p_c = subvec(2)
					case (4) ! f separates from job		
						kp_c = k_c
						w1p_c=0
						h1p_c=0	
					case default
						call myerror("sim_couple: sf dur_c out of bounds")
				end select
			else
				call myerror("sim_hh: single female state not well defined")
			endif 
			! Pack new state vars to index x1
			x1 = state2index_single(w1p_c,h1p_c,kp_c,sp_c) 
		else
			call myerror("sim_hh: single-gender input (sg_c) out of bounds.")	
		endif  !sg_c
		
		
		
		end subroutine sim_hh
    !=======================================================================
    	subroutine spell2panel(x_hh,dur_hh,birth_hh,monthlydata_hh)
		implicit none
		! This subroutine converts spell data into monthly panel data.
		! Note: unit period in the model is one year, not month! 
		!	T_sim is in terms of years; Tm_sim is in terms of months (Tm_sim = T_sim*12)
		!Declare Input/Output variables
		integer,intent(in) :: x_hh(:),birth_hh(:)
		real(8),intent(in) :: dur_hh(:)
		integer,intent(out) :: monthlydata_hh(:,:)
		!Declare local variables
		integer :: t_c,w1_c,h1_c,w2_c,h2_c,spell_c,spell0_c,emp1_c,emp2_c,birth_flag
		real(8),allocatable :: cdur(:)
		
		
		! Check size consistency
		if (size(x_hh) /= size(birth_hh)) call myerror("spell2panel: size(x_hh) /= size(birth_hh)")
		if (size(x_hh) /= size(dur_hh)+1) call myerror("spell2panel: size(x_hh) /= size(dur_hh)+1")
		if (size(monthlydata_hh,1) /= Tm_sim) call myerror("spell2panel: size(monthlydata_hh,1) /= Tm_sim")
		if (size(monthlydata_hh,2) /= 3) call myerror("spell2panel: size(monthlydata_hh,2) /= 3")
		
		monthlydata_hh=0
		cdur = cumsum(dur_hh)
		spell0_c = 0 ! previous spell number
		
		! lfs (hours of m and f) of each month is defined as the state in the middle of the month. 

		do t_c = 1,Tm_sim
			
			spell_c = locate(cdur,dble(t_c-1)/12.0d0+0.5d0) + 1
			if (spell_c<=size(x_hh)) then
				emp1_c = state_couple(x_hh(spell_c),1)
				emp2_c = state_couple(x_hh(spell_c),2)
				h1_c = state_emp(emp1_c,2)
				h2_c = state_emp(emp2_c,2)
				
				birth_flag = birth_hh(spell_c)
				! If the current month and the previous month correspond to the same spell, 
				!	set birth_flag to zero.
				if (spell_c == spell0_c) birth_flag = 0 
			else ! if spell_c out of bounds
				h1_c = -1
				h2_c = -1
				birth_flag = -1
			endif
			spell0_c = spell_c
			monthlydata_hh(t_c,1) = h1_c
			monthlydata_hh(t_c,2) = h2_c
			monthlydata_hh(t_c,3) = birth_flag
			
		enddo
		
					
		
		end subroutine spell2panel
	 !=======================================================================
    	subroutine spell2panel_single(x_hh,dur_hh,birth_hh,monthlydata_hh)
		implicit none
		!Declare Input/Output variables
		integer,intent(in) :: x_hh(:),birth_hh(:)
		real(8),intent(in) :: dur_hh(:)
		integer,intent(out) :: monthlydata_hh(:,:)
		!Declare local variables
		integer :: t_c,w1_c,h1_c,spell_c,spell0_c,emp1_c,birth_flag
		real(8),allocatable :: cdur(:)
		
		
		! Check size consistency
		if (size(x_hh) /= size(birth_hh)) call myerror("spell2panel: size(x_hh) /= size(birth_hh)")
		if (size(x_hh) /= size(dur_hh)+1) call myerror("spell2panel: size(x_hh) /= size(dur_hh)+1")
		if (size(monthlydata_hh,1) /= Tm_sim) call myerror("spell2panel: size(monthlydata_hh,1) /= Tm_sim")
		if (size(monthlydata_hh,2) /= 2) call myerror("spell2panel: size(monthlydata_hh,2) /= 2")
		
		monthlydata_hh=0
		cdur = cumsum(dur_hh)
		spell0_c = 0 ! previous spell number
		
		! lfs (hours of m and f) of each month is defined as the state in the middle of the month. 

		do t_c = 1,Tm_sim
			
			spell_c = locate(cdur,dble(t_c-1)/12.0d0+0.5d0) + 1
			if (spell_c<=size(x_hh)) then
				emp1_c = state_single(x_hh(spell_c),1)
				h1_c = state_emp(emp1_c,2)				
				birth_flag = birth_hh(spell_c)
				! If the current month and the previous month correspond to the same spell, 
				!	set birth_flag to zero.
				if (spell_c == spell0_c) birth_flag = 0 
			else ! if spell_c out of bounds
				h1_c = -1
				birth_flag = -1
			endif
			spell0_c = spell_c
			monthlydata_hh(t_c,1) = h1_c
			monthlydata_hh(t_c,2) = birth_flag
			
		enddo
		
					
		
		end subroutine spell2panel_single
		!=======================================================================
    	subroutine lfs_trans(trans_mat)
    	! This subroutine calculates the monthly labor force transition matrix.
		implicit none
		!Declare Input/Output variables
		! trans_mat: transition matrix for men and women
		real(8), intent(inout) :: trans_mat(0:,0:)
		! This subroutine also uses sim_panel as input.
		!	sim_panel has dimension: (1:N_sim_couple+N_sim_single/2,1:2,1:Tm_sim,2)
		! 	- First dimension: individual index
		!	- Second dimension: (1) male, (2) female
		!	- Third dimension: simulated time period
		!	- Fourth dimension: (1) labor force state, (2) birth flag
				
		!Declare local variables
		integer :: t_c,l0_c,l1_c,g_c,N
		real(8) :: n_base,n_numerator,trans_t(0:3,0:3,Tm_sim-1)
		logical,allocatable :: base(:,:),numerator(:,:)
		
		if (lbound(trans_mat,1)/=0 .or. lbound(trans_mat,2)/=0) call myerror("trans_mat wrong lower bound")
		
		! Allocate base and numerator
		N = N_sim_couple+N_sim_single/2
		allocate(base(N,2),numerator(N,2),STAT = istat)
    	if (istat /= 0) call myerror("Failed to allocation base and numerator.")
		
		! Calculate transition rates for each t
		do t_c = 1,Tm_sim-1
			do l0_c = 0,3
				! base a vector of logicals, true if the nth individual is in the labor force state l0_c in period t_c
				!	and the birth_flag in period t_c+1 is zero.
				base = ((sim_panel(:,:,t_c,1) == l0_c) .and. (sim_panel(:,:,t_c+1,2) == 0 )  &
					.and. ((sim_panel(:,:,t_c+1,1) >=0) .and. (sim_panel(:,:,t_c+1,1) <=3) ))
				n_base = dble(count(base))
				do l1_c = 0,3
					numerator = (base .and. (sim_panel(:,:,t_c+1,1) == l1_c))
					n_numerator = dble(count(numerator))
					trans_t(l0_c,l1_c,t_c) = n_numerator/n_base
				enddo ! l1_c
				
			enddo!l0_c	
		enddo !t_c
		
		!Average over Tm_sim
		do l0_c = 0,3
			do l1_c = 0,3
				trans_mat(l0_c,l1_c) = sum(trans_t(l0_c,l1_c,1:Tm_sim-1), &
					mask = (trans_t(l0_c,l1_c,1:Tm_sim-1)==trans_t(l0_c,l1_c,1:Tm_sim-1)))/ &
						(tiny(1.0d0)+dble(count(trans_t(l0_c,l1_c,1:Tm_sim-1)==trans_t(l0_c,l1_c,1:Tm_sim-1))))
			enddo !l1_c
		enddo !l0_c
		
		
		
		end subroutine lfs_trans
    !=======================================================================
    	subroutine lfs_trans_j(trans_mat_j,trans_emp_j)
    	! This subroutine calculates 
		!	- trans_mat_j: the monthly labor force (0=NE, 1=ME, 2=PT, 3=FT) transition matrix
    	!	for male and female.
		!	- trans_emp_j: the monthly employment (0=NE, 1=emp (including all types)) transition
		!	matrix for male and female.
		implicit none
		!Declare Input/Output variables
		! trans_mat: transition matrix for men and women
		real(8), intent(inout) :: trans_mat_j(0:,0:,:),trans_emp_j(0:,0:,:)
		! This subroutine also uses sim_panel as input.
		!	sim_panel has size: (1:N_sim_couple+N_sim_single/2,1:2,1:Tm_sim,2)
		! First dimension: household index
		! Second dimension: gender 1=male, 2=female
		! Third dimension: time period
		! Fourth dimension: (1) labor force state, (2) birth flag

		!Declare local variables
		integer :: t_c,l0_c,l1_c,g_c,N
		real(8) :: n_base,n_numerator
		real(8),allocatable :: trans_t_j(:,:,:,:)
		logical,allocatable :: base(:),numerator(:)
		

		! Allocate base and numerator
		N = N_sim_couple+N_sim_single/2
		allocate(base(N),numerator(N),STAT = istat)
    	if (istat /= 0) call myerror("Failed to allocation base and numerator.")
		

		! -----Compurte trans_mat_j

		! Calculate transition rates for each t
		allocate( trans_t_j(0:3,0:3,Tm_sim-1,2))

		do g_c =1,2
		do t_c = 1,Tm_sim-1
			do l0_c = 0,3
				! base a vector of logicals, true if the nth individual is in the labor force state l0_c in period t_c
				!	and the birth_flag in period t_c+1 is zero.
				base = ((sim_panel(:,g_c,t_c,1) == l0_c) .and. (sim_panel(:,g_c,t_c+1,2) == 0 )  &
					.and. ((sim_panel(:,g_c,t_c+1,1) >=0) .and. (sim_panel(:,g_c,t_c+1,1) <=3) ))
				n_base = dble(count(base))
				do l1_c = 0,3
					numerator = (base .and. (sim_panel(:,g_c,t_c+1,1) == l1_c))
					n_numerator = dble(count(numerator))
					trans_t_j(l0_c,l1_c,t_c,g_c) = n_numerator/n_base
				enddo ! l1_c
				
			enddo!l0_c	
		enddo !t_c
		enddo !g_c
		
		!  Average over Tm_sim
		do g_c = 1,2
			do l0_c = 0,3
			do l1_c = 0,3
				trans_mat_j(l0_c,l1_c,g_c) = sum(trans_t_j(l0_c,l1_c,1:Tm_sim-1,g_c), &
					mask = (trans_t_j(l0_c,l1_c,1:Tm_sim-1,g_c)==trans_t_j(l0_c,l1_c,1:Tm_sim-1,g_c)))/ &
						(tiny(1.0d0)+dble(count(trans_t_j(l0_c,l1_c,1:Tm_sim-1,g_c)==trans_t_j(l0_c,l1_c,1:Tm_sim-1,g_c))))
			enddo !l1_c
			enddo !l0_c
		enddo !g_c
		
		! -----Compurte trans_emp_j
		! Calculate transition rates for each t
		deallocate(trans_t_j)
		allocate( trans_t_j(0:1,0:1,Tm_sim-1,2))

		do g_c =1,2
		do t_c = 1,Tm_sim-1
			! From NE 
			l0_c = 0
			! base a vector of logicals, true if the nth individual is in the labor force state l0_c in period t_c
			!	and the birth_flag in period t_c+1 is zero.
			base = ((sim_panel(:,g_c,t_c,1) == l0_c) .and. (sim_panel(:,g_c,t_c+1,2) == 0 )  &
				.and. ((sim_panel(:,g_c,t_c+1,1) >=0) .and. (sim_panel(:,g_c,t_c+1,1) <=3) ))
			n_base = dble(count(base))
			! - NE to NE
				l1_c = 0
				numerator = (base .and. (sim_panel(:,g_c,t_c+1,1) == l1_c))
				n_numerator = dble(count(numerator))
				trans_t_j(l0_c,l1_c,t_c,g_c) = n_numerator/n_base
			! - NE to emp
				l1_c = 1
				numerator = (base .and. (sim_panel(:,g_c,t_c+1,1) >=1 .and. sim_panel(:,g_c,t_c+1,1) <=3))
				n_numerator = dble(count(numerator))
				trans_t_j(l0_c,l1_c,t_c,g_c) = n_numerator/n_base

			! From emp 
			l0_c = 1
			! base a vector of logicals, true if the nth individual is in the labor force state l0_c in period t_c
			!	and the birth_flag in period t_c+1 is zero.
			base = ((sim_panel(:,g_c,t_c,1) >=1 .and. sim_panel(:,g_c,t_c,1) <=3)  &
				.and. (sim_panel(:,g_c,t_c+1,2) == 0)  &
				.and. ((sim_panel(:,g_c,t_c+1,1) >=0) .and. (sim_panel(:,g_c,t_c+1,1) <=3) ))
			n_base = dble(count(base))
			! - emp to NE
				l1_c = 0
				numerator = (base .and. (sim_panel(:,g_c,t_c+1,1) == l1_c))
				n_numerator = dble(count(numerator))
				trans_t_j(l0_c,l1_c,t_c,g_c) = n_numerator/n_base
			! - emp to emp
				l1_c = 1
				numerator = (base .and. (sim_panel(:,g_c,t_c+1,1) >=1 .and. sim_panel(:,g_c,t_c+1,1) <=3))
				n_numerator = dble(count(numerator))
				trans_t_j(l0_c,l1_c,t_c,g_c) = n_numerator/n_base
				
		enddo !t_c
		enddo !g_c
		
		!  Average over Tm_sim
		do g_c = 1,2
			do l0_c = 0,1
			do l1_c = 0,1
				trans_emp_j(l0_c,l1_c,g_c) = sum(trans_t_j(l0_c,l1_c,1:Tm_sim-1,g_c), &
					mask = (trans_t_j(l0_c,l1_c,1:Tm_sim-1,g_c)==trans_t_j(l0_c,l1_c,1:Tm_sim-1,g_c)))/ &
						(tiny(1.0d0)+dble(count(trans_t_j(l0_c,l1_c,1:Tm_sim-1,g_c)==trans_t_j(l0_c,l1_c,1:Tm_sim-1,g_c))))
			enddo !l1_c
			enddo !l0_c
		enddo !g_c
		
		end subroutine lfs_trans_j


     !=======================================================================   
    end subroutine sim_transition
end module mod_simulation
