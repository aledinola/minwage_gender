module solve_household
    
    !=======================================================================
    !               SOLVE HOUSEHOLD MODULE 
    ! Here we collect programs to solve household problem
    ! Main programs: vfi, vfi_singles, acceptance_rules
    ! Notation: Member 1 => m, Member 2 => f
    ! Notation: nn => uu, en => eu, etc.
    ! Notation: subscript _c denotes the index counter
    ! so e.g. w1_c is the counter for the wage of member 1 (male), etc.
    !=======================================================================
	!=======================================================================

    use mod_globals
    use mod_utilities 
    use omp_lib
    use mod_numerical, only: sub_logsum, fun_logsum

    implicit none
    
    !By keeping the *private* clause empty, all your subroutines/data types will be private 
    !to the module by default. Then you export things by putting it into the *public* clause.
    private 
    public vfi, vfi_singles, acceptance_rules, acceptance_rules_s
    contains 
    !====================================================!
    subroutine bellman_uu(V_uu,V_eu,V_ue, V_uu_new)
        
        !Declare inputs:
        real(8), intent(in) :: V_uu(:)     !states: (k)
        real(8), intent(in) :: V_eu(:,:,:) !states: (w1,h1,k)
        real(8), intent(in) :: V_ue(:,:,:) !states: (w2,h2,k)
        
        !Declare output:
        real(8), intent(out) :: V_uu_new(:) 
    
        !Declare local variables
        real(8) :: disc_uu, int_m, int_f !"int" stands for "integral", see memo
        real(8):: cons,h_vec(2),inc(2)
        integer:: k_c,k_next,wp_c,hp_c,emp(2)
        real(8) :: v_vec(2), EMax
        !------------------!
        
        !$omp parallel if (par_fortran==1) default(shared) private(k_c,k_next,int_m,int_f,&
        !$ wp_c,hp_c, Emax, v_vec, inc,cons,h_vec,disc_uu)
        !$omp do collapse(1)
        do k_c = 1,n_kids
            
            k_next = min(k_c+1,n_kids) !num. kids next period
            !Conditional on (k), integral terms are scalars
            int_m = 0.0d0
            int_f = 0.0d0
            !Integrate over (w',h'). @ALE: should vectorize over w'
            do wp_c = 1,n_w
            do hp_c = 1,n_hours
				!Member 1 (male) gets a job offer
				! Accept value
				v_vec(1) = V_eu(wp_c,hp_c,k_c)
				! Reject value
				v_vec(2) = V_uu(k_c)
				EMax =  fun_logsum(v_vec,p_sigma_alpha(k_c,1))
				int_m = int_m + hpol(hp_c)*EMax*F_prob(wp_c,hp_c)
				!Member 2 (female) gets a job offer
				! Accept value
				v_vec(1) = V_ue(wp_c,hp_c,k_c)
				! Reject value
				v_vec(2) = V_uu(k_c)
				EMax =  fun_logsum(v_vec,p_sigma_alpha(k_c,2))
				int_f = int_f + hpol(hp_c)*EMax*F_prob(wp_c,hp_c)
            enddo
            enddo
            inc   = [0.0d0, 0.0d0]
            emp = [0,0]
            cons  = netinc_couple(inc,emp,k_c)
            h_vec = [0.0d0, 0.0d0] ! both unemployed
        
            disc_uu = p_rho+p_rho_d+p_phi(k_c,1,2)+p_lambda_u*sum(hpol)+p_lambda_u*sum(hpol)
            
            V_uu_new(k_c) = util_couple(cons,h_vec,k_c)+&
                p_phi(k_c,1,2)*V_uu(k_next)+ &
                p_lambda_u*int_m+p_lambda_u*int_f
            
            V_uu_new(k_c) = V_uu_new(k_c)/disc_uu
            
        enddo !end k
        !$omp enddo
        !$omp end parallel
        
    end subroutine bellman_uu
    !===============================================================!
    
    subroutine bellman_eu(V_uu,V_eu,V_ue,V_ee, V_eu_new)
    !SINGLE EARNER HOUSEHOLD "en"
    !Household member 1 (i.e. the male) is employed, member 2 is unemployed
    
    !Declare inputs:
    real(8), intent(in) :: V_uu(:) !states: (k)
    real(8), intent(in) :: V_eu(:,:,:) !states: (w1,h1,k)
    real(8), intent(in) :: V_ue(:,:,:) !states: (w2,h2,k)
    real(8), intent(in) :: V_ee(:,:,:,:,:) !states: (w1,h1,w2,h2,k)
    
    !Declare output:
    real(8), intent(out) :: V_eu_new(:,:,:)
    
    !Declare local variables
    integer :: w1_c, h1_c, k_c, wp_c, hp_c, k_next
    integer :: emp(2)
    real(8):: disc_eu,int_m,int_f,cons,w1,h1
    real(8) :: h_vec(2), inc(2)
    real(8) :: v_vec(2),EMax
    
    !$omp parallel if (par_fortran==1) default(shared) private(k_c,k_next,w1_c,h1_c,int_m,&
    !$ int_f,wp_c,hp_c,EMax,v_vec, w1,h1,h_vec,inc,cons,disc_eu)
    !$omp do collapse(3)
    do k_c = 1,n_kids    
        do w1_c = 1,n_w !Current wage member 1
		do h1_c = 1,n_hours !Current hours-type member 1
			k_next = min(k_c+1,n_kids) !num. kids next period
			
				!Conditional on (w1,h1,k), integrals are scalars
				int_m = 0.0d0
				int_f = 0.0d0
				!Integrate over (w',h'). @ALE: should vectorize over w'
				! h' is hp
				do wp_c = 1,n_w
				do hp_c = 1,n_hours
					!Member 1 gets a job offer (while on the job)
					! Accept value
					v_vec(1) = V_eu(wp_c,hp_c,k_c)
					! Reject value
					v_vec(2) = V_eu(w1_c,h1_c,k_c)
					EMax =  fun_logsum(v_vec,p_sigma_alpha(k_c,1))
					int_m = int_m + hpol(hp_c)*EMax*F_prob(wp_c,hp_c) 
					
					!Member 2 gets a job offer (while unemployed)
					! Accept value; 1 either stays or quits
					v_vec(1) = max(V_ee(w1_c,h1_c,wp_c,hp_c,k_c),V_ue(wp_c,hp_c,k_c))
					! Reject value
					v_vec(2) = V_eu(w1_c,h1_c,k_c)
					EMax =  fun_logsum(v_vec,p_sigma_alpha(k_c,2))
					int_f = int_f + hpol(hp_c)*EMax*F_prob(wp_c,hp_c) 
				enddo
				enddo
				w1    = w_grid(w1_c,h1_c)
				h1    = hours_grid(h1_c) !hours in values
				h_vec = [h1, 0.0d0]      !second member is unemployed
				inc   = [w1*h1, 0.0d0]   ! inc is real(8)
				emp   = [h1_c,0]         ! emp is integer
				cons  = netinc_couple(inc,emp,k_c)
				
				disc_eu = p_rho+p_rho_d+p_phi(k_c,1,2)+ &
					p_lambda_e*sum(hpol)+p_lambda_u*sum(hpol)+p_delta_m(h1_c)
			
				V_eu_new(w1_c,h1_c,k_c) = util_couple(cons,h_vec,k_c)+&
					p_phi(k_c,1,2)*max(V_eu(w1_c,h1_c,k_next), V_uu(k_next)) + &
					p_lambda_e*int_m+p_lambda_u*int_f+&
					p_delta_m(h1_c)*V_uu(k_c)
			
				V_eu_new(w1_c,h1_c,k_c) = V_eu_new(w1_c,h1_c,k_c)/disc_eu
		enddo !end h1
        enddo !end w1
    enddo !end k
    !$omp enddo
    !$omp end parallel
    
    end subroutine bellman_eu
    !!===========================================================!
    !
    subroutine bellman_ue(V_uu,V_eu,V_ue,V_ee, V_ue_new)
    !SINGLE EARNER HOUSEHOLD "ne"
    !Household member 2 (i.e. the female) is employed, member 1 is non-employed
    !Declare inputs:
    real(8), intent(in) :: V_uu(:) !states: (k)
    real(8), intent(in) :: V_eu(:,:,:) !states: (w1,h1,k)
    real(8), intent(in) :: V_ue(:,:,:) !states: (w2,h2,k)
    real(8), intent(in) :: V_ee(:,:,:,:,:) !states: (w1,h1,w2,h2,k)
    
    !Declare output:
    real(8), intent(out) :: V_ue_new(:,:,:)
    
    !Declare local variables
    integer :: w2_c, h2_c, k_c, wp_c, hp_c, k_next,emp(2)
    real(8) :: disc_ue, int_m, int_f, cons, inc(2), w2, h2,h_vec(2)
    real(8) :: v_vec(2),EMax
    
    !$omp parallel if (par_fortran==1) default(shared) private(k_c,k_next,w2_c,h2_c,int_m,&
    !$ int_f,wp_c,hp_c, EMax,v_vec, w2,h2,h_vec,inc,cons,disc_ue)
    !$omp do collapse(3)
    do k_c = 1,n_kids      
        do w2_c = 1,n_w !Current wage member 2
		do h2_c = 1,n_hours !Current hours-type member 2
			k_next = min(k_c+1,n_kids) !num. kids next period
			!Conditional on (w2,h2,k), integral terms int_m, int_f are scalars
			int_m = 0.0d0
			int_f = 0.0d0
			!Integrate over (w',h'). @ALE: should vectorize over w'
			! h' is hp
			do wp_c = 1,n_w
			do hp_c = 1,n_hours
				!Member 1 (unemployed) gets a job offer
				! Accept value
				v_vec(1) = max(V_ee(wp_c,hp_c,w2_c,h2_c,k_c),V_eu(wp_c,hp_c,k_c))
				! Reject value
				v_vec(2) = V_ue(w2_c,h2_c,k_c)
				EMax =  fun_logsum(v_vec,p_sigma_alpha(k_c,1))
				int_m = int_m + hpol(hp_c)*EMax*F_prob(wp_c,hp_c) 
				
				!Member 2 gets a job offer (while on the job, ue)
				! Accept value
				v_vec(1) = V_ue(wp_c,hp_c,k_c)
				! Reject value
				v_vec(2) = V_ue(w2_c,h2_c,k_c)
				EMax =  fun_logsum(v_vec,p_sigma_alpha(k_c,2))
				int_f = int_f + hpol(hp_c)*EMax*F_prob(wp_c,hp_c) 
			enddo
			enddo
			w2    = p_a(h2_c)*w_grid(w2_c,h2_c)
			h2    = hours_grid(h2_c)    ! hours in values
			h_vec = [0.0d0, h2]         ! member 1 is unemployed
			inc   = [0.0d0, w2*h2]
			emp   = [0,h2_c]            ! this is integer vector
			cons  = netinc_couple(inc,emp,k_c)
		
			disc_ue = p_rho+p_rho_d+p_phi(k_c,1,2)+&
				p_lambda_u*sum(hpol)+p_lambda_e*sum(hpol)+p_delta_f(h2_c)
		
			V_ue_new(w2_c,h2_c,k_c) = util_couple(cons,h_vec,k_c)+&
				p_phi(k_c,1,2)*max(V_ue(w2_c,h2_c,k_next), V_uu(k_next)) + &
				p_lambda_u*int_m+p_lambda_e*int_f+&
				p_delta_f(h2_c)*V_uu(k_c)
		
			V_ue_new(w2_c,h2_c,k_c) = V_ue_new(w2_c,h2_c,k_c)/disc_ue
		enddo !end h2
        enddo !end w2
    enddo !end k
    !$omp enddo
    !$omp end parallel
    
    end subroutine bellman_ue
    !====================================================!
    
    subroutine bellman_ee(V_uu,V_eu,V_ue,V_ee, V_ee_new)
    ! DUAL EARNER HOUSEHOLD "ee"
    
    !Declare inputs:
    real(8), intent(in) :: V_uu(:) !states: (k)
    real(8), intent(in) :: V_eu(:,:,:) !states: (w1,h1,k)
    real(8), intent(in) :: V_ue(:,:,:) !states: (w2,h2,k)
    real(8), intent(in) :: V_ee(:,:,:,:,:) !states: (w1,h1,w2,h2,k)
    
    !Declare output:
    real(8), intent(out) :: V_ee_new(:,:,:,:,:)
    
    !Declare local variables
    integer :: w1_c,h1_c,w2_c,h2_c,k_c,wp_c,hp_c,k_next,emp(2)
    real(8):: disc_ee,int_m,int_f,cons,inc(2),w1,h1,w2,h2,h_vec(2)
    real(8) :: v_vec(2),EMax
    
    !$omp parallel if (par_fortran==1) default(shared) private(k_c,k_next,h2_c,w2_c,h1_c,&
    !$ EMax,v_vec, w1_c,int_m,int_f,wp_c,hp_c,w1,w2,h1,h2,h_vec,inc,cons,disc_ee)
    !$omp do collapse(5)
    do k_c = 1,n_kids ! 
    do h2_c = 1,n_hours
    do w2_c = 1,n_w
    do h1_c = 1,n_hours
    do w1_c = 1,n_w !Current wage member 1
		k_next = min(k_c+1,n_kids) !num. kids next period
		!Conditional on (w1,h1,w2,h2,k), integrals are scalars
		int_m = 0.0d0
		int_f = 0.0d0
		!Integrate over (w',h'). @ALE should vectorize over w'
		do hp_c = 1,n_hours
		do wp_c = 1,n_w
			!Member 1-m gets an offer
			! Accept value
			v_vec(1) = max(V_ee(wp_c,hp_c,w2_c,h2_c,k_c),V_eu(wp_c,hp_c,k_c))
			! Reject value
			v_vec(2) = V_ee(w1_c,h1_c,w2_c,h2_c,k_c)
			EMax =  fun_logsum(v_vec,p_sigma_alpha(k_c,1))
			int_m = int_m + hpol(hp_c)*EMax*F_prob(wp_c,hp_c) 
			!Member 2-f gets an offer
			! Accept value
			v_vec(1) = max(V_ee(w1_c,h1_c,wp_c,hp_c,k_c),V_ue(wp_c,hp_c,k_c))
			! Reject value
			v_vec(2) = V_ee(w1_c,h1_c,w2_c,h2_c,k_c)
			EMax =  fun_logsum(v_vec,p_sigma_alpha(k_c,2))
			int_f = int_f + hpol(hp_c)*EMax*F_prob(wp_c,hp_c)
		enddo
		enddo
		w1  = w_grid(w1_c,h1_c)
		w2  = p_a(h2_c)*w_grid(w2_c,h2_c)
		h1  = hours_grid(h1_c) !hours in values
		h2  = hours_grid(h2_c) !hours in values
		h_vec = [h1,h2]
		inc = [w1*h1, w2*h2]
		emp = [0,0]            !integer vector
		cons = netinc_couple(inc,emp,k_c)
	
		disc_ee = p_rho+p_rho_d+p_phi(k_c,1,2)+&
			p_lambda_e*sum(hpol)+p_lambda_e*sum(hpol)+p_delta_m(h1_c)+p_delta_f(h2_c)
	
		V_ee_new(w1_c,h1_c,w2_c,h2_c,k_c) = util_couple(cons,h_vec,k_c)+&
			p_phi(k_c,1,2)*max(V_ee(w1_c,h1_c,w2_c,h2_c,k_next),V_eu(w1_c,h1_c,k_next),V_ue(w2_c,h2_c,k_next),V_uu(k_next) ) &
			+p_lambda_e*int_m+p_lambda_e*int_f &
			+p_delta_m(h1_c)*V_ue(w2_c,h2_c,k_c)+p_delta_f(h2_c)*V_eu(w1_c,h1_c,k_c)
	
		V_ee_new(w1_c,h1_c,w2_c,h2_c,k_c) = V_ee_new(w1_c,h1_c,w2_c,h2_c,k_c)/disc_ee
    enddo !end w1
    enddo !end h1
    enddo !end w2
    enddo !end h2
    enddo !end k
    !$omp enddo
    !$omp end parallel 
    
    end subroutine bellman_ee
    !================================================================================================!
    
    subroutine vfi( V,conv_flag )
    !-------------------------------------------------------------!
    ! DESCRIPTION:
    ! Perform VFI for married couples.
    ! V_inout is both input and output and is a derived data type.
    !-------------------------------------------------------------!
    
    implicit none

    !Declare inputs/outputs:
    type(valfun), intent(inout) :: V
    logical, intent(out) :: conv_flag
    
    !Declare local variables
    integer :: istat
    character(len=80) :: err_msg
    
    real(8),allocatable :: V_uu_new(:)         !states: (k)
    real(8),allocatable :: V_eu_new(:,:,:)     !states: (w1,h1,k)
    real(8),allocatable :: V_ue_new(:,:,:)     !states: (w2,h2,k)
    real(8),allocatable :: V_ee_new(:,:,:,:,:) !states: (w1,h1,w2,h2,k)
    
    
    real(8) :: dist_uu, dist_eu, dist_ue, dist_ee, diff, t1, t2
    integer :: itervf
    
    !Allocate some other arrays
    allocate(V_uu_new(n_kids),V_eu_new(n_w,n_hours,n_kids),&
        V_ue_new(n_w,n_hours,n_kids),V_ee_new(n_w,n_hours,n_w,n_hours,n_kids),&
        stat=istat,errmsg=err_msg)
    if (istat/=0) then
        print *, err_msg
        pause
        stop
    endif
    
	if (verbose==2) then
    write(*,*) "============================================================="
    write(*,*) "VALUE FUNCTION ITERATION FOR COUPLES..."
    write(*,*) "============================================================="
    endif 
    if (verbose==2) write(*,*) "Check OpenMP inside vfi.."
    !$omp parallel if (par_fortran==1)
    if (verbose==2) write(*,*) "Parallel hello to you!"
    !$omp end parallel

    diff    = 10.0d0
    itervf  = 0
    conv_flag = .true. !This will be changed to false if convergence fails
    
    t1 = omp_get_wtime()
    
    do while ((diff > tolV .and. itervf <= maxitV) .or. itervf <= minitV)
    
    
        !Number of iterations           
        itervf = itervf + 1
        
        !Bellman equation for (u,u)
        call bellman_uu(V%V_uu,V%V_eu,V%V_ue, V_uu_new)
        
        !Bellman equation for (e,u)
        call bellman_eu(V%V_uu,V%V_eu,V%V_ue,V%V_ee, V_eu_new)
        
        !Bellman equation for (u,e)
        call bellman_ue(V%V_uu,V%V_eu,V%V_ue,V%V_ee, V_ue_new)
        
        !Bellman equation for (e,e)
        call bellman_ee(V%V_uu,V%V_eu,V%V_ue,V%V_ee, V_ee_new)
        
        !Compute dist b/w iterations
        dist_uu = maxval(abs(V%V_uu-V_uu_new))
        dist_eu = maxval(abs(V%V_eu-V_eu_new))
        dist_ue = maxval(abs(V%V_ue-V_ue_new))
        dist_ee = maxval(abs(V%V_ee-V_ee_new))
        
        !Error for VFI
        diff = max(dist_uu,dist_eu,dist_ue,dist_ee)
        
        if (verbose==2) then
            write(*,*) "-----------------------------------"
            write(*,'(a,I4)')    " iteration  = ", itervf
            write(*,'(a,f0.12)') " norm_total = ", diff
            write(*,'(a,f0.12)') " dist_uu = ", dist_uu
            write(*,'(a,f0.12)') " dist_eu = ", dist_eu
            write(*,'(a,f0.12)') " dist_ue = ", dist_ue
            write(*,'(a,f0.12)') " dist_ee = ", dist_ee
            write(*,*) "-----------------------------------"
        endif
    
        !Update the guess for the value function

        V%V_uu = V_uu_new 

		V%V_eu = V_eu_new 
		
		V%V_ue = V_ue_new 
		
		V%V_ee = V_ee_new 
        
    enddo !while loop ends here
    
    t2 = omp_get_wtime()
    if (verbose==2) then
    write(*,*) "============================================================="
    write(*,*) "VALUE FUNCTION FOUND"
    write(*,*) "============================================================="
    write(*,*) "============================================================="
    write(*,*) 'VFI runs for',real(t2-t1),'seconds.'
    write(*,*) "============================================================="
    endif 
    if (diff > tolV) conv_flag = .false.
    
    if (debug) pause
 
    end subroutine vfi
    !================================================================================================!
    
    subroutine vfi_singles(V, conv_flag)
    
    !-------------------------------------------------------------!
    ! DESCRIPTION:
    ! It does VFI for single agents
    ! V is both input and output and is a derived data type,
    ! declared in mod_globals.f90
    ! NOTATION:
    ! u_sm: Unemployed single male
    ! u_sf: Unemployed single female
    ! e_sm: Employed single male
    ! e_sf: Employed single female
    !-------------------------------------------------------------!
    
    implicit none

    !Declare inputs/outputs:
    type(valfun_s), intent(inout) :: V
    logical, intent(out) :: conv_flag

    !Declare local variables:
    character(len=200) :: err_msg
    real(8),allocatable :: V_u_sm_new(:,:)     !states: (k)
    real(8),allocatable :: V_u_sf_new(:,:)     !states: (k)
    real(8),allocatable :: V_e_sm_new(:,:,:,:) !states: (w,h,k)
    real(8),allocatable :: V_e_sf_new(:,:,:,:) !states: (w,h,k)
    integer:: itervf,istat
    real(8) :: diff,t1,t2,dist_um, dist_uf,dist_em,dist_ef
    
    ! Initialize indicator for min wage compliance
    call initialize_I_mw(I_mw,w_grid)
    
    !Allocate some local arrays
    allocate(V_u_sm_new(n_kids,n_spinc),V_u_sf_new(n_kids,n_spinc),V_e_sm_new(n_w,n_hours,n_kids,n_spinc),&
        V_e_sf_new(n_w,n_hours,n_kids,n_spinc),stat=istat,errmsg=err_msg)
    if (istat/=0) then
        print *, err_msg
        pause
        stop
    endif
    
    if (verbose==2) then
    write(*,*) "============================================================="
    write(*,*) "VALUE FUNCTION ITERATION for SINGLES..."
    write(*,*) "============================================================="
    endif 
    !Initialize variables for iterations
    diff    = 10.0d0
    itervf  = 0
    conv_flag = .true. !This will be changed to false if convergence fails
    
    call CPU_TIME(t1)
    
    do while ((diff > tolV .and. itervf <= maxitV) .or. itervf <= minitV)
    
    
        !Increase the iterations counter           
        itervf = itervf + 1
        
        !------ MAIN BODY-----------!
         !Bellman equation for (u_sm)
        call bellman_u_sm(V%V_u_sm,V%V_e_sm, V_u_sm_new)
        
        !Bellman equation for (u_sf)
        call bellman_u_sf(V%V_u_sf,V%V_e_sf, V_u_sf_new)
        
        !Bellman equation for (e_sm)
        call bellman_e_sm(V%V_u_sm,V%V_e_sm, V_e_sm_new)
        
        !Bellman equation for (e_sf)
        call bellman_e_sf(V%V_u_sf,V%V_e_sf, V_e_sf_new)
        
        !Compute dist b/w iterations
        dist_um = maxval(abs(V%V_u_sm - V_u_sm_new))
        dist_uf = maxval(abs(V%V_u_sf - V_u_sf_new))
        dist_em = maxval(abs(V%V_e_sm - V_e_sm_new))
        dist_ef = maxval(abs(V%V_e_sf - V_e_sf_new))
        
        !Error for VFI
        diff = max(dist_um,dist_uf,dist_em,dist_ef)
        
        if (verbose==2) then
            write(*,*) "-----------------------------------"
            write(*,'(a,I4)') " iteration = ", itervf
            write(*,'(a,f0.7)') " norm_total = ", diff
            write(*,'(a,f0.7)') " dist_um = ", dist_um
            write(*,'(a,f0.7)') " dist_uf = ", dist_uf
            write(*,'(a,f0.7)') " dist_em = ", dist_em
            write(*,'(a,f0.7)') " dist_ef = ", dist_ef
            write(*,*) "-----------------------------------"
        endif
        !Update the guess for the value function    
        !i.e. set OLD = NEW
        V%V_u_sm = V_u_sm_new    
        V%V_u_sf = V_u_sf_new    
        V%V_e_sm = V_e_sm_new 
        V%V_e_sf = V_e_sf_new 
        
    
    enddo !while loop ends here
    
    call CPU_TIME(t2)
    
    if (verbose>=2) then
    write(*,*) "============================================================="
    write(*,*) "VALUE FUNCTION SINGLES FOUND"
    write(*,*) "============================================================="
    write(*,*) "============================================================="
    write(*,*) 'VFI SINGLES runs for',real(t2-t1),'seconds.'
    write(*,*) "============================================================="
    endif 
    if (diff > tolV) conv_flag = .false.
    
    end subroutine vfi_singles
    !================================================================================================!
    
    subroutine bellman_u_sm(V_u_sm,V_e_sm, V_u_sm_new)
    !Bellman equation for (u_sm), i.e. unemployed single male
    
        !Declare inputs:
        real(8), intent(in) :: V_u_sm(:,:)     !states: (k)
        real(8), intent(in) :: V_e_sm(:,:,:,:) !states: (w1,h1,k)
        
        !Declare output:
        real(8), intent(out) :: V_u_sm_new(:,:) !states: (k)
        
        !Declare local variables
        real(8) :: disc_u_sm, int_m !"int" stands for "integral", see memo
        real(8):: cons,inc,h
        integer:: sp_c,k_c,k_next,wp_c,hp_c,emp,g_c,i_single
        real(8) :: v_vec(2),EMax,disutil,f_eff
        real(8) :: offer_weight(n_w,n_hours),f_eff_raw,hpol_sum
        !------------------!
        
        g_c = 1 ! male
        hpol_sum = sum(hpol)
        f_eff_raw = 0.0d0
        do hp_c = 1,n_hours
        do wp_c = 1,n_w
            offer_weight(wp_c,hp_c) = hpol(hp_c)*F_prob(wp_c,hp_c)*I_mw(wp_c,hp_c,1)
            f_eff_raw = f_eff_raw + offer_weight(wp_c,hp_c)
        enddo
        enddo
        
        !$omp parallel if (par_fortran==1) default(shared) private(k_c,sp_c,k_next,&
        !$ int_m,wp_c,hp_c,v_vec,Emax,h,inc,emp,cons,disc_u_sm,i_single,disutil,f_eff)
        !$omp do collapse(2)
        do sp_c = 1,n_spinc
        do k_c = 1,n_kids !
        	if (sp_c == 1) then
				i_single = 1
			else
				i_single = 2
			endif
            k_next = min(k_c+1,n_kids) !num. kids next period
            
            !Conditional on (k), integral terms are scalars
            int_m = 0.0d0
            !Integrate over (w',h'). @ALE should vectorize over w'  
            f_eff = f_eff_raw
            do wp_c = 1,n_w
            do hp_c = 1,n_hours  
                ! Accept value
                if (hp_c == n_hours) then
                	disutil = p_mu_alpha(k_c,g_c)
                else
                	disutil = 0.0d0
                endif
				v_vec(1) = V_e_sm(wp_c,hp_c,k_c,sp_c)-disutil
				! Reject value
				v_vec(2) = V_u_sm(k_c,sp_c)
				EMax =  fun_logsum(v_vec,p_sigma_alpha(k_c,g_c))
                int_m = int_m + offer_weight(wp_c,hp_c)*EMax
			enddo ! end hp_c
            enddo ! end wp_c
            
            h      = 0.0d0 ! he is unemployed
            inc    = 0.0d0 ! he is unemployed
            emp    = 0     ! scalar integer
            cons   = netinc(g_c,inc,emp,k_c,sp_c)
            
           
            disc_u_sm = p_rho+p_rho_d+p_phi(k_c,1,i_single)+p_lambda_u*f_eff
            
            V_u_sm_new(k_c,sp_c) = util_sm(cons,h,k_c)+&
                p_phi(k_c,1,i_single)*V_u_sm(k_next,sp_c)+p_lambda_u*int_m
            
            V_u_sm_new(k_c,sp_c) = V_u_sm_new(k_c,sp_c)/disc_u_sm
            
        enddo !end k
        enddo !end sp_c
        !$omp enddo
        !$omp end parallel

    end subroutine bellman_u_sm
    !================================================================================================!
    
     subroutine bellman_u_sf(V_u_sf,V_e_sf, V_u_sf_new)
    !Bellman equation for (u_sf), i.e. unemployed single female
    
        !Declare inputs:
        real(8), intent(in) :: V_u_sf(:,:)     !states: (k)
        real(8), intent(in) :: V_e_sf(:,:,:,:) !states: (w1,h1,k)
        
        !Declare output:
        real(8), intent(out) :: V_u_sf_new(:,:) !states: (k)
        
        !Declare local variables
        real(8) :: disc_u_sf, int_f !"int" stands for "integral", see memo
        real(8):: cons,inc,h
        integer:: sp_c,k_c,k_next,wp_c,hp_c,emp,g_c,i_single
        real(8) :: v_vec(2),EMax,disutil,f_eff
        real(8) :: offer_weight(n_w,n_hours),f_eff_raw,hpol_sum
        !------------------!
        
        g_c = 2 ! female
        hpol_sum = sum(hpol)
        f_eff_raw = 0.0d0
        do hp_c = 1,n_hours
        do wp_c = 1,n_w
            offer_weight(wp_c,hp_c) = hpol(hp_c)*F_prob(wp_c,hp_c)*I_mw(wp_c,hp_c,2)
            f_eff_raw = f_eff_raw + offer_weight(wp_c,hp_c)
        enddo
        enddo
        
        !$omp parallel if (par_fortran==1) default(shared) private(k_c,sp_c,k_next,&
        !$ int_f,wp_c,hp_c,v_vec,Emax,h,inc,emp,cons,disc_u_sf,i_single,disutil,f_eff)
        !$omp do collapse(2)
        do sp_c = 1,n_spinc
        do k_c = 1,n_kids !
            k_next = min(k_c+1,n_kids) !num. kids next period
            if (sp_c == 1) then
				i_single = 1
			else
				i_single = 2
			endif
            !Conditional on (k), integral terms are scalars
            int_f = 0.0d0
            !Integrate over (w',h'). @ALE should vectorize over w'   
            f_eff = f_eff_raw
            do wp_c = 1,n_w
            do hp_c = 1,n_hours  
                ! Accept value
                if (hp_c == n_hours) then
                	disutil = p_mu_alpha(k_c,g_c)
                else
                	disutil = 0.0d0
                endif
				v_vec(1) = V_e_sf(wp_c,hp_c,k_c,sp_c)-disutil
				! Reject value
				v_vec(2) = V_u_sf(k_c,sp_c)
				EMax =  fun_logsum(v_vec,p_sigma_alpha(k_c,g_c))
                int_f = int_f + offer_weight(wp_c,hp_c)*EMax 
			enddo
            enddo
            
            h      = 0.0d0 ! he is unemployed
            inc    = 0.0d0 ! he is unemployed
            emp    = 0     ! scalar integer
            cons   = netinc(g_c,inc,emp,k_c,sp_c)
            
            disc_u_sf = p_rho+p_rho_d+p_phi(k_c,2,i_single)+p_lambda_u*f_eff
            
            V_u_sf_new(k_c,sp_c) = util_sf(cons,h,k_c)+&
                p_phi(k_c,2,i_single)*V_u_sf(k_next,sp_c)+p_lambda_u*int_f
            
            V_u_sf_new(k_c,sp_c) = V_u_sf_new(k_c,sp_c)/disc_u_sf
            
        enddo !end k
        enddo !end sp_c
        !$omp enddo
        !$omp end parallel

    end subroutine bellman_u_sf
    !================================================================================================!
    
    subroutine bellman_e_sm(V_u_sm,V_e_sm, V_e_sm_new)
    
    !Bellman equation for (e_sm), i.e. employed single male
    
        !Declare inputs:
        real(8), intent(in) :: V_u_sm(:,:)     !states: (k)
        real(8), intent(in) :: V_e_sm(:,:,:,:) !states: (w1,h1,k)
        
        !Declare output:
        real(8), intent(out) :: V_e_sm_new(:,:,:,:) !states: (w1,h1,k)
        
        !Declare local variables
        real(8) :: disc, int !"int" stands for "integral", see memo
        real(8) :: cons, inc, h1,w1
        integer :: g_c,sp_c,k_c, k_next, hp_c,wp_c, w1_c, h1_c, emp,i_single
        real(8) :: v_vec(2),EMax,disutil,EMax_k_next,f_eff
        real(8) :: offer_weight(n_w,n_hours),f_eff_raw,hpol_sum
        !------------------!
        
        !RECALL: 1 is m, 2 is f
        g_c = 1
        hpol_sum = sum(hpol)
        f_eff_raw = 0.0d0
        do hp_c = 1,n_hours
        do wp_c = 1,n_w
            offer_weight(wp_c,hp_c) = hpol(hp_c)*F_prob(wp_c,hp_c)*I_mw(wp_c,hp_c,1)
            f_eff_raw = f_eff_raw + offer_weight(wp_c,hp_c)
        enddo
        enddo
        
        !$omp parallel if (par_fortran==1) default(shared) private(k_c,sp_c,h1_c,w1_c,k_next,&
        !$ int,wp_c,hp_c,v_vec,Emax,h1,w1,inc,emp,cons,disc,i_single,disutil,EMax_k_next,f_eff)
        !$omp do collapse(4)
        do sp_c = 1,n_spinc
        do k_c = 1,n_kids
        do h1_c = 1,n_hours
        do w1_c = 1,n_w
        	if (sp_c == 1) then
				i_single = 1
			else
				i_single = 2
			endif
            k_next = min(k_c+1,n_kids) !num. kids next period
			!Conditional on (k), integral terms are scalars
			int = 0.0d0
			!Integrate over (w',h'). ! @ALE should vectorize over w'
            f_eff = f_eff_raw
			do wp_c = 1,n_w
			do hp_c = 1,n_hours  
				! Accept value
				if (hp_c == n_hours) then
                	disutil = p_mu_alpha(k_c,g_c)
                else
                	disutil = 0.0d0
                endif
				v_vec(1) =V_e_sm(wp_c,hp_c,k_c,sp_c)-disutil
				! Reject value
				v_vec(2) = V_e_sm(w1_c,h1_c,k_c,sp_c)
				EMax =  fun_logsum(v_vec,p_sigma_alpha(k_c,g_c))
				int = int + offer_weight(wp_c,hp_c)*EMax
			enddo
			enddo
			! EMax for continuation value after a childstate change
			! Value of keeping job
			if (h1_c == n_hours) then
				disutil = p_mu_alpha(k_c,g_c)
			else
				disutil = 0.0d0
			endif
			v_vec(1) = V_e_sm(w1_c,h1_c,k_next,sp_c)-disutil
			! Value of quitting job
			v_vec(2) = V_u_sm(k_next,sp_c)
			EMax_k_next =  fun_logsum(v_vec,p_sigma_alpha(k_c,g_c))
			h1     = hours_grid(h1_c) !hours in values
			w1     = w_grid(w1_c,h1_c)
			inc    = w1*h1 
			emp    = h1_c
			cons   = netinc(g_c,inc,emp,k_c,sp_c)

            disc = p_rho+p_rho_d+p_phi(k_c,1,i_single)+p_lambda_e*f_eff+p_delta_m(h1_c)
		
			V_e_sm_new(w1_c,h1_c,k_c,sp_c) = util_sm(cons,h1,k_c)+&
				p_phi(k_c,1,i_single)*EMax_k_next+&
				p_lambda_e*int+p_delta_m(h1_c)*V_u_sm(k_c,sp_c)
		
			V_e_sm_new(w1_c,h1_c,k_c,sp_c) = V_e_sm_new(w1_c,h1_c,k_c,sp_c)/disc
        enddo !end w1
        enddo !end h1
        enddo !end k
        enddo !end sp_c
        !$omp enddo
        !$omp end parallel

    end subroutine bellman_e_sm
    !================================================================================================!
    
    subroutine bellman_e_sf(V_u_sf,V_e_sf, V_e_sf_new)
    !Bellman equation for (e_sf), i.e. employed single female
    
        !Declare inputs:
        real(8), intent(in) :: V_u_sf(:,:)     !states: (k)
        real(8), intent(in) :: V_e_sf(:,:,:,:) !states: (w2,h2,k)
        
        !Declare output:
        real(8), intent(out) :: V_e_sf_new(:,:,:,:) !states: (w2,h2,k)
        
        !Declare local variables
        real(8) :: disc, int !"int" stands for "integral", see memo
        real(8) :: cons, inc, h2,w2
        integer :: g_c,sp_c,k_c, k_next, hp_c,wp_c, w2_c, h2_c, emp,i_single
        real(8) :: v_vec(2),EMax,disutil,EMax_k_next,f_eff
        real(8) :: offer_weight(n_w,n_hours),f_eff_raw,hpol_sum
        !------------------!
        
        !RECALL: 1 is m, 2 is f
        g_c = 2
        hpol_sum = sum(hpol)
        f_eff_raw = 0.0d0
        do hp_c = 1,n_hours
        do wp_c = 1,n_w
            offer_weight(wp_c,hp_c) = hpol(hp_c)*F_prob(wp_c,hp_c)*I_mw(wp_c,hp_c,2)
            f_eff_raw = f_eff_raw + offer_weight(wp_c,hp_c)
        enddo
        enddo
        !$omp parallel if (par_fortran==1) default(shared) private(k_c,sp_c,h2_c,w2_c,k_next,&
        !$ int,wp_c,hp_c,v_vec,Emax,h2,w2,inc,emp,cons,disc,i_single,disutil,EMax_k_next,f_eff)
        !$omp do collapse(4)
        do sp_c = 1,n_spinc
        do k_c = 1,n_kids
        do h2_c = 1,n_hours
        do w2_c = 1,n_w
        	if (sp_c == 1) then
				i_single = 1
			else
				i_single = 2
			endif
            k_next = min(k_c+1,n_kids) !num. kids next period
			!Conditional on (k), integral terms are scalars
			int = 0.0d0
			!Integrate over (w',h'). ! @ALE should vectorize over w'
            f_eff = f_eff_raw
			do wp_c = 1,n_w
			do hp_c = 1,n_hours 
				! Accept value
				if (hp_c == n_hours) then
                	disutil = p_mu_alpha(k_c,g_c)
                else
                	disutil = 0.0d0
                endif
				v_vec(1) =V_e_sf(wp_c,hp_c,k_c,sp_c)-disutil
				! Reject value
				v_vec(2) = V_e_sf(w2_c,h2_c,k_c,sp_c)
				EMax =  fun_logsum(v_vec,p_sigma_alpha(k_c,g_c))
				int = int + offer_weight(wp_c,hp_c)*EMax
			enddo
			enddo
   
			! EMax for continuation value after a childstate change
			! Value of keeping job
			if (h2_c == n_hours) then
				disutil = p_mu_alpha(k_c,g_c)
			else
				disutil = 0.0d0
			endif
			v_vec(1) = V_e_sf(w2_c,h2_c,k_next,sp_c)-disutil
			! Value of quitting job
			v_vec(2) = V_u_sf(k_next,sp_c)
			EMax_k_next =  fun_logsum(v_vec,p_sigma_alpha(k_c,g_c))
			h2     = hours_grid(h2_c) !hours in values
			w2     = p_a(h2_c)*w_grid(w2_c,h2_c)
			inc    = w2*h2 
			emp    = h2_c 
			cons   = netinc(g_c,inc,emp,k_c,sp_c)

            disc = p_rho+p_rho_d+p_phi(k_c,2,i_single)+p_lambda_e*f_eff+p_delta_f(h2_c)
		
			V_e_sf_new(w2_c,h2_c,k_c,sp_c) = util_sf(cons,h2,k_c)+&
				p_phi(k_c,2,i_single)*EMax_k_next+&
				p_lambda_e*int+p_delta_f(h2_c)*V_u_sf(k_c,sp_c)
		
			V_e_sf_new(w2_c,h2_c,k_c,sp_c) = V_e_sf_new(w2_c,h2_c,k_c,sp_c)/disc
        
        enddo !end w2
        enddo !end h2 
        enddo !end k
        enddo !end sp_c
        !$omp enddo
        !$omp end parallel
        
    end subroutine bellman_e_sf
    !================================================================================================!
    
    subroutine acceptance_rules(V)
    !DESCRITPION: 
    ! Compute acceptance probabilities and sets for couples
    
    !Declare inputs
    type(valfun), intent(in) :: V
    
    !Declare outputs --> The Omegas/Ohs are GLOBAL variables, see mod_globals.f90
    
    !Declare local variables
    character(len=80) :: err_msg
    integer:: w1_c,h1_c,w2_c,h2_c,k_c,hp_c,wp_c,istat
    logical :: aux1,aux2,aux3,aux
    real(8) :: p_vec(2),v_vec(2),EMax
    
    if (verbose==2) then
    write(*,*) "============================================================="
    write(*,*) "COMPUTE OMEGAS..."
    write(*,*) "============================================================="
    endif
    
    ! - DUAL SEARCHER households (uu) --> omega_uu1,omega_uu2
    !Allocate omega arrays (defined as global variables)
    allocate(omega_uu1(n_w,n_hours,n_kids),&
        omega_uu2(n_w,n_hours,n_kids),stat=istat,errmsg=err_msg)
    if (istat/=0) then 
        print *,err_msg
        pause
        stop 
    endif

    do k_c = 1,n_kids 
        do hp_c = 1,n_hours !index h'
        do wp_c = 1,n_w     !index w'
			!Member 1 receives offer
			! Accept value
			v_vec(1) =V%V_eu(wp_c,hp_c,k_c)
			! Reject value
			v_vec(2) = V%V_uu(k_c)
			call sub_logsum(EMax,p_vec, v_vec,p_sigma_alpha(k_c,1))
			omega_uu1(wp_c,hp_c,k_c) = p_vec(1)
			!Member 2 receives offer
			! Accept value
			v_vec(1) =V%V_ue(wp_c,hp_c,k_c)
			! Reject value
			v_vec(2) = V%V_uu(k_c)
			call sub_logsum(EMax,p_vec, v_vec,p_sigma_alpha(k_c,2))
			! probability of acceptance
			omega_uu2(wp_c,hp_c,k_c) = p_vec(1)
        enddo
        enddo
    enddo
    
    
    !Allocate omegas and Ohs for (eu)
    allocate(omega_eu1(n_w,n_hours,n_w,n_hours,n_kids),&
        omega_eu2(n_w,n_hours,n_w,n_hours,n_kids))
    
    allocate(Oh_eu1(n_w,n_hours,n_kids),Oh_eu2_1stays(n_w,n_hours,n_w,n_hours,n_kids), &
     stat=istat,errmsg=err_msg)
    if (istat/=0) then 
        print *,err_msg
        pause
        stop 
    endif
    
    ! - SINGLE EARNER households (eu) --> omega_eu1,omega_eu2
    do k_c = 1,n_kids 
    do w1_c = 1,n_w ! current job
    do h1_c = 1,n_hours ! current job
        do hp_c = 1,n_hours !index h' offer
        do wp_c = 1,n_w     !index w' offer
			!Member 1 receives offer
			! Accept value
			v_vec(1) =V%V_eu(wp_c,hp_c,k_c)
			! Reject value
			v_vec(2) = V%V_eu(w1_c,h1_c,k_c)
			call sub_logsum(EMax,p_vec, v_vec,p_sigma_alpha(k_c,1))
			! probability of acceptance
			omega_eu1(wp_c,hp_c,w1_c,h1_c,k_c) = p_vec(1)
		
			!Member 2 receives job offer:
			! Accept value
			v_vec(1) =max(V%V_ee(w1_c,h1_c,wp_c,hp_c,k_c),V%V_ue(wp_c,hp_c,k_c))
			! Reject value
			v_vec(2) = V%V_eu(w1_c,h1_c,k_c)
			call sub_logsum(EMax,p_vec, v_vec,p_sigma_alpha(k_c,2))
			! probability of acceptance
			omega_eu2(wp_c,hp_c,w1_c,h1_c,k_c) = p_vec(1)
        enddo
        enddo
    enddo
    enddo
    enddo


    ! - SINGLE EARNER households (eu) --> Oh_eu1,Oh_eu2_1stays
  
    do k_c = 1,n_kids 
    do h1_c = 1,n_hours ! current job member 1
    do w1_c = 1,n_w ! Current job member 1
		aux1 = V%V_eu(w1_c,h1_c,k_c) >= V%V_uu(k_c)
		Oh_eu1(w1_c,h1_c,k_c) = aux1
		do h2_c = 1,n_hours
		do w2_c = 1,n_w
			aux1 = V%V_ee(w1_c,h1_c,w2_c,h2_c,k_c) >=V%V_ue(w2_c,h2_c,k_c)
			Oh_eu2_1stays(w1_c,h1_c,w2_c,h2_c,k_c) = aux1		
		enddo
		enddo
    enddo
    enddo
    enddo

    
    !Allocate omegas and Ohs for (ue)
    allocate(omega_ue2(n_w,n_hours,n_w,n_hours,n_kids),&
        omega_ue1(n_w,n_hours,n_w,n_hours,n_kids))    
   
	allocate(Oh_ue2(n_w,n_hours,n_kids),Oh_ue1_2stays(n_w,n_hours,n_w,n_hours,n_kids), &
			stat=istat,errmsg=err_msg)
	if (istat/=0) then 
		print *,err_msg
		pause
		stop 
	endif
    
    ! - SINGLE EARNER households (ue) --> omega_ue2,omega_ue1
    do k_c = 1,n_kids 
    do w2_c = 1,n_w ! current job
    do h2_c = 1,n_hours ! current job
        do hp_c = 1,n_hours !index h' offer
        do wp_c = 1,n_w     !index w' offer
			!Member 2 receives offer
			! Accept value
			v_vec(1) =V%V_ue(wp_c,hp_c,k_c)
			! Reject value
			v_vec(2) = V%V_ue(w2_c,h2_c,k_c)
			call sub_logsum(EMax,p_vec, v_vec,p_sigma_alpha(k_c,2))
			! probability of acceptance
			omega_ue2(wp_c,hp_c,w2_c,h2_c,k_c) = p_vec(1)
		
			!Member 1 receives offer
			! Accept value
			v_vec(1) =max(V%V_ee(wp_c,hp_c,w2_c,h2_c,k_c),V%V_eu(wp_c,hp_c,k_c))
			! Reject value
			v_vec(2) = V%V_ue(w2_c,h2_c,k_c)
			call sub_logsum(EMax,p_vec, v_vec,p_sigma_alpha(k_c,1))
			! probability of acceptance
			omega_ue1(wp_c,hp_c,w2_c,h2_c,k_c) = p_vec(1)
        enddo
        enddo
    enddo
    enddo
    enddo

    ! - SINGLE EARNER households (ue) --> Oh_ue2,Oh_ue1_2stays
    do k_c = 1,n_kids 
    do h2_c = 1,n_hours ! current job member 2
    do w2_c = 1,n_w ! Current job member 2
		aux1 = V%V_ue(w2_c,h2_c,k_c) >= V%V_uu(k_c)
		Oh_ue2(w2_c,h2_c,k_c) = aux1
		do h1_c = 1,n_hours
		do w1_c = 1,n_w
			aux1 = V%V_ee(w1_c,h1_c,w2_c,h2_c,k_c) >=V%V_eu(w1_c,h1_c,k_c)
			Oh_ue1_2stays(w1_c,h1_c,w2_c,h2_c,k_c) = aux1		
		enddo
		enddo
    enddo
    enddo
    enddo
    
    !Allocate omegas and Ohs for (ee)
    if (debug) pause
        allocate(Omega_ee1(n_w,n_hours,n_w,n_hours,n_w,n_hours,n_kids),&
        Omega_ee2(n_w,n_hours,n_w,n_hours,n_w,n_hours,n_kids))
    if (verbose==2) write(*,*) "Allocation done!"
    if (debug) pause
        
    allocate(Oh_ee12(n_w,n_hours,n_w,n_hours,n_kids), &
        Oh_ee1(n_w,n_hours,n_w,n_hours,n_kids), &
        Oh_ee2(n_w,n_hours,n_w,n_hours,n_kids), &
        Oh_ee_minus(n_w,n_hours,n_w,n_hours,n_kids), &
        Oh_ee1_2stays(n_w,n_hours,n_w,n_hours,n_kids), &
        Oh_ee2_1stays(n_w,n_hours,n_w,n_hours,n_kids), &
        stat=istat,errmsg=err_msg)
    if (istat/=0) then 
        print *,err_msg
        pause
        stop 
    endif
    
    ! - DUAL EARNER households (ee) --> omega_ee1,omega_ee2
    
    do k_c = 1,n_kids 
    do h2_c = 1,n_hours ! current job member 2
    do w2_c = 1,n_w ! current job member 2
    do h1_c = 1,n_hours ! current job member 1
    do w1_c = 1,n_w ! Current job member 1
		do hp_c = 1,n_hours !index h' offer
        do wp_c = 1,n_w     !index w' offer
			!Member 1 receives offer
			! Accept value
			v_vec(1) = max(V%V_ee(wp_c,hp_c,w2_c,h2_c,k_c),V%V_eu(wp_c,hp_c,k_c))
			! Reject value
			v_vec(2) = V%V_ee(w1_c,h1_c,w2_c,h2_c,k_c)
			call sub_logsum(EMax,p_vec, v_vec,p_sigma_alpha(k_c,1))
			! probability of acceptance
			omega_ee1(wp_c,hp_c,w1_c,h1_c,w2_c,h2_c,k_c) = p_vec(1)
		
			!Member 2 receives offer
			! Accept value
			v_vec(1) = max(V%V_ee(w1_c,h1_c,wp_c,hp_c,k_c),V%V_ue(wp_c,hp_c,k_c))
			! Reject value
			v_vec(2) = V%V_ee(w1_c,h1_c,w2_c,h2_c,k_c)
			call sub_logsum(EMax,p_vec, v_vec,p_sigma_alpha(k_c,2))
			! probability of acceptance
			omega_ee2(wp_c,hp_c,w1_c,h1_c,w2_c,h2_c,k_c) = p_vec(1)
        enddo
        enddo       
    enddo
    enddo
    enddo
    enddo
    enddo

    ! - DUAL EARNER households (ee) --> Oh_ee12,Oh_ee1, Oh_ee2,Oh_ee2_1stays,Oh_ee1_2stays
    do k_c = 1,n_kids 
    do h2_c = 1,n_hours ! current job member 2
    do w2_c = 1,n_w ! current job member 2
    do h1_c = 1,n_hours ! current job member 1
    do w1_c = 1,n_w ! Current job member 1 			
		!Given offers (w1,h1,w2,h2), both members are employed
		aux1 = V%V_ee(w1_c,h1_c,w2_c,h2_c,k_c) >= V%V_uu(k_c)
		aux2 = V%V_ee(w1_c,h1_c,w2_c,h2_c,k_c) >= V%V_eu(w1_c,h1_c,k_c)
		aux3 = V%V_ee(w1_c,h1_c,w2_c,h2_c,k_c) >= V%V_ue(w2_c,h2_c,k_c)
		Oh_ee12(w1_c,h1_c,w2_c,h2_c,k_c) = (aux1 .and. aux2 .and. aux3 )
	
		!Given offers (w1,h1,w2,h2), only member 1 is employed
		aux1 = V%V_eu(w1_c,h1_c,k_c) >= V%V_uu(k_c)
		aux2 = V%V_eu(w1_c,h1_c,k_c) >= V%V_ue(w2_c,h2_c,k_c)
		aux3 = V%V_eu(w1_c,h1_c,k_c) > V%V_ee(w1_c,h1_c,w2_c,h2_c,k_c)
		Oh_ee1(w1_c,h1_c,w2_c,h2_c,k_c) = (aux1 .and. aux2 .and. aux3 )
	
		!Given offers (w1,h1,w2,h2), only member 2 is employed
		aux1 = V%V_ue(w2_c,h2_c,k_c) >= V%V_uu(k_c)
		aux2 = V%V_ue(w2_c,h2_c,k_c) > V%V_eu(w1_c,h1_c,k_c)
		aux3 = V%V_ue(w2_c,h2_c,k_c) > V%V_ee(w1_c,h1_c,w2_c,h2_c,k_c)
		Oh_ee2(w1_c,h1_c,w2_c,h2_c,k_c) = (aux1 .and. aux2 .and. aux3 )
	
		!Given offers (w1,h1,w2,h2), none is employed
		aux1 = V%V_uu(k_c) > V%V_ue(w2_c,h2_c,k_c) 
		aux2 = V%V_uu(k_c) > V%V_eu(w1_c,h1_c,k_c)
		aux3 = V%V_uu(k_c) > V%V_ee(w1_c,h1_c,w2_c,h2_c,k_c)
		Oh_ee_minus(w1_c,h1_c,w2_c,h2_c,k_c) = (aux1 .and. aux2 .and. aux3 )
		
    
 		aux1 = V%V_ee(w1_c,h1_c,w2_c,h2_c,k_c) >= V%V_ue(w2_c,h2_c,k_c) 
		Oh_ee2_1stays(w1_c,h1_c,w2_c,h2_c,k_c) = aux1
 		aux2 = V%V_ee(w1_c,h1_c,w2_c,h2_c,k_c) >= V%V_eu(w1_c,h1_c,k_c) 
 		Oh_ee1_2stays(w1_c,h1_c,w2_c,h2_c,k_c) = aux2
 		
    enddo
    enddo
    enddo
    enddo
    enddo 

	aux = .false.
	if (any(omega_uu1<0.0d0) .or. any(omega_uu1>1.0d0) ) then
		write(*,*) "omega_uu1 contains element out of bound."
		aux = .true.
	endif
	if (any(omega_uu2<0.0d0) .or. any(omega_uu2>1.0d0) ) then
		write(*,*) "omega_uu2 contains element out of bound."
		aux = .true.
	endif
	if (any(omega_eu1<0.0d0) .or. any(omega_eu1>1.0d0) ) then
		write(*,*) "omega_eu1 contains element out of bound."
		aux = .true.
	endif
	if (any(omega_eu2<0.0d0) .or. any(omega_eu2>1.0d0) ) then
		write(*,*) "omega_eu2 contains element out of bound."
		aux = .true.
	endif
	if (any(omega_ue1<0.0d0) .or. any(omega_ue1>1.0d0) ) then
		write(*,*) "omega_ue1 contains element out of bound."
		aux = .true.
	endif
	if (any(omega_ue2<0.0d0) .or. any(omega_ue2>1.0d0) ) then
		write(*,*) "omega_ue2 contains element out of bound."
		aux = .true.
	endif
	if (any(omega_ee1<0.0d0) .or. any(omega_ee1>1.0d0) ) then
		write(*,*) "omega_ee1 contains element out of bound."
		aux = .true.
	endif
	if (any(omega_ee2<0.0d0) .or. any(omega_ee2>1.0d0) ) then
		write(*,*) "omega_ee2 contains element out of bound."
		aux = .true.
	endif
	if (aux) then 
        stop
    endif
    
    if (verbose==2) then
        write(*,*) "============================================================="
        write(*,*) "...OMEGAS and Ohs COMPUTED!"
        write(*,*) "============================================================="
    endif
    
    end subroutine acceptance_rules
 
    !================================================================================================!
    
    subroutine acceptance_rules_s(V_s)
    implicit none 
    !DESCRITPION: 
    ! Compute acceptance sets for singles
    ! See model section
    
    !Declare inputs
    type(valfun_s), intent(in) :: V_s
    
    !Declare outputs --> The Omegas are GLOBAL variables
    
    !Declare local variables
    character(len=80) :: err_msg
    integer :: sp_c,k_c,hp_c,wp_c,h_c,w_c,istat
    logical:: aux1
    real(8) :: p_vec(2),v_vec(2),EMax,disutil
    
    if (verbose==2) then
    write(*,*) "============================================================="
    write(*,*) "COMPUTE OMEGAS FOR SINGLES..."
    write(*,*) "============================================================="
    endif
    
    !Allocate omega arrays (defined as global variables)
    if (allocated(omega_u_sf)) deallocate(omega_u_sf)
    if (allocated(omega_u_sm)) deallocate(omega_u_sm)
    if (allocated(omega_e_sf)) deallocate(omega_e_sf)
    if (allocated(omega_e_sm)) deallocate(omega_e_sm)
    allocate(omega_u_sf(n_w,n_hours,n_kids,n_spinc),omega_u_sm(n_w,n_hours,n_kids,n_spinc),&
        omega_e_sf(n_w,n_hours,n_w,n_hours,n_kids,n_spinc), &
        omega_e_sm(n_w,n_hours,n_w,n_hours,n_kids,n_spinc), &
        stat=istat,errmsg=err_msg)
    if (istat/=0) then 
    	write(*,*) "omega_u_sf, omega_u_sm, etc."
        print *,err_msg
        pause
        stop 
    endif
    
    ! - Unemployed Singles: omega_u_sf & omega_u_sm
    do sp_c = 1,n_spinc
    do k_c = 1,n_kids 
        do hp_c = 1,n_hours !index h'
        do wp_c = 1,n_w     !index w'
			! Single unemployed female receives an offer
			! Accept value
			if (hp_c == n_hours) then
                	disutil = p_mu_alpha(k_c,2)
			else
				disutil = 0.0d0
			endif
			v_vec(1) = V_s%V_e_sf(wp_c,hp_c,k_c,sp_c)-disutil
			! Reject value
			v_vec(2) = V_s%V_u_sf(k_c,sp_c)
			call sub_logsum(EMax,p_vec, v_vec,p_sigma_alpha(k_c,2))
			! probability of acceptance
			omega_u_sf(wp_c,hp_c,k_c,sp_c) = p_vec(1)
			! Single unemployed male accepts
			! Accept value
			if (hp_c == n_hours) then
                disutil = p_mu_alpha(k_c,1)
			else
				disutil = 0.0d0
			endif
			v_vec(1) = V_s%V_e_sm(wp_c,hp_c,k_c,sp_c)-disutil
			! Reject value
			v_vec(2) = V_s%V_u_sm(k_c,sp_c)
			call sub_logsum(EMax,p_vec, v_vec,p_sigma_alpha(k_c,1))
			! probability of acceptance
			omega_u_sm(wp_c,hp_c,k_c,sp_c) = p_vec(1)
        enddo
        enddo
    enddo
    enddo
    
    ! - Employed Singles: omega_e_sf & omega_e_sm
    do sp_c = 1,n_spinc
    do k_c = 1,n_kids 
    do w_c = 1,n_w ! current job
    do h_c = 1,n_hours ! current job
        do hp_c = 1,n_hours !index h' offer
        do wp_c = 1,n_w     !index w' offer
			! Single employed female accepts
			! Accept value
			if (hp_c == n_hours) then
                disutil = p_mu_alpha(k_c,2)
			else
				disutil = 0.0d0
			endif
			v_vec(1) = V_s%V_e_sf(wp_c,hp_c,k_c,sp_c)-disutil
			! Reject value
			v_vec(2) = V_s%V_e_sf(w_c,h_c,k_c,sp_c)
			call sub_logsum(EMax,p_vec, v_vec,p_sigma_alpha(k_c,2))
			! probability of acceptance				
			omega_e_sf(wp_c,hp_c,w_c,h_c,k_c,sp_c) = p_vec(1)
		
			! Single employed male accepts
			! Accept value
			if (hp_c == n_hours) then
                disutil = p_mu_alpha(k_c,1)
			else
				disutil = 0.0d0
			endif
			v_vec(1) = V_s%V_e_sm(wp_c,hp_c,k_c,sp_c)-disutil
			! Reject value
			v_vec(2) = V_s%V_e_sm(w_c,h_c,k_c,sp_c)
			call sub_logsum(EMax,p_vec, v_vec,p_sigma_alpha(k_c,1))
			! probability of acceptance				
			omega_e_sm(wp_c,hp_c,w_c,h_c,k_c,sp_c) = p_vec(1)               
        enddo
        enddo
    enddo
    enddo
    enddo
	enddo
	
    if (allocated(Oh_e_sm)) deallocate(Oh_e_sm)
    if (allocated(Oh_e_sf)) deallocate(Oh_e_sf)
	allocate(Oh_e_sm(n_w,n_hours,n_kids,n_spinc), Oh_e_sf(n_w,n_hours,n_kids,n_spinc), &
		stat=istat,errmsg=err_msg)
	if (istat/=0) then 
		write(*,*) "Oh_e_sm, Oh_e_sf"
		print *,err_msg
		pause
		stop 
    endif
    
    do sp_c = 1,n_spinc
    do k_c = 1,n_kids 
	do h_c = 1,n_hours ! current job 
	do w_c = 1,n_w ! Current job 
		! Single male
		aux1 = V_s%V_e_sm(w_c,h_c,k_c,sp_c) >= V_s%V_u_sm(k_c,sp_c)
		Oh_e_sm(w_c,h_c,k_c,sp_c) = aux1
		! Single female
		aux1 = V_s%V_e_sf(w_c,h_c,k_c,sp_c) >= V_s%V_u_sf(k_c,sp_c)
		Oh_e_sf(w_c,h_c,k_c,sp_c) = aux1
	enddo
	enddo
	enddo
    enddo
    
    if (verbose==2) then
    write(*,*) "============================================================="
    write(*,*) "...OMEGAS for Singles COMPUTED!"
    write(*,*) "============================================================="
    endif
    
    end subroutine acceptance_rules_s
    
    !================================================================================================!
    
end module solve_household
