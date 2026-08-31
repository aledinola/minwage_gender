module mod_distribution
    
    !=======================================================================
    !               SOLVE FLOW EQUATIONS
    ! Programs to find the stationary g() and labor supply l()
	!=======================================================================
    
    use mod_globals
    use mod_utilities 
    implicit none
    
    private
    public get_distribution, get_distribution_s, labor_supply
    
    !Declare here module-wide variables
    type(gdist)   :: g_current 
	type(gdist_s) :: g_current_s
    real(8)       :: offer_weight_s(n_w,n_hours,2)
    
    contains
    
    !=========================outflow_uu=============================================!
  	function outflow_uu(k_c)
    implicit none

    !Declare inputs:
    integer, intent(in) :: k_c 
    !Declare function output:
    real(8) :: outflow_uu
    !Declare local variables
    integer :: kp_c,hp_c,wp_c
    real(8) :: temp
    !initialize
    outflow_uu = 0.0d0
    
    do kp_c = 1,n_kids
    	if (kp_c==1 .and. (k_c==2 .or. k_c==3)) then
    		! death/rebirth
    		outflow_uu = outflow_uu + p_rho_d
    	elseif (kp_c==k_c) then
    		do hp_c = 1,n_hours
    		do wp_c = 1,n_w
    			! job finding of member 1
    			temp = omega_uu1(wp_c,hp_c,kp_c) 
    			outflow_uu = outflow_uu + hpol(hp_c)*F_prob(wp_c,hp_c)*p_lambda_u*temp
    			! job finding of member 2
    			temp = omega_uu2(wp_c,hp_c,kp_c) 
    			outflow_uu = outflow_uu + hpol(hp_c)*F_prob(wp_c,hp_c)*p_lambda_u*temp
    		enddo !wp_c
    		enddo !hp_c
    	endif 
    	if ((kp_c==2 .and. k_c==1) .or. (kp_c==3 .and. k_c==2)) then
    		! child status change
    		outflow_uu = outflow_uu + p_phi(k_c,1,2)
    	endif 
    enddo !kp_c
   	outflow_uu = outflow_uu * g_current%uu(k_c)
	
    end function outflow_uu
    !=========================inflow_uu=============================================!
    pure function inflow_uu(kp_c)
    !Note: if you need to write output for debug, remove the attribute "pure"
    implicit none
    !Declare inputs:
    integer, intent(in) :: kp_c
    !Declare function output:
    real(8) :: inflow_uu
    !Declare local variables
    integer :: k_c,w_c,h_c,ww_c,hh_c
    
    ! initialize
    inflow_uu=0.0d0
    
    do k_c = 1,n_kids
    	! inflows due to death/rebirth
    	if (kp_c==1) then
    		if (k_c==2 .or. k_c==3) then
    			! death from uu
    			inflow_uu = inflow_uu + g_current%uu(k_c)*p_rho_d
    		endif 
    		do h_c = 1,n_hours
    		do w_c = 1,n_w
    			! death from eu or ue
				inflow_uu = inflow_uu + p_rho_d &
					*(g_current%ue(w_c,h_c,k_c)+g_current%eu(w_c,h_c,k_c))
    			do hh_c = 1,n_hours
    			do ww_c = 1,n_w
    				! death from ee
					inflow_uu = inflow_uu + p_rho_d*g_current%ee(w_c,h_c,ww_c,hh_c,k_c)
    			enddo !ww_c
    			enddo !hh_c
    		enddo !w_c
    		enddo !h_c
    	endif 
    	
    	if (kp_c == k_c) then
    		do h_c = 1,n_hours
    		do w_c = 1,n_w
    			! job loss of 1 or 2
    			inflow_uu = inflow_uu + p_delta_f(h_c)*g_current%ue(w_c,h_c,kp_c) &
    				+p_delta_m(h_c)*g_current%eu(w_c,h_c,kp_c)
    		enddo !w_c
    		enddo !h_c
    	endif 
    	
    	if ((kp_c==2 .and. k_c==1) .or. (kp_c==3 .and. k_c==2))  then
    		! child status change from uu
    		inflow_uu = inflow_uu + g_current%uu(k_c)*p_phi(k_c,1,2)
    		do h_c = 1,n_hours
    		do w_c = 1,n_w
    			! child status change from eu
    			if (.not. Oh_eu1(w_c,h_c,kp_c)) then
    			inflow_uu = inflow_uu + g_current%eu(w_c,h_c,k_c)*p_phi(k_c,1,2)
    			endif 
				! child status change from ue
    			if  (.not. Oh_ue2(w_c,h_c,kp_c)) then
    			inflow_uu = inflow_uu + g_current%ue(w_c,h_c,k_c)*p_phi(k_c,1,2)
    			endif 
				do hh_c = 1,n_hours
				do ww_c = 1,n_w
					! child status change from ee
					if (Oh_ee_minus(w_c,h_c,ww_c,hh_c,kp_c)) then
    				inflow_uu = inflow_uu + g_current%ee(w_c,h_c,ww_c,hh_c,k_c) &
    					*p_phi(k_c,1,2)	
    				endif		
				enddo !ww_c
				enddo !hh_c
    		enddo !w_c
    		enddo !h_c
    	endif 
    		
    enddo !k_c
    
    end function inflow_uu
    
    !=========================outflow_eu=============================================!
    pure function outflow_eu(w1_c,h1_c,k_c)
    implicit none

    !Declare inputs:
    integer, intent(in) :: w1_c,h1_c,k_c
    !Declare output:
    real(8) :: outflow_eu
    !Declare local variables:
    real(8):: temp
    integer :: kp_c,wp_c,hp_c
    
    ! initialize
    outflow_eu = 0.0d0
    
    do kp_c = 1,n_kids
    	!  / 1 accepts new job / 2 accepts job 
    	if (kp_c==1) then
    		! either 1 death
    		outflow_eu = outflow_eu + p_rho_d
    	endif 
    	if (kp_c==k_c) then 
    		! 1 job loss
    		outflow_eu = outflow_eu + p_delta_m(h1_c)
    		do hp_c = 1,n_hours
    		do wp_c = 1,n_w
    			! member 1 finds new job
    			if (h1_c/=hp_c .or. w1_c/=wp_c) then
    				temp = omega_eu1(wp_c,hp_c,w1_c,h1_c,kp_c)
    				outflow_eu = outflow_eu + hpol(hp_c)*F_prob(wp_c,hp_c)*p_lambda_e*temp
    			endif
    			! job finding of member 2
    			temp = omega_eu2(wp_c,hp_c,w1_c,h1_c,kp_c)
    			outflow_eu = outflow_eu + hpol(hp_c)*F_prob(wp_c,hp_c)*p_lambda_u*temp
    			
    		enddo !wp_c
    		enddo !hp_c
    	endif 
    	if ((kp_c==2 .and. k_c==1) .or. (kp_c==3 .and. k_c==2))  then
    		! child status change: 
    		outflow_eu = outflow_eu + p_phi(k_c,1,2)
    	endif 
    
    enddo ! kp_c
    

	outflow_eu = outflow_eu * g_current%eu(w1_c,h1_c,k_c)
	
	
    end function outflow_eu
    
    !=========================inflow_eu=============================================!
    pure function inflow_eu(w1p_c,h1p_c,kp_c)
    implicit none

    !Declare inputs:
    integer, intent(in) :: w1p_c,h1p_c,kp_c
    !Declare output:
    real(8) :: inflow_eu
    !Declare local variables:
    real(8) :: temp
    integer :: k_c,h_c,w_c,hh_c,ww_c
    
    ! initialize
    inflow_eu = 0.0d0
    
    do k_c = 1,n_kids
    	if (kp_c == k_c) then
    		! job finding of member 1 from uu
    		temp = omega_uu1(w1p_c,h1p_c,kp_c)
    		inflow_eu = inflow_eu + g_current%uu(kp_c)*hpol(h1p_c)*F_prob(w1p_c,h1p_c)*p_lambda_u*temp
    	
    		do h_c = 1,n_hours
        	do w_c = 1,n_w   			
    			! job finding of member 1 from eu
    			if (h_c/=h1p_c .or. w_c/=w1p_c) then
    				temp = omega_eu1(w1p_c,h1p_c,w_c,h_c,kp_c)
    				inflow_eu = inflow_eu + g_current%eu(w_c,h_c,kp_c)*hpol(h1p_c)*F_prob(w1p_c,h1p_c)*p_lambda_e*temp
    			endif 
    			! job finding of member 1 from ue
    			if (.not. Oh_ee1_2stays(w1p_c,h1p_c,w_c,h_c,kp_c)) then
    				temp = omega_ue1(w1p_c,h1p_c,w_c,h_c,kp_c)
    				inflow_eu = inflow_eu + g_current%ue(w_c,h_c,kp_c)*hpol(h1p_c)*F_prob(w1p_c,h1p_c)*p_lambda_u*temp   
    			endif			
    			! job loss of member 2
    			inflow_eu = inflow_eu + g_current%ee(w1p_c,h1p_c,w_c,h_c,kp_c)*p_delta_f(h_c)
    			do hh_c = 1,n_hours
    			do ww_c = 1,n_w
    			    ! member 1 finds new job from ee, 2 quits 
    			    if (h_c/=h1p_c .or. w_c/=w1p_c) then
    			    	! 1's new job is different from 1's old job
    			    	if (.not. Oh_ee1_2stays(w1p_c,h1p_c,ww_c,hh_c,kp_c)) then
    			    		! 2 quits when 1 accepts job (w1p_c,h1p_c)
    						temp = Omega_ee1(w1p_c,h1p_c,w_c,h_c,ww_c,hh_c,kp_c)
       						inflow_eu = inflow_eu + g_current%ee(w_c,h_c,ww_c,hh_c,kp_c)* &
       							hpol(h1p_c)*F_prob(w1p_c,h1p_c)*p_lambda_e*temp
       					endif
       				endif
    			enddo !ww_c
    			enddo !hh_c
    		enddo !w_c
        	enddo !h_c
    	elseif (kp_c-k_c==1 .and. (kp_c==2 .or. kp_c==3)) then
    		! child status change
    		temp = merge(1.0d0,0.0d0,Oh_eu1(w1p_c,h1p_c,kp_c))
        	inflow_eu = inflow_eu + g_current%eu(w1p_c,h1p_c,k_c)*p_phi(k_c,1,2)*temp 
        	do h_c = 1,n_hours
        	do w_c = 1,n_w
        		! child status change
    			temp = merge(1.0d0,0.0d0,Oh_ee1(w1p_c,h1p_c,w_c,h_c,kp_c))
      		 	inflow_eu = inflow_eu + g_current%ee(w1p_c,h1p_c,w_c,h_c,k_c)* p_phi(k_c,1,2)*temp
        	enddo !w_c
        	enddo !h_c
        	
    	endif 
    
    enddo !k_c
    
    end function inflow_eu
    
    !=========================outflow_ue=============================================!
    pure function outflow_ue(w2_c,h2_c,k_c)
    implicit none

    !Declare inputs:
    integer, intent(in) :: w2_c,h2_c,k_c
    !Declare output:
    real(8) :: outflow_ue
    !Declare local variables:
    real(8):: temp
    integer :: kp_c,wp_c,hp_c
    
    ! initialize
    outflow_ue = 0.0d0
    
    do kp_c = 1,n_kids
    	if (kp_c==1) then
    		! death
    		outflow_ue = outflow_ue + p_rho_d
    	endif 
    	if (kp_c==k_c) then 
    		! 2 job loss
    		outflow_ue = outflow_ue + p_delta_f(h2_c)
    		do hp_c = 1,n_hours
    		do wp_c = 1,n_w
    			! member 2 finds new job
    			if (h2_c/=hp_c .or. w2_c/=wp_c) then
    				temp = omega_ue2(wp_c,hp_c,w2_c,h2_c,kp_c)
    				outflow_ue = outflow_ue + hpol(hp_c)*F_prob(wp_c,hp_c)*p_lambda_e*temp
    			endif
    			! job finding of member 1
    			temp = omega_ue1(wp_c,hp_c,w2_c,h2_c,kp_c)
    			outflow_ue = outflow_ue + hpol(hp_c)*F_prob(wp_c,hp_c)*p_lambda_u*temp 
    			
    		enddo !wp_c
    		enddo !hp_c
    	endif 
      	if ((kp_c==2 .and. k_c==1) .or. (kp_c==3 .and. k_c==2))  then
    		! child status change: 
    		outflow_ue = outflow_ue + p_phi(k_c,1,2)
    	endif 
    
    enddo ! kp_c
    

	outflow_ue = outflow_ue * g_current%ue(w2_c,h2_c,k_c)
	
    end function outflow_ue
    
    !=========================inflow_ue=============================================!
    pure function inflow_ue(w2p_c,h2p_c,kp_c)
    implicit none

    !Declare inputs:
    integer, intent(in) :: w2p_c,h2p_c,kp_c
    !Declare output:
    real(8) :: inflow_ue
    !Declare local variables:
    real(8) :: temp
    integer :: k_c,h_c,w_c,hh_c,ww_c
    
    
    ! initialize
    inflow_ue = 0.0d0
    
    
    do k_c = 1,n_kids
    	if (kp_c == k_c) then
    		! job finding of member 2 from uu
    		temp = omega_uu2(w2p_c,h2p_c,kp_c)
    		inflow_ue = inflow_ue + g_current%uu(kp_c)*hpol(h2p_c)*F_prob(w2p_c,h2p_c)*p_lambda_u*temp
    	
    		do h_c = 1,n_hours
        	do w_c = 1,n_w   			
    			! job finding of member 2 from ue
    			if (h_c/=h2p_c .or. w_c/=w2p_c) then
    				! if member 2's new job is different member 2's old job
    				temp = omega_ue2(w2p_c,h2p_c,w_c,h_c,kp_c)
    				inflow_ue = inflow_ue + g_current%ue(w_c,h_c,kp_c)*hpol(h2p_c)*F_prob(w2p_c,h2p_c)*p_lambda_e*temp
    			endif 
    			! job finding of member 2 from eu
    			if (.not. Oh_eu2_1stays(w_c,h_c,w2p_c,h2p_c,kp_c)) then 
    				! if member 1 quits when member 2 accepts job (w2p_c,h2p_c)
    				temp = omega_eu2(w2p_c,h2p_c,w_c,h_c,kp_c)
    				inflow_ue = inflow_ue + g_current%eu(w_c,h_c,kp_c)*hpol(h2p_c)*F_prob(w2p_c,h2p_c)*p_lambda_u*temp   			
    			endif
    			! job loss of member 1
    			inflow_ue = inflow_ue + g_current%ee(w_c,h_c,w2p_c,h2p_c,kp_c)*p_delta_m(h_c)
    			do hh_c = 1,n_hours
    			do ww_c = 1,n_w
    			    ! member 2 finds new job from ee, 1 quits 
    			    if (hh_c/=h2p_c .or. ww_c/=w2p_c) then
    			    ! if member 2's new job is different member 2's old job
    			    	if (.not. Oh_ee2_1stays(w_c,h_c,w2p_c,h2p_c,kp_c)) then
    			    	! if member 1 quits when 2 accepts job (w2p_c,h2p_c)
    						temp = Omega_ee2(w2p_c,h2p_c,w_c,h_c,ww_c,hh_c,kp_c)
       						inflow_ue = inflow_ue + g_current%ee(w_c,h_c,ww_c,hh_c,kp_c)* &
       							hpol(h2p_c)*F_prob(w2p_c,h2p_c)*p_lambda_e*temp
       					endif
       				endif
    			enddo !ww_c
    			enddo !hh_c
    		enddo !w_c
        	enddo !h_c
    	elseif (kp_c-k_c==1 .and. (kp_c==2 .or. kp_c==3)) then
    		! child status change
    		temp = merge(1.0d0,0.0d0,Oh_ue2(w2p_c,h2p_c,kp_c))
        	inflow_ue = inflow_ue + g_current%ue(w2p_c,h2p_c,k_c)*p_phi(k_c,1,2)*temp 
        	do h_c = 1,n_hours
        	do w_c = 1,n_w
        		! child status change
    			temp = merge(1.0d0,0.0d0,Oh_ee2(w_c,h_c,w2p_c,h2p_c,kp_c))
      		 	inflow_ue = inflow_ue + g_current%ee(w_c,h_c,w2p_c,h2p_c,k_c)* p_phi(k_c,1,2)*temp
        	enddo !w_c
        	enddo !h_c
        	
    	endif 
    
    enddo !k_c
    
    end function inflow_ue
    
    !==============================outflow_ee========================================!
    pure function outflow_ee(w1_c,h1_c,w2_c,h2_c,k_c)
    implicit none
    !This function returns the transition rate 
    !	from ee(w1_c,h1_c,w2_c,h2_c,k_c) to eu(w1p_c,h1p_c,kp_c).

    !Declare inputs:
    integer, intent(in) :: w1_c,h1_c,w2_c,h2_c,k_c
    !Declare output:
    real(8) :: outflow_ee
    !Declare local variables:
    real(8) :: temp
    integer :: kp_c,wp_c,hp_c
    
    ! initialize
    outflow_ee = 0.0d0
    
    do kp_c = 1,n_kids
    	if ((kp_c==2 .and. k_c==1) .or. (kp_c==3 .and. k_c==2))  then
    		! change in child state
    		outflow_ee = outflow_ee + p_phi(k_c,1,2)
    	endif 
    	   		
		if (kp_c==1) then
			! death
			outflow_ee = outflow_ee + p_rho_d
		endif 
		
		if (kp_c == k_c) then
			 ! job loss of member 1 or member 2
			 outflow_ee = outflow_ee + p_delta_m(h1_c) + p_delta_f(h2_c)
		
			 do hp_c = 1,n_hours
			 do wp_c = 1,n_w
				 ! member 1 accepts new job
				 if (hp_c /= h1_c .or. wp_c /= w1_c ) then
					! if member 1's new job is different from old job
				 	temp = Omega_ee1(wp_c,hp_c,w1_c,h1_c,w2_c,h2_c,kp_c)
				 	outflow_ee = outflow_ee + hpol(hp_c)*F_prob(wp_c,hp_c)*p_lambda_e*temp
				 endif
				 ! member 2 accepts new job
				 if (hp_c /= h2_c .or. wp_c /= w2_c ) then
				 ! if member 2's new job is different from old job
				 	temp = Omega_ee2(wp_c,hp_c,w1_c,h1_c,w2_c,h2_c,kp_c)
				 	outflow_ee = outflow_ee + hpol(hp_c)*F_prob(wp_c,hp_c)*p_lambda_e*temp
				 endif 
			enddo !wp_c
			enddo !hp_c
		endif
	enddo !kp_c
	
	outflow_ee = g_current%ee(w1_c,h1_c,w2_c,h2_c,k_c) * outflow_ee
	
    end function outflow_ee
    
    !==============================inflow_ee========================================!
    pure function inflow_ee(w1p_c,h1p_c,w2p_c,h2p_c,kp_c)
    implicit none

    !Declare inputs:
    integer, intent(in) :: w1p_c,h1p_c,w2p_c,h2p_c,kp_c
    !Declare output:
    real(8) :: inflow_ee
    !Declare local variables:
    real(8) :: temp
    integer :: k_c,w_c,h_c
    
    ! initialize
    inflow_ee = 0.0d0
    
    do k_c = 1,n_kids
    	if (kp_c-k_c==1 .and. (kp_c==2 .or. kp_c==3)) then
    		! child status change
    		if (Oh_ee12(w1p_c,h1p_c,w2p_c,h2p_c,kp_c)) then
	        	inflow_ee = inflow_ee + g_current%ee(w1p_c,h1p_c,w2p_c,h2p_c,k_c)*p_phi(k_c,1,2)
	        endif 
        elseif (kp_c == k_c) then
		   
		   if (Oh_ue1_2stays(w1p_c,h1p_c,w2p_c,h2p_c,kp_c)) then
			   !member 1 accepts job from ue, 2 stays
			   temp = omega_ue1(w1p_c,h1p_c,w2p_c,h2p_c,kp_c)
			   inflow_ee = inflow_ee + g_current%ue(w2p_c,h2p_c,kp_c)*hpol(h1p_c)*F_prob(w1p_c,h1p_c)*p_lambda_u*temp
		   endif
		   if (Oh_eu2_1stays(w1p_c,h1p_c,w2p_c,h2p_c,kp_c)) then
			   !member 2 accepts job from eu, 1 stays
			   temp = omega_eu2(w2p_c,h2p_c,w1p_c,h1p_c,kp_c)
			   inflow_ee = inflow_ee + g_current%eu(w1p_c,h1p_c,kp_c)*hpol(h2p_c)*F_prob(w2p_c,h2p_c)*p_lambda_u*temp
		   endif
		   do h_c = 1,n_hours
		   do w_c = 1,n_w
				if (h_c /= h1p_c .or. w_c /= w1p_c) then
		   		! member 1 finds new job from ee, 2 stays 
					if (Oh_ee1_2stays(w1p_c,h1p_c,w2p_c,h2p_c,kp_c)) then
						temp = Omega_ee1(w1p_c,h1p_c,w_c,h_c,w2p_c,h2p_c,kp_c)
						inflow_ee = inflow_ee + g_current%ee(w_c,h_c,w2p_c,h2p_c,kp_c)* &
							hpol(h1p_c)*F_prob(w1p_c,h1p_c)*p_lambda_e*temp
					endif
       			endif 
       			if (h_c /= h2p_c .or. w_c /= w2p_c) then
       			! member 2 finds new job from ee, 1 stays 
       				if (Oh_ee2_1stays(w1p_c,h1p_c,w2p_c,h2p_c,kp_c)) then
						temp = Omega_ee2(w2p_c,h2p_c,w1p_c,h1p_c,w_c,h_c,kp_c)
						inflow_ee = inflow_ee + g_current%ee(w1p_c,h1p_c,w_c,h_c,kp_c)* &
							hpol(h2p_c)*F_prob(w2p_c,h2p_c)*p_lambda_e*temp
					endif
		   		endif 
		   enddo ! w_c
		   enddo ! h_c
		endif 
		
    enddo !k_c

    end function inflow_ee
    
    !=========================outflow_u_sf=============================================!
  	pure function outflow_u_sf(k_c,sp_c)
    ! Unemployed single-female
    implicit none

    !Declare inputs:
    integer, intent(in) :: k_c,sp_c
    !Note: This function also uses g_current_s, which is module-global
    !Declare function output:
    real(8) :: outflow_u_sf
    !Declare local variables
    integer :: kp_c,hp_c,wp_c,i_single
    real(8) :: temp
    
    if (sp_c == 1) then 
    	i_single = 1
    else
    	i_single = 2
    endif
    
    !initialize
    outflow_u_sf = 0.0d0
    
    do kp_c = 1,n_kids
    	if (kp_c==1 .and. (k_c==2 .or. k_c==3)) then
    		! death/rebirth
    		outflow_u_sf = outflow_u_sf + p_rho_d
    	elseif (kp_c==k_c) then
    		do hp_c = 1,n_hours
    		do wp_c = 1,n_w
    			! job finding
    			temp = Omega_u_sf(wp_c,hp_c,kp_c,sp_c)
    			outflow_u_sf = outflow_u_sf + offer_weight_s(wp_c,hp_c,2)*p_lambda_u*temp
    		enddo !wp_c
    		enddo !hp_c
    	endif 
    	if ((kp_c==2 .and. k_c==1) .or. (kp_c==3 .and. k_c==2)) then
    		! child status change
    		outflow_u_sf = outflow_u_sf + p_phi(kp_c,2,i_single)
    	endif 
    enddo !kp_c
    
   	outflow_u_sf = outflow_u_sf * g_current_s%u_sf(k_c,sp_c)
	
    end function outflow_u_sf
    
    !=========================outflow_u_sm=============================================!
  	pure function outflow_u_sm(k_c,sp_c)
    ! Unemployed single-male
    implicit none

    !Declare inputs:
    integer, intent(in) :: k_c ,sp_c
    !Note: This function also uses g_current_s, which is module-global
    !Declare function output:
    real(8) :: outflow_u_sm
    !Declare local variables
    integer :: kp_c,hp_c,wp_c,i_single
    real(8) :: temp
    
    if (sp_c == 1) then 
    	i_single = 1
    else
    	i_single = 2
    endif
    
    !initialize
    outflow_u_sm = 0.0d0
    
    do kp_c = 1,n_kids
    	if (kp_c==1 .and. (k_c==2 .or. k_c==3)) then
    		! death/rebirth
    		outflow_u_sm = outflow_u_sm + p_rho_d
    	elseif (kp_c==k_c) then
    		do hp_c = 1,n_hours
    		do wp_c = 1,n_w
    			! job finding
    			temp = omega_u_sm(wp_c,hp_c,kp_c,sp_c)
    			outflow_u_sm = outflow_u_sm + offer_weight_s(wp_c,hp_c,1)*p_lambda_u*temp
    		enddo !wp_c
    		enddo !hp_c
    	endif 
    	if ((kp_c==2 .and. k_c==1) .or. (kp_c==3 .and. k_c==2)) then
    		! child status change
    		outflow_u_sm = outflow_u_sm + p_phi(kp_c,1,i_single)
    	endif 
    enddo !kp_c
    
   	outflow_u_sm = outflow_u_sm * g_current_s%u_sm(k_c,sp_c)
	
    end function outflow_u_sm
       
    !=========================inflow_u_sf=============================================!
    pure function inflow_u_sf(kp_c,sp_c)
    !Note: if you need to write output for debug, remove the attribute "pure"
    implicit none
    !Declare inputs:
    integer, intent(in) :: kp_c ,sp_c
    !Declare function output:
    real(8) :: inflow_u_sf
    !Declare local variables
    integer :: k_c,w_c,h_c,i_single
    
    if (sp_c == 1) then 
    	i_single = 1
    else
    	i_single = 2
    endif
    
    ! initialize
    inflow_u_sf=0.0d0
    
    do k_c = 1,n_kids
    	! inflows due to death/rebirth
    	if (kp_c==1) then
    		if (k_c==2 .or. k_c==3) then
    			! death from u_sf
    			inflow_u_sf = inflow_u_sf + g_current_s%u_sf(k_c,sp_c)*p_rho_d
    		endif 
    		do h_c = 1,n_hours
    		do w_c = 1,n_w
    			! death from e_sf
				inflow_u_sf = inflow_u_sf + p_rho_d*g_current_s%e_sf(w_c,h_c,k_c,sp_c)
    		enddo !w_c
    		enddo !h_c
    	endif 
    	
    	if (kp_c == k_c) then
    		do h_c = 1,n_hours
    		do w_c = 1,n_w
    			! job loss
    			inflow_u_sf = inflow_u_sf + p_delta_f(h_c)*g_current_s%e_sf(w_c,h_c,kp_c,sp_c)
    		enddo !w_c
    		enddo !h_c
    	endif 
    	
    	if ((kp_c==2 .and. k_c==1) .or. (kp_c==3 .and. k_c==2))  then
    		! child status change from u_sf
    		inflow_u_sf = inflow_u_sf + g_current_s%u_sf(k_c,sp_c)*p_phi(kp_c,2,i_single)
    		do h_c = 1,n_hours
    		do w_c = 1,n_w
    			! child status change from e_sf and quit
    			inflow_u_sf = inflow_u_sf &
    			+ g_current_s%e_sf(w_c,h_c,k_c,sp_c)*p_phi(kp_c,2,i_single)*(1.0d0-omega_u_sf(w_c,h_c,kp_c,sp_c))
    			 
    		enddo !w_c
    		enddo !h_c
    	endif 
    		
    enddo !k_c
    
    end function inflow_u_sf
    !=========================inflow_u_sm=============================================!
    pure function inflow_u_sm(kp_c,sp_c)
    !Note: if you need to write output for debug, remove the attribute "pure"
    implicit none
    !Declare inputs:
    integer, intent(in) :: kp_c ,sp_c
    !Declare function output:
    real(8) :: inflow_u_sm
    !Declare local variables
    integer :: k_c,w_c,h_c,i_single
    
    if (sp_c == 1) then 
    	i_single = 1
    else
    	i_single = 2
    endif
    ! initialize
    inflow_u_sm=0.0d0
    
    do k_c = 1,n_kids
    	! inflows due to death/rebirth
    	if (kp_c==1) then
    		if (k_c==2 .or. k_c==3) then
    			! death from u_sm
    			inflow_u_sm = inflow_u_sm + g_current_s%u_sm(k_c,sp_c)*p_rho_d
    		endif 
    		do h_c = 1,n_hours
    		do w_c = 1,n_w
    			! death from e_sm
				inflow_u_sm = inflow_u_sm + p_rho_d*g_current_s%e_sm(w_c,h_c,k_c,sp_c)
    		enddo !w_c
    		enddo !h_c
    	endif 
    	
    	if (kp_c == k_c) then
    		do h_c = 1,n_hours
    		do w_c = 1,n_w
    			! job loss
    			inflow_u_sm = inflow_u_sm + p_delta_m(h_c)*g_current_s%e_sm(w_c,h_c,kp_c,sp_c)
    		enddo !w_c
    		enddo !h_c
    	endif 
    	
    	if ((kp_c==2 .and. k_c==1) .or. (kp_c==3 .and. k_c==2))  then
    		! child status change from u_sm
    		inflow_u_sm = inflow_u_sm + g_current_s%u_sm(k_c,sp_c)*p_phi(kp_c,1,i_single)
    		do h_c = 1,n_hours
    		do w_c = 1,n_w
    			! child status change from e_sm and quit
    			inflow_u_sm = inflow_u_sm &
    			+ g_current_s%e_sm(w_c,h_c,k_c,sp_c)*p_phi(kp_c,1,i_single)*(1.0d0-omega_u_sm(w_c,h_c,kp_c,sp_c))
    			 
    		enddo !w_c
    		enddo !h_c
    	endif 
    		
    enddo !k_c
    
    end function inflow_u_sm
    
    !=========================outflow_e_sf=============================================!
    pure function outflow_e_sf(w_c,h_c,k_c,sp_c)
    implicit none

    !Declare inputs:
    integer, intent(in) :: w_c,h_c,k_c,sp_c
    !Declare output:
    real(8) :: outflow_e_sf
    !Declare local variables:
    real(8) :: temp
    integer :: kp_c,wp_c,hp_c,i_single
    
    if (sp_c == 1) then 
    	i_single = 1
    else
    	i_single = 2
    endif
    ! initialize
    outflow_e_sf = 0.0d0
    
    do kp_c = 1,n_kids
    	if (kp_c==1) then
    		! death
    		outflow_e_sf = outflow_e_sf + p_rho_d
    	endif 
    	if (kp_c==k_c) then 
    		! 1 job loss
    		outflow_e_sf = outflow_e_sf + p_delta_f(h_c)
    		do hp_c = 1,n_hours
    		do wp_c = 1,n_w
    			! finds new job
    			if (h_c/=hp_c .or. w_c/=wp_c) then
    			temp = omega_e_sf(wp_c,hp_c,w_c,h_c,kp_c,sp_c)
    			outflow_e_sf = outflow_e_sf + offer_weight_s(wp_c,hp_c,2)*p_lambda_e*temp 			
    			endif
    		enddo !wp_c
    		enddo !hp_c
    	endif 
    	if ((kp_c==2 .and. k_c==1) .or. (kp_c==3 .and. k_c==2))  then
    		! child status change: 
    		outflow_e_sf = outflow_e_sf + p_phi(kp_c,2,i_single)
    	endif 
    
    enddo ! kp_c

	outflow_e_sf = outflow_e_sf * g_current_s%e_sf(w_c,h_c,k_c,sp_c)
	
    end function outflow_e_sf
    
    !=========================outflow_e_sm=============================================!
    pure function outflow_e_sm(w_c,h_c,k_c,sp_c)
    implicit none

    !Declare inputs:
    integer, intent(in) :: w_c,h_c,k_c,sp_c
    !Declare output:
    real(8) :: outflow_e_sm
    !Declare local variables:
    real(8) :: temp
    integer :: kp_c,wp_c,hp_c,i_single
    
    if (sp_c == 1) then 
    	i_single = 1
    else
    	i_single = 2
    endif
    ! initialize
    outflow_e_sm = 0.0d0
    
    do kp_c = 1,n_kids
    	if (kp_c==1) then
    		! death
    		outflow_e_sm = outflow_e_sm + p_rho_d
    	endif 
    	if (kp_c==k_c) then 
    		! 1 job loss
    		outflow_e_sm = outflow_e_sm + p_delta_m(h_c)
    		do hp_c = 1,n_hours
    		do wp_c = 1,n_w
    			! finds new job
    			if (h_c/=hp_c .or. w_c/=wp_c) then
    			temp = omega_e_sm(wp_c,hp_c,w_c,h_c,kp_c,sp_c)
    			outflow_e_sm = outflow_e_sm + offer_weight_s(wp_c,hp_c,1)*p_lambda_e*temp 			
    			endif
    		enddo !wp_c
    		enddo !hp_c
    	endif 
    	if ((kp_c==2 .and. k_c==1) .or. (kp_c==3 .and. k_c==2))  then
    		! child status change: 
    		outflow_e_sm = outflow_e_sm + p_phi(kp_c,1,i_single)
    	endif 
    
    enddo ! kp_c

	outflow_e_sm = outflow_e_sm * g_current_s%e_sm(w_c,h_c,k_c,sp_c)
	
    end function outflow_e_sm
    
    !=========================inflow_e_sf=============================================!
    pure function inflow_e_sf(wp_c,hp_c,kp_c,sp_c)
    implicit none

    !Declare inputs:
    integer, intent(in) :: wp_c,hp_c,kp_c,sp_c
    !Declare output:
    real(8) :: inflow_e_sf
    !Declare local variables:
    real(8) :: temp
    integer :: k_c,h_c,w_c,i_single
    
    if (sp_c == 1) then 
    	i_single = 1
    else
    	i_single = 2
    endif
    
    ! initialize
    inflow_e_sf = 0.0d0
    
    do k_c = 1,n_kids
    	if (kp_c == k_c) then
    		! job finding from u_sf
    		temp = omega_u_sf(wp_c,hp_c,kp_c,sp_c)
    		inflow_e_sf = inflow_e_sf + &
    			g_current_s%u_sf(kp_c,sp_c)*offer_weight_s(wp_c,hp_c,2)*p_lambda_u*temp
    	
    		do h_c = 1,n_hours
        	do w_c = 1,n_w   			
    			! job finding from e_sf
    			if (h_c/=hp_c .or. w_c/=wp_c) then
    			temp = omega_e_sf(wp_c,hp_c,w_c,h_c,kp_c,sp_c)
    			inflow_e_sf = inflow_e_sf + &
    				g_current_s%e_sf(w_c,h_c,kp_c,sp_c)*offer_weight_s(wp_c,hp_c,2)*p_lambda_e*temp
    			endif 
    		enddo !w_c
        	enddo !h_c
    	elseif ((kp_c==2 .and. k_c==1) .or. (kp_c==3 .and. k_c==2)) then
    		! child status change and keep the job
        	inflow_e_sf = inflow_e_sf &
        	+ g_current_s%e_sf(wp_c,hp_c,k_c,sp_c)*p_phi(kp_c,2,i_single)*omega_u_sf(wp_c,hp_c,kp_c,sp_c) 
    		
    	endif 
    
    enddo !k_c
    
    end function inflow_e_sf
    !=========================inflow_e_sm=============================================!
    pure function inflow_e_sm(wp_c,hp_c,kp_c,sp_c)
    implicit none

    !Declare inputs:
    integer, intent(in) :: wp_c,hp_c,kp_c,sp_c
    !Declare output:
    real(8) :: inflow_e_sm
    !Declare local variables:
    real(8) :: temp
    integer :: k_c,h_c,w_c,i_single
    
    if (sp_c == 1) then 
    	i_single = 1
    else
    	i_single = 2
    endif
    
    ! initialize
    inflow_e_sm = 0.0d0
    
    do k_c = 1,n_kids
    	if (kp_c == k_c) then
    		! job finding from u_sm
    		temp = omega_u_sm(wp_c,hp_c,kp_c,sp_c)
    		inflow_e_sm = inflow_e_sm + &
    			g_current_s%u_sm(kp_c,sp_c)*offer_weight_s(wp_c,hp_c,1)*p_lambda_u*temp
    	
    		do h_c = 1,n_hours
        	do w_c = 1,n_w   			
    			! job finding from e_sm
    			if (h_c/=hp_c .or. w_c/=wp_c) then
    			temp = omega_e_sm(wp_c,hp_c,w_c,h_c,kp_c,sp_c)
    			inflow_e_sm = inflow_e_sm + &
    				g_current_s%e_sm(w_c,h_c,kp_c,sp_c)*offer_weight_s(wp_c,hp_c,1)*p_lambda_e*temp
    			endif 
    		enddo !w_c
        	enddo !h_c
    	elseif ((kp_c==2 .and. k_c==1) .or. (kp_c==3 .and. k_c==2)) then
    		! child status change and keep the job
        	inflow_e_sm = inflow_e_sm &
        	+ g_current_s%e_sm(wp_c,hp_c,k_c,sp_c)*p_phi(kp_c,1,i_single)*omega_u_sm(wp_c,hp_c,kp_c,sp_c)
        	 
    	endif 
    
    enddo !k_c
    
    end function inflow_e_sm
    !======================================================================!
    !MAIN SUBROUTINES
    !----------------Couples
    subroutine get_distribution(g,conv_flag)
    
    use omp_lib 
    use mod_numerical, only: myerror
    
    implicit none
    
    !Declare inputs/outputs:
    type(gdist), intent(inout) :: g !Fields: uu,eu,ue,ee
    logical, intent(out) :: conv_flag
    
    !Declare locals:
    integer :: istat,iter
    integer:: k_c,w1_c,h1_c,w2_c,h2_c
	real(8) :: t1,t2,diff,dist_uu,dist_eu,dist_ue,dist_ee,sum_uu,sum_eu,sum_ue,sum_ee,sum_tot
	real(8) :: in_uu,in_eu,in_ue,in_ee,out_uu,out_eu,out_ue,out_ee     
    
    ! initialize the module variable g_current
    allocate(g_current%uu(n_kids),&
        g_current%eu(n_w,n_hours,n_kids),&
        g_current%ue(n_w,n_hours,n_kids),&
        g_current%ee(n_w,n_hours,n_w,n_hours,n_kids),stat=istat)
    if (istat/=0) then
        call myerror("get_distribution: Allocation of g_current failed")
    endif
    
    
    !g_current is a module-wide variable, accessible to all module-programs
	g_current = g
	
    if (verbose==2) write(*,*) "Before openMP.."
    if (debug) pause
    
    !$omp parallel if (par_fortran_g==1)
    if (verbose==2) write(*,*) "Parallel hello to you!"
    !$omp end parallel
    
   if (verbose==2) write(*,*) "After openMP.."
    
    if (debug) pause
    
    if (verbose==2) then
    write(*,*) "============================================================="
    write(*,*) "ACCEPTED OFFER DISTRIB. ITERATION..."
    write(*,*) "============================================================="
	endif 
    diff = 10.0d0
    iter = 0
    conv_flag = .true. !This will be changed to .false. if convergence fails

    t1 = omp_get_wtime()
    
     if (verbose>=2) then
        	!Checks
			sum_uu = sum(g%uu)
			sum_eu = sum(g%eu)
			sum_ue = sum(g%ue)
			sum_ee = sum(g%ee)
      	 	sum_tot = sum_uu+sum_eu+sum_ue+sum_ee
            write(*,*) "-----------------------------------"
            write(*,'(a,f0.10)') " sum_uu = ", sum_uu
            write(*,'(a,f0.10)') " sum_eu = ", sum_eu
            write(*,'(a,f0.10)') " sum_ue = ", sum_ue
            write(*,'(a,f0.10)') " sum_ee = ", sum_ee
            write(*,'(a,f0.7)') " check_sum = ", sum_tot
            write(*,*) "-----------------------------------"
            pause
    endif
    
    do while ((diff > tol_dist .and. iter <= maxit_dist)  .or. iter <= minit_dist)
        iter = iter + 1
                
        !MAIN BODY OF THE LOOP 
        
        !------- UU ---------!
        if (debug) write(*,*) "Start UU"
        
        !$omp parallel if (par_fortran_g==1) default(shared) private(k_c,&
        !$ out_uu,in_uu)
        !$omp do collapse(1)
        do k_c  = 1,n_kids
            
            !--- outflow from uu ---!    
        	out_uu = outflow_uu(k_c)
            
    	    !--- inflow into uu ---!
            in_uu = inflow_uu(k_c)
            
            ! Update by adding inflow and subtracting outflow
            g%uu(k_c) = g_current%uu(k_c) + adjust_g*(in_uu - out_uu) 
            
        enddo !k
    	!$omp enddo
        !$omp end parallel
        
        !!------- EU ------------------------------------!
        
        if (debug) write(*,*) "Start EU"
        
        !$omp parallel if (par_fortran_g==1) default(shared) private(k_c,&
        !$ h1_c,w1_c,out_eu,in_eu)
        !$omp do collapse(3)
        do k_c = 1,n_kids
        do h1_c = 1,n_hours
        do w1_c = 1,n_w
				!--- outflow from eu ---!  
				!Initialize outflow at zero
				out_eu = outflow_eu(w1_c,h1_c,k_c)
			
				!--- inflow into EU ---!  
				!Initialize inflow at zero
			
				in_eu = inflow_eu(w1_c,h1_c,k_c)
	 
				! Add the inflow and subtract the outflow
				g%eu(w1_c,h1_c,k_c) = g_current%eu(w1_c,h1_c,k_c) + adjust_g*(in_eu - out_eu) 
            
        enddo !w1
        enddo !h1
        enddo !k
        !$omp enddo
        !$omp end parallel
        
    	!!------- UE ---------------------------------------! 
        
        if (debug) write(*,*) "Start UE"
        !$omp parallel if (par_fortran_g==1) default(shared) private(k_c,&
        !$ h2_c,w2_c,out_ue,in_ue)
        !$omp do collapse(3)
        do k_c = 1,n_kids
        do h2_c = 1,n_hours
        do w2_c = 1,n_w
				 !--- outflow from ue ---!  
				out_ue = outflow_ue(w2_c,h2_c,k_c)
			
				!--- inflow into ue ---!  
				in_ue = inflow_ue(w2_c,h2_c,k_c)
						
				! Add the inflow and subtract the outflow
				g%ue(w2_c,h2_c,k_c) = g_current%ue(w2_c,h2_c,k_c) + adjust_g*(in_ue - out_ue) 
        enddo !w2 
        enddo !h2
        enddo !k
        !$omp enddo
        !$omp end parallel
        
    	!!------- EE ------------------------------------------------!
        
        if (debug) write(*,*) "Start EE"
        !$omp parallel if (par_fortran_g==1) default(shared) private(k_c,&
        !$ h2_c,w2_c,h1_c,w1_c,out_ee,in_ee)
        !$omp do collapse(5)
        do k_c  = 1,n_kids
        do h2_c = 1,n_hours
        do w2_c = 1,n_w
        do h1_c = 1,n_hours
        do w1_c = 1,n_w    
				!--- outflow from ee ---!  
				out_ee = outflow_ee(w1_c,h1_c,w2_c,h2_c,k_c)
				
				!--- inflow into ee ---!  
				in_ee = inflow_ee(w1_c,h1_c,w2_c,h2_c,k_c)
			 
				! Add the inflow and subtract the outflow
				g%ee(w1_c,h1_c,w2_c,h2_c,k_c)= g_current%ee(w1_c,h1_c,w2_c,h2_c,k_c) &
					+adjust_g*(in_ee-out_ee) 
        enddo !w1
        enddo !h1
        enddo !w2
        enddo !h2
        enddo !k
        !$omp enddo
        !$omp end parallel
    
        !Compute dist b/w iterations
        dist_uu = maxval(abs(g_current%uu-g%uu))
        dist_eu = maxval(abs(g_current%eu-g%eu))
        dist_ue = maxval(abs(g_current%ue-g%ue))
        dist_ee = maxval(abs(g_current%ee-g%ee))
        
        !Error for distrib. g
        diff = max(dist_uu,dist_eu,dist_ue,dist_ee)
        
        !Update the guess for the distribution
          
        g_current%uu = g%uu
        g_current%eu = g%eu
        g_current%ue = g%ue
        g_current%ee = g%ee
        
        
        if (verbose>=2) then
        	!Checks
			sum_uu = sum(g%uu)
			sum_eu = sum(g%eu)
			sum_ue = sum(g%ue)
			sum_ee = sum(g%ee)			
      	 	sum_tot = sum_uu+sum_eu+sum_ue+sum_ee
            write(*,*) "-----------------------------------"
            write(*,'(a,I4)') " iteration on G = ", iter
            write(*,'(a,f0.10)') " norm_total = ", diff
            write(*,'(a,f0.10)') " dist_uu = ", dist_uu
            write(*,'(a,f0.10)') " dist_eu = ", dist_eu
            write(*,'(a,f0.10)') " dist_ue = ", dist_ue
            write(*,'(a,f0.10)') " dist_ee = ", dist_ee
            
            !
            !
            write(*,'(a,f0.7)') " check_sum = ", sum_tot
            write(*,*) "-----------------------------------"
            
        endif
        
        if (debug) then
            if (iter<=5) pause
        endif
        
    enddo !while loop ends here
    
    t2 = omp_get_wtime()
    if (verbose==2) then
    write(*,*) "============================================================="
    write(*,*) "ACCEPTED OFFER DISTRIB. FOUND"
    write(*,*) "============================================================="
    write(*,*) "============================================================="
    write(*,*) 'Program runs for',real(t2-t1),'seconds.'
    write(*,*) "============================================================="
    endif 
    if (diff > tol_dist) conv_flag = .false.

    deallocate(g_current%uu,g_current%eu,g_current%ue,g_current%ee,stat=istat)
    if (istat/=0) then
        call myerror("Deallocation failed!")
    endif

    if (debug) pause
    
    end subroutine get_distribution
    !======================================================================!    
    
    !----------------Singles
    subroutine get_distribution_s(g_s,conv_flag)
    
    use omp_lib 
    use mod_numerical, only: myerror
    
    implicit none
    
    !Declare inputs/outputs:
    type(gdist_s), intent(inout) :: g_s 
    logical,       intent(out)   :: conv_flag
    
    !Declare locals:
    integer :: istat,iter
    integer :: sp_c,k_c,h_c,w_c,hp_c,wp_c
	real(8) :: t1,t2,diff,dist_u_sf,dist_e_sf,dist_u_sm,dist_e_sm
	real(8) :: sum_u_sf,sum_u_sm,sum_e_sf,sum_e_sm,sum_tot
	real(8) :: in_u_sf,in_u_sm,in_e_sf,in_e_sm,out_u_sf,out_u_sm,out_e_sf,out_e_sm
    
    ! initialize the module variable g_current
    allocate(g_current_s%u_sf(n_kids,n_spinc),&
        g_current_s%u_sm(n_kids,n_spinc),&
        g_current_s%e_sf(n_w,n_hours,n_kids,n_spinc),&
        g_current_s%e_sm(n_w,n_hours,n_kids,n_spinc),stat=istat)
    if (istat/=0) then
        call myerror("get_distribution_s: Allocation of g_current_s failed")
    endif
    
    !g_current is a module-wide variable, accessible to all module-programs
	g_current_s = g_s

    do hp_c = 1,n_hours
    do wp_c = 1,n_w
        offer_weight_s(wp_c,hp_c,1) = hpol(hp_c)*F_prob(wp_c,hp_c)*I_mw(wp_c,hp_c,1)
        offer_weight_s(wp_c,hp_c,2) = hpol(hp_c)*F_prob(wp_c,hp_c)*I_mw(wp_c,hp_c,2)
    enddo
    enddo
	
    if (verbose==2) write(*,*) "Before openMP.."
    if (debug) pause
    
    !$omp parallel if (par_fortran_g==1)
    if (verbose==2) write(*,*) "Parallel hello to you!"
    !$omp end parallel
    
   if (verbose==2) write(*,*) "After openMP.."
    
    if (debug) pause
    
    if (verbose==2) then
    write(*,*) "============================================================="
    write(*,*) "ACCEPTED OFFER DISTRIB. FOR SINGLES ITERATION..."
    write(*,*) "============================================================="
	endif 
    diff = 10.0d0
    iter = 0
    conv_flag = .true. !This will be changed to .false. if convergence fails

    t1 = omp_get_wtime()
    
    do while ((diff > tol_dist .and. iter <= maxit_dist) .or. iter <= minit_dist)
        iter = iter + 1
                
        !MAIN BODY OF THE LOOP 
        
        !------- Unemployed Single Females ---------!
        if (debug)  write(*,*) "Start U_SF"
        
        !$omp parallel if (par_fortran_g==1) default(shared) private(k_c,sp_c,&
        !$ out_u_sf,in_u_sf)
        !$omp do collapse(2)
        do sp_c = 1,n_spinc
        do k_c  = 1,n_kids
            
            !--- outflow from u_sf ---!    
        	out_u_sf = outflow_u_sf(k_c,sp_c)
            
    	    !--- inflow into u_sf ---!
            in_u_sf = inflow_u_sf(k_c,sp_c)
            
            ! Update by adding inflow and subtracting outflow
            g_s%u_sf(k_c,sp_c) = g_current_s%u_sf(k_c,sp_c) + adjust_gs*(in_u_sf - out_u_sf) 
        enddo !k
        enddo !sp_c
    	!$omp enddo
        !$omp end parallel

        !------- Unemployed Single Males ---------!
        if (debug) write(*,*) "Start U_SM"
        
        !$omp parallel if (par_fortran_g==1) default(shared) private(k_c,sp_c,&
        !$ out_u_sm,in_u_sm)
        !$omp do collapse(2)
        do sp_c = 1,n_spinc
        do k_c  = 1,n_kids
            
            !--- outflow from u_sm ---!    
        	out_u_sm = outflow_u_sm(k_c,sp_c)
            
    	    !--- inflow into u_sm ---!
            in_u_sm = inflow_u_sm(k_c,sp_c)
            
            ! Update by adding inflow and subtracting outflow
            g_s%u_sm(k_c,sp_c) = g_current_s%u_sm(k_c,sp_c) + adjust_gs*(in_u_sm - out_u_sm) 
            
        enddo !k
        enddo ! sp
    	!$omp enddo
        !$omp end parallel
        
        !!------- Employed Single Females ------------------------------------!
        
        if (debug) write(*,*) "Start E_SF"
        
        !$omp parallel if (par_fortran_g==1) default(shared) private(k_c,sp_c,&
        !$ h_c,w_c,out_e_sf,in_e_sf)
        !$omp do collapse(4)
        do sp_c = 1,n_spinc
        do k_c = 1,n_kids
        do h_c = 1,n_hours
        do w_c = 1,n_w
				!--- outflow from e_sf ---!  
				out_e_sf = outflow_e_sf(w_c,h_c,k_c,sp_c)
			
				!--- inflow into e_sf ---!  			
				in_e_sf = inflow_e_sf(w_c,h_c,k_c,sp_c)
	 
				! Add the inflow and subtract the outflow
				g_s%e_sf(w_c,h_c,k_c,sp_c) = g_current_s%e_sf(w_c,h_c,k_c,sp_c) + adjust_gs*(in_e_sf - out_e_sf) 
            
        enddo !w
        enddo !h
        enddo !k
        enddo ! sp
        !$omp enddo
        !$omp end parallel
       
        !!------- Employed Single Males ------------------------------------!
        
        if (debug) write(*,*) "Start E_SM"
        
        !$omp parallel if (par_fortran_g==1) default(shared) private(k_c,sp_c,&
        !$ h_c,w_c,out_e_sm,in_e_sm)
        !$omp do collapse(4)
        do sp_c = 1,n_spinc
        do k_c = 1,n_kids
        do h_c = 1,n_hours
        do w_c = 1,n_w
				!--- outflow from e_sm ---!  
				out_e_sm = outflow_e_sm(w_c,h_c,k_c,sp_c)
			
				!--- inflow into e_sm ---!  			
				in_e_sm = inflow_e_sm(w_c,h_c,k_c,sp_c)
	 
				! Add the inflow and subtract the outflow
				g_s%e_sm(w_c,h_c,k_c,sp_c) = g_current_s%e_sm(w_c,h_c,k_c,sp_c) + adjust_gs*(in_e_sm - out_e_sm) 
            
        enddo !w
        enddo !h
        enddo !k
        enddo !sp
        !$omp enddo
        !$omp end parallel       
    	
    
        !Compute dist b/w iterations
        dist_u_sf = maxval(abs(g_current_s%u_sf-g_s%u_sf))
        dist_u_sm = maxval(abs(g_current_s%u_sm-g_s%u_sm))
        dist_e_sf = maxval(abs(g_current_s%e_sf-g_s%e_sf))
        dist_e_sm = maxval(abs(g_current_s%e_sm-g_s%e_sm))
        
        !Error for distrib. g_s
        diff = max(dist_u_sf,dist_u_sm,dist_e_sf,dist_e_sm)
        
        !Update the guess for the distribution  
        g_current_s%u_sf = g_s%u_sf
        g_current_s%e_sf = g_s%e_sf
        g_current_s%u_sm = g_s%u_sm
        g_current_s%e_sm = g_s%e_sm
        
        
        if (verbose==2) then
        	!Checks
        	
			sum_u_sf = sum(g_s%u_sf)
			sum_e_sf = sum(g_s%e_sf)
			sum_u_sm = sum(g_s%u_sm)
			sum_e_sm = sum(g_s%e_sm)
		
      	 	sum_tot = sum_u_sf+sum_e_sf+sum_u_sm+sum_e_sm
            write(*,*) "-----------------------------------"
            write(*,'(a,I4)') " iteration on G_s = ", iter
            write(*,'(a,f0.7)') " norm_total = ", diff
            write(*,'(a,f0.7)') " dist_u_sf = ", dist_u_sf
            write(*,'(a,f0.7)') " dist_u_sm = ", dist_u_sm
            write(*,'(a,f0.7)') " dist_e_sf = ", dist_e_sf
            write(*,'(a,f0.7)') " dist_e_sm = ", dist_e_sm
            
            !
            !
            write(*,'(a,f0.7)') " check_sum = ", sum_tot
            write(*,*) "-----------------------------------"
            
        endif
        
        if (debug) then
            if (iter<=5) pause
        endif
        
    enddo !while loop ends here
    
    t2 = omp_get_wtime()
    if (verbose==2) then
    write(*,*) "============================================================="
    write(*,*) "ACCEPTED OFFER DISTRIB. FOUND"
    write(*,*) "============================================================="
    write(*,*) "============================================================="
    write(*,*) 'Program runs for',real(t2-t1),'seconds.'
    write(*,*) "============================================================="
    endif 
    if (diff > tol_dist) conv_flag = .false.

    deallocate(g_current_s%u_sf,g_current_s%u_sm,g_current_s%e_sf,g_current_s%e_sm,stat=istat)
    if (istat/=0) then
        call myerror("Deallocation failed!")
    endif

    if (debug) pause
    
    end subroutine get_distribution_s
    !======================================================================!
    
    subroutine labor_supply(g,g_s,w_c,h_c,labsup)
    
    implicit none 
	! Computes efficiency units of labor supply of a firm that offers a job (w,h),
	! given steady-state distributions g and g_s


    !Declare inputs:
    type(gdist), intent(in)   :: g   !steady state distribution 
    type(gdist_s), intent(in) :: g_s !steady state distribution of singles
    integer,intent(in) :: w_c,h_c ! Current labor supply state
    
    !Declare outputs:
    real(8), intent(out) :: labsup ! Labor supply
    
    !Declare locals
    real(8) :: temp_m,temp_f,l_m,l_f
    integer :: k_c,sp_c
    
	labsup = 0.0d0
	! Calculate labor supply
	! Males
		temp_m = 0.0d0
		if (do_nocouple == .false.) then
			!married men: Integral over alpha and sum over k
			do k_c  = 1,n_kids
				if (hpol(h_c)*F_prob(w_c,h_c) > 0.0d0) then
					! Measure of workers in job (p,h_c)
					l_m = g%eu(w_c,h_c,k_c) + sum(g%ee(w_c,h_c,:,:,k_c)) 
					temp_m = temp_m + l_m
				endif 
			enddo
		endif 
		!single men: 
		do sp_c = 1,n_spinc
		do k_c  = 1,n_kids
			if (hpol(h_c)*F_prob(w_c,h_c)*I_mw(w_c,h_c,1) > 0.0d0) then
				! Measure of workers in job (p,h_c)
				l_m = g_s%e_sm(w_c,h_c,k_c,sp_c)
				temp_m = temp_m + l_m
			endif 
		enddo
		enddo


	! Females
		temp_f = 0.0d0
		if (do_nocouple == .false.) then
			!Married women: Integral over alpha and sum over k
			do k_c  = 1,n_kids
				! Females
				if (hpol(h_c)*F_prob(w_c,h_c)> 0.0d0) then
					! Measure of workers in job (p,h_c)
					l_f = g%ue(w_c,h_c,k_c) + sum(g%ee(:,:,w_c,h_c,k_c))
					temp_f = temp_f + l_f
				endif 
			enddo
		endif

		!Single Women: 
		do sp_c = 1,n_spinc
		do k_c  = 1,n_kids
			if (hpol(h_c)*F_prob(w_c,h_c)*I_mw(w_c,h_c,2) > 0.0d0) then
				! Measure of workers in job (p,h_c)
				l_f = g_s%e_sf(w_c,h_c,k_c,sp_c)
				
				temp_f = temp_f + l_f
			endif 
		enddo
		enddo
		
		! Efficiency unit of labor supply by each man is normalized to 1
		! Efficiency unit of labor supply by each woman is p_a
		labsup =( temp_m + p_a(h_c)*temp_f) / (hpol(h_c)*F_prob(w_c,h_c) )
	
    end subroutine labor_supply
    !====================================================!
    
    
end module mod_distribution
    
