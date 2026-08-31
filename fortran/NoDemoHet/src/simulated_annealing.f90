!#####
! A PARALLEL SIMULATED ANNEALING METHOD FOR SOLVING GLOBAL OPTIMIZATION PROBLEMS
!#####
module simulated_annealing
    
use toolbox
use mod_numerical, only:  myerror

implicit none   

private
public :: SA_minsearch_omp,SA_minsearch

real*8, parameter :: pi = 3.14159265358979323846d0
    
contains

!=====================================================================
	    subroutine SA_minsearch(p, fret, minimum, maximum, func, factor, output)
    	! serial version of simulated annealing    
        implicit none
        
        ! INPUT VARIABLES 
        ! gives as output the approximated global minimum
        real*8, intent(out) :: p(:)           
        ! gives as output the function value at the approximated minimum
        real*8, intent(out) :: fret        
        ! the left and right interval bounds on which to search on each dimension
        real*8, intent(in) :: minimum(:), maximum(:)       
        ! the decaying factor, by which the temperature decreases
        real*8, intent(in), optional :: factor        
        ! Output file
        character(*), intent(in), optional :: output
		! interface for the function
        interface
            function func(p)
                implicit none
                real*8, intent(in) :: p(:)
                real*8 :: func
            end function func
        end interface  
        
        ! PARAMETERS OF THE METHOD    
        ! number of steps for each temperature
        integer, parameter :: n_step = 100
        ! maximum number of iterations
        integer, parameter :: itermax = 100000  
        ! level of significance
        real*8, parameter :: sig = 1d-4    
        ! number of times significance level needs to be reached
        integer, parameter :: n_sig = 3  
        ! factor of temperature decay (can be overwritten)
        real*8 :: factor_use = 0.95d0   
        
        ! SOME ARRAYS
        real*8 :: x_opt(size(p,1)), f_opt, T(size(p,1)), unif(size(p,1)), x_new(size(p,1))
        real*8:: unif_1,prob,f_new
        
        ! OTHER VARIABLES      
        integer:: i_sig,is,n,it,time(8),unitno,ierr
        character(len=12) :: time_string
        real*8 :: x_par(size(p,1)), f_par


        ! PERFORM INITIAL CHECKS
        n = size(p, 1)
        call initial_checks()
        
        ! INITIALIZE STARTING VALUES
        i_sig = 0
        
        ! DETERMINE STARTING TEMPERATURE IN EACH DIRECTION
        ! (idea: when starting guess is the midpoint, then method should draw a 
        !        point outside of the interval (min, max) with only 1% probability
        
        ! get midpoint
        x_opt = (minimum + maximum)/2d0
        f_opt = func(x_opt)
        
        ! get initial temperature
        T = (minimum - x_opt)/tan(pi*(0.005d0-0.5d0))
            
        ! SIMULATE STARTING POINTS FOR THE FIRST RUN USING A UNIFORM DISTRIBUTION ON (MIN, MAX)
        call simulate_uniform(unif)
        x_par = minimum + unif*(maximum-minimum)
    	f_par = func(x_par)
        
        
        ! START THE ACTUAL SIMULATED ANNEALING PROCESS
        
        ! iteration counter
        do it = 1, itermax
                            
			! perform all the different iteration steps
			do is = 1, n_step
			
				! simulate a neighboring point using inverse cauchy distribution
				call RANDOM_NUMBER(unif)  
				x_new = x_par + T*tan(pi*(unif - 0.5d0))
				
				! restrict point to minimum and maximum
				x_new = min(x_new, maximum)
				x_new = max(x_new, minimum)
				
				! evaluate function value at neighboring point
				f_new = func(x_new)
				
				! if new function value is smaller than old function value then use new one
				if(f_new <= f_par)then
					x_par = x_new
					f_par = f_new                        
				else
					! otherwise calculate Eukledian distance between x_new and old optimum 
					!     (relative to temperature T, negative is needed, 
					!      otherwise probability would rise in distance)
					prob = -sqrt(sum(((x_new - x_par)/T)**2))
					
					! transform into a cauchy style probability
					prob = 0.5d0 + atan(prob)/pi
					
					! simulate one draw from a uniform distribution
					call RANDOM_NUMBER(unif_1) 
					
					! take worse value only with probability prob, otherwise keep
					if(unif_1 <= prob)then
						x_par = x_new
						f_par = f_new
					endif 
				endif
			enddo ! is               
                 
            ! check for convergence
            if(f_par <= f_opt .and. maxval(abs(x_par - x_opt)/max(abs(x_opt), 1d-10)) <= sig)then
                i_sig = i_sig + 1
            else
                i_sig = 0
            endif
            
            ! return if significance reached
            if(i_sig == n_sig)then
                p = x_par
                fret = f_par
                
                if(present(output))then
                    open(newunit=unitno,file=output,form='formatted',status='old',position='append',iostat=ierr)
					if (ierr/=0) then
						call myerror("Error in simulated_annealing (SA_minsearch): cannot open file")
					endif
					write(unitno,'(a)')'| '
					write(unitno,'(a,i10,a)')'| CONVERGED AFTER ',it,' ITERATIONS'
					write(unitno,'(a)')'| '
					do is = 1, n
						write(unitno,'(a,i3,a,f15.8)')'| x_opt(',is,') = ', p(is)
					enddo
					write(unitno,'(a)')'| '
					write(unitno,'(a,f15.8)')'| f_opt      = ',fret
					write(unitno,'(a)')'| '
					write(unitno,'(a)')'------------------------------------------------------------'
                    close(unitno)
                endif
                
                return
            endif
            
            ! write output line
            if(present(output))then
                open(newunit=unitno,file=output,form='formatted',status='old',position='append',iostat=ierr)
				if (ierr/=0) then
					call myerror("Error in simulated_annealing (SA_minsearch): cannot open file")
				endif
				call DATE_AND_TIME(values=time)
				call format_time(time, time_string)
				write(unitno,'(a,i10,2f16.8,4x,a)')'| ', it, f_opt, maxval(abs(x_par - x_opt)/max(abs(x_opt), 1d-10)), time_string
                close(unitno)
            endif
            
            ! set new approximated optimum
            x_opt = x_par
            f_opt = f_par
            
            ! adjust temperature and restart            
            T = factor_use*T
        enddo  ! it
        
        ! maximum iterations
        
        if(present(output))then
            open(newunit=unitno,file=output,form='formatted',status='old',position='append',iostat=ierr)
			if (ierr/=0) then
				call myerror("Error in simulated_annealing (SA_minsearch): cannot open file")
			endif
			write(unitno,'(a)')'| '
			write(unitno,'(a)')'| SIMULATED ANNEALING: MAXIMUM ITERATIONS REACHED'
			write(unitno,'(a)')'| '
			do is = 1, n
				write(unitno,'(a,i3,a,f15.8)')'| x_opt(',is,') = ', p(is)
			enddo
			write(unitno,'(a)')'| '
			write(unitno,'(a)')'| f_opt = ',fret
			write(unitno,'(a)')'| '
			write(unitno,'(a)')'------------------------------------------------------------'
            close(unitno)
       
        endif
                
        p = x_opt
        fret = f_opt
        
    contains
        
        
        
        ! perform all initial checks
        subroutine initial_checks()
        
            ! check dimension of the problem
            if(size(minimum, 1) /= n .or. size(maximum, 1) /= n)then
                write(*, '(a)')"ERROR IN SIMULATED ANNEALING: INPUT ARRAYS WITH WRONG SIZE"
                p = 1d100
                fret = 1d100
                return
            endif
        
            ! check that minimum and maximum are well ordered
            if(any(minimum > maximum))then
                write(*, '(a)')"ERROR IN SIMULATED ANNEALING: SOME MINIMUM GREATER THAN MAXIMUM"
                p = 1d100
                fret = 1d100
                return
            endif
        
            ! check for presence of factor variable
            if(present(factor))then
                if(factor > 0d0 .and. factor < 1d0)then
                    factor_use = factor
                endif
            endif
        
  
            ! initialize the random seed
            call init_random_seed()
            
            if (present(output)) then
            	open(newunit=unitno,file=output,form='formatted',status='old',position='append',iostat=ierr)
				if (ierr/=0) then
					call myerror("Error in simulated_annealing (init_checks): cannot open file")
				endif
				write(unitno,'(a)')'------------------------------------------------------------'
				write(unitno,'(a)')'| SIMULATED ANNEALING'
				write(unitno,'(a)')'| '
				write(unitno,'(a,i8)')'| maximum iterations:  ', itermax
				write(unitno,'(a,i8)')'| steps per iteration: ', n_step
				write(unitno,'(a,f8.5)')'| factor:              ', factor_use
				write(unitno,'(a)')'| '
				write(unitno,'(a)')'| RUNNING...'
				write(unitno,'(a)')'| '
				write(unitno,'(a)')'|  iteration           f_new            diff            time'
                close(unitno)
            endif
        
        end subroutine
    end subroutine sa_minsearch 
!=====================================================================
    
    subroutine SA_minsearch_omp(p, fret, minimum, maximum, func, factor, n_par, output)
    	! simulated annealing using openMP
        use omp_lib
    
        implicit none
        
        ! INPUT VARIABLES
    
        ! gives as output the approximated global minimum
        real*8, intent(out) :: p(:)    
        
        ! gives as output the function value at the approximated minimum
        real*8, intent(out) :: fret
        
        ! the left and right interval bounds on which to search on each dimension
        real*8, intent(in) :: minimum(:), maximum(:)
        
        ! the decaying factor, by which the temperature decreases
        real*8, intent(in), optional :: factor
        
        ! the number of parallel processors to use
        integer, intent(in), optional :: n_par
        
        ! should output be written to the console
        character(*), intent(in), optional :: output
        
        
        ! PARAMETERS OF THE METHOD
        
        ! number of steps for each temperature
        integer, parameter :: n_step = 100
        
        ! maximum number of iterations
        integer, parameter :: itermax = 100000
        
        ! level of significance
        real*8, parameter :: sig = 1d-4
        
        ! number of times significance level needs to be reached
        integer, parameter :: n_sig = 3
        
        ! factor of temperature decay (can be overwritten)
        real*8 :: factor_use = 0.95d0
        
        
        ! SOME ARRAYS
        real*8 :: x_opt(size(p,1)), f_opt, T(size(p,1)), unif(size(p,1)), x_new(size(p,1))
        real*8 :: unif_1, prob, f_new, f_best
        real*8, allocatable :: x_par(:, :), f_par(:)

        
        ! OTHER VARIABLES
        
        integer :: i_sig, i_par, n_par_use, is, n, it, i_best, time(8),unitno,ierr
        character(len=12) :: time_string
        
        
        ! interface for the function
        interface
            function func(p)
                implicit none
                real*8, intent(in) :: p(:)
                real*8 :: func
            end function func
        end interface
        
        
        ! PERFORM INITIAL CHECKS
        call initial_checks()
        
        ! ALLOCATE ARRAYS
        if(allocated(x_par))deallocate(x_par)        
        if(allocated(f_par))deallocate(f_par)
        allocate(f_par(n_par_use),x_par(n, n_par_use))
        
        ! INITIALIZE STARTING VALUES
        i_sig = 0
        
        
        ! DETERMINE STARTING TEMPERATURE IN EACH DIRECTION
        ! (idea: when starting guess is the midpoint, then method should draw a 
        !        point outside of the interval (min, max) with only 1% probability
        
        ! get midpoint
        x_opt(:) = (minimum(:) + maximum(:))/2d0
        f_opt = func(x_opt)
        
        ! get initial temperature
        T(:) = (minimum(:) - x_opt(:))/tan(pi*(0.005d0-0.5d0))
        
        
        ! SIMULATE STARTING POINTS FOR THE FIRST RUN USING A UNIFORM DISTRIBUTION ON (MIN, MAX)
        do i_par = 1, n_par_use
            call simulate_uniform(unif)
            x_par(:, i_par) = minimum + unif*(maximum-minimum)
            f_par(i_par) = func(x_par(:, i_par))
        enddo
        
        ! START THE ACTUAL SIMULATED ANNEALING PROCESS
        
        ! iteration counter
        do it = 1, itermax
            
            !$omp parallel  do private(i_par, is, unif, x_new, f_new, unif_1, prob)
            do i_par = 1, n_par_use
                
                ! perform all the different iteration steps
                do is = 1, n_step
                
                    ! simulate a neighboring point using inverse cauchy distribution
                    call RANDOM_NUMBER(unif)  
                    x_new = x_par(:, i_par) + T(:)*tan(pi*(unif(:) - 0.5d0))
                    
                    ! restrict point to minimum and maximum
                    x_new = min(x_new, maximum)
                    x_new = max(x_new, minimum)
                    
                    ! evaluate function value at neighboring point
                    f_new = func(x_new)
                    
                    ! if new function value is smaller than old function value then use new one
                    if(f_new <= f_par(i_par))then
                        x_par(:, i_par) = x_new
                        f_par(i_par) = f_new                        
                    else
                        
                        ! otherwise calculate Eukledian distance between x_new and old optimum 
                        !     (relative to temperature T, negative is needed, 
                        !      otherwise probability would rise in distance)
                        prob = -sqrt(sum(((x_new(:) - x_par(:, i_par))/T)**2))
                        
                        ! transform into a cauchy style probability
                        prob = 0.5d0 + atan(prob)/pi
                        
                        ! simulate one draw from a uniform distribution
                        call RANDOM_NUMBER(unif_1) 
                        
                        ! take worse value only with probability prob, otherwise keep
                        if(unif_1 <= prob)then
                            x_par(:, i_par) = x_new
                            f_par(i_par) = f_new
                        endif 
                    endif
                enddo                
            
            enddo
            !$omp end parallel do
            
            ! now search for the best function value
            i_best = minloc(f_par, 1)
            f_best = f_par(i_best)
            
            ! check for convergence
            if(f_best <= f_opt .and. maxval(abs(x_par(:, i_best) - x_opt)/max(abs(x_opt), 1d-10)) <= sig)then
                i_sig = i_sig + 1
            else
                i_sig = 0
            endif
            
            ! return if significance reached
            if(i_sig == n_sig)then
                p = x_par(:, i_best)
                fret = f_best
                
                if(present(output))then
                    open(newunit=unitno,file=output,form='formatted',status='old',position='append',iostat=ierr)
					if (ierr/=0) then
						call myerror("Error in simulated_annealing (SA_minsearch): cannot open file")
					endif
					write(unitno,'(a)')'| '
					write(unitno,'(a,i10,a)')'| CONVERGED AFTER ',it,' ITERATIONS'
					write(unitno,'(a)')'| '
					do is = 1, n
						write(unitno,'(a,i3,a,f15.8)')'| x_opt(',is,') = ', p(is)
					enddo
					write(unitno,'(a)')'| '
					write(unitno,'(a,f15.8)')'| f_opt      = ',fret
					write(unitno,'(a)')'| '
					write(unitno,'(a)')'------------------------------------------------------------'
                    close(unitno)
                endif
                
                return
            endif
            
            ! write output line
            if(present(output))then
                open(newunit=unitno,file=output,form='formatted',status='old',position='append',iostat=ierr)
				if (ierr/=0) then
					call myerror("Error in simulated_annealing (SA_minsearch): cannot open file")
				endif
				call DATE_AND_TIME(values=time)
				call format_time(time, time_string)
				write(unitno,'(a,i10,2f16.8,4x,a)')'| ', it, f_opt, maxval(abs(x_par(:, i_best) - x_opt)/max(abs(x_opt), 1d-10)), time_string
                close(unitno)
            endif
            
            ! set new approximated optimum
            x_opt = x_par(:, i_best)
            f_opt = func(x_opt)
            
            
            ! simulate a set of points around this optimum using old T
            x_par(:, 1) = x_opt
            do i_par = 2, n_par_use
                call RANDOM_NUMBER(unif)  
                x_par(:, i_par) = x_opt + T(:)*tan(pi*(unif(:) - 0.5d0))
                f_par(i_par) = func(x_par(:, i_par))
            enddo
            
            ! adjust temperature and restart            
            T = factor_use*T
        enddo 
        
        ! maximum iterations
        
        if(present(output))then
            open(newunit=unitno,file=output,form='formatted',status='old',position='append',iostat=ierr)
			if (ierr/=0) then
				call myerror("Error in simulated_annealing (SA_minsearch): cannot open file")
			endif
			write(unitno,'(a)')'| '
			write(unitno,'(a)')'| SIMULATED ANNEALING: MAXIMUM ITERATIONS REACHED'
			write(unitno,'(a)')'| '
			do is = 1, n
				write(unitno,'(a,i3,a,f15.8)')'| x_opt(',is,') = ', p(is)
			enddo
			write(unitno,'(a)')'| '
			write(unitno,'(a)')'| f_opt = ',fret
			write(unitno,'(a)')'| '
			write(unitno,'(a)')'------------------------------------------------------------'
            close(unitno)
       
        endif
                
        p = x_opt
        fret = f_opt
        
    contains
        

        
        
        ! perform all initial checks
        subroutine initial_checks()
        
            ! check dimension of the problem
            n = size(p, 1)
            if(size(minimum, 1) /= n .or. size(maximum, 1) /= n)then
                write(*, '(a)')"ERROR IN SIMULATED ANNEALING: INPUT ARRAYS WITH WRONG SIZE"
                p = 1d100
                fret = 1d100
                return
            endif
        
            ! check that minimum and maximum are well ordered
            if(any(minimum > maximum))then
                write(*, '(a)')"ERROR IN SIMULATED ANNEALING: SOME MINIMUM GREATER THAN MAXIMUM"
                p = 1d100
                fret = 1d100
                return
            endif
        
            ! check for presence of factor variable
            if(present(factor))then
                if(factor > 0d0 .and. factor < 1d0)then
                    factor_use = factor
                endif
            endif
        
            ! check for presence of parallel variable
            n_par_use = omp_get_max_threads()-1
            if(present(n_par))then
                if(n_par <= n_par_use .and. n_par > 0)then
                    n_par_use = n_par
                endif
            endif
            
            ! set the number of threads to use
            call omp_set_num_threads(n_par_use)
            
            ! initialize the random seed
            call init_random_seed()
            
            if (present(output)) then
            	open(newunit=unitno,file=output,form='formatted',status='old',position='append',iostat=ierr)
				if (ierr/=0) then
					call myerror("Error in simulated_annealing (init_checks): cannot open file")
				endif
				write(unitno,'(a)')'------------------------------------------------------------'
				write(unitno,'(a)')'| SIMULATED ANNEALING'
				write(unitno,'(a)')'| '
				write(unitno,'(a,i8)')'| # threads:           ', n_par_use
				write(unitno,'(a,i8)')'| maximum iterations:  ', itermax
				write(unitno,'(a,i8)')'| steps per iteration: ', n_step
				write(unitno,'(a,f8.5)')'| factor:              ', factor_use
				write(unitno,'(a)')'| '
				write(unitno,'(a)')'| RUNNING...'
				write(unitno,'(a)')'| '
				write(unitno,'(a)')'|  iteration           f_new            diff            time'
                close(unitno)
            endif
        
        end subroutine
    end subroutine sa_minsearch_omp ! sa_minsearch_omp
   
 
	
	! to write time in a nice format
	subroutine format_time(time, time_string)
	
		implicit none
		integer, intent(in) :: time(8)
		character(LEN=12), intent(out) :: time_string
		integer :: ic
		
		write(time_string,'(a)')' '
		
		do ic = 5, 7        
			if(time(ic) < 10)then
				write(time_string, '(a,a,i1,a)')trim(time_string), '0', time(ic) ,':'
			else
				write(time_string, '(a,i2,a)')trim(time_string), time(ic), ':'
			endif
		enddo
		
		if(time(8) < 10)then
			write(time_string, '(a,a,i1)')trim(time_string), '00', time(ic)
		elseif(time(8) < 100)then
			write(time_string, '(a,a,i2)')trim(time_string), '0', time(ic)
		else
			write(time_string, '(a,i3)')trim(time_string), time(ic)
		endif
	
	end subroutine
    
    

end module
