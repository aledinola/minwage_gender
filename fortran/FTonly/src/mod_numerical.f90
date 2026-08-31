module mod_numerical
	!=====================================================================!
	! Written by Alessandro Di Nola 
	! Main sources:
	! https://github.com/drarnau/Replication_Krusell-et-al-2017/blob/master/Utils.f90
	! https://sites.google.com/site/greygordon/code
	! Numerical recipes in F90
	! http://karibzhanov.com/, TA of F.Guvenen
	!=====================================================================!
	! USE
	implicit none
	
	!private !Variables and internal proc are not visible outside of this module
	
	!public :: grid, interp1, utilfun
	
	!Declaring parameters:	
	
	! swap real scalar, integer scalar, real vector
	interface swap
    	module procedure swap_rs
    	module procedure swap_is
    	module procedure swap_v
    end interface
    
    ! Same as Matlab cumprod
    interface cumprod
        module procedure cumprod_r, cumprod_i
    end interface cumprod
   
    ! Numerical derivative 
    ! For univariate functions and also n-variable functions
    ! In the latter case, the derivative is the gradient
    interface numgrad_cent
        module procedure numgrad_cent_scal
        module procedure numgrad_cent_vec
    end interface
    	
	!Module procedures
CONTAINS
	
	subroutine myerror(string)

	implicit none
	character(len=*), intent(in) :: string
	write(*,*) "myerror: ", string
	write(*,*) "Program will terminate.."
	pause
	stop

    end subroutine myerror
    !=====================================================================!
    ! swap real vector
    SUBROUTINE swap_v(a,b)
	REAL(8), DIMENSION(:), INTENT(INOUT) :: a,b
	REAL(8), DIMENSION(SIZE(a)) :: dum
	dum=a
	a=b
	b=dum
	END SUBROUTINE swap_v
	!swap real scalar
	SUBROUTINE swap_rs(a,b)
    REAL(8), INTENT(INOUT) :: a,b
    REAL(8) :: dum
    dum=a
    a=b
    b=dum
	END SUBROUTINE swap_rs
	!swap integer scalar
	SUBROUTINE swap_is(a,b)
    INTEGER, INTENT(INOUT) :: a,b
    INTEGER :: dum
    dum=a
    a=b
    b=dum
	END SUBROUTINE swap_is
    !=====================================================================!
    
    elemental function isnan(x) result(tf)
    ! Purpose: this replicates the matlab function isnan
    implicit none
    real(8), intent(in) :: x
    logical :: tf
    
    tf = (x/=x)
    
    end function isnan
    !=====================================================================!
    
    function linspace(my_start, my_stop, n)
    ! Purpose: this replicates the matlab function linspace
    implicit none
    integer :: n, i
    real(8) :: my_start, my_stop, step
    real(8), dimension(n) :: grid, linspace

    if (n.eq.1) then
      grid(n) = (my_start+my_stop)/2.d0
    elseif (n.ge.2) then
      grid(1) = my_start
      if (n.gt.2) then
        step = (my_stop-my_start)/(real(n-1))
        do i = 2, n-1
          grid(i) = grid(i-1) + step
        end do
      end if
      grid(n) = my_stop
    endif
    linspace = grid
    end function linspace
    !=======================================================================!
    
    FUNCTION cumsum(arr) RESULT(ans)
    ! Purpose: it replicates matlab cumsum function
    real(8), INTENT(IN) :: arr(:)
    ! Note that a Fortran function can return a value with an ALLOCATABLE
    ! attribute. The function result must be declared as ALLOCATABLE
    ! in the calling unit but will not be allocated on entry to the function.
    real(8), allocatable :: ans(:)
    INTEGER :: n,j

    n=size(arr)

    allocate(ans(n))
    ans(1)=arr(1)

    do j=2,n
	    ans(j)=ans(j-1)+arr(j)
    end do
	
    END FUNCTION cumsum
    !=======================================================================!
    
    !=====================================================================!
    
    function my_ss(tmatrix,gp)
    ! Purpose: computes the invariant distribution of gp*gp Markov chain 
    !          <tmatrix>. Tolerance 1d-10 is hard-coded.
    implicit none
    integer :: gp
    integer :: row, col, iter
    real(8) :: aux_sum
    real(8), dimension(gp) :: my_ss
    real(8), dimension(gp) :: dist, ndist
    real(8), dimension(gp,gp) :: tmatrix

    ! Initialise distribution
    dist = 1.d0/real(gp)

    do iter = 1, 10000
      ndist = 0.d0
      do col = 1, gp
        do row = 1, gp
          ndist(col) = ndist(col) + (dist(row)*tmatrix(row,col))
        end do
      end do
      aux_sum = sum(abs(ndist-dist))
      dist = ndist
      if (aux_sum .lt. 1.0d-10) then
        exit
      end if
    end do

    my_ss = dist
    end function my_ss
    !=====================================================================!
  
    SUBROUTINE grid(x,xmin,xmax,s)
    ! Purpose: Generate grid x on [xmin,xmax] using spacing parameter s set as follows:
    ! s=1		linear spacing
    ! s>1		left skewed grid spacing with power s
    ! 0<s<1		right skewed grid spacing with power s
    ! s<0		geometric spacing with distances changing by a factor -s^(1/(n-1)), (>1 grow, <1 shrink)
    ! s=-1		logarithmic spacing with distances changing by a factor (xmax-xmin+1)^(1/(n-1))
    ! s=0		logarithmic spacing with distances changing by a factor (xmax/xmin)^(1/(n-1)), only if xmax,xmin>0
	IMPLICIT NONE
	REAL(8), DIMENSION(:), INTENT(OUT) :: x
	REAL(8), INTENT(IN) :: xmin,xmax,s
	REAL(8) :: c ! growth rate of grid subintervals for logarithmic spacing
	INTEGER :: n,i
	n=size(x)
	FORALL(i=1:n) x(i)=(i-1)/real(n-1,8)
	IF (s>0.0d0) THEN
		x=x**s*(xmax-xmin)+xmin
		!	PRINT '(a,i8,a,f7.3,a,f7.3,a)', 'Using ',n,' equally spaced grid points over domain [',xmin,',',xmax,']'
		!	PRINT '(a,i8,a,f7.3,a,f7.3,a,f7.3,a)', 'Using ',n,' skewed spaced grid points with power ',s,' over domain [',xmin,',',xmax,']'
	ELSE
		IF (s==-1.0d0) THEN
			c=xmax-xmin+1
		ELSE
			c=-s
		END IF
		PRINT '(a,i8,a,f6.3,a,f6.3,a,f6.3,a)', 'Using ',n,' logarithmically spaced grid points with growth rate ',c,' over domain [',xmin,',',xmax,']'
		x=((xmax-xmin)/(c-1))*(c**x)-((xmax-c*xmin)/(c-1))
	END IF
    END SUBROUTINE grid
    !=====================================================================!

    FUNCTION linint(x,y,xi)
    ! Purpose: linear interpolation of function y on grid x at interpolation point xi
	IMPLICIT NONE
	REAL(8), DIMENSION(:), INTENT(IN) :: x,y
	REAL(8), INTENT(IN) :: xi
	REAL(8) :: linint
	REAL(8) :: a,b,d
	INTEGER :: n,i
	
    n=size(x)
    IF (size(y)/=n) THEN
		PRINT *, 'linint: x and y must be of the same size'
		STOP 'program terminated by linint'
        PAUSE
    END IF
	
    i=max(min(locate(x,xi),n-1),1)
	d=x(i+1)-x(i)
	a=(x(i+1)-xi)/d
	b=(xi-x(i))/d
	linint=a*y(i)+b*y(i+1)
    
    END FUNCTION linint
    !=======================================================================!
   
    PURE FUNCTION locate(xx,x)
	IMPLICIT NONE
	REAL(8), DIMENSION(:), INTENT(IN) :: xx
	REAL(8), INTENT(IN) :: x
	INTEGER :: locate
	INTEGER :: n,il,im,iu
	n=size(xx)
	il=0
	iu=n+1
	do
		if (iu-il <= 1) exit
		im=(iu+il)/2
		if (x >= xx(im)) then
			il=im
		else
			iu=im
		end if
	end do
	if (x == xx(1)) then
		locate=1
	else if (x == xx(n)) then
		locate=n-1
	else
		locate=il
	end if
    END FUNCTION locate
    !=======================================================================!
    
    FUNCTION numder(x,fx,xi) RESULT(df)
    ! Computes numerical derivative of a function tabulated at points {x,fx}
    ! using linear interpolation
	IMPLICIT NONE
	! Input variables:
	real(8), intent(in) :: x(:), fx(:)
	real(8), intent(in) :: xi !point where derivative is desired
	real(8) :: df
	! Local variables:
	integer :: i,n
	real(8) :: step
	
	n=size(x)
    IF (size(fx)/=n) THEN
		PRINT *, 'linint: x and fx must be of the same size'
		STOP 'program terminated by linint'
        PAUSE
    END IF

    ! - Step 1: locate bracketing interval
    i    = max(min(locate(x,xi),n-1),1)
	step = x(i+1)-x(i)
	
    ! - Step 2: Compute slope of linear interpolant
    df = (fx(i+1)-fx(i))/step

	END FUNCTION numder
    !=======================================================================!
    
    FUNCTION jacob(myfun,x,op) RESULT(df)
    ! PURPOSE
    ! One-sided finite difference derivative
    ! See also function numder
    ! Input:  myfun - function implementing f(x)
    !         x    - point where to calculate f'(x)
    !		  op   - 1 = right derivative, 2 = two-sided derivative, 3 = left derivative
	IMPLICIT NONE
	! Input variables:
	real(8), intent(in) :: x
	integer,intent(in) :: op 
    INTERFACE
		FUNCTION myfun(x) RESULT(y)
		IMPLICIT NONE
		REAL(8), INTENT(IN) :: x
		REAL(8) :: y
		END FUNCTION myfun
    END INTERFACE
	real(8) :: df
	! Local variables:
	real(8) :: small, h, y, y1
	
    small = epsilon(1d0)
	h  = sqrt(small)*max(abs(x),1d0)
	if (op==1) then		
    	y  = myfun(x)
    	y1 = myfun(x+h)
    	df = (y1-y)/h
	elseif (op == 2) then
		y  = myfun(x-h)
    	y1 = myfun(x+h)
    	df = (y1-y)/(2.0d0*h)
	elseif (op == 3) then
		y  = myfun(x-h)
    	y1 = myfun(x)
    	df = (y1-y)/h
	endif
	
    END FUNCTION jacob
    !=======================================================================!
    
    function numgrad_cent_vec(x,func) 
    ! PURPOSE
    ! Calculate numerical gradient (central) of func at x.
    ! func: R^n-->R
    ! For scalar functions with only one argument, see numgrad_cent_scal
    implicit none
    real(8), dimension(:), intent(in) :: x
    real(8), dimension(size(x)) :: numgrad_cent_vec
    INTERFACE
       FUNCTION func(xx) !function evaluation
         IMPLICIT NONE
         REAL(8), DIMENSION(:), INTENT(IN) :: xx
         REAL(8) :: func
       END FUNCTION func
    END INTERFACE
    real(8), parameter :: h=.5e-7 ! Arbitrary choice
    real(8), dimension(size(x)) :: vec
    integer :: i
    do i=1,size(x)
       vec=0
       vec(i)=1.0d0
       numgrad_cent_vec(i)=(func(x+vec*h)-func(x-vec*h))/(2.0d0*h)
    end do
    end function numgrad_cent_vec
    !=======================================================================!
    
    function numgrad_cent_scal(x,func) 
    ! PURPOSE
    ! Calculate numerical derivative (central) of func at x.
    ! func: R-->R
    ! For functions of n arguments, see numgrad_cent_vec
    implicit none
    real(8), intent(in) :: x
    real(8)             :: numgrad_cent_scal
    INTERFACE
       FUNCTION func(xx) !function evaluation
         IMPLICIT NONE
         REAL(8), INTENT(IN) :: xx
         REAL(8) :: func
       END FUNCTION func
    END INTERFACE
    real(8), parameter :: h=.5e-7 ! Arbitrary choice
    real(8) :: vec
    
    vec = 1.0d0
    numgrad_cent_scal = (func(x+vec*h)-func(x-vec*h))/(2.0d0*h)
    
    end function numgrad_cent_scal
    !=======================================================================!
    
    !===============================================================================!
    
!===============================================================================!
    
    
    !=======================================================================!
    
    FUNCTION rtbis(func,x1,x2,xacc,MAXIT)
     !-------------------------------------------------------------!
     ! DESCRIPTION:
     ! Bisection to find root x of a nonlinear function func(x)=0
     ! INPUTS:
     ! func: function to pass to the rootfinder
     ! [x1 x2]: bracketing interval
     ! xacc: accuracy criterion
     ! MAXIT: maximum number of iterations
     ! OUTPUT
     ! rtbis: root of the function in [x1 x2]
     ! SOURCE:
     ! http://numerical.recipes/
     !-------------------------------------------------------------!
 
	IMPLICIT NONE
	REAL(8), INTENT(IN) :: x1,x2,xacc
    INTEGER, INTENT(IN) :: MAXIT ! Maximum no. of iterations
	REAL(8) :: rtbis
	INTERFACE
		FUNCTION func(x)
		IMPLICIT NONE
		REAL(8), INTENT(IN) :: x
		REAL(8) :: func
		END FUNCTION func
	END INTERFACE
	INTEGER :: j
	REAL(8) :: dx,f,fmid,xmid
	fmid = func(x2)
	f    = func(x1)
	if (f*fmid >= 0.0d0) then 
	    write(*,*) "Error: rtbis: root must be bracketed"
		pause
		stop
	endif
	if (f < 0.0d0) then
		rtbis=x1
		dx=x2-x1
	else
		rtbis=x2
		dx=x1-x2
	end if
	do j=1,MAXIT
		dx=dx*0.5d0
		xmid=rtbis+dx
		fmid=func(xmid)
		if (fmid <= 0.0d0) rtbis=xmid
		if (abs(dx) < xacc .or. fmid == 0.0d0) RETURN
	end do
	write(*,*) "Warning: rtbis: too many bisections"
    END FUNCTION rtbis
    !=======================================================================!
    
    FUNCTION zbrent(func,x1,x2,tol,ITMAX)
     !-------------------------------------------------------------!
     ! DESCRIPTION:
     ! Brent's method to find root x of a nonlinear function func(x)=0
     ! It is the same as fzero in Matlab. Brent's method is generally
     ! better than bisection.
     ! INPUTS:
     ! func: function to pass to the rootfinder
     ! [x1 x2]: bracketing interval
     ! tol: accuracy criterion
     ! OUTPUT
     ! rtbis: root of the function in [x1 x2]
     ! SOURCE:
     ! http://numerical.recipes/
     !-------------------------------------------------------------!
	    IMPLICIT NONE
	    REAL(8), INTENT(IN) :: x1,x2,tol
        INTEGER, INTENT(IN) :: ITMAX
	    REAL(8) :: zbrent
	    INTERFACE
		    FUNCTION func(x)
		    IMPLICIT NONE
		    REAL(8), INTENT(IN) :: x
		    REAL(8) :: func
		    END FUNCTION func
	    END INTERFACE
	    REAL(8), PARAMETER :: EPS=epsilon(x1)
	    INTEGER :: iter
	    REAL(8) :: a,b,c,d,e,fa,fb,fc,p,q,r,s,tol1,xm
	    a=x1
	    b=x2
	    fa=func(a)
	    fb=func(b)
	    if ((fa > 0.0 .and. fb > 0.0) .or. (fa < 0.0 .and. fb < 0.0)) &
		    STOP 'root must be bracketed for zbrent'
	    c=b
	    fc=fb
	    do iter=1,ITMAX
		    if ((fb > 0.0 .and. fc > 0.0) .or. (fb < 0.0 .and. fc < 0.0)) then
			    c=a
			    fc=fa
			    d=b-a
			    e=d
		    end if
		    if (abs(fc) < abs(fb)) then
			    a=b
			    b=c
			    c=a
			    fa=fb
			    fb=fc
			    fc=fa
		    end if
		    tol1=2.0d0*EPS*abs(b)+0.5d0*tol
		    xm=0.5d0*(c-b)
		    if (abs(xm) <= tol1 .or. fb == 0.0) then
			    zbrent=b
			    RETURN
		    end if
		    if (abs(e) >= tol1 .and. abs(fa) > abs(fb)) then
			    s=fb/fa
			    if (a == c) then
				    p=2.0d0*xm*s
				    q=1.0d0-s
			    else
				    q=fa/fc
				    r=fb/fc
				    p=s*(2.0d0*xm*q*(q-r)-(b-a)*(r-1.0d0))
				    q=(q-1.0d0)*(r-1.0d0)*(s-1.0d0)
			    end if
			    if (p > 0.0) q=-q
			    p=abs(p)
			    if (2.0d0*p  <  min(3.0d0*xm*q-abs(tol1*q),abs(e*q))) then
				    e=d
				    d=p/q
			    else
				    d=xm
				    e=d
			    end if
		    else
			    d=xm
			    e=d
		    end if
		    a=b
		    fa=fb
		    b=b+merge(d,sign(tol1,xm), abs(d) > tol1 )
		    fb=func(b)
	    end do
	    STOP 'zbrent: exceeded maximum iterations'
	    zbrent=b
    END FUNCTION zbrent
    !=======================================================================!
    
    subroutine discretize_lognormal_AF(n,grid,mu,sig, prob)
    !-------------------- LEGEND ----------------------------------------------------!
    ! This function discretizes a lognormal distribution using a very simple method.
    
    !INPUTS:
    ! n: num of points for the grid
    ! grid: grid (length must be equal to n)
    ! mu,sigma: mean and st.dev. of lognormal distribution

    !OUTPUTS:
    ! prob: vector with probability mass function (PMF)
    !--------------------------------------------------------------------------------!
    ! We use simple discretization
    
    use toolbox, only: normalPDF
    implicit none
    
    !Declare inputs.
    integer, intent(in) :: n
    real(8), intent(in) :: grid(:)
    real(8), intent(in) :: mu
    real(8), intent(in) :: sig
     
    !Deckare output
    real(8), intent(out) :: prob(:)
  
  	!Declare locals 
  	integer :: ii
  
    !Check if inputs have the correct dimension
    if (size(grid)/=n .or. size(prob)/=n) then
    	call myerror("Error in discretize_lognormal_AF: inputs dimension mismatch")
    endif
    
    !If the random variable X is log-normally distributed, 
    !then Y = ln(X) has a normal distribution
    do ii = 1,n
    	prob(ii) = normalPDF(log(grid(ii)),mu,sig)
    enddo
    
    prob = prob/sum(prob)
  
    end subroutine discretize_lognormal_AF
    !===============================================================================!
    
    subroutine discretize_lognormal(n,grid,mu,sig, prob)
    
    !-------------------- LEGEND ----------------------------------------------------!
    !This function discretizes a (truncated) lognormal distribution
    !The c.d.f. of the LN is log_normalCDF(x,mu,sigma), where sigma is the variance.
    
    !INPUTS:
    ! n: num of points for the grid
    ! grid: grid (length must be equal to n)
    ! supp: support of the truncated lognormal distribution, dim: (1,2)
    ! mu,sigma: mean and st.dev. of lognormal distribution

    !OUTPUTS:
    ! prob: vector with probability mass function
    !--------------------------------------------------------------------------------!
    use toolbox, only: log_normalCDF
    implicit none
    
    !Declare inputs:
    integer, intent(in) :: n !number of grid points
    real(8), intent(in) :: grid(:)
    real(8), intent(in) :: mu
    real(8), intent(in) :: sig
    
    !Declare output(s):
    real(8), intent(out) :: prob(:)
    
    !Declare locals:
    integer :: ii
    real(8) :: first_point, last_point, lb, ub, sum_prob
    
    
    !Check if inputs have the correct dimension
    if (size(grid)/=n .or. size(prob)/=n) then
    	write(*,*) "Error in discretize_lognormal: inputs dimension mismatch"
    	stop
    endif
    
    !First gridpoint
    first_point   = (grid(1)+grid(2))/2.0d0
    prob(1)       = log_normalCDF(first_point,mu,sig)
    
    !Last gridpoint
    last_point    = (grid(n-1)+grid(n))/2.0d0
    prob(n)       = 1 - log_normalCDF(last_point,mu,sig)
    
    !Interior points
    do ii = 2,n-1
        ub = (grid(ii+1)+grid(ii))/2.0d0
        lb = (grid(ii)+grid(ii-1))/2.0d0
        prob(ii) = log_normalCDF(ub,mu,sig) - log_normalCDF(lb,mu,sig)
    enddo
    
    sum_prob = sum(prob)
    if (abs(sum_prob-1.0d0)>1d-8) then
        write(*,'(a)') "Warning: discretize_lognormal:"
        write(*,'(a)') " prob does not sum to 1!"
    endif
    
    
    end subroutine discretize_lognormal
    
    
    !===============================================================================!
    subroutine discretize_norm(n,mu,sig,mx, grid,prob)
    
    !This function creates a linear grid of n points for a variable that is distributed
    !according to an normal distribution with mean mu and std deviation sig.  
    !It also gives the probability prob that each gridpoint is chosen.
    use toolbox, only: normalCDF
    implicit none
    
    !Declare inputs
    integer, intent(in) :: n !number of grid points
    real(8), intent(in) :: mu !mean
    real(8), intent(in) :: sig !standard deviation
    real(8), intent(in) :: mx !spacing [mean - mx*stdev, mean + mx*stdev]
    
    !Declare outputs
    real(8), intent(out) :: grid(:)
    real(8), intent(out) :: prob(:)
    
    !Declare locals
    integer :: ii
    real(8) :: where_to_call, ub, lb, sigma 
    
    !Check if outputs have the correct dimension
    if (size(grid)/=n .or. size(prob)/=n) then
    	write(*,*) "Error in discretize_norm: output dimension mismatch"
    	stop
    endif
    
    
    !Compute variance
    sigma = sig**2.0d0 
    
    
    if (n==1) then
    
        grid = mu    !just set the gridpoint as the mean in the deterministic case
        prob = 1.0d0
        
    else
      
        !Initialize the grid:      
        grid = linspace(mu - mx*sig,mu + mx*sig,n)
        

        !first grid point:
        where_to_call = (grid(1)+grid(2))/2.0d0
        prob(1)       = normalCDF(where_to_call,mu,sigma)
    
        !last grid point:
        where_to_call = (grid(n-1)+grid(n))/2.0d0
        prob(n)       = 1 - normalCDF(where_to_call,mu,sigma)
    
        !interior points:
        do ii=2,n-1
            ub = (grid(ii+1)+grid(ii))/2.0d0
            lb = (grid(ii)+grid(ii-1))/2.0d0
            prob(ii) = normalCDF(ub,mu,sigma)-normalCDF(lb,mu,sigma)
        enddo

    endif 
    
    end subroutine discretize_norm
    !===============================================================================!
        subroutine discretize_beta(a,b,grid,prob)
    
    ! Purpose: This function discretizes a Beta distribution with parameters 
    !   a and b with [0,1] support. It takes the grid as input and returns
    !   the vector "prob" (which has the same no. of elements as "grid").
    ! Use betaPDF to preserve the shape of the probability density.
    
    
    use toolbox, only: betaPDF
    implicit none
    
    !Declare inputs
    real(8), intent(in) :: a        ! Parameter for beta
    real(8), intent(in) :: b        ! Parameter for beta
    real(8), intent(in) :: grid(:) ! Grid
     
    !Declare outputs
    real(8), intent(out) :: prob(:)
    
    !Declare locals
    integer :: n, ii
    
    n = size(grid)
    
    !Check if outputs have the correct dimension
    if (size(prob)/=n) then
    	call myerror("Error in discretize_beta: output dimension mismatch")
    endif
    
    if (grid(1)<0.001d0 .or. grid(n)>0.999d0) then
    	call myerror("discretize_beta: grid is not in (0,1)")
    endif
    	
    do ii = 1,n
    	prob(ii) = betaPDF(grid(ii),a,b)
    enddo
    ! Rescale so that the sum is 1
    prob = prob/sum(prob)
    
    end subroutine discretize_beta
        
    !===============================================================================!
    !===============================================================================!
    
    subroutine discretize_grid_beta(a,b,grid,prob)
    
        ! Purpose: This subroutine discretizes a Beta distribution with parameters 
        !   a and b with [0,1] support. It returns "grid" and "prob" as outputs.
        !   prob is 1/n, where n=size(prob). Therefore, each grid point is the 1/n
        ! percentile.
        use mod_stats, only: pinvbeta_r
        implicit none
    
        !Declare inputs
        real(8), intent(in) :: a        ! Parameter for beta
        real(8), intent(in) :: b        ! Parameter for beta
        
        !Declare outputs
        real(8), intent(out) :: grid(:) ! Grid
        real(8), intent(out) :: prob(:) ! Probability vector
        
        !Local variables
        integer :: n, i
        real(8) :: point
        
        n = size(grid)
    
        ! TEST THE ARGUMENTS
        if (size(prob)/=n) then
    	    call myerror("discretize_grid_beta: grid and prob have different dimensions")
        endif
        
        if ( a<=0.1d0 .or. b<=0.1d0 ) then
            call myerror( "discretize_grid_beta: Either A or B <= 0.1 !" )
        end if
        
        ! COMPUTATION
        
        !Divide the probability measure evenly:
        !(See Proposition B here: http://www.ssc.wisc.edu/~jkennan/research/DiscreteApprox.pdf)
        prob = 1d0/real(n,8)
        
        do i=1,n
            point = real(i,8)/real(n,8)
            grid(i) = pinvbeta_r(point,a,b)
        enddo
        
        if (any(grid<0d0) .or. any(grid>1d0)) then
            call myerror("discretize_grid_beta: grid is not on (0,1)")
        endif
        
    end subroutine discretize_grid_beta
    !===============================================================================!
    
    subroutine unique(x, x_u,ind_u)
	! Purpose: Find unique values in array x 
	! Return a smaller array with only unique values and corresponding (last) indeces 
	! Identical to matlab unique with 'last' option 
    ! WARNING: The input array x must be sorted in ascending order

	implicit none

	real(8), intent(in) :: x(:)
	real(8), intent(out), allocatable :: x_u(:)
	integer, intent(out), allocatable :: ind_u(:) 
	! Local variables
	integer :: i, n, i_u

	! Execution 

	n = size(x)

	allocate(x_u(n),ind_u(n))

	x_u(1) = x(1)
	i_u = 1

	do i=2,n
		if (x(i)>x(i-1)) then
			ind_u(i_u) = i-1
			x_u(i_u+1) = x(i) 
			i_u = i_u+1
		endif 
	enddo
	
	ind_u(i_u) = n
	
	! Outputs
	x_u = x_u(1:i_u)
	ind_u = ind_u(1:i_u)

    end subroutine unique
    !===============================================================================!

    function quantili(x,w,q) result(y)
    ! Purpose: computes quantiles q of x with weights w
    ! w is discrete prob function (or PMF)
    ! need not be sorted or normalized
    ! Sources: Cagetti and De Nardi M-function quantili.m
    ! Note: takes care of repeated values
    ! Dependencies: calls subroutine unique
    use toolbox, only: sort    
    implicit none
    
    !Declare inputs:
    real(8), intent(in) :: x(:)
    real(8), intent(in) :: w(:)
    real(8), intent(in) :: q(:)
    !Declare output:
    real(8) :: y(size(q))

    !Declare locals
    integer :: i, n, istat
    integer, allocatable :: ix(:), ind_u(:)
    real(8), allocatable :: xs(:), ws(:), cums(:)
    real(8), allocatable :: cums_u(:), xs_u(:)

    n = size(x)
    allocate( ix(n), xs(n),ws(n),cums(n),stat=istat )
    if (istat/=0) then
        call myerror("quantili: Allocation failed!")
    endif

    !!Check inputs
    if (size(w)/=n) then
        call myerror("quantili: x (variable) and w (pmf) must be of the same size")
    endif

    !xs is x sorted, ix is the sorting index
    xs = x 
    call sort(xs,ix)
    ws=w(ix)
    ws=ws/sum(ws)
    cums=cumsum(ws)
    
    ![xs_u,ind_u] = unique(xs,'last')
    call unique(xs, xs_u,ind_u)
    cums_u = cums(ind_u)

    y = 0.0d0
    do i=1,size(q)
        if (cums_u(1)<=q(i)) then
            y(i) = linint(cums_u,xs_u,q(i))
        else
            y(i) = xs_u(1)    
        endif
    enddo

    end function quantili
    !===============================================================================!
    
!=======================================================================!
 !===============================================================================!
    
    function sample_pmf_fast(pi,rand) result(F)
        implicit none
        ! PURPOSE:
        ! Draw random number from probability mass function
        ! described in vector pi, where sum(pi)=1 (not necessary).
        ! It requires a random uniform number *rand* as input.
        ! Declare inputs:
        real(8), intent(in) :: pi(:)
     	real(8),intent(in) :: rand
        ! Declare function result:
        integer :: F
        ! Local variables
        real(8) :: pi_norm(size(pi)), pi_cdist(size(pi))
        !--------------------------------------------------------!
     		
     	! normalize pi so that the sum is equal to 1	
     	pi_norm = pi/sum(pi)

        ! Cumulative 
        pi_cdist = cumsum(pi_norm)
        
        F = locate(pi_cdist,rand)+1
        
        if (F<1 .or. F>size(pi)) then
            call myerror("sample_pmf_fast: result is out of bounds")
        endif
        
     
    end function sample_pmf_fast    
!===============================================================================!
    function rand_exponential(u,lambda) result(x)
    	 implicit none
     
        !Declare input/output variables     
        real(8), intent(in) :: u !standard uniform random number
        real(8), intent(in) :: lambda ! exponential distribution parameter
        real(8) :: x ! exponential random number such that urand = CDF_exponential(x) 
   		!Declare local variables
			
		if (u<0.0d0 .or. u>1.0d0) then
			call myerror("rand_exponential: u is out of bounds")
		endif
		if (lambda<=0.0d0) then
			call myerror("rand_exponential: lambda is non-positive")
		endif
     	x = - (1.0d0/lambda) * log(1.0d0-u)
     
    end function rand_exponential
     !===============================================================================!
    
    function fun_logsum(V,sigma) result(LogSum)
    ! DESCRIPTION
    ! fun_logsum calculates the log-sum (but not the probabilities) The computation 
    ! avoids overflows. See also sub_logsum.
    ! REFERENCES:
    ! https://jblevins.org/log/log-sum-exp
    ! https://gregorygundersen.com/blog/2020/02/09/log-sum-exp/
    ! https://nhigham.com/2021/01/05/what-is-the-log-sum-exp-function/
    
    ! Declare inputs and function result:
    real(8), intent(in) :: V(:)   ! Input vector
    real(8), intent(in) :: sigma  ! St. dev. of the preference shock
	real(8) :: LogSum             ! Result is log(sum(exp(v)))
	! Declare locals:
	real(8) :: mxm  

	if (sigma<0d0) then
        call myerror("sub_logsum: sigma must be non-negative")
    endif

	! Choose the rescaling factor mxm to be the max of vector V.
	mxm = maxval(V)
    
    if (sigma>1d-10) then
        ! a. numerically robust log-sum
        LogSum = mxm + sigma*log(sum(exp((V-mxm)/sigma)))
    else ! no smoothing -> max operator
        LogSum   = mxm
    endif    
	
	if (LogSum/=LogSum) then
		call myerror("fun_logsum: LogSum is either NaN or Inf")
	endif
		
    end function fun_logsum
    !===============================================================================!

    subroutine sub_logsum(LogSum,Prob, V,sigma,dbg_in) 
    ! DESCRIPTION
    ! Calculates the log-sum and choice-probabilities. The computation avoids
    ! overflows. See for more info:
    ! https://gregorygundersen.com/blog/2020/02/09/log-sum-exp/
    ! https://nhigham.com/2021/01/05/what-is-the-log-sum-exp-function/
    ! INPUTS
    !   V:     Vector with values of the different choices
    !   sigma: Standard deviation of the taste shock
    ! OUTPUTS
    !   LogSum: Log sum of exponentials
    !   Prob:   Choice probabilities
    ! AUTHOR
    ! Alessandro Di Nola, March 2022.

    real(8), intent(in)  :: V(:)
    real(8), intent(in)  :: sigma
    integer, intent(in), optional :: dbg_in
    real(8), intent(out) :: LogSum
    real(8), intent(out) :: Prob(:)
    ! Local variables:
    integer :: n, id, dbg
    real(8) :: mxm, sum_prob
    
    ! Body of fun_logsum
    
    if (present(dbg_in)) then
    	dbg = dbg_in
    else
    	dbg = 1
    endif
    		
    n = size(V)
    
    ! Check inputs
    if (n==1) then
    	LogSum = V(1)
    	Prob   = 1d0
    	return
    endif
    if (n/=size(Prob)) then
        call myerror("sub_logsum: V and Prob are not compatible")
    endif
    if (sigma<0d0) then
        call myerror("sub_logsum: sigma must be zero or positive")
    endif
    
    ! Maximum over the discrete choice
    id  = maxloc(V,dim=1)
    mxm = V(id)
    
    ! Logsum and probabilities
    if (sigma>1d-10) then
        ! a. numerically robust log-sum
        LogSum = mxm + sigma*log(sum(exp((V-mxm)/sigma)))
        
        ! b. numerically robust probability
        Prob = exp((V-LogSum)/sigma)
        
    else ! no smoothing -> max operator
        
        LogSum   = mxm
        Prob     = 0d0 ! dim: (n,1)
        Prob(id) = 1d0
        
    endif    
    
    Prob = Prob/sum(Prob)
    sum_prob = sum(Prob)

    !-----------------------------------------------------------------!
    ! Check if outputs are correct. Comment out this section for speed
    if (dbg==1) then
		if (LogSum/=LogSum) then
			call myerror("sub_logsum: LogSum is either NaN or Inf")
		endif
		if (any(Prob<0d0)) then
			call myerror("sub_logsum: Some elements of Prob are negative")
		endif
		if (any(Prob>1d0)) then
			call myerror("sub_logsum: Some elements of Prob are >1")
		endif
		if (abs(sum_prob-1d0)>1d-8) then
			write(*,*) "Prob", Prob
			write(*,*) "sum_prob = ", sum_prob
			error stop "sub_logsum: Prob does not sum to one"
		endif
		
    endif
    
    !-----------------------------------------------------------------!
    end subroutine sub_logsum
    !===============================================================================!

    function is_monotone(arr) result(ans)
    ! Purpose: Check if a real array is monotonically increasing
    implicit none
    !Declare inputs:
    real(8), intent(in) :: arr(:)
    !Declare function result:
    logical :: ans
    !Declare locals:
    integer :: i,n

    n = size(arr)
    if (n==1) then
        ans = .true.
        return
    endif

    do i=1,n-1
        if ( arr(i)<arr(i+1) ) then
            ans = .true.
        else
            ans = .false.
            return
        endif
    enddo

    end function is_monotone
     !===============================================================================!
     
! Compute the cumulative product of the vector x
function cumprod_r(x) result(y)
    implicit none
    real(8), intent(in) :: x(1:)
    real(8) :: y(size(x))
    !local
    real(8) :: tmp
    integer :: i


    y(1) = x(1)
    tmp = x(1)
    do i = 2,size(x)
        tmp = tmp*x(i)
        y(i) = tmp
    end do

end function cumprod_r
!===============================================================================!

function cumprod_i(x) result(y)
    implicit none
    integer, intent(in) :: x(1:)
    integer  :: y(size(x))
    !local
    integer :: tmp
    integer :: i

    y(1) = x(1)
    tmp = x(1)
    do i = 2,size(x)
        tmp = tmp*x(i)
        y(i) = tmp
    end do

end function cumprod_i
!===============================================================================!
    
! Code is based of the matlab ind2sub.m
! Given an integer vector siz that gives the shape of
! some array and the linear index i, convert the linear
! index into an i1,i2,... such that 
!  a(i) = a(i1,i2,...)
function ind2sub(siz,i) result(ivec)
    integer, intent(in) :: siz(:)
    integer :: i
    integer :: ivec(size(siz))
    ! local
    integer :: d
    integer :: j,k
    integer :: cumprodivec(size(siz))

    cumprodivec(1) = 1
    cumprodivec(2:size(siz)) = cumprod(siz(1:size(siz)-1))

    j = i
    do d = size(siz),1,-1
        k = mod(j-1,cumprodivec(d)) + 1
        ivec(d) = (j-k)/cumprodivec(d) + 1
        j = k
    end do

end function ind2sub

!===============================================================================!

    
end module mod_numerical
