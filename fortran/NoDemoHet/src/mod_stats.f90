module mod_stats
    
implicit none

private !Variables and internal proc are not visible outside of this module 
        !unless if they are declared as public (see below)

! Collection of statistical routines. For examples, see test at the end of this file

public :: pinvbeta_r ! INVERSE BETA PROBABILITY DISTRIBUTION FUNCTION (actually, the inverse of the cdf)
public :: probbeta_r ! BETA PROBABILITY DISTRIBUTION FUNCTION (actually, the cdf)
public :: lngamma_r  ! LOGARITHM OF THE GAMMA FUNCTION

integer, parameter :: i1b = selected_int_kind( 2  ),      & 
                      i2b = selected_int_kind( 4  ),      & 
                      i4b = selected_int_kind( 9  ),      & 
                      i8b = selected_int_kind( 10 )
    
integer, parameter :: dp   = kind( 1.0d0 )
integer, parameter :: dp2  = dp
integer, parameter :: stnd = dp
integer, parameter :: extd = dp2
integer, parameter :: lgl  = kind( .true. ) 
    

!   FREQUENTLY USED MATHEMATICAL CONSTANTS IN EXTENDED PRECISION
!
real(extd), parameter ::                                                      &
    pi_extd        = 3.1415926535897932384626433832795028841971693993751_extd,&
    pio2_extd      = 1.57079632679489661923132169163975144209858_extd,        &
    twopi_extd     = 6.283185307179586476925286766559005768394_extd,          &  
    sqrt2_extd     = 1.41421356237309504880168872420969807856967_extd,        &
    euler_extd     = 0.5772156649015328606065120900824024310422_extd,         &
    lnsqrt2pi_extd = 0.9189385332046727_extd
    
!    MISCELLANEOUS REAL VALUES IN EXTENDED PRECISION
!
!    FORM: d.dd          NAMED AS  cd_dd_extd
!                   nn
!    FORM: d.dd x 10     NAMED AS  cd_ddenn_extd
!                   -nn
!    FORM: d.dd x 10     NAMED AS  cd_ddmnn_extd
!
real(extd), parameter ::           &
    c1_m15_extd    = 1.0e-15_extd, &
    c1_m16_extd    = 1.0e-16_extd, &
    c0_65_extd     = 0.65_extd,    &
    c6_5_extd      = 6.5_extd,     &
    c365_25_extd   = 365.25_extd,  &
    c0_0004_extd   = .0004_extd

!   DIGITS IN EXTENDED PRECISION
!
real(extd), parameter ::           &
    zero_extd    = 0,              & 
    one_extd     = 1,              & 
    two_extd     = 2,              & 
    three_extd   = 3,              &
    four_extd    = 4,              &
    five_extd    = 5,              &
    six_extd     = 6,              &
    seven_extd   = 7,              &
    eight_extd   = 8,              &
    nine_extd    = 9,              &
    ten_extd     =10               

real(stnd), parameter :: &
    zero    = 0,              &
    tenth   = 0.1_stnd,       &
    fifth   = 0.2_stnd,       &
    half    = 0.5_stnd,       &
    one     = 1,              & 
    two     = 2,              & 
    three   = 3,              &
    four    = 4,              &
    five    = 5,              &
    six     = 6,              &
    nine    = 9,              &
    ten     = 10,             &
    c13     = 13
    
!
!   RECIPROCALS IN EXTENDED PRECISION
!
real(extd), parameter ::                     &
    tenth_extd   = 0.1_extd,                 &
    ninth_extd   = one_extd/nine_extd,       &
    eighth_extd  = 0.125_extd,               &
    seventh_extd = one_extd/seven_extd,      &
    sixth_extd   = one_extd/six_extd,        &
    fifth_extd   = 0.2_extd,                 &
    quarter_extd = 0.25_extd,                &
    third_extd   = one_extd/three_extd,      &
    half_extd    = 0.5_extd    

! VALUES OF MACHINE NUMERIC CHARACTERISTICS
! WITH INTRINSIC VALUES IN FORTRAN90.
real(stnd), save ::   &
        unitrnd             ! UNIT ROUNDOFF OF THE MACHINE

integer(i4b), parameter ::             &
       maxexp      = maxexponent(unitrnd), & ! LARGEST EXPONENT BEFORE OVERFLOW
       minexp      = minexponent(unitrnd), & ! MINIMUM EXPONENT BEFORE (GRADUAL) UNDERFLOW
       base        = radix(unitrnd),       & ! BASE OF THE MACHINE
       nbasedigits = digits(unitrnd),      & ! NUMBER OF (base) DIGITS IN THE MANTISSA
       decprec     = precision(unitrnd),   & ! NUMBER OF EQUIVALENT DECIMAL DIGITS IN THE MANTISSA
       decexpr     = range(unitrnd)          ! EQUIVALENT DECIMAL EXPONENT RANGE


real(stnd), parameter ::                &
    machdecprec     = decprec,           & ! NUMBER OF EQUIVALENT DECIMAL DIGITS IN THE MANTISSA
    machdecexpr     = decexpr,           & ! EQUIVALENT DECIMAL EXPONENT RANGE
    macheps         = epsilon(unitrnd)     ! MACHINE EPSILON     :  base**(1-nbasedigits)
    
! ERROR MESSAGES FOR MODULE Prob_Procedures .
!
    character(len=*), parameter ::                                                    &
    prob_error1  = ' : P outside range (0, 1) !',                                     &
    prob_error2  = ' : Number of degrees of freedom < 1 !',                           &
    prob_error3  = ' : Number of degrees of freedom < 1 or P outside range (0, 1) !', &
    prob_error4  = ' : Number of degrees of freedom < 1 or X2 < 0  !',                &
    prob_error5  = ' : Numbers of degrees of freedom < 1 or F < 0  !',                &
    prob_error6  = ' : X <= 0  !',                                                    &
    prob_error7  = ' : Either A or B <= 0 !',                                         &
    prob_error8  = ' : X outside range (0, 1) !',                                     &
    prob_error9  = ' : ACU <= 0 !',                                                   &
    prob_error10 = ' : Either DF1 or DF2 <= 0 !',                                     &
    prob_error11 = ' : F < 0  !',                                                     &
    prob_error12 = ' : X < 0  !',                                                     &
    prob_error13 = ' : GAMP<=0 !',                                                    &
    prob_error14 = ' : X2<0 !',                                                       &
    prob_error15 = ' : DF<0.5 !',                                                     &
    prob_error16 = ' : DF<0.5 or DF>2.E5 !'  ,                                        &
    prob_error17 = ' : Maximum number of iterations exceeded !',                      &
    prob_error18 = ' : GAMP<0.25 !',                                                  &
    prob_error19 = ' : Sample size <= 1 or X <= 0  !',                                &
    prob_error20 = ' : N <= 0  !',                                                    &
    prob_error21 = ' : K < 0 or K>N  !',                                              &
    prob_error22 = ' : Either A or B <= 0.1 !',                                       &
    prob_error23 = ' : Either DF1 or DF2 <= 0.2 !'
    
logical, parameter  :: true  = .true.
logical, parameter  :: false = .false.
    
contains
    
subroutine merror(string)

	implicit none
	character(len=*), intent(in) :: string
	write(*,*) "myerror: ", string
	write(*,*) "Program will terminate.."
	pause
	stop

end subroutine merror
!=====================================================================!
    
    
! =========================================================================================
!                            INVERSE BETA PROBABILITY DISTRIBUTION FUNCTION
! =========================================================================================
!
!
function pinvbeta_r( p, a, b, beta, acu, maxiter )
!
! Purpose
! _______
!
!   Evaluates the inverse beta probability distribution function (e.g. the incomplete beta function).
!
!   For given arguments P (0<=P<=1), A (A>0.1), B (B>0.1), PINVBETA returns the value X such that P is 
!   the probability that a random variable distributed as beta(A,B) is less than or equal to X.
!
!
! Arguments
! _________
!
!   P       (INPUT) real(stnd)
!           On entry, input probability.
!           P must be in the range (0,1) inclusive.
!
!   A       (INPUT) real(stnd)
!           on entry, the (1st) parameter of the beta distribution.
!           A must be greater than 0.1 .
!
!   B       (INPUT) real(stnd)
!           On entry, the (2nd) parameter of the beta distribution.
!           B must be greater than 0.1 .
!
!   BETA    (INPUT, OPTIONAL) real(stnd)
!           On entry, the logarithm of the complete beta function beta(A,B).
!           If BETA is not given, the logarithm of the beta function is
!           computed with the help of function LNGAMMA.
!
!   ACU     (INPUT, OPTIONAL) real(stnd)
!           On entry, the desired accuracy of the result, when computing the
!           incomplete beta function. The "integrating" process for evaluating
!           the incomplete beta function is terminated when the
!           relative contribution to the integral is not greater than
!           the value of ACU. ACU  is a small strictly positive integer.
!           See the description of the PROBBETA function for more details
!           on this argument.
!          
!           The default value for ACU is epsilon( ACU ).
!
!   MAXITER (INPUT, OPTIONAL) integer(i4b)
!           On entry, MAXITER controls the maximum number of iterations when evaluating
!           the power series representation of the incomplete Beta integral.
!           The default value is 2000.
!           See the description of the PROBBETA function for more details
!           on this argument.
!
!
! Further Details
! _______________
!
!   This function is not very accurate for small values of A and/or B (e.g. less than 0.5).
!
!   This function is adapted from
!
!   (1)     Majumder, K.L., and Bhattacharjee, G.P., 1973: Algorithm AS 64: Inverse of
!                the Incomplete Beta Function Ratio. Appl. Statist., Vol.22, No.3, pp 411-414.
!
!   (2)     Cran, G.W., Martin, K.J., and  Thomas, G.E., 1977: Remark AS R19 and Algorithm AS 109:
!                A remark on Algorithms: AS 63 the Incomplete Beta integral, AS 64 Inverse of
!                the Incomplete Beta Function Ratio. Appl. Statist., Vol.26, No.1, pp 111-114.
!
!   (3)     Berry, K.J.,  Mielke, P.W., and Cran, G.W., 1990: Algorithm AS R83: A remark on Algorithm
!                AS 109: Inverse of the Incomplete Beta Function Ratio. Appl. Statist., Vol.39,
!                No 2, pp. 309-310.
!
!   (4)     Berry, K.J.,  Mielke, P.W., and Cran, G.W., 1991: Correction to Algorithm AS R83: A remark
!                on Algorithm AS 109: Inverse of the Incomplete Beta Function Ratio.
!                Appl. Statist. Vol. 40, No. 1, p.236
!
!
! _________________________________________________________________________________________
!
!
! USED MODULES
! ____________
!
!
!
! SPECIFICATIONS FOR ARGUMENTS
! ____________________________
!
    ! Declare function inputs
    real(stnd), intent(in)           :: p, a, b
    real(stnd), intent(in), optional :: beta, acu
    ! Declare function result
    real(stnd)                       :: pinvbeta_r
!
    integer(i4b), intent(in), optional    :: maxiter
!
!
! SPECIFICATIONS FOR LOCAL VARIABLES
! __________________________________
!
    real(stnd) :: acu2, beta2, asb, sae, fpu, iex, eps2, x, pp, aa, bb,  &
                  g, h, r, s, t, w, y, yprev, sq, prev, adj, tx, xl, xr, &
                  fxl, fcs, xrmxl
!
    integer(i4b) :: i
!
    logical(lgl) :: indx, use_bisect
!   
!   
! PARAMETERS 
! __________
!
!   DEFINE ACCURACY FOR BISECTION METHOD.
!
    real(stnd),  parameter :: eps=0.00001_stnd, sig=0.000001_stnd
!
!   MAXIMUM NUMBER OF ITERATIONS FOR BISECTION AND NEWTON METHODS.
!
    integer(i4b),  parameter :: itmax=50_i4b
!
    character(len=*),  parameter :: name_proc='pinvbeta'
!
!
! EXECUTABLE STATEMENTS
! _____________________
!
!
!   TEST THE ARGUMENTS.
!
    if ( a<=tenth .or. b<=tenth ) then
        call merror( name_proc//prob_error22 )
    end if
!
    if ( p<zero .or. p>one ) then
        call merror( name_proc//prob_error1 )
    end if
!
!   QUICK RETURN IF POSSIBLE.
!
    if ( p==zero .or. p==one ) then
!
       pinvbeta_r = p
!
       return
!
    end if
!
!   DEFINE ACCURACY FOR INCOMPLETE BETA FUNCTION IF REQUIRED.
!
    acu2 = macheps
    if ( present(acu) ) then
        if ( acu<=zero ) call merror( name_proc//prob_error9 )
        acu2 = acu
    end if
!
!   CALCULATE log( BETA(A,B)) IF REQUIRED.
!
    asb  = a + b
!
    if ( present(beta) ) then
        beta2 = beta
    else
        beta2 = lngamma_r( a ) + lngamma_r( b ) - lngamma_r( asb )
    end if
!
    xl = min( a, b )
    xr = max( a, b )
!
    i = 0_i4b
    use_bisect = xl>one .and. ten*xl>xr
!
    if ( use_bisect ) then
!
!      USE A BISECTION METHOD.
!
       xl = zero
       xr = one
!
       fxl = -p
!
       do i = 1_i4b, itmax
!
           x = ( xl + xr )*half
           pp = probbeta_r( x, a, b, beta=beta2, acu=acu2, maxiter=maxiter )
           fcs = pp - p
!
           if ( fcs*fxl>zero ) then
               xl  = x
               fxl = fcs
           else
               xr  = x
           end if
!
           xrmxl = xr - xl
!
           if ( xrmxl<=sig .and. abs(fcs)<=eps ) exit
!
       end do
!
    end if
!
    if ( i>itmax ) use_bisect = false
!
    if ( .not.use_bisect ) then
!
!       USE NEWTON METHOD FOR SKEWED CASES OR
!       NO CONVERGENCE WITH BISECTION METHOD.
!
!       CHANGE TAIL IF NECESSARY.
!
        if ( p>half ) then
!
            pp   = one - p
            aa   = b
            bb   = a
            indx = true
!
        else
!
            pp   = p
            aa   = a
            bb   = b
            indx = false
!
        end if
!
!       CALCULATE THE INITIAL APPROXIMATION.
!
        r = sqrt( -log(pp*pp) )
        y = r - ( 2.30753_stnd+0.27061_stnd*r )/( one + (0.99229_stnd+0.04481_stnd*r)*r )
!
        if ( aa>one .and. bb>one ) then
!
            r = (y*y-three)/six
            s = one/( aa + aa -one )
            t = one/( bb + bb - one )
            h = two/( s + t )
            w = y*sqrt( h+r )/h - (t-s)*( r + five/six -two/(three*h) )
            x = aa/( aa + bb*exp( w + w ) )
!
        else
!
            r = bb + bb
            t = one/( nine*bb )
            t = r*( one - t +y*sqrt(t) )**3
!
            if ( t<=zero ) then
!
                x = one - exp( ( log( (one-pp)*bb) + beta2 )/bb )
!
            else
!
                t = (four*aa + r - two )/t
                if ( t<=one ) then
                    x = exp( ( log(pp*aa)+beta2 )/aa )
                else
                    x = one - two/( t + one )
                end if
!
            end if
!
        end if
!
!       SOLVE FOR x BY A MODIFIED NEWTON-RAPHSON METHOD,
!       USING THE FUNCTION probbeta_r .
!
        r     = one - aa
        t     = one - bb
        yprev = zero
        sq    = one
        prev  = one
!
        x =  min( max( x, 0.0001_stnd ), 0.9999_stnd ) 
!
!
!       DEFINE ACCURACY.
!       sae BELOW IS THE MOST NEGATIVE DECIMAL EXPONENT WHICH DOES NOT 
!       CAUSE AN UNDERFLOW.
!
        sae = -machdecexpr
        fpu = ten**sae
        iex = max(-five/aa**2 - one/pp**fifth - c13, sae)
        eps2 = ten**iex
!
        b_ext : do i = 1_i4b, itmax
!
            y   = probbeta_r( x, aa, bb, beta=beta2, acu=acu2, maxiter=maxiter )
            y   = ( y - pp )*exp( beta2 + r*log(x) + t*log(one-x) )
!
            if ( y*yprev<=zero ) prev = max( sq, fpu )
!
            g = one
!
            do
!
                adj = g*y
                sq  = adj*adj
!
                if ( sq<prev ) then
!
                    tx = x - adj
!
                    if ( tx>=zero .and. tx<=one ) then
!
                        if ( prev<=eps2 .or. y*y<=eps2 ) exit b_ext
!
                        if ( tx/=zero .and. tx/=one ) then
!
                            if ( tx==x ) exit b_ext
!
                            x     = tx
                            yprev  = y
!
                            cycle b_ext
!
                        end if
!
                    end if
!
                end if
!
                g = g/three
!
            end do
!
        end do b_ext
!
        if ( indx ) then
            x = one - x
        end if
!
    end if
!
    pinvbeta_r = x
!
!
end function pinvbeta_r

! END OF FUNCTION pinvbeta_r
! __________________________
!
! =========================================================================================

! =========================================================================================
!                            BETA PROBABILITY DISTRIBUTION FUNCTION
! =========================================================================================
!
!
    function probbeta_r( x, a, b, beta, acu, maxiter, failure )
!
! Purpose
! _______
!
!   Evaluates the beta probability distribution function (e.g the incomplete beta function).
!
!   For given arguments X (0<=X<=1), A (A>0), B (B>0), PROBBETA returns the probability that
!   a random variable from a beta distribution having parameters A and B will be less than or
!   equal to X.
!
!
! Arguments
! _________
!
!   X       (INPUT) real(stnd)
!           On entry, the value to which function is to be integrated.
!           X must be in range (0,1) inclusive.
!
!   A       (INPUT) real(stnd)
!           on entry, the (1st) parameter of the beta distribution.
!           A must be greater than 0.
!
!   B       (INPUT) real(stnd)
!           On entry, the (2nd) parameter of the beta distribution.
!           B must be greater than 0.
!
!   BETA    (INPUT, OPTIONAL) real(stnd)
!           On entry, the logarithm of the complete beta function BETA(A,B).
!           If BETA is not given, the logarithm of the beta function is
!           computed with the help of function LNGAMMA.
!
!   ACU     (INPUT, OPTIONAL) real(stnd)
!           On entry, the desired accuracy of the result. The "integrating"
!           process is terminated when both the absolute and relative contributions
!           to the integral is not greater than the value of ACU.
!
!           ACU  is a small strictly positive integer. If the number of decimal digits'
!           accuracy required is r, ACU should be set to 10**(-(r+1)).
!          
!           The default value for ACU is epsilon( ACU ).
!
!   MAXITER (INPUT, OPTIONAL) integer(i4b)
!           On entry, MAXITER controls the maximum number of iterations when evaluating
!           the power series representation of the incomplete Beta integral.
!           The default value is 2000.
!
!   FAILURE (INPUT, OPTIONAL) logical(lgl)
!           On entry, if FAILURE is set to true, the values for which the "integrating"
!           process did not converge to the desired accuracy are set to -1. The algorithm
!           fails to converge if the number of iterations exceeds MAXITER.
!           The default value is false.
!
!
! Further Details
! _______________
!
!   This function is adapted from
!
!   (1)     Majumder, K.L., and Bhattacharjee, G.P., 1973: Algorithm AS 63: the Incomplete
!                   Beta Integral. Appl. Statist., Vol.22, No.3, pp 409-411.
!
!   (2)     Cran, G.W., Martin, K.J., and  Thomas, G.E., 1977: Remark AS R19 and Algorithm AS 109:
!                A remark on Algorithms: AS 63 the Incomplete Beta integral, AS 64 Inverse of
!                the Incomplete Beta Function Ratio. Appl. Statist., Vol.26, No.1, pp 111-114.
!
!
! _________________________________________________________________________________________
!
!
! USED MODULES
! ____________
!
!
!
! SPECIFICATIONS FOR ARGUMENTS
! ____________________________
!
    real(stnd), intent(in)           :: x, a, b
    real(stnd), intent(in), optional :: beta, acu
    real(stnd)                       :: probbeta_r
!
    integer(i4b), intent(in), optional    :: maxiter
!
    logical(lgl), intent(in), optional    :: failure
!
!
! SPECIFICATIONS FOR LOCAL VARIABLES
! __________________________________
!
    real(stnd) :: acu2, beta2, betain, asb, cx, xx, aa, bb, term, ai, rx, temp
!
    integer(i4b) :: ns, i, maxit
!
    logical(lgl) :: indx, fail
!   
!   
! PARAMETERS 
! __________
!
    character(len=*),  parameter :: name_proc='probbeta'
!
!
! EXECUTABLE STATEMENTS
! _____________________
!
!
!   TEST THE ARGUMENTS.
!
    if ( a<=zero .or. b<=zero ) then
        call merror( name_proc//prob_error7 )
    end if
!
    if ( x<zero .or. x>one ) then
        call merror( name_proc//prob_error8 )
    end if
!
    acu2 = macheps
    if ( present(acu) ) then
        if ( acu<=zero ) call merror( name_proc//prob_error9 )
        acu2 = acu
    end if
!
    maxit = 2000_i4b
    if ( present(maxiter) ) then
        maxit = max( maxiter, 10_i4b )
    end if
!
    fail = false
    if ( present(failure) ) then
        fail = failure
    end if
!
!   QUICK RETURN IF POSSIBLE.
!
    if ( x==zero .or. x==one ) then
!
        probbeta_r = x
!
        return
!
    end if
!
!   CALCULATE log( BETA(a,b)) IF REQUIRED.
!
    asb  = a + b
!
    if ( present(beta) ) then
        beta2 = beta
    else
        beta2 = lngamma_r( a ) + lngamma_r( b ) - lngamma_r( asb )
    end if
!
!   CHANGE TAIL FOR MORE EFFICIENT USE OF THE POWER SERIES.
!
    if ( a<asb*x ) then
!
        xx   = one - x
        cx   = x
        aa   = b
        bb   = a
        indx = true
!
    else
!
        xx   = x
        cx   = one - x
        aa   = a
        bb   = b
        indx = false
!
    end if
!
    term   = one
    ai     = one
    betain = one
    ns     = int( bb + cx*asb, i4b )
!
!   USE SOPER'S REDUCTION FORMULAE.
!
!  
    if ( ns==0_i4b ) then
        rx = xx
    else
        rx = xx/cx
    end if
!
    temp = bb - ai
!
    do i = 1_i4b, maxit
!
        term   = term*temp*rx/(aa+ai)
        betain = betain + term
        temp   = abs( term )
!
        if ( temp<=acu2 .and. temp<=acu2*betain ) exit
!
        ai = ai + one
        ns = ns - 1_i4b
!
        if ( ns>=0_i4b ) then
            temp = bb - ai
            if ( ns==0_i4b ) rx = xx
        else
            temp = asb
            asb = asb + one
        end if
!
    end do
!
    if ( fail .and. i>maxit ) then
!
        probbeta_r = -one
!
    else
!
        betain = betain*exp( aa*log(xx) + (bb-one)*log(cx) - beta2 )/aa
!
        if ( indx ) then
            probbeta_r = one - betain
        else
            probbeta_r = betain
        end if
!
    end if
!
!
! END OF FUNCTION probbeta_r
! __________________________
!
end function probbeta_r
!
! =========================================================================================

! =========================================================================================
!                            LOGARITHM OF THE GAMMA FUNCTION
! =========================================================================================
!
!
function lngamma_r( x )
!
! Purpose
! _______
!
!   Evaluates the logarithm of the gamma function: ln( gamma(X) )
!   for a strictly positive real argument X.
!
!
! Arguments
! _________
!
!   X      (INPUT) real(stnd)
!          On entry, a strictly positive real argument X.
!
!
! Further Details
! _______________
!
!   This function uses a Lanczos-type approximation to ln(gamma(X)) for X > 0. Its accuracy
!   is about 14 significant digits except for small regions in the vicinity of 1 and 2.
!
!   This function is adapted from
!
!   (1)    Lanczos, C., 1964: A precision approximation of the gamma function,
!                              J. SIAM Numer. Anal., B, 1, 86-96.
!
!
! _________________________________________________________________________________________
!
!
! USED MODULES
! ____________
!
!
!
! SPECIFICATIONS FOR ARGUMENTS
! ____________________________
!
    real(stnd), intent(in) :: x
    real(stnd)             :: lngamma_r
!
!
! SPECIFICATIONS FOR LOCAL VARIABLES
! __________________________________
!
    real(extd)    :: lanczos, tmp
!
    integer(i4b)  :: j
!   
!   
! PARAMETERS 
! __________
!
    real(extd), dimension(9), parameter ::     a=                                    &
                           (/ 0.9999999999995183_extd, 676.5203681218835_extd,       &
                              -1259.139216722289_extd, 771.3234287757674_extd,       &
                              -176.6150291498386_extd, 12.50734324009056_extd,       &
                              -0.1385710331296526_extd, 0.9934937113930748E-05_extd, &
                               0.1659470187408462E-06_extd /)
!
    character(len=*),  parameter :: name_proc='lngamma'
!
!
! EXECUTABLE STATEMENTS
! _____________________
!
!
!   TEST THE ARGUMENT.
!
    if ( x<=zero )  then
        call merror( name_proc//prob_error6 )
    end if
!
    lanczos = zero_extd
    tmp     = x + seven_extd
!
    do j = 9_i4b, 2_i4b, -1_i4b
!
        lanczos = lanczos + a(j)/tmp
        tmp     = tmp - one_extd
!
    end do
!
    lanczos   = lanczos + a(1_i4b)
    lngamma_r = log( lanczos ) + lnsqrt2pi_extd - (x + c6_5_extd) &
                + (x - half_extd)*log( x + c6_5_extd )
!
!
! END OF FUNCTION lngamma_r
! _________________________
!
end function lngamma_r
!
! =========================================================================================


end module mod_stats
