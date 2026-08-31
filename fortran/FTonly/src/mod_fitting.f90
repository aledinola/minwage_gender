module mod_fitting
    ! DESCRIPTION
    ! This module contains two functions, polyfit and polyval
    ! that are meant to replicate in Fortran the Matlab functions
    ! with the same name (except that they are not vectorized).
    ! We also have other functions for power fit.
    ! NOTES
    ! polyfit uses the LAPACK library. With Intel compiler, just add
    ! the option /Qmkl in the makefile
    implicit none
    
    private
    public :: polyfit, polyval, powerfit, powerval, powerint 
    
    contains
 
!=================================================================!
    
subroutine powerfit(x_data,y_data,fitresult,do_disp)
    use toolbox, only: fminsearch
    ! PURPOSE: Fit a power function f(x) = a*x^b+c on a discrete
    ! dataset {x_data,y_data}
    ! USAGE: call powerfit(x_data,y_data,fitresult) or
    ! INPUTS:  x_data,y_data: vectors of dimension n
    ! OUTPUT:  fitresult, vector with 3 elements (a,b,c) 
    implicit none
    ! INPUTS/OUTPUTS
    real(8), intent(in)  :: x_data(:), y_data(:)
    real(8), intent(out) :: fitresult(:)
    logical, intent(in), optional :: do_disp
    ! LOCALS
    integer :: n
    real(8) :: fun_val
    real(8) :: x_lb(3), x_ub(3)
    logical :: do_disp_tmp
    
    ! Default value of do_disp is false
    if (present(do_disp)) then
        do_disp_tmp = do_disp
    else
        do_disp_tmp = .false.
    endif
    
    ! Input checks
    ! For speed, we do not check that x is monotonically increasing
    if (size(x_data)/=size(y_data)) then
        write(*,*) "mod_fitting:powerfit: x and y have different size"
        pause
        stop
    endif
    if (size(fitresult)/=3) then
        write(*,*) "mod_fitting:powerfit: fitresult must have size 3"
        pause
        stop
    endif
    
    n = size(x_data)
    
    ! call minimizing routine
    x_lb = [-1000d0,-1000d0,-1000d0] ! lower bounds
    x_ub = [1000d0, 0d0, 1000d0]  ! upper bounds
    fitresult = [0.05267d0,-0.603d0,-0.00115d0]
    call fminsearch(fitresult, fun_val, x_lb, x_ub, power_obj_fun)

    if (do_disp_tmp) then
        write(*,*) "----------------------------------"
        write(*,*) "Coeff. of a*x^b+c"
        write(*,*) "a = ", fitresult(1)
        write(*,*) "b = ", fitresult(2)
        write(*,*) "c = ", fitresult(3)
        write(*,*) "----------------------------------"
    endif
    
    ! Assign coefficients of "a*x^b+c" as output
    
    contains

    function power_obj_fun(cvec) result(F)
        implicit none
        ! INPUT: coefficient vector, cvec(3)
        ! OUTPUT: squared distance to be minimized (non-linear least squares)
        real(8), intent(in) :: cvec(:)
        real(8) :: F
        ! LOCALS
        integer :: i
        real(8) :: y_fit
        
        F = 0d0
        do i=1,n
            y_fit = powerval(cvec,x_data(i))
            F = F + (y_data(i)-y_fit)**2d0    
        enddo
        
    end function
    
end subroutine powerfit

function powerval(coef,x) result(F)
! Evaluate power function f(x) = a*x^b+c
! To be used after powerfit
    implicit none
    ! INPUTS/OUTPUT
    real(8), intent(in) :: coef(:)
    real(8) :: x
    real(8) :: F
    ! LOCALS
    real(8) :: a,b,c
    
    if (size(coef)/=3) then
        write(*,*) "mod_fitting:powerval: coef must have size 3"
        pause
        stop
    endif
    
    a = coef(1)
    b = coef(2)
    c = coef(3)
    
    F = a*x**b+c

end function powerval

function powerint(coef,x) result(F)
! powerint computes the integral of the power function f(x) = a*x^b+c
    implicit none
    ! INPUTS/OUTPUT
    real(8), intent(in) :: coef(:)
    real(8) :: x
    real(8) :: F
    ! LOCALS
    real(8) :: a,b,c
    
    if (size(coef)/=3) then
        write(*,*) "mod_fitting:powerint: coef must have size 3"
        stop
    endif
    
    a = coef(1)
    b = coef(2)
    c = coef(3)
    
    F = (a/(b+1d0))*x**(b+1d0)+c*x

end function powerint

function polyfit(vx, vy, d)
    ! PURPOSE
    ! Fit a polynomial of degree d on data points {vx,vy}
    ! For example, if you set d=1 you perform a linear regression
    ! (you are fitting a line i.e. a polynomial of degree 1 with 2 coefficients)
    implicit none
    integer, intent(in)                   :: d
    integer, parameter                    :: dp = selected_real_kind(15, 307)
    real(dp), dimension(d+1)              :: polyfit
    real(dp), dimension(:), intent(in)    :: vx, vy
 
    real(dp), dimension(:,:), allocatable :: X
    real(dp), dimension(:,:), allocatable :: XT
    real(dp), dimension(:,:), allocatable :: XTX
 
    integer :: i, j
 
    integer     :: n, lda, lwork
    integer, dimension(:), allocatable :: ipiv
    real(dp), dimension(:), allocatable :: work
 
    n = d+1
    lda = n
    lwork = n
 
    allocate(ipiv(n))
    allocate(work(lwork))
    allocate(XT(n, size(vx)))
    allocate(X(size(vx), n))
    allocate(XTX(n, n))
 
    ! prepare the matrix
    do i = 0, d
       do j = 1, size(vx)
          X(j, i+1) = vx(j)**i
       end do
    end do
 
    XT  = transpose(X)
    XTX = matmul(XT, X)
 
    ! The active model does not call polyfit; disable its dormant LAPACK path.
    error stop "polyfit is unavailable because LAPACK is not linked"
 
    polyfit = matmul( matmul(XTX, XT), vy)
 
    deallocate(ipiv)
    deallocate(work)
    deallocate(X)
    deallocate(XT)
    deallocate(XTX)
 
end function polyfit
!=================================================================!
  
function polyval(A,x) result(y)
      !DESCRIPTION: Evaluate a polynomial using Horner's method
      ! See Judd, page 34-35 for an explanation
      ! N-th degree polynomial has (N+1) coefficients:
      ! a(1)+a(2)*x+a(3)*x^2+..+a(N+1)*x^N
      ! For example, a 3rd degree polynomial
      ! a(1)+a(2)*x+a(3)*x^2+a(4)*x^4
      !INPUTS:
      ! A - Vector with N+1 coefficients
      ! x - Point where to evaluate the polynomial
      !OUTPUT:
      ! y - Polynomial evaluated at x
  
      ! Declare input/outputs
      real(8), intent(in) :: A(:)
      real(8), intent(in) :: x
      real(8) :: y
      ! Declare locals
      real(8) :: sum
      integer :: ii, N
      
      ! A polynomial of degree N has N+1 coefficients
      N = size(A)-1 

      sum = A(N+1)
      do ii = N,1,-1
          sum = sum*x+A(ii)
      enddo

      y = sum
  
end function polyval
 
end module mod_fitting
