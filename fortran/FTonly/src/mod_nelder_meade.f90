module mod_nelder_meade

    implicit none

    private
    public :: nelder_meade, nelder_meade_bnd,inf
    real(8), parameter :: inf = huge(0.0d0)
    real(8), parameter :: pi = 3.14159265358979d0
    
    contains

    !===============================================================================!
    subroutine myerror(string)

    implicit none
    character(len=*), intent(in) :: string
    write(*,*) "ERROR: ", string
    write(*,*) "Program will terminate.."
    pause
    stop

    end subroutine myerror
    !===============================================================================!

    ! Sort: Sorts in ascending the elements of a one dimensional array of type real(8)
    !       It also gives the original indeces of the sorted elements
    !
    ! Usage: Call Sort(n,A,A_sort,Sort_ind)
    !
    ! Input: n   , integer(4), the number of elements in A
    !        A   , real(8), the array whose elements are to be sorted
    !
    ! Output: A_sort, real(8), array with sorted elements (in ascending order)
    !		  Sort_ind, integer(4), array with original indeces of the elements in A_sort
    !
    ! Source: Sergio Ocampo's Github page

    subroutine sort(n,A,A_sort,Sort_ind)
    integer, intent(in) :: n    !Number of elements in A
    real(8), intent(in) , dimension(n) :: A
    real(8), intent(out), dimension(n) :: A_sort
    integer, intent(out), dimension(n) :: Sort_ind
    integer :: i,j

    A_sort = A
    do i=1,n
        Sort_ind(i)=i
    enddo

    do i=1,(n-1)
        do j=i+1,n
            if (A_sort(i) >= A_sort(j)) then
                A_sort((/i,j/))   = A_sort((/j,i/))
                Sort_ind((/i,j/)) = Sort_ind((/j,i/))
            endif
        enddo
    enddo

    end subroutine sort
    !===============================================================================!

    function ones(n) result(vec)
    ! Purpose: create a column vector of ones
    ! Input/output
    integer, intent(in) :: n
    real(8) :: vec(n)
    !Local variables
    integer :: i

    ! Body of ones
    do i = 1,n
        vec(i) = 1.0d0
    end do

    end function ones
    !===============================================================================!
    ! Acts just like the colon in Matlab
    pure function colon(a,b)
    implicit none
    integer, intent(in) :: a,b
    integer, dimension(1:b-a+1) :: colon
    integer :: i

    do i = a,b
        colon(i-a+1) = i
    enddo

    end function colon
    !===============================================================================!
    
    subroutine nelder_meade_bnd(xval,fval,exitflag,funfcn,x0,LB,UB,mytolx,mytolf,mymaxfun,mymaxiter,iterations,funcCount)
    !nelder_meade uses the Nelder-Mead simplex (direct search) method to
    ! minimize a multivariate function fun(x), subject to LB<=x<=UB.
    ! nelder_meade_bnd calls nelder_meade.
    ! This is a replica of fminsearchbnd in Matlab
    ! The method is a local, derivative-free method.
    ! USAGE
    ! NOTE: other inputs and outputs are optional.
    ! Author: Alessandro Di Nola
    implicit none 
    ! - INTERFACE BLOCK
    interface
    function funfcn(x)
    implicit none
    real(8), intent(in) :: x(:)
    real(8) :: funfcn
    end function funfcn
    end interface
    ! - REQUIRED INPUTS
    real(8), intent(in) :: x0(:),LB(:),UB(:)
    ! - Optional inputs
    real(8), optional :: mytolx,mytolf,mymaxfun,mymaxiter
    ! - REQUIRED OUTPUTS   
    real(8), intent(out) :: xval(:)
    real(8), intent(out) :: fval
    integer, intent(out) :: exitflag
    ! - Optional outputs
    integer, optional :: iterations, funcCount
    ! - Local variables
    integer :: maxfun,maxiter
    real(8) :: tolx,tolf
    integer :: nx,k,numberOfVariables,jj
    real(8), allocatable :: x0u(:), xval_trans(:)
    
    ! - START EXECUTION
    nx = size(x0)
    numberOfVariables = nx
    
    ! Assign values to optional inputs
    if (present(mytolx)) then
        tolx = mytolx
    else
        tolx = 1.0d-4
    endif
    if (present(mytolf)) then
        tolf = mytolf
    else
        tolf = 1.0d-4
    endif
    if (present(mymaxfun)) then
        maxfun = mymaxfun
    else
        maxfun = 200*numberOfVariables
    endif
    if (present(mymaxiter)) then
        maxiter = mymaxiter
    else
        maxiter = 200*numberOfVariables
    endif
    
    if (nx/=size(LB) .or. nx/=size(UB)) then
        call myerror("x0 is incompatible in size with either LB or UB.")
    endif
    if (any(x0<LB)) then
        call myerror('Initial guess violates lower bounds!')
    elseif (any(x0>UB)) then
        call myerror('Initial guess violates upper bounds!')
    endif
    
    ! STEP 1 - Transform starting values into their unconstrained counterparts
    ! x0 is in [LB,UB] vs x0u in [-inf,inf]
    allocate(x0u(nx))
    do k=1,nx
        x0u(k) = fun_transform_inv(x0(k),LB(k),UB(k))
    enddo
    
    ! STEP 2 - Call unconstrained Nelder-Meade with funfcn_aux
    ! funfcn_aux takes care of enforcing the bounds on x
    allocate(xval_trans(nx))
    call nelder_meade(xval_trans,fval,exitflag,wrap_funfcn_aux,x0u,tolx,tolf,maxfun,maxiter,iterations,funcCount)
    
    ! STEP 3 - Undo the variable transformations into the original space
    xval = 0d0
    do jj = 1,nx
        xval(jj) = fun_transform(xval_trans(jj),LB(jj),UB(jj))
    enddo
    
    contains 
    
    function wrap_funfcn_aux(x)
        real(8), intent(in) :: x(:)
        real(8) :: wrap_funfcn_aux
        
        wrap_funfcn_aux = funfcn_aux(x,funfcn,LB,UB)
    
    end function wrap_funfcn_aux
        
    end subroutine nelder_meade_bnd
    !===============================================================================!
    
    function funfcn_aux(x,funfcn,LB,UB) result(F)
    ! PURPOSE: funfcn_aux transforms the free variables x into the bounded variables x_trans
    ! and then calls the original function "funfcn".
        implicit none
        real(8), intent(in) :: x(:)
        real(8), intent(in) :: LB(:),UB(:)
        interface
            function funfcn(x)
            implicit none
            real(8), intent(in) :: x(:)
            real(8) :: funfcn
            end function funfcn
        end interface
        real(8) :: F
        !Locals
        integer :: nx,ii
        real(8), allocatable :: x_trans(:)
        
        nx = size(x)
        allocate(x_trans(nx))
        do ii=1,nx
            x_trans(ii) = fun_transform(x(ii),LB(ii),UB(ii))
        enddo
        F = funfcn(x_trans)
    
    end function funfcn_aux
    !===============================================================================!
    function fun_transform_inv(x0,lb,ub) result(x0u)
        implicit none
        real(8), intent(in) :: x0,lb,ub
        real(8) :: x0u
        ! This maps [lb,ub] to [-inf,inf]. It is the inverse mapping of
        ! fun_transform
        if (lb==-inf .and. ub==inf) then
            ! Free
            x0u = x0
        elseif (lb>-inf .and. ub==inf) then
            ! Only lower bound
            x0u  = sqrt(x0-lb)
        elseif (lb==-inf .and. ub<inf) then
            ! Only upper bound
            x0u = sqrt(ub-x0)
        else
            ! Both lb and ub are finite
            x0u = 2d0*(x0 - lb)/(ub-lb) - 1d0
            ! shift by 2*pi to avoid problems at zero in fminsearch
            ! otherwise, the initial simplex is vanishingly small
            x0u = 2d0*pi+asin(max(-1d0,min(1d0,x0u)))
        endif
    
    end function fun_transform_inv
    !===============================================================================!
    function fun_transform(x,lb,ub) result(xtrans)
        implicit none
        real(8), intent(in) :: x,lb,ub
        real(8) :: xtrans
        ! This maps [-inf,inf] to [lb,ub]
        ! Think of xtrans=g(x) where x is unconstrained and g maps [-inf,inf] to [lb,ub]
        if (lb==-inf .and. ub==inf) then
            ! Free
            xtrans = x
        elseif (lb>-inf .and. ub==inf) then
            ! Only lower bound e.g. [-3.5,inf]
            xtrans = lb+x**2
        elseif (lb==-inf .and. ub<inf) then 
            ! Only upper bound e.g. [-inf,3.5]
            xtrans = ub-x**2
        else
            ! Both lb and ub are finite
            xtrans = (sin(x)+1d0)/2d0
            xtrans = xtrans*(ub - lb) + lb
            ! just in case of any floating point problems
            xtrans = max(lb,min(ub,xtrans))
        endif
    
    end function fun_transform
   !===============================================================================!
    
    subroutine nelder_meade(x_opt,fval,exitflag,funfcn,x0,mytolx,mytolf,mymaxfun,mymaxiter,iterations,funcCount)
    ! nelder_meade minimizes the function funfcn(x), where x is a vector
    ! using the Nelder-Meade algorithm (local, derivative-free method).
    ! This is a replica of **fminsearch in Matlab**
    ! USAGE
    ! Note: The inputs *mytolx,mytolf,mymaxfun,mymaxiter* are OPTIONAL
    !       The outputs *iterations,funcCount* are OPTIONAL
    !INPUTS
    interface
        function funfcn(x)
        implicit none
        real(8), intent(in) :: x(:)
        real(8) :: funfcn
        end function funfcn
    end interface
    real(8), intent(in) :: x0(:)
    !Some optional inputs
    real(8), optional :: mytolx, mytolf
    integer,  optional :: mymaxfun, mymaxiter
    !Some optional outputs
    integer, optional :: iterations, funcCount
    !OUTPUTS
    real(8), intent(out) :: x_opt(:)
    real(8), intent(out) :: fval
    integer, intent(out) :: exitflag
    !LOCALS
    integer :: maxfun,maxiter
    integer :: n,numberOfVariables,itercount,np1,func_evals,j
    real(8) :: tolx,tolf,usual_delta,zero_term_delta
    real(8) :: rho,chi,psi,sigma,err_x,err_f
    real(8) :: fxr,fxcc,fxe,fxc,f
    real(8), allocatable :: x(:),xin(:),two2np1(:),one2n(:),onesn(:)
    real(8), allocatable :: xbar(:),xr(:),xe(:),xc(:),xcc(:),y(:)
    real(8), allocatable :: v(:,:), fv(:)
    integer, allocatable :: j_sort(:)
    character(:), allocatable :: how  !how is a character array or string

    x = x0 !so x0 is not destroyed, and I work with x insider the subroutine
    n = size(x)
    numberOfVariables = n

    ! Assign values to optional inputs
    if (present(mytolx)) then
        tolx = mytolx
    else
        tolx = 1.0d-4
    endif
    if (present(mytolf)) then
        tolf = mytolf
    else
        tolf = 1.0d-4
    endif
    if (present(mymaxfun)) then
        maxfun = mymaxfun
    else
        maxfun = 200*numberOfVariables
    endif
    if (present(mymaxiter)) then
        maxiter = mymaxiter
    else
        maxiter = 200*numberOfVariables
    endif

    ! Initialize parameters
    rho = 1d0; chi = 2d0; psi = 0.5d0; sigma = 0.5d0
    onesn   = ones(n) !this is ones(1,n)
    two2np1 = colon(2,n+1)
    one2n   = colon(1,n)

    ! Set up a simplex near the initial guess.
    xin = x(:) ! Force xin to be a column vector
    ! The simplex {v,fv} contains n+1 points in R^n, where v are the coordinates
    ! and fv is a vector of function values
    allocate(v(n,n+1),fv(n+1)) ! fv is row vector (1,n+1) in Matlab
    v(:,1) = xin  ! Place input guess in the simplex! (credit L.Pfeffer at Stanford)
    x(:) = xin    ! Change x to the form expected by funfcn
    fv(1) = funfcn(x)
    itercount = 0

    ! Continue setting up the initial simplex.
    ! Following improvement suggested by L.Pfeffer at Stanford
    usual_delta     = 0.05d0     ! 5 percent deltas for non-zero terms
    zero_term_delta = 0.00025d0  ! Even smaller delta for zero elements of x
    allocate(y(n))
    do j = 1,n
        y = xin
        if (y(j) /= 0d0) then
            y(j) = (1d0 + usual_delta)*y(j)
        else
            y(j) = zero_term_delta
        endif
        v(:,j+1) = y
        x(:) = y; f = funfcn(x);
        fv(j+1) = f;
    enddo

    ! sort so v(1,:) has the lowest function value
    ![fv,j_sort] = sort(fv); %MATLAB
    np1 = size(fv) ! this is np1
    allocate(j_sort(np1))
    call sort(np1,fv,fv,j_sort)
    v = v(:,j_sort)

    itercount = itercount + 1
    func_evals = n+1

    !% Main algorithm: iterate until
    !% (a) the maximum coordinate difference between the current best point and the
    !% other points in the simplex is less than or equal to TolX. Specifically,
    !% until max(||v2-v1||,||v3-v1||,...,||v(n+1)-v1||) <= TolX,
    !% where ||.|| is the infinity-norm, and v1 holds the
    !% vertex with the current lowest value; AND
    !% (b) the corresponding difference in function values is less than or equal
    !% to TolFun. (Cannot use OR instead of AND.)
    !% The iteration stops if the maximum number of iterations or function evaluations
    !% are exceeded

    allocate(xbar(n),xr(n),xe(n),xc(n),xcc(n))

    err_f = tolf+1d0
    err_x = tolx+1d0

    do while (func_evals < maxfun .and. itercount < maxiter)

        if (err_f <= max(tolf,10*epsilon(fv(1))) .and. err_x <= max(tolx,10*epsilon(maxval(v(:,1)))) ) then
            exit
        endif

        ! Compute the reflection point

        ! xbar = average of the n (NOT n+1) best points
        ! I exclude the N+1 point which is the worst
        xbar = sum(v(:,one2n), dim=2)/real(n,8) !xbar is n*1 vector
        xr = (1d0 + rho)*xbar - rho*v(:,n+1)    !xr   is n*1 vector
        x(:) = xr
        fxr = funfcn(x) ! scalar
        func_evals = func_evals+1

        if (fxr < fv(1)) then
            !Calculate the expansion point
            xe = (1d0 + rho*chi)*xbar - rho*chi*v(:,n+1)
            x(:) = xe; fxe = funfcn(x)
            func_evals = func_evals+1
            if (fxe < fxr) then
                v(:,n+1) = xe
                fv(n+1) = fxe
                how = 'expand'
            else
                v(:,n+1)  = xr
                fv(n+1) = fxr
                how = 'reflect'
            endif
        else ! fv(1) <= fxr
            if (fxr < fv(n)) then
                v(:,n+1) = xr
                fv(n+1) = fxr
                how = 'reflect'
            else ! fxr >= fv(n)
                ! Perform contraction
                if (fxr < fv(n+1)) then
                    ! Perform an outside contraction
                    xc = (1d0 + psi*rho)*xbar - psi*rho*v(:,n+1)
                    x(:) = xc; fxc = funfcn(x)
                    func_evals = func_evals+1
                    if (fxc <= fxr) then
                        v(:,n+1) = xc
                        fv(n+1) = fxc
                        how = 'contract outside'
                    else
                        ! perform a shrink
                        how = 'shrink'
                    endif
                else
                    ! Perform an inside contraction
                    xcc = (1d0-psi)*xbar + psi*v(:,n+1)
                    x(:) = xcc; fxcc = funfcn(x)
                    func_evals = func_evals+1
                    if (fxcc < fv(n+1)) then
                        v(:,n+1) = xcc
                        fv(n+1)  = fxcc
                        how = 'contract inside'
                    else
                        ! perform a shrink
                        how = 'shrink'
                    endif
                endif
                if (how=='shrink') then
                    do j=2,n+1
                        v(:,j)=v(:,1)+sigma*(v(:,j) - v(:,1))
                        x(:) = v(:,j); fv(j) = funfcn(x)
                    enddo
                    func_evals = func_evals + n
                endif
            endif
        endif
        ![fv,j_sort] = sort(fv)
        call sort(np1,fv,fv,j_sort)
        v = v(:,j_sort)
        itercount = itercount + 1

        !Compute errors
        err_f = maxval(abs(fv(1)-fv(two2np1)))
        err_x = maxval(abs(v(:,two2np1)-v(:,onesn)))

    enddo ! end big while loop

    x(:) = v(:,1)
    fval = fv(1)
    x_opt = x

    if (func_evals >= maxfun) then
        exitflag = 0
    elseif (itercount >= maxiter) then
        exitflag = 0
    else
        exitflag = 1
    endif

    ! Export also number of iterations and function evaluations
    if (present(iterations)) then
        iterations = itercount
    endif
    if (present(funcCount)) then
        funcCount = func_evals
    endif

    end subroutine nelder_meade

end module mod_nelder_meade
