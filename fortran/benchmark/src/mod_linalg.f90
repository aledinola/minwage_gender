module linalg
    implicit none
    
    public :: lu_solve
    
    !##############################################################################
    ! INTERFACE assert_eq
    !
    ! Interface for equality assertions by assert_eqx functions.
    !##############################################################################
    interface assert_eq

    module procedure assert_eq2, assert_eq3, assert_eq4, assert_eq5,assert_eqn

    end interface
    
    contains 
    
    !##############################################################################
    !
    ! Throws error message and stops program.
    !##############################################################################
    subroutine error(routine, message)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! routine in which error occured
        character(len=*), intent(in) :: routine
        
        ! error message
        character(len=*), intent(in) :: message
        
        
        !##### ROUTINE CODE #######################################################
        
        ! write error message
        write(*,'(/a,a,a,a/)')'ERROR ',routine,': ',message
        
        stop
    
    end subroutine error
    
    
    !##############################################################################
    !
    ! Checks equality for two integers.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    function assert_eq2(n1, n2, string)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! integers to compare for equality
        integer, intent(in) :: n1, n2
        
        ! routine from which error should be thrown
        character(len=*), intent(in) :: string
        
        ! return value
        integer :: assert_eq2
        
        
        !##### ROUTINE CODE #######################################################
        
        ! if equality, set return value to n1
        if (n1 == n2)then
            assert_eq2 = n1
        
        ! else throw error message
        else
            call error(string, 'an assertion failed in assert_eq2')
        end if
    
    end function assert_eq2
    
    
    !##############################################################################
    !
    ! Checks equality for three integers.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    function assert_eq3(n1, n2, n3, string)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! integers to compare for equality
        integer, intent(in) :: n1, n2, n3
        
        ! routine from which error should be thrown
        character(len=*), intent(in) :: string
        
        ! return value
        integer :: assert_eq3
        
        
        !##### ROUTINE CODE #######################################################
        
        ! if equality, set return value to n1
        if (n1 == n2 .and. n2 == n3)then
            assert_eq3 = n1
        
        ! else throw error message
        else
            call error(string, 'an assertion failed in assert_eq3')
        end if
    
    end function assert_eq3
    
    
    !##############################################################################
    !
    ! Checks equality for four integers.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    function assert_eq4(n1, n2, n3, n4, string)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! integers to compare for equality
        integer, intent(in) :: n1, n2, n3, n4
        
        ! routine from which error should be thrown
        character(len=*), intent(in) :: string
        
        ! return value
        integer :: assert_eq4
        
        
        !##### ROUTINE CODE #######################################################
        
        ! if equality, set return value to n1
        if (n1 == n2 .and. n2 == n3 .and. n3 == n4)then
            assert_eq4 = n1
        
        ! else throw error message
        else
            call error(string, 'an assertion failed in assert_eq4')
        end if
    
    end function assert_eq4
    
    
    !##############################################################################
    !
    ! Checks equality for five integers.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    function assert_eq5(n1, n2, n3, n4, n5, string)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! integers to compare for equality
        integer, intent(in) :: n1, n2, n3, n4, n5
        
        ! routine from which error should be thrown
        character(len=*), intent(in) :: string
        
        ! return value
        integer :: assert_eq5
        
        
        !##### ROUTINE CODE #######################################################
        
        ! if equality, set return value to n1
        if (n1 == n2 .and. n2 == n3 .and. n3 == n4 .and. n4 == n5)then
            assert_eq5 = n1
        
        ! else throw error message
        else
            call error(string, 'an assertion failed in assert_eq5')
        end if
    
    end function assert_eq5
    
    
    !##############################################################################
    !
    ! Checks equality for n integers.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    function assert_eqn(nn, string)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! integers to compare for equality
        integer, intent(in) :: nn(:)
        
        ! routine from which error should be thrown
        character(len=*), intent(in) :: string
        
        ! return value
        integer :: assert_eqn
        
        
        !##### ROUTINE CODE #######################################################
        
        ! if equality, set return value to n1
        if (all(nn(2:) == nn(1)))then
            assert_eqn = nn(1)
        
        ! else throw error message
        else
            call error(string, 'an assertion failed in assert_eqn')
        end if
    
    end function assert_eqn
 
    
    !##############################################################################
    !
    ! Solves a linear equation system by lu-decomposition.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    subroutine lu_solve(a, b)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! matrix of the system
        real(8), intent(in) :: a(:, :)
        
        ! right side of equation and solution of the system
        real(8), intent(inout) :: b(:)
        
        
        !##### OTHER VARIABLES ####################################################
        
        integer :: indx(size(b))
        real(8) :: worka(size(a, 1), size(a, 2))
        real(8) :: d
        integer :: n
        
        
        !##### ROUTINE CODE #######################################################
        
        ! assert size equality
        n = assert_eq(size(a,1), size(a,2), size(b), 'lu_solve')
        n = n
        
        ! copy matrix to working matrix
        worka = a
        
        ! decompose matrix
        call lu_decomp(worka, indx, d)
        
        ! solve system
        call lu_back(worka, indx, b)
    
    end subroutine lu_solve
    
    
    !##############################################################################
    !
    ! Inverts a matrix by lu-decomposition.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    function lu_invert(a)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! matrix of the system
        real(8), intent(in) :: a(:, :)
        
        ! the inverse of the matrix
        real(8) :: lu_invert(size(a, 1), size(a, 2))
        
        
        !##### OTHER VARIABLES ####################################################
        
        integer :: j, n
        
        
        !##### ROUTINE CODE #######################################################
        
        ! assert size equality
        n = assert_eq(size(a,1), size(a,2), 'lu_invert')
        
        ! set up unity matrix
        lu_invert = 0d0
        do j = 1, n
            lu_invert(j, j) = 1.0d0
        enddo
        
        ! succesively solve the system with unity matrix
        do j = 1, n
            call lu_solve(a, lu_invert(:, j))
        enddo
    
    end function lu_invert
    
    
    !##############################################################################
    !
    ! Calculates lu-decomposition of matrices.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    subroutine lu_decomp(a, indx, d)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! matrix that shall be decomposed
        real(8), intent(inout) :: a(:, :)
        
        ! row permutation indicator due to pivoting
        integer, intent(out) :: indx(:)
        
        ! indicates whether number of row permutations was even or odd
        real(8), intent(out) :: d
        
        
        !##### OTHER VARIABLES ####################################################
        
        real(8) :: vv(size(a,1))
        integer :: j, n, imax
        
        
        !##### ROUTINE CODE #######################################################
        
        ! check array sizes
        n = assert_eq(size(a,1), size(a,2), size(indx),'lu_decomp')
        
        ! initialize permutation indicator
        d = 1.0d0
        
        ! get maximum value in every row
        vv = maxval(abs(a), dim=2)
        
        ! if there is a zero row then matrix is singular
        if (any(abs(vv) <= 1d-100)) call error('lu_decomp', 'matrix is singular')
        
        ! invert v
        vv = 1.0d0/vv
        
        ! start lu-decomposition process
        do j = 1, n
        
            ! get index of pivot element
            imax = (j-1) + imaxloc(vv(j:n)*abs(a(j:n,j)))
            
            ! do pivoting if pivot element is not the first element
            if(j /= imax)then
                call swap(a(imax,:), a(j,:))
                d = -d
                vv(imax) = vv(j)
            endif
            
            ! indicate pivot element
            indx(j) = imax
            
            ! prevent division by 0
            if(abs(a(j,j))  <= 1d-100)call error('lu_decomp', 'matrix is singular')
            
            ! calculate new elements
            a(j+1:n, j) = a(j+1:n,j)/a(j,j)
            a(j+1:n, j+1:n) = a(j+1:n,j+1:n)-outerprod(a(j+1:n,j), a(j,j+1:n))
        enddo
        
        
    !##### SUBROUTINES AND FUNCTIONS ##########################################
        
    contains
        
        
        function outerprod(a, b)
        
            real(8), intent(in) :: a(:), b(:)
            real(8) :: outerprod(size(a, 1),size(b, 1))
        
            outerprod = spread(a, dim=2, ncopies=size(b, 1)) * &
            spread(b, dim=1, ncopies=size(a, 1))
        
        end function outerprod
    !===============================================================================!
        
        
        subroutine swap(a, b)
        
            real(8), intent(inout) :: a(:), b(:)
            real(8) :: dum(size(a))
        
            dum = a
            a = b
            b = dum
        
        end subroutine swap
        
        
        function imaxloc(arr)
        
            real(8), intent(in) :: arr(:)
            integer :: imaxloc
            integer :: imax(1)
        
            imax = maxloc(arr(:))
            imaxloc = imax(1)
        
        end function imaxloc
    
    end subroutine lu_decomp
    !##############################################################################
    !
    ! Solves a lu decomposed linear equation system by backsubstitution.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    subroutine lu_back(a, indx, b)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! lu-decomposed matrix that defines system
        real(8), intent(in) :: a(:, :)
        
        ! row permutation indicator due to pivoting
        integer, intent(in) :: indx(:)
        
        ! right side of equation and solution of the system
        real(8), intent(inout) :: b(:)
        
        
        !##### OTHER VARIABLES ####################################################
        
        real(8) :: summ
        integer :: i, n, ii, ll
        
        
        !##### ROUTINE CODE #######################################################
        
        ! assert size equality
        n = assert_eq(size(a,1), size(a,2), size(indx), size(b), 'lu_back')
        
        ! start backward solving provess
        ii = 0
        do i = 1, n
        
            ll = indx(i)
            summ = b(ll)
            b(ll) = b(i)
            if(ii /= 0)then
                summ = summ-dot_product(a(i,ii:i-1), b(ii:i-1))
            elseif (abs(summ) >= 1d-100) then
                ii = i
            endif
            b(i)=summ
        enddo
        
        do i=n, 1, -1
            b(i) = (b(i)-dot_product(a(i,i+1:n), b(i+1:n)))/a(i,i)
        enddo
    
    end subroutine lu_back
    
    
end module linalg