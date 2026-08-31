Module spline !Module contains spline routines and a tridiagonal solver.
  implicit none
  private
  public splint,splintder,nkcspline
contains
  subroutine splint(X,Y,Y2,eval,outp) !Evaluation of cubic spline. Takes vector of y''(n), n=1...N as input along with x,y as the original fuction evaluation grid.
    implicit none
    real(8), intent(in) :: X(:),Y(:),Y2(:),eval
    real(8), intent(out) :: outp
    integer :: klo,khi,N,k
    real(8) :: h,a,b
    N=size(X)
    klo=1
    khi=N
    do while (khi-klo .gt. 1)
       k=(khi+klo)/2
       if(x(k) .gt. eval) then
          khi=k
       else
          klo=k
       end if
    end do
    h=x(khi)-x(klo)
    if(h .eq. dble(0)) then
    	write(*,*) 'Bad X input. Press return to continue.'
    	read(*,*) 
	end if
    a=(x(khi)-eval)/h
    b=(eval-x(klo))/h
    outp=a*y(klo)+b*y(khi)+((a**3-a)*y2(klo)+(b**3-b)*y2(khi))*(h**2)/dble(6)
  end subroutine splint

  subroutine splintder(X,Y,Y2,eval,outp) !Evaluation of cubic spline first derivative. Takes vector of y''(n), n=1...N as input along with x,y as the original fuction evaluation grid.
    implicit none
    real(8), intent(in) :: X(:),Y(:),Y2(:),eval
    real(8), intent(out) :: outp
    integer :: klo,khi,N,k
    real(8) :: h,g,a,b
    N=size(X)
    klo=1
    khi=N
    do while (khi-klo .gt. 1)
       k=(khi+klo)/2
       if(x(k) .gt. eval) then
          khi=k
       else
          klo=k
       end if
    end do
    h=x(khi)-x(klo)
    g=y(khi)-y(klo)
    if(h .eq. dble(0)) then
    	write(*,*) 'Bad X input. Press return to continue.'
    	read(*,*) 
	end if
    a=(x(khi)-eval)/h
    b=(eval-x(klo))/h
    outp=g/h - ((3.0d0*a**2-1.0d0)*y2(klo)+(3.0d0*b**2-1.0d0)*y2(khi))*h/dble(6)
  end subroutine splintder

  subroutine nkcspline(X,Y,Y2) !Not-a-knot cubic spline. Third derivatives are continuous at x(2) and x(N-1)
    implicit none
    real(8), intent(in) :: X(:),Y(:)
    real(8), intent(out) :: Y2(size(X))
    integer :: N,i
    real(8) :: A(size(X)),B(size(X)),C(size(X)),R(size(X)),l1,l2
    N=size(X)
    A(1)=dble(0)
    l1=x(2)-x(1)
    l2=x(3)-x(2)
    B(1)=(l1*l1-l2*l2)/l1
    C(1)=(x(3)-x(1))*(l2+dble(2)*l1)/l1
    R(1)=dble(6)*((y(3)-y(2))/l2-(y(2)-y(1))/l1)
    do i=2,N-1
       A(i)=(x(i)-x(i-1))/dble(6)
       B(i)=(x(i+1)-x(i-1))/dble(3)
       C(i)=(x(i+1)-x(i))/dble(6)
       R(i)=(y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1))
    end do
    l1=x(N)-x(N-1)
    l2=x(N-1)-x(N-2)
    A(N)=(x(N)-x(N-2))*(dble(2)*l1+l2)/l1
    B(N)=(l1*l1-l2*l2)/l1
    C(N)=dble(0)
    R(N)=dble(6)*((y(N)-y(N-1))/l1-(y(N-1)-y(N-2))/l2)
    call tridiag(A,B,C,R,Y2)
  end subroutine nkcspline

  subroutine tridiag(inp_A,inp_B,inp_C,inp_R,outp) !Solver for Tridiagonal linear system.
    implicit none
    real(8), intent(in) :: inp_A(:),inp_B(:),inp_C(:),inp_R(:)
    real(8), intent(out) :: outp(size(inp_A))
    real(8), allocatable :: A(:),B(:),C(:),R(:),U(:),gam(:)
    real(8) :: bet
    integer :: ldim,i
    if(inp_B(1) .eq. dble(0)) then		!True branch transforms the problem with the trivial solution for outp(2)
       ldim=size(inp_A)-1
       allocate(A(ldim),B(ldim),C(ldim),R(ldim),U(ldim),gam(ldim))
       A=dble(0)
       B=dble(0)
       C=dble(0)
       R(1)=inp_R(2)-inp_B(2)*inp_R(1)/inp_C(1)
       R(2)=inp_R(3)-inp_A(3)*inp_R(1)/inp_C(1)
       B(1)=inp_A(2)
       C(1)=inp_C(2)
       B(2)=inp_B(3)
       C(2)=inp_C(3)
       do i=3,ldim
          A(i)=inp_A(i+1)
          B(i)=inp_B(i+1)
          C(i)=inp_C(i+1)
          R(i)=inp_R(i+1)
       end do
    else								!False branch uses the original tridiagonal structure
       ldim=size(inp_A)
       allocate(A(ldim),B(ldim),C(ldim),R(ldim),U(ldim),gam(ldim))
       A=inp_A
       B=inp_B
       C=inp_C
       R=inp_R
    end if
    bet=B(1)
    U(1)=R(1)/bet
    do i=2,ldim
       gam(i)=C(i-1)/bet
       bet=B(i)-A(i)*gam(i)
       if(bet .eq. dble(0)) then
       		write(*,*) 'Tridiag algorithm failed. Press return to continue.'
       		read(*,*)
       	end if
       U(i)=(R(i)-A(i)*U(i-1))/bet
    end do
    do i=ldim-1,1,-1
       U(i)=U(i)-gam(i+1)*U(i+1)
    end do
    if(ldim .eq. size(outp)) then
       outp=U
    else
       outp(1)=U(1)
       outp(2)=inp_R(1)/inp_C(1)
       do i=2,ldim
          outp(i+1)=U(i)
       end do
    end if
  end subroutine tridiag
end Module spline
