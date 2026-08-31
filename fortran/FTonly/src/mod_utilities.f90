module mod_utilities
	! Description:
	! - Write arrays (of real and integer type) of up to
	!   7 dimensions to text file or binary file columnwise
	! - Print a matrix to a file as (row,col)
	!
    
	! USE other modules
	implicit none
    
    ! Visible outside 
		
	!Declare module variables

	!Declare generic interface
    
    interface print1dim
		module procedure print1dim_i
		module procedure print1dim_r
    end interface
    
	interface printMatrix
		module procedure printMatrix_i
		module procedure printMatrix_r
    end interface
	
    interface writescalar
		module procedure writescalar_i
		module procedure writescalar_r
    end interface
    
    interface write1dimBinary
		module procedure write1dimBinary_i
		module procedure write1dimBinary_r
    end interface
    
    interface writeArr1
        module procedure writeArr1_i
        module procedure writeArr1_r
    end interface
    
    ! - write X-DIM array
	interface writedim
		module procedure write1dim_i
		module procedure write1dim_r
		module procedure write2dim_i
		module procedure write2dim_r
		module procedure write3dim_i
		module procedure write3dim_r
		module procedure write4dim_i
		module procedure write4dim_r
		module procedure write5dim_i
		module procedure write5dim_r  
		module procedure write6dim_i
		module procedure write6dim_r  
		module procedure write7dim_i
		module procedure write7dim_r  
    end interface
	
    ! - Read scalar
    interface readscalar
        module procedure readscalar_i
        module procedure readscalar_r
    end interface
    
	! - Read X-DIM array
    interface readdim
        module procedure read1dim_i
        module procedure read1dim_r
        module procedure read2dim_i
        module procedure read2dim_r
        module procedure read3dim_i
        module procedure read3dim_r
    end interface
    contains
    
    !Module procedures
    
  !**********************************************************
  ! PRINT VECTOR BASH
  !**********************************************************  
  subroutine print_vector(v)
  !**********************************************************
  ! PRINT_VECTOR prints a vector in the terminal
  ! Usage: 

  ! INPUTS
  !	v     	: vector to print 
  !  
  !***********************************************************
    !Local
    integer :: i
    
    !Dummy
    real(8), intent(in) :: v(:)

    do i = 1,size(v)
       write(*,*) v(i)
    end do
  end subroutine print_vector

  !**********************************************************
  ! PRINT MATRIX BASH
  !**********************************************************  
  subroutine print_matrix(v)
  !**********************************************************
  ! PRINT_MATRIX prints matrix in the terminal  
  ! Usage: 
    
  ! INPUTS
  !	v     : matrix to print 
  !
  !***********************************************************
      
    !Local
    integer :: i
    
    !Dummy
    real, intent(in) :: v(:,:)

    do i = 1,size(v,1)
       write(*,*) v(i,:)
    end do
  end subroutine print_matrix

  !**********************************************************
  ! PRINT MATRIX DATA FILE (.dat)
  !**********************************************************  
  subroutine print_matrix_dat(namefile,v)
  !**********************************************************
  ! PRINT_MATRIX_DAT print matrix in a dat file   
  ! Usage: 
    
  ! INPUTS
  ! namefile : name of the file    
  !	v        : matrix to print 
  !
  !***********************************************************
    !Local
    integer :: i
    
    !Dummy
    real, intent(in) :: v(:,:)
    character (len=*) :: namefile

    open(1, file=namefile, action='write', status='replace')

    do i = 1,size(v,1)
       write(1,*) v(i,:)
    end do
    close(1)
  end subroutine print_matrix_dat

  !**********************************************************
  ! PRINT VECTOR DATA FILE (.dat)
  !**********************************************************  
  subroutine print_vector_dat(namefile,v) 
  !**********************************************************
  ! PRINT_VECTOR_DAT prints vector in a dat file  
  ! Usage: 
    
  ! INPUTS
  !	namefile     : name of the dat file 
  !	v            : vector to print  
  !     n            : size of the vector
  !
  !***********************************************************
    !Local
    integer :: i
    
    !Dummy
    real, intent(in) :: v(:)
    character (len=*) :: namefile

    open(1, file=namefile, action='write', status = 'replace')

    do i = 1,size(v)
       write(1,*) v(i)
    end do
    close(1)
    
  end subroutine print_vector_dat
	
    !-----------------------------------------------------------------!
    !   PRINT VECTOR  
    !-----------------------------------------------------------------!
    subroutine print1dim_r(x)
    ! This subroutine prints a vector (1-dim array) x on the screen
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:)
    integer :: i
     
    !Display vector x on screen
    do i = 1,size(x)
        write(*,'(f12.6," ")'), x(i)
	enddo
	write(*,"()")  
    write(*,*) " "
    
    end subroutine print1dim_r
    !-----------------------------------------------------------------!
    
    subroutine print1dim_i(x)
    ! This subroutine prints a vector (1-dim array) x on the screen
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:)
    integer :: i
     
    !Display vector x on screen
    do i = 1,size(x)
        write(*,'(I6," ")'), x(i)
	enddo
	write(*,"()")  
    write(*,*) " "
    
    end subroutine print1dim_i
    
    !-----------------------------------------------------------------!
    !   PRINT MATRIX 
    !-----------------------------------------------------------------!
    subroutine printMatrix_r(x,file_name)
    ! This subroutine prints matrix (2-dim array) x on file file_name.
    ! If filename is not present, matrix is displayed on the screen.
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:,:)
    character(len=*), intent(in), optional :: file_name
    integer :: unitno, ierr, i, j
    
    if (present(file_name)) then
        !Write matrix x(row,col) into a txt file
        open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
        if (ierr/=0) then
            write(*,*) "Error: printMatrix: cannot open file"
		    pause
		    stop 
        endif
    
        do i = 1,size(x,1)
		    do j = 1,size(x,2)    
			    write(unitno,'(f12.6," ")',advance='no'), x(i,j)
                write(unitno,'(" ")', advance = 'no')
		    enddo
		    write(unitno,"()")  
        enddo
        write(unitno,*) " "
    
        close(unitno)
    
    else
        
        !Display matrix x(row,col) on screen
        do i = 1,size(x,1)
		do j = 1,size(x,2)
			write(*,'(f12.6," ")',advance='no'), x(i,j)
                	write(*,'(" ")', advance = 'no')
		enddo
		write(*,"()")  
        enddo
        write(*,*) " "
        
    endif
    
    end subroutine printMatrix_r
    !-----------------------------------------------------------------!
    subroutine printMatrix_i(x,file_name)
    ! This subroutine prints matrix (2-dim array) x on file file_name.
    ! If filename is not present, matrix is displayed on the screen.
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:,:)
    character(len=*), intent(in), optional :: file_name
    integer :: unitno, ierr, i, j
    
    if (present(file_name)) then
        !Write integer matrix x(row,col) into a txt file
        open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
        if (ierr/=0) then
            write(*,*) "Error: printMatrix: cannot open file"
		    pause
		    stop 
        endif
    
        do i = 1,size(x,1)
		    do j = 1,size(x,2)
			    write(unitno,'(I6)',advance='no'), x(i,j)
		    enddo
		    write(unitno,*) " "
        enddo
        write(unitno,*) " "
    
        close(unitno)
    
    else
        
        !Write integer matrix x(row,col) into a txt file
        do i = 1,size(x,1)
		    do j = 1,size(x,2)
			    write(*,'(I6)',advance='no'), x(i,j)
		    enddo
		    write(*,*) " "
        enddo
        write(*,*) " "
        
    endif
    
    end subroutine printMatrix_i
    
    !-----------------------------------------------------------------!
    !   WRITE SCALARS
    !-----------------------------------------------------------------!
    subroutine writescalar_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x
    character(len=*), intent(in) :: file_name
    integer:: unitno,ierr
    
    !Write scalar x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: writescalar: cannot open file"
		pause
		stop 
    endif
    
    write(unitno,*) x

    close(unitno)
    
    end subroutine writescalar_r
    !-----------------------------------------------------------------!
    subroutine writescalar_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x
    character(len=*), intent(in) :: file_name
    integer:: unitno,ierr
    
    !Write scalar x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: writescalar: cannot open file"
		pause
		stop 
    endif
    
    write(unitno,*) x

    close(unitno)
    
    end subroutine writescalar_i
    !-----------------------------------------------------------------!
    
    subroutine write1dimBinary_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:)
    character(len=*), intent(in) :: file_name
    integer:: unitno,ierr
    
    !Write 1 dim array x into a BINARY file
    !Note the commands FORM="unformatted", ACCESS="stream"
    !and the fact that write(unitno) instead of write(unitno,*)
    OPEN(NEWUNIT=unitno, FILE=file_name, FORM="unformatted", ACCESS="stream", STATUS="unknown", iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write1dimBinary: cannot open file"
		pause
		stop 
    endif
    
     
    write(unitno) x
    
    close(unitno)
    
    end subroutine write1dimBinary_r
    !-----------------------------------------------------------------!
    
    subroutine write1dimBinary_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:)
    character(len=*), intent(in) :: file_name
    integer :: unitno, ierr
    
    !Write 1 dim array x into a BINARY file
    !Note the commands FORM="unformatted", ACCESS="stream"
    !and the fact that write(unitno) instead of write(unitno,*)
    OPEN(NEWUNIT=unitno, FILE=file_name, FORM="unformatted", ACCESS="stream", STATUS="unknown", iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write1dimBinary: cannot open file"
		pause
		stop 
    endif
    
        
    write(unitno) x
    
    close(unitno)
    
    end subroutine write1dimBinary_i
    !-----------------------------------------------------------------!
    
    !-----------------------------------------------------------------!
    !   WRITE 1-D ARRAY as single line (faster than write1dim) 
    !-----------------------------------------------------------------!
    subroutine writeArr1_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:)
    character(len=*), intent(in) :: file_name
    character(len=50) :: file_format
    integer :: unitno, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write1dim: cannot open file"
		pause
		stop 
    endif
    
   
    write(file_format,*) "(",size(x),"(F,:,','))"
    
    write(unitno,trim(file_format)) x
    
    close(unitno)
    
    end subroutine writeArr1_r
    !-----------------------------------------------------------------!
    
    subroutine writeArr1_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:)
    character(len=*), intent(in) :: file_name
    character(len=50) :: file_format
    integer:: unitno,ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write1dim: cannot open file"
		pause
		stop 
    endif
    
    !Assume size(x) is equal to 330000
    write(file_format,*) "(",size(x),"(I,:,','))"
    
    write(unitno,trim(file_format)) x
    
    close(unitno)
    
    end subroutine writeArr1_i
    !-----------------------------------------------------------------!
    
    !-----------------------------------------------------------------!
    !   WRITE 1 DIM ARRAYS as text files
    !-----------------------------------------------------------------!
    subroutine write1dim_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:)
    character(len=*), intent(in) :: file_name
    integer :: unitno, i, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write1dim: cannot open file"
		pause
		stop 
    endif
    
    do i = 1,size(x)
        write(unitno,*) x(i)
    enddo
    
    close(unitno)
    
    end subroutine write1dim_r
    !-----------------------------------------------------------------!
    subroutine write1dim_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write1dim: cannot open file"
		pause
		stop 
    endif
    
    
    do i = 1,size(x)
        write(unitno,*) x(i)
    enddo
    
    close(unitno)
    
    end subroutine write1dim_i
    !-----------------------------------------------------------------!
    !   WRITE 2 DIM ARRAYS
    !-----------------------------------------------------------------!
    subroutine write2dim_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write2dim: cannot open file"
		pause
		stop 
    endif
    
    do i2 = 1,size(x,dim=2)
        do i1 = 1,size(x,dim=1)
            write(unitno,*) x(i1,i2)
        enddo
    enddo
    
    close(unitno)
    
    end subroutine write2dim_r
    !-----------------------------------------------------------------!
    subroutine write2dim_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2,ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write2dim: cannot open file"
		pause
		stop 
    endif
    
    do i2 = 1,size(x,dim=2)
        do i1 = 1,size(x,dim=1)
            write(unitno,*) x(i1,i2)
        enddo
    enddo
    
    close(unitno)
    
    end subroutine write2dim_i
    !-----------------------------------------------------------------!
    !   WRITE 3 DIM ARRAYS
    !-----------------------------------------------------------------!
    subroutine write3dim_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
    	
        write(*,*) "Error: write3dim: cannot open file ", file_name
        write(*,*) "ierr = ",ierr
		pause
		stop 
    endif
    
    do i3 = 1,size(x,dim=3)
        do i2 = 1,size(x,dim=2)
            do i1 = 1,size(x,dim=1)
                write(unitno,*) x(i1,i2,i3)
            enddo
        enddo
    enddo
    
    close(unitno)
    
    end subroutine write3dim_r
    !-----------------------------------------------------------------!
    subroutine write3dim_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write3dim: cannot open file ", file_name
		pause
		stop 
    endif
    
    do i3 = 1,size(x,dim=3)
        do i2 = 1,size(x,dim=2)
            do i1 = 1,size(x,dim=1)
                write(unitno,*) x(i1,i2,i3)
            enddo
        enddo
    enddo
    
    close(unitno)
    
    end subroutine write3dim_i
    !-----------------------------------------------------------------!
    !   WRITE 4 DIM ARRAYS
    !-----------------------------------------------------------------!
    subroutine write4dim_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:,:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, i4, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write4dim: cannot open file"
		pause
		stop 
    endif
    
    do i4 = 1,size(x,dim=4)
        do i3 = 1,size(x,dim=3)
            do i2 = 1,size(x,dim=2)
                do i1 = 1,size(x,dim=1)
                    write(unitno,*) x(i1,i2,i3,i4)
                enddo
            enddo
        enddo
    enddo
    
    close(unitno)
    
    end subroutine write4dim_r
    !-----------------------------------------------------------------!
    subroutine write4dim_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:,:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, i4, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write4dim: cannot open file"
		pause
		stop 
    endif
    
    do i4 = 1,size(x,dim=4)
        do i3 = 1,size(x,dim=3)
            do i2 = 1,size(x,dim=2)
                do i1 = 1,size(x,dim=1)
                    write(unitno,*) x(i1,i2,i3,i4)
                enddo
            enddo
        enddo
    enddo
    
    close(unitno)
    
    end subroutine write4dim_i
    !-----------------------------------------------------------------!
    
    !-----------------------------------------------------------------!
    !   WRITE 5 DIM ARRAYS
    !-----------------------------------------------------------------!
    subroutine write5dim_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:,:,:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, i4,i5, ierr
    
    !Write 5-dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write6dim: cannot open file"
		pause
		stop 
    endif
    
    do i5 = 1,size(x,dim=5)
    do i4 = 1,size(x,dim=4)
    do i3 = 1,size(x,dim=3)
    do i2 = 1,size(x,dim=2)
    do i1 = 1,size(x,dim=1)
        write(unitno,*) x(i1,i2,i3,i4,i5)
    enddo
    enddo
    enddo
    enddo
    enddo
    
    close(unitno)
    
    end subroutine write5dim_r
    !-----------------------------------------------------------------!
    subroutine write5dim_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:,:,:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, i4,i5, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write6dim: cannot open file"
		pause
		stop 
    endif
    
    do i5 = 1,size(x,dim=5)
    do i4 = 1,size(x,dim=4)
    do i3 = 1,size(x,dim=3)
    do i2 = 1,size(x,dim=2)
    do i1 = 1,size(x,dim=1)
        write(unitno,*) x(i1,i2,i3,i4,i5)
    enddo
    enddo
    enddo
    enddo
    enddo
    
    close(unitno)
    
    end subroutine write5dim_i
    !-----------------------------------------------------------------!
    
    !-----------------------------------------------------------------!
    !   WRITE 6 DIM ARRAYS
    !-----------------------------------------------------------------!
    subroutine write6dim_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:,:,:,:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, i4,i5,i6, ierr
    
    !Write 6-dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write6dim: cannot open file"
		pause
		stop 
    endif
    
    do i6 = 1,size(x,dim=6)
    do i5 = 1,size(x,dim=5)
    do i4 = 1,size(x,dim=4)
    do i3 = 1,size(x,dim=3)
    do i2 = 1,size(x,dim=2)
    do i1 = 1,size(x,dim=1)
        write(unitno,*) x(i1,i2,i3,i4,i5,i6)
    enddo
    enddo
    enddo
    enddo
    enddo
    enddo
    
    close(unitno)
    
    end subroutine write6dim_r
    !-----------------------------------------------------------------!
    subroutine write6dim_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:,:,:,:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, i4,i5,i6, ierr
    
    !Write 1 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write6dim: cannot open file"
		pause
		stop 
    endif
    
    do i6 = 1,size(x,dim=6)
    do i5 = 1,size(x,dim=5)
    do i4 = 1,size(x,dim=4)
    do i3 = 1,size(x,dim=3)
    do i2 = 1,size(x,dim=2)
    do i1 = 1,size(x,dim=1)
        write(unitno,*) x(i1,i2,i3,i4,i5,i6)
    enddo
    enddo
    enddo
    enddo
    enddo
    enddo
    
    close(unitno)
    
    end subroutine write6dim_i
    !-----------------------------------------------------------------!
    
    !-----------------------------------------------------------------!
    !   WRITE 7 DIM ARRAYS
    !-----------------------------------------------------------------!
    subroutine write7dim_r(x,file_name)
        
    implicit none
    !Declare inputs:
    real(8), intent(in) :: x(:,:,:,:,:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, i4,i5,i6,i7, ierr
    
    !Write 7 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write7dim: cannot open file"
		pause
		stop 
    endif
    
    do i7 = 1,size(x,dim=7)
    do i6 = 1,size(x,dim=6)
    do i5 = 1,size(x,dim=5)
    do i4 = 1,size(x,dim=4)
    do i3 = 1,size(x,dim=3)
    do i2 = 1,size(x,dim=2)
    do i1 = 1,size(x,dim=1)
        write(unitno,*) x(i1,i2,i3,i4,i5,i6,i7)
    enddo
    enddo
    enddo
    enddo
    enddo
    enddo
    enddo
    
    close(unitno)
    
    end subroutine write7dim_r
    !-----------------------------------------------------------------!
    subroutine write7dim_i(x,file_name)
        
    implicit none
    !Declare inputs:
    integer, intent(in) :: x(:,:,:,:,:,:,:)
    character(len=*), intent(in) :: file_name
    integer :: unitno,i1,i2, i3, i4,i5,i6,i7, ierr
    
    !Write 7 dim array x into a txt file
    open(newunit=unitno, file=file_name, status='replace',  iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error: write7dim: cannot open file"
		pause
		stop 
    endif
    
    do i7 = 1,size(x,dim=7)
    do i6 = 1,size(x,dim=6)
    do i5 = 1,size(x,dim=5)
    do i4 = 1,size(x,dim=4)
    do i3 = 1,size(x,dim=3)
    do i2 = 1,size(x,dim=2)
    do i1 = 1,size(x,dim=1)
        write(unitno,*) x(i1,i2,i3,i4,i5,i6,i7)
    enddo
    enddo
    enddo
    enddo
    enddo
    enddo
    enddo
    
    close(unitno)
    
    end subroutine write7dim_i
    !-----------------------------------------------------------------!
    
    !=======================================================================!
    subroutine readscalar_i(x,file_name)
    
    implicit none
    
    character(len=*), intent(in) :: file_name
    integer, intent(out) :: x
    integer:: unitno,ierr
    
    open(newunit=unitno, file=file_name, status='old', iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error in readscalar: cannot open file"
		pause
		stop 
    endif
    
    read(unitno,*) x
    
    close(unitno)
    
    end subroutine readscalar_i
    !=======================================================================!
    
    subroutine readscalar_r(x,file_name)
    
    implicit none
    
    character(len=*), intent(in) :: file_name
    real(8), intent(out) :: x
    integer:: unitno,ierr
    
    open(newunit=unitno, file=file_name, status='old', iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error in readscalar: cannot open file"
		pause
		stop 
    endif
    
    read(unitno,*) x
    
    close(unitno)
    
    end subroutine readscalar_r
    !=======================================================================!
    
    subroutine read1dim_i(x,file_name)
    
    implicit none
    
    character(len=*), intent(in) :: file_name
    integer, intent(out) :: x(:)
    integer :: unitno,i, ierr
    
    open(newunit=unitno, file=file_name, status='old', iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error in read1dim: cannot open file"
		pause
		stop 
    endif
    
    do i = 1,size(x)
        read(unitno,*) x(i)
    enddo
    close(unitno)
    
    end subroutine read1dim_i
    !=======================================================================!
    
    subroutine read1dim_r(x,file_name)
    
    implicit none
    
    character(len=*), intent(in) :: file_name
    real(8), intent(out) :: x(:)
    integer :: unitno,i, ierr
    
    open(newunit=unitno, file=file_name, status='old', iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error in read1dim: cannot open file"
		pause
		stop 
    endif
    
    do i = 1,size(x)
        read(unitno,*) x(i)
    enddo
    close(unitno)
    
    end subroutine read1dim_r
    !=======================================================================!
    
    subroutine read2dim_i(x,file_name)
    
    implicit none
    
    character(len=*), intent(in) :: file_name
    integer, intent(out) :: x(:,:)
    integer :: unitno,i1,i2, ierr
    
    open(newunit=unitno, file=file_name, status='old', iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error in read2dim: cannot open file"
		pause
		stop 
    endif
    
    do i2 = 1,size(x,dim=2)
        do i1 = 1,size(x,dim=1)
            read(unitno,*) x(i1,i2)
        enddo
    enddo
    
    close(unitno)
    
    end subroutine read2dim_i
    !=======================================================================!
    
    subroutine read2dim_r(x,file_name)
    
    implicit none
    
    external getcwd
    
    character(len=*), intent(in) :: file_name
    real(8), intent(out) :: x(:,:)
    integer :: unitno,i1,i2, ierr
    
    open(newunit=unitno, file=file_name, status='old', iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error in read2dim: cannot open file"
		pause
		stop 
    endif
    
    do i2 = 1,size(x,dim=2)
        do i1 = 1,size(x,dim=1)
            read(unitno,*) x(i1,i2)
        enddo
    enddo
    
    close(unitno)
    
    end subroutine read2dim_r
    !=======================================================================!
    
    subroutine read3dim_i(x,file_name)
    
    implicit none
    
    character(len=*), intent(in) :: file_name
    integer, intent(out) :: x(:,:,:)
    integer :: unitno,i1,i2,i3, ierr
    
    open(newunit=unitno, file=file_name, status='old', iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error in read3dim: cannot open file"
		pause
		stop 
    endif
    
    do i3 = 1,size(x,dim=3)
        do i2 = 1,size(x,dim=2)
            do i1 = 1,size(x,dim=1)
                read(unitno,*) x(i1,i2,i3)
            enddo
        enddo
    enddo
    
    close(unitno)
    
    end subroutine read3dim_i
    !=======================================================================!
    
    subroutine read3dim_r(x,file_name)
    
    implicit none
    
    external getcwd
    
    character(len=*), intent(in) :: file_name
    real(8), intent(out) :: x(:,:,:)
    integer :: unitno,i1,i2,i3, ierr
    
    open(newunit=unitno, file=file_name, status='old', iostat=ierr)
    if (ierr/=0) then
        write(*,*) "Error in read3dim: cannot open file"
		pause
		stop 
    endif
    
    do i3 = 1,size(x,dim=3)
        do i2 = 1,size(x,dim=2)
            do i1 = 1,size(x,dim=1)
                read(unitno,*) x(i1,i2,i3)
            enddo
        enddo
    enddo
    
    close(unitno)
    
    end subroutine read3dim_r
    !=======================================================================!
	! Acts just like the colon in Matlab
	pure function colon(a,b)
		implicit none
		integer, intent(in) :: a,b
		integer, dimension(1:b-a+1) :: colon
		integer :: i
		do i = a,b
			colon(i-a+1) = i
		end do
	end function colon

    
end module mod_utilities
