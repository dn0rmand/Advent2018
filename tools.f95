module tools
  implicit none

  public :: openFile
  public :: closeFile
  public :: readLine
  public :: readInteger
  ! public :: isEOF

  public :: resizeArray

contains

integer function openFile(day)
  integer :: day
  integer :: ios
  integer, parameter :: handle = 10
  character(len=512) :: filename

  if (day > 9) then
    write(filename, '(A,I2,A)') './data/day', day, '.data'
  else
    write(filename, '(A,I1,A)') './data/day', day, '.data'
  end if
  ! print *, filename  

  open(unit=handle, file=trim(filename), iostat=ios)
  if (ios .ne. 0) stop 'Error opening file ' // trim(filename)

  openFile = handle
end function openFile

subroutine closeFile(handle)
  integer :: handle
  close(handle)
end subroutine closeFile

! logical function isEOF(handle)
!   integer :: handle
!   logical :: isOpened

!   INQUIRE(unit=handle, opened=isOpened)
!   isEOF = .not. isOpened
! end function isEOF

logical function readLine(handle, resultLine)
  integer :: handle
  integer :: ios
  character(len=60000) :: line
  character(:), allocatable, intent(out) :: resultLine

  read(handle, '(A)', iostat=ios) line
  if (ios == 0) then 
    resultLine = trim(line)
    readLine = .true.
  else 
    resultLine = ''
    readLine = .false.
  end if
end function readLine

logical function readInteger(handle, number)
  integer :: handle
  integer, intent(out) :: number
  character(:), allocatable :: line

  if (readLine(handle, line)) then
    read(line , *) number
    readInteger = .true.
  else
    number = 0
    readInteger = .false.
  end if
end function readInteger

subroutine resizeArray(A, size, newSize)
  implicit none
  integer, dimension(:), allocatable, intent(inout) :: A
  integer, intent(in) :: size
  integer, intent(in) :: newSize
 
  integer, dimension(:), allocatable :: B
  
  allocate(B(0:size))
    
  B = A
  
  deallocate(A)
  allocate(A(0:newSize))
  A(0:size) = B
  
  deallocate(B)
end subroutine resizeArray

end module tools