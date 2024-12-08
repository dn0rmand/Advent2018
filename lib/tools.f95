module tools
   implicit none

   public :: openFile
   public :: closeFile
   public :: readLine
   public :: readInteger
   public :: itoa, itoHex
   public :: resizeArray
   public :: pause

   type, abstract :: sortable
      integer :: size = 0
   contains
      procedure :: sort => quickSort
      procedure(compare_shape), deferred :: compare
      procedure(swap_shape), deferred :: swap
   end type

   interface
      integer function compare_shape(this, i1, i2)
         import :: sortable
         class(sortable), intent(in) :: this
         integer, intent(in) :: i1, i2
      end function

      subroutine swap_shape(this, i1, i2)
         import :: sortable
         class(sortable), intent(inout) :: this
         integer, intent(in) :: i1, i2
      end subroutine
   end interface

contains

   integer function openFile(day)
      integer :: day
      integer :: ios
      integer, parameter :: handle = 10
      character(len=512) :: filename

      if (day > 9) then
         write (filename, '(A,I2,A)') './data/day', day, '.data'
      else
         write (filename, '(A,I1,A)') './data/day', day, '.data'
      end if
      ! print *, filename

      open (unit=handle, file=trim(filename), iostat=ios)
      if (ios .ne. 0) stop 'Error opening file '//trim(filename)

      openFile = handle
   end function openFile

   subroutine closeFile(handle)
      integer :: handle
      close (handle)
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

      read (handle, '(A)', iostat=ios) line
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
         read (line, *) number
         deallocate (line)
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

      allocate (B(0:size))

      B = A

      deallocate (A)
      allocate (A(0:newSize))
      A(0:size) = B

      deallocate (B)
   end subroutine resizeArray

   integer function partition(a, low, high)
      class(sortable), intent(inout) :: a
      integer, intent(in) :: low, high
      integer :: pivot, i, j, cmp

      pivot = high
      i = low - 1

      do j = low, high - 1
         cmp = a%compare(j, pivot)
         if (cmp <= 0) then
            i = i + 1
            call a%swap(i, j)
         end if
      end do

      call a%swap(i + 1, high)

      partition = i + 1
   end function

   recursive subroutine innerQuickSort(a, low, high)
      class(sortable), intent(inout) :: a
      integer, intent(in) :: low, high
      integer :: p

      if (low < high) then
         p = partition(a, low, high)
         call innerQuickSort(a, low, p - 1)
         call innerQuickSort(a, p + 1, high)
      end if
   end subroutine

   subroutine bubbleSort(a)
      class(sortable), intent(inout) :: a
      integer :: i

      i = 1
      do while (i < a%size)
         if (a%compare(i, i + 1) > 0) then
            call a%swap(i, i + 1)
            if (i > 1) then
               i = i - 1
            end if
         else
            i = i + 1
         end if
      end do
   end subroutine

   subroutine quickSort(this)
      class(sortable), intent(inout) :: this

      call innerQuickSort(this, 1, this%size)

      ! call bubbleSort(this)
   end subroutine

   function itoa(value) result(a)
      integer :: value
      character(20) :: result
      character(:), allocatable :: a

      write (result, '(I10)') value

      a = trim(adjustl(result))
   end function

   character(6) function itoHex(value)
      integer :: value
      character(6) :: result

      write (result, '(Z6.6)') value

      itoHex = result
   end function

   subroutine pause(delay)
      integer :: delay
      integer(kind=8) ::  endTime, i

      endTime = mclock8() + int(delay, 8)*1000
      i = mclock8()

      do while (i < endTime)
         i = mclock8()
      end do
   end subroutine

end module tools
