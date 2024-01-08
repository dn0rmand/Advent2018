module Day8

   use tools

   implicit none

   private :: Part1, Part2
   public :: Day8Solve

   integer, parameter :: input_size = 16721

   type :: t_input
      integer, dimension(input_size) :: data
   contains
      procedure :: checksum => input_checksum
      procedure :: value => input_value
   end type

contains

   type(t_input) function loadInput()
      integer :: file
      integer :: ios
      type(t_input), allocatable :: input

      allocate (input)
      file = openFile(8)
      read (file, *, iostat=ios) input%data
      call closeFile(file)

      loadInput = input
   end function

   integer recursive function input_checksum(input, index)
      class(t_input), intent(in) :: input
      integer, intent(inout) :: index
      integer ::  metadataCount, childCount, i, sum

      childCount = input%data(index)
      metadataCount = input%data(index + 1)
      index = index + 2

      sum = 0
      do i = 1, childCount
         sum = sum + input%checksum(index)
      end do

      do i = 1, metadataCount
         sum = sum + input%data(index)
         index = index + 1
      end do

      input_checksum = sum
   end function

   integer recursive function input_value(input, index)
      class(t_input), intent(in) :: input
      integer, intent(inout) :: index
      integer, dimension(:), allocatable :: values
      integer ::  metadataCount, childCount, i, sum, idx

      childCount = input%data(index)
      metadataCount = input%data(index + 1)
      index = index + 2

      sum = 0

      if (childCount > 0) then
         allocate (values(childCount))

         do i = 1, childCount
            values(i) = input%value(index)
         end do

         do i = 1, metadataCount
            idx = input%data(index)
            index = index + 1

            if (idx >= 1 .and. idx <= childCount) then
               sum = sum + values(idx)
            end if
         end do

         deallocate (values)
      else
         do i = 1, metadataCount
            sum = sum + input%data(index)
            index = index + 1
         end do
      end if

      input_value = sum
   end function

   integer function Part1()
      type(t_input), allocatable :: input
      integer :: index = 1

      input = loadInput()
      Part1 = input%checksum(index)

      deallocate (input)
   end function

   integer function Part2()
      type(t_input), allocatable :: input
      integer :: index = 1

      input = loadInput()
      Part2 = input%value(index)

      deallocate (input)
   end function

   subroutine Day8Solve()
      integer :: answer

      print *, '--- Day 8 ---'

      answer = Part1()
      print *, 'Answer to part 1 is ', answer

      answer = Part2()
      print *, 'Answer to part 2 is ', answer
   end subroutine

end module
