module Day23

   use tools

   implicit none

   private :: Part1, Part2
   public :: Day23Solve

   type :: t_nanobot
      integer :: x, y, z, r
   end type

   type, extends(sortable) :: t_input
      type(t_nanobot), dimension(1100) :: bots
   contains
      procedure :: distance
      procedure :: compare => input_compare
      procedure :: swap => input_swap
   end type

contains

   integer function input_compare(this, i1, i2)
      class(t_input), intent(in) :: this
      integer, intent(in) :: i1, i2

      input_compare = this%bots(i2)%r - this%bots(i1)%r
   end function

   subroutine input_swap(this, i1, i2)
      class(t_input), intent(inout) :: this
      integer, intent(in) :: i1, i2
      type(t_nanobot) :: b1, b2

      b1 = this%bots(i1)
      b2 = this%bots(i2)
      this%bots(i1) = b2
      this%bots(i2) = b1
   end subroutine

   type(t_input) function loadInput()
      integer :: file, i
      type(t_nanobot) :: bot
      type(t_input) :: input
      character :: c
      character(:), allocatable :: line

      file = openFile(23)

      do while (readLine(file, line))
         ! cleanup the line
         do i = 1, len(line)
            c = line(i:i)
            if ((c < '0' .or. c > '9') .and. c /= '-') then
               line(i:i) = ' '
            end if
         end do

         read (line, *) bot%x, bot%y, bot%z, bot%r
         input%size = input%size + 1
         input%bots(input%size) = bot
         deallocate (line)
      end do

      call closeFile(file)

      call input%sort()

      loadInput = input
   end function

   integer function distance(this, i1, i2)
      class(t_input), intent(in) :: this
      integer, intent(in) :: i1, i2
      type(t_nanobot) :: p1, p2

      p1 = this%bots(i1)
      p2 = this%bots(i2)
      distance = abs(p1%x - p2%x) + abs(p1%y - p2%y) + abs(p1%z - p2%z)
   end function

   integer function Part1()
      type(t_input) :: input
      integer :: i, count
      integer :: r

      input = loadInput()

      count = 1
      r = input%bots(1)%r
      do i = 2, input%size
         if (input%distance(1, i) <= r) then
            count = count + 1
         end if
      end do

      Part1 = count
   end function

   integer function Part2()
      Part2 = 0
   end function

   subroutine Day23Solve()
      integer :: answer

      print *, '--- Day 23 ---'

      answer = Part1()
      print *, 'Answer to part 1 is ', answer

      answer = Part2()
      print *, 'Answer to part 2 is ', answer
   end subroutine

end module
