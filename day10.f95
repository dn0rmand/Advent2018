module Day10

   use tools
   use ocr
   use screen

   implicit none

   private :: Part1, Part2
   public :: Day10Solve

   type :: t_point
      integer ::  x, y, vx, vy
   contains
      procedure :: move => point_move
   end type

   type :: t_input
      integer :: size, minX, maxX, minY, maxY
      type(t_point) :: points(350)
   contains
      procedure :: move => input_move
      procedure :: translate => input_translate
      procedure :: height => input_height
      procedure :: width => input_width
   end type

contains

   type(t_point) function point_move(point, time)
      class(t_point), intent(in)  :: point
      type(t_point) :: pt
      integer, intent(in) :: time

      pt%x = point%x + time*point%vx
      pt%y = point%y + time*point%vy

      point_move = pt
   end function

   type(t_input) function input_move(input, time)
      class(t_input), intent(in), target :: input
      integer, intent(in) :: time
      type(t_input) :: output
      type(t_point) :: point
      integer :: i

      output%size = input%size
      output%minx = 9999
      output%miny = 9999
      output%maxx = -9999
      output%maxy = -9999

      do i = 1, input%size
         point = input%points(i)%move(time)
         output%points(i) = point

         output%minX = min(output%minX, point%x)
         output%maxX = max(output%maxX, point%x)
         output%minY = min(output%minY, point%y)
         output%maxY = max(output%maxY, point%y)
      end do

      input_move = output
   end function

   integer function input_height(input)
      class(t_input), intent(in)  :: input

      input_height = abs(input%maxY - input%minY) + 1
   end function

   integer function input_width(input)
      class(t_input), intent(in)  :: input

      input_width = abs(input%maxX - input%minX) + 1
   end function

   character(10) function input_translate(input)
      class(t_input), intent(in), target :: input
      type(t_point), pointer :: point
      type(t_screen) :: screen
      integer :: i, width, height, x, y

      width = input%width()
      height = input%height()

      call screen%create(width, height)

      do i = 1, input%size
         point => input%points(i)
         x = point%x - input%minX + 1
         y = point%y - input%minY + 1
         call screen%set(x, y, .true.)
      end do
      input_translate = translate(screen)
      ! x = 1
      ! y = 0
      ! call screen%print(x)
      ! call screen%print(y)
      call screen%free()
   end function

   type(t_input) function loadInput()
      type(t_input) :: input
      integer :: file
      character(:), allocatable :: line
      type(t_point) :: point

      file = openFile(10)

      input%size = 0
      do while (readLine(file, line))
         read (line(11:24), *) point%x, point%y
         read (line(37:42), *) point%vx, point%vy
         deallocate (line)
         input%size = input%size + 1
         input%points(input%size) = point
         if (input%size == 1) then
            input%minX = point%x
            input%maxX = point%x
            input%minY = point%y
            input%maxY = point%y
         else
            input%minX = min(input%minX, point%x)
            input%maxX = max(input%maxX, point%x)
            input%minY = min(input%minY, point%y)
            input%maxY = max(input%maxY, point%y)
         end if
      end do

      call closeFile(file)

      loadInput = input
   end function

   character(10) function Part1(input, time)
      type(t_input) :: input
      integer :: time

      input = input%move(time)

      Part1 = input%translate()
   end function

   integer function Part2(input)
      type(t_input) :: input
      type(t_input) :: output
      integer :: time, height, previousHeight

      time = -1
      previousHeight = input%height()
      height = input%height()

      do while (height <= previousHeight)
         time = time + 1
         previousHeight = height
         output = input%move(time + 1)
         height = output%height()
      end do

      Part2 = time
   end function

   subroutine Day10Solve()
      character(10) :: answer1
      integer :: answer2

      type(t_input) :: input

      print *, '--- Day 10 ---'

      input = loadInput()

      answer2 = Part2(input)
      answer1 = Part1(input, answer2)

      print *, 'Answer to part 1 is ', answer1
      print *, 'Answer to part 2 is ', answer2
   end subroutine

end module
