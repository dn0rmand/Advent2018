module Day17

   use tools

   implicit none

   private :: Part1, Part2
   public :: Day17Solve

   integer, parameter :: SPRING_X = 500, SPRING_Y = 0
   integer, parameter :: MIN_X = 450, MAX_X = 750, MIN_Y = 0, MAX_Y = 2000

   integer(kind=1), parameter :: WALL = 1, WATER = 2

   type :: t_input
      integer(kind=1), dimension(MIN_X:MAX_X, 0:MAX_Y) :: data
      integer :: minX, maxX, maxY
   contains
      procedure :: dump => input_dump
   end type
contains

   subroutine input_dump(input)
      class(t_input), intent(in) :: input
      integer :: x, y, i
      character(300) :: line

      line = ''
      i = 500 - input%minX + 1
      line(i:i) = '+'
      print *, trim(line)
      do y = 1, input%maxY
         line = ''
         do x = input%minX, input%maxX
            i = x - input%minX + 1
            if (input%data(x, y) == WALL) then
               line(i:i) = '#'
            else if (input%data(x, y) == WATER) then
               line(i:i) = '~'
            end if
         end do
         print *, trim(line)
      end do
   end subroutine

   type(t_input) function loadInput()
      integer :: file
      character(:), allocatable :: line
      character :: ch
      integer :: a, b, c, x, y
      type(t_input), allocatable  :: input

      allocate (input)

      input%minX = 10000
      input%maxX = 0
      input%maxY = 0

      file = openFile(17)

      do while (readLine(file, line))
         ch = line(1:1)
         do a = 1, len(line)
            if (line(a:a) < '0' .or. line(a:a) > '9') then
               line(a:a) = ' '
            end if
         end do
         read (line, *) a, b, c
         if (ch == 'x') then
            input%minX = min(a - 1, input%minX)
            input%maxX = max(a + 1, input%maxX)
            input%maxY = max(c, input%maxY)
            x = a
            do y = b, c
               input%data(x, y) = WALL
            end do
         else
            input%minX = min(b - 1, input%minX)
            input%maxX = max(c + 1, input%maxX)
            input%maxY = max(a, input%maxY)

            y = a
            do x = b, c
               input%data(x, y) = WALL
            end do
         end if

         deallocate (line)
      end do

      call closeFile(file)

      loadInput = input
   end function

   integer function Part1()
      type(t_input), allocatable :: input
      input = loadInput(); 
      call input%dump()
      Part1 = 0
      deallocate (input)
   end function

   integer function Part2()
      type(t_input), allocatable :: input
      input = loadInput(); 
      Part2 = 0
      deallocate (input)
   end function

   subroutine Day17Solve()
      integer :: answer

      print *, '--- Day 17 ---'

      answer = Part1()
      print *, 'Answer to part 1 is ', answer

      answer = Part2()
      print *, 'Answer to part 2 is ', answer
   end subroutine

end module
