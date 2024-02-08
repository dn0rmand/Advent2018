module Day17

   use tools

   implicit none

   public :: Day17Solve

   integer, parameter :: SPRING_X = 500, SPRING_Y = 0
   integer, parameter :: MIN_X = 450, MAX_X = 750, MIN_Y = 0, MAX_Y = 2000

   integer(kind=1), parameter :: EMPTY = 0, WALL = 1, STILL_WATER = 2, FALLING_WATER = 3, ERROR = 4

   type :: t_input
      integer(kind=1), dimension(MIN_X:MAX_X, 0:MAX_Y) :: data
      integer :: minX, maxX, maxY, minY
   contains
      procedure :: dump => input_dump
      procedure :: fall => input_fall
      procedure :: calculate => input_calculate
      procedure :: set => input_set
   end type

contains

   subroutine input_dump(input, x0, y0)
      class(t_input), intent(in) :: input
      integer, intent(in) :: x0, y0
      integer :: x, y, i, minX, maxX, minY, maxY
      character(105) :: line

      minX = max(input%minX, x0 - 50)
      maxX = min(minX + 100, input%maxX)
      minY = max(0, y0 - 10); 
      maxY = min(minY + 25, input%maxY)

      do y = minY, maxY
         line = ''
         do x = minX, maxX
            i = x - minX + 1
            select case (input%data(x, y))
            case (WALL)
               line(i:i) = '#'
            case (STILL_WATER)
               line(i:i) = '~'
            case (FALLING_WATER)
               line(i:i) = '|'
            case (ERROR)
               line(i:i) = 'X'
            end select
         end do
         print *, input%maxY - y, trim(line)
      end do
   end subroutine

   subroutine input_set(this, x, y, value)
      class(t_input), intent(inout) :: this
      integer, intent(in) :: x, y
      integer(kind=1), intent(in) :: value
      integer(kind=1) :: c

      if (value == WALL) then
         stop 'What do  you think you are doing?'
      end if

      c = this%data(x, y)

      if (c == value) then
         return
      end if

      if (c == WALL) then
         stop 'Cannot change WALL to something else'
      end if

      if (c == STILL_WATER .and. value == FALLING_WATER) then
         stop 'Cannot go from still to falling'
      end if

      if (c == FALLING_WATER .and. value == EMPTY) then
         stop 'Cannot undo it'
      end if

      this%data(x, y) = value
   end subroutine

   type(t_input) function loadInput()
      integer :: file
      character(:), allocatable :: line
      character :: ch
      integer :: a, b, c, x, y
      type(t_input), allocatable  :: input

      allocate (input)

      input%minX = 10000
      input%minY = 10000
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
            input%minY = min(b, input%minY)
            input%maxY = max(c, input%maxY)
            x = a
            do y = b, c
               input%data(x, y) = WALL
            end do
         else
            input%minX = min(b - 1, input%minX)
            input%maxX = max(c + 1, input%maxX)
            input%maxY = max(a, input%maxY)
            input%minY = min(a, input%minY)

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

   recursive subroutine input_fall(this, x0, y0)
      class(t_input), intent(inout) :: this
      integer, intent(in) :: x0, y0
      integer :: i, x, y, xl, xr, fx
      integer(kind=1) :: w
      logical :: lFall, rFall

      x = x0
      y = y0

      do while (.true.)
         call this%set(x, y, FALLING_WATER)

         if (y >= this%maxY) then
            exit
         end if

         w = this%data(x, y + 1)

         if (w == EMPTY) then
            y = y + 1
            cycle
         end if

         if (w == FALLING_WATER) then
            exit ! already processed
         end if

         xl = x - 1
         xr = x + 1
         lFall = .false.
         rFall = .false.
         ! look left
         do while (xl > this%minX)
            if (this%data(xl, y) == WALL) then
               xl = xl + 1
               exit
            end if
            if (this%data(xl, y + 1) == EMPTY .or. this%data(xl, y + 1) == FALLING_WATER) then
               lFall = .true.
               exit
            end if
            xl = xl - 1
         end do
         ! look right
         do while (xr <= this%maxX)
            if (this%data(xr, y) == WALL) then
               xr = xr - 1
               exit
            end if
            if (this%data(xr, y + 1) == EMPTY .or. this%data(xr, y + 1) == FALLING_WATER) then
               rFall = .true.
               exit
            end if
            xr = xr + 1
         end do

         if (lFall .or. rFall) then
            do i = xl, xr
               call this%set(i, y, FALLING_WATER)
            end do

            if (lFall .and. rFall) then
               call this%fall(xl, y)
               !  trace = .true.
               if (this%data(xr, y + 1) /= EMPTY) then
                  exit
               else
                  x = xr
               end if
            else if (lFall) then
               x = xl
            else if (rFall) then
               x = xr
            else
               exit
            end if
         else
            do i = xl, xr
               call this%set(i, y, STILL_WATER)
            end do
            y = y - 1
            if (y == MIN_Y) then
               exit
            end if
            if (this%data(x, y) /= FALLING_WATER) then
               do i = xl, xr
                  if (this%data(i, y) == FALLING_WATER) then
                     x = i
                     exit
                  end if
               end do
            end if
            if (this%data(x, y) /= FALLING_WATER) then
               this%data(x, y) = ERROR
               this%data(xl, y) = ERROR
               this%data(xr, y) = ERROR
               call this%dump(x, y)
               print *, x, fx
               stop 'WHAT !!!!'
            end if
         end if
      end do
   end subroutine

   function input_calculate(this) result(counts)
      class(t_input) :: this
      integer :: x, y, c
      integer :: counts(EMPTY:ERROR)

      counts = 0
      do x = this%minX, this%maxX
         do y = this%minY, this%maxY
            c = this%data(x, y)
            counts(c) = counts(c) + 1
         end do
      end do
   end function

   subroutine Day17Solve()
      integer :: answers(EMPTY:ERROR)
      type(t_input), allocatable :: input

      print *, '--- Day 17 ---'

      input = loadInput()
      call input%fall(SPRING_X, SPRING_Y)
      answers = input%calculate()

      print *, 'Answer to part 1 is ', answers(STILL_WATER) + answers(FALLING_WATER)
      print *, 'Answer to part 2 is ', answers(STILL_WATER)
   end subroutine

end module
