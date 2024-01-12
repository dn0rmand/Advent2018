module Day11

   use tools

   implicit none

   private :: Part1, Part2
   public :: Day11Solve

   type :: t_max_power
      integer :: x, y, power, size
   end type

   type :: t_input
      integer :: input
      integer, dimension(:, :), allocatable :: map
   contains
      procedure :: create => input_create
      procedure :: free => input_free
      procedure :: get => input_get
      procedure :: square => input_square_power
      procedure :: maxPower => input_max_power
   end type

contains
   integer function powerLevel(input, x, y)
      integer :: input, x, y
      integer :: rackId
      integer :: power_level

      rackId = mod(x + 10, 1000)
      power_level = mod(rackId*y, 1000)
      power_level = mod(power_level + input, 1000)
      power_level = mod(power_level*rackId, 1000)
      power_level = (power_level - mod(power_level, 100))/100
      powerLevel = mod(power_level, 10) - 5
   end function

   integer function input_get(input, x, y)
      class(t_input), intent(in) :: input
      integer, intent(in) :: x, y
      if (x < 1 .or. y < 1) then
         input_get = 0
      else
         input_get = input%map(x, y)
      end if
   end function

   subroutine input_free(input)
      class(t_input), intent(inout) :: input
      deallocate (input%map)
   end subroutine

   subroutine input_create(input, value)
      class(t_input), intent(inout) :: input
      integer, intent(in) :: value
      integer ::  x, y, v, a, b, c

      allocate (input%map(300, 300))

      do y = 1, 300
         do x = 1, 300
            v = powerLevel(value, x, y)
            a = input%get(x - 1, y)
            b = input%get(x, y - 1)
            c = input%get(x - 1, y - 1)
            input%map(x, y) = v + a + b - c
         end do
      end do
   end subroutine

   integer function input_square_power(input, size, x, y)
      class(t_input), intent(in) :: input
      integer, intent(in) :: size, x, y
      integer :: v, a, b, c, x1, y1

      x1 = x + size - 1
      y1 = y + size - 1

      v = input%get(x1, y1)
      a = input%get(x1 - size, y1)
      b = input%get(x1, y1 - size)
      c = input%get(x1 - size, y1 - size)

      input_square_power = v - a - b + c
   end function

   type(t_max_power) function input_max_power(input, size)
      class(t_input), intent(in) :: input
      integer, intent(in) :: size
      integer :: x, y, power
      type(t_max_power) :: max_power

      max_power%x = 1
      max_power%x = 1
      max_power%size = size
      max_power%power = input%square(size, 1, 1)

      do y = 1, 300 - size
         do x = 1, 300 - size
            power = input%square(size, x, y)
            if (power > max_power%power) then
               max_power%power = power
               max_power%x = x
               max_power%y = y
            end if
         end do
      end do

      input_max_power = max_power
   end function

   type(t_input) function loadInput()
      integer :: file, output
      type(t_input) :: input

      file = openFile(11)
      read (file, *) output
      call closeFile(file)

      call input%create(output)

      loadInput = input
   end function

   character(7) function Part1()
      type(t_input) :: input
      type(t_max_power) :: max_power
      character(3) :: line1, line2

      input = loadInput(); 
      max_power = input%maxPower(3)

      call input%free()

      write (line1, '(I3)') max_power%x
      write (line2, '(I3)') max_power%y

      Part1 = trim(adjustl(line1))//','//trim(adjustl(line2))
   end function

   character(11) function Part2()
      type(t_input) :: input
      integer :: size
      type(t_max_power) :: max_power, mp
      character(3) :: line1, line2, line3

      input = loadInput(); 
      max_power = input%maxPower(2)
      do size = 3, 300
         mp = input%maxPower(size)
         if (mp%power > max_power%power) then
            max_power = mp
         end if
      end do

      call input%free()

      write (line1, '(I3)') max_power%x
      write (line2, '(I3)') max_power%y
      write (line3, '(I3)') max_power%size
      Part2 = trim(adjustl(line1))//','//trim(adjustl(line2))//','//trim(adjustl(line3))
   end function

   subroutine Day11Solve()
      character(7) :: answer1
      character(11) :: answer2

      print *, '--- Day 0 ---'

      answer1 = Part1()
      print *, 'Answer to part 1 is ', answer1

      answer2 = Part2()
      print *, 'Answer to part 2 is ', answer2
   end subroutine

end module
