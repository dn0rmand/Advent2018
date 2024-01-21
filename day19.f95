module Day19

   use tools
   use opcodes

   implicit none

   private :: Part1, Part2
   public :: Day19Solve

   type :: t_program
      integer :: ip_register
      type(t_instruction), dimension(0:50) :: instructions
      integer :: size
   contains
      procedure :: execute => program_execute
   end type
contains

   type(t_program) function loadInput()
      integer :: file, ios
      type(t_program) :: input
      character(3) :: dummy

      input%size = 0

      file = openFile(19)
      read (file, *, iostat=ios) dummy, input%ip_register
      do while (ios == 0)
         call input%instructions(input%size)%load(file, ios)
         if (ios == 0) then
            input%size = input%size + 1
         end if
      end do

      call closeFile(file)

      loadInput = input
   end function

   subroutine divisor_sum(registers)
      integer, dimension(0:5), intent(inout) :: registers
      integer :: i, sum, max, value, a

      value = registers(4)
      max = value
      sum = 1 + max
      i = 2

      do while (i < max)
         if (modulo(value, i) == 0) then
            a = value/i
            sum = sum + i
            if (i /= a) then
               sum = sum + a
            end if
            if (a < max) then
               max = a
            end if
         end if
         i = i + 1
      end do

      registers(0) = sum
   end subroutine

   subroutine program_execute(input, registers)
      class(t_program), intent(in), target :: input
      integer, dimension(0:5), intent(inout), target :: registers
      type(t_instruction), pointer :: current
      integer, dimension(:), pointer :: r
      integer :: ip, B
      logical :: hack

      r => registers
      ip = registers(input%ip_register)
      do while (ip < input%size)
         if (ip == 2) then
            call divisor_sum(registers)
            exit
         end if

         current => input%instructions(ip)
         call current%execute(r)
         registers(input%ip_register) = registers(input%ip_register) + 1
         ip = registers(input%ip_register)
      end do
   end subroutine

   integer function Part1()
      type(t_program) :: input
      integer, dimension(0:5) :: registers

      registers = 0

      input = loadInput()
      call input%execute(registers)

      Part1 = registers(0)
   end function

   integer function Part2()
      type(t_program) :: input
      integer, dimension(0:5) :: registers

      registers = 0
      registers(0) = 1

      input = loadInput()
      call input%execute(registers)

      Part2 = registers(0)
   end function

   subroutine Day19Solve()
      integer :: answer

      print *, '--- Day 19 ---'

      answer = Part1()
      print *, 'Answer to part 1 is ', answer

      answer = Part2()
      print *, 'Answer to part 2 is ', answer
   end subroutine

end module
