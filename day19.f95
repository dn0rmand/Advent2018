module Day19

   use tools
   use opcodes

   implicit none

   private :: Part1, Part2
   public :: Day19Solve

contains

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

   integer function execute_program(input, reg0)
      type(t_program) :: input
      integer :: reg0
      integer, dimension(0:5), target :: registers
      integer, dimension(:), pointer :: r

      registers = 0
      registers(0) = reg0
      r => registers
      call input%execute(r, 2)
      call divisor_sum(registers)
      execute_program = registers(0)
   end function

   integer function Part1()
      type(t_program) :: input
      input = loadProgram(19)
      Part1 = execute_program(input, 0)
   end function

   integer function Part2()
      type(t_program) :: input
      input = loadProgram(19)
      Part2 = execute_program(input, 1)
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
