module Day21

   use tools
   use opcodes

   implicit none

   private :: Part1, Part2
   public :: Day21Solve

contains

   integer function Part1()
      type(t_program) :: input
      integer, dimension(0:5), target :: registers

      registers = 0
      input = loadProgram(21)
      call input%execute(registers, 28)

      Part1 = registers(1)
   end function

   type(t_instruction) function make_instruction(opcode, A, B, C)
      integer :: opcode, B
      character :: A, C
      type(t_instruction) :: i

      i%opcode = opcode
      if (opcode == seti) then
         i%A = B
      else
         i%A = iachar(A) - iachar('a')
      end if
      i%B = B
      i%C = iachar(C) - iachar('a')
      make_instruction = i
   end function

   integer function Part2()
      type(t_program) :: input
      integer, dimension(0:5), target :: registers
      integer :: minValue, b
      logical, dimension(:), allocatable :: visited

      allocate (visited(20000000))
      registers = 0
      input = loadProgram(21)

      input%instructions(17) = make_instruction(divi, 'd', 256, 'd')
      input%instructions(18) = make_instruction(seti, 'a', 7, achar(iachar('a') + input%ip_register)) ! jump to 8
      do b = 19, 27
         input%instructions(b) = make_instruction(noop, 'a', 0, 'a')
      end do

      call input%execute(registers, 29)
      do while (.not. visited(registers(1)))
         registers(4) = 0 ! prevent jump
         visited(registers(1)) = .true.
         minValue = registers(1)
         call input%resume(registers, 29, 29)
      end do
      deallocate (visited)
      Part2 = minValue
   end function

   subroutine Day21Solve()
      integer :: answer

      print *, '--- Day 21 ---'

      answer = Part1()
      print *, 'Answer to part 1 is ', answer

      answer = Part2()
      print *, 'Answer to part 2 is ', answer
   end subroutine

end module

!   b = 0
! L6:
!   d = b|0x10000
!   b = 10905776
! L8:
!   e = d&0xFF
!   b = b + e
!   b = b&0xFFFFFF
!   b = b*65899
!   b = b&0xFFFFFF
!   e = d > 256 ? 1 : 0
!   ip = d > 256 ? 28 : 17
!   JMP L17
!   JMP L28
! L17:
!   e = 0
!   f = e + 1
!   f = f*256
!   f = f > d?1:0
!   ip = f + 22
!   JMP L24
!   JMP L26
! L24:
!   e = e + 1
!   ip = 18
! L26:
!   d = e
!   JMP L8
! L28:
!   e = b == a?1:0
!   ip = e + 30
! JMP L6
