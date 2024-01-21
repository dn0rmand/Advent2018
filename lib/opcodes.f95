module opcodes

   use tools

   implicit none

   integer, parameter :: addr = 0
   integer, parameter :: addi = 1

   integer, parameter :: mulr = 2
   integer, parameter :: muli = 3

   integer, parameter :: banr = 4
   integer, parameter :: bani = 5

   integer, parameter :: borr = 6
   integer, parameter :: bori = 7

   integer, parameter :: setr = 8
   integer, parameter :: seti = 9

   integer, parameter :: gtir = 10
   integer, parameter :: gtri = 11
   integer, parameter :: gtrr = 12

   integer, parameter :: eqir = 13
   integer, parameter :: eqri = 14
   integer, parameter :: eqrr = 15

   type :: t_instruction
      integer :: opcode, A, B, C
   contains
      procedure :: execute => instruction_execute
      procedure :: load => instruction_load
   end type

   public :: t_instruction
   public :: addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtri, gtir, gtrr, eqri, eqir, eqrr

contains

   integer function asInt(value)
      logical :: value

      if (value) then
         asInt = 1
      else
         asInt = 0
      end if
   end function

   subroutine instruction_execute(this, registers)
      class(t_instruction), intent(in) :: this
      integer, dimension(:), pointer :: registers

      select case (this%opcode)
      case (addr)
         registers(this%C) = registers(this%A) + registers(this%B)
      case (addi)
         registers(this%C) = registers(this%A) + this%B

      case (mulr)
         registers(this%C) = registers(this%A)*registers(this%B)
      case (muli)
         registers(this%C) = registers(this%A)*this%B

      case (banr)
         registers(this%C) = iand(registers(this%A), registers(this%B))
      case (bani)
         registers(this%C) = iand(registers(this%A), this%B)

      case (borr)
         registers(this%C) = ior(registers(this%A), registers(this%B))
      case (bori)
         registers(this%C) = ior(registers(this%A), this%B)

      case (setr)
         registers(this%C) = registers(this%A)
      case (seti)
         registers(this%C) = this%A

      case (gtir)
         registers(this%C) = asInt(this%A > registers(this%B))
      case (gtri)
         registers(this%C) = asInt(registers(this%A) > this%B)
      case (gtrr)
         registers(this%C) = asInt(registers(this%A) > registers(this%B))

      case (eqir)
         registers(this%C) = asInt(this%A == registers(this%B))
      case (eqri)
         registers(this%C) = asInt(registers(this%A) == this%B)
      case (eqrr)
         registers(this%C) = asInt(registers(this%A) == registers(this%B))

      case default
         stop 'Invalid opcode'
      end select
   end subroutine

   character(2) function registerName(index)
      integer :: index, a

      if (index == 5) then
         registerName = 'IP'
      else
         a = iachar('A')
         registerName = achar(index + a)
      end if
   end function

   subroutine instruction_load(this, file, ios)
      class(t_instruction), intent(inout) :: this
      integer, intent(in) :: file
      integer, intent(inout) :: ios
      character(4) :: opcode

      read (file, *, iostat=ios) opcode, this%A, this%B, this%C
      if (ios /= 0) then
         return
      end if

      select case (opcode)
      case ('addr')
         !  print *, registerName(this%C), ' = ', registerName(this%A), ' + ', registerName(this%B)
         this%opcode = addr
      case ('addi')
         !  print *, registerName(this%C), ' = ', registerName(this%A), ' +', int(this%B, 1)
         this%opcode = addi

      case ('mulr')
         !  print *, registerName(this%C), ' = ', registerName(this%A), ' * ', registerName(this%B)
         this%opcode = mulr
      case ('muli')
         !  print *, registerName(this%C), ' = ', registerName(this%A), ' *', int(this%B, 1)
         this%opcode = muli

      case ('banr')
         !  print *, registerName(this%C), ' = ', registerName(this%A), ' & ', registerName(this%B)
         this%opcode = banr
      case ('bani')
         !  print *, registerName(this%C), ' = ', registerName(this%A), ' &', int(this%B, 1)
         this%opcode = bani

      case ('borr')
         !  print *, registerName(this%C), ' = ', registerName(this%A), ' | ', registerName(this%B)
         this%opcode = borr
      case ('bori')
         !  print *, registerName(this%C), ' = ', registerName(this%A), ' |', int(this%B, 1)
         this%opcode = bori

      case ('setr')
         ! print *, registerName(this%C), ' = ', registerName(this%A)
         this%opcode = setr
      case ('seti')
         !  print *, registerName(this%C), ' =', int(this%A, 1)
         this%opcode = seti

      case ('gtir')
         !  print *, registerName(this%C), ' =', int(this%A, 1), ' > ', registerName(this%B), ' ? 1 : 0'
         this%opcode = gtir
      case ('gtri')
         !  print *, registerName(this%C), ' = ', registerName(this%A), ' >', int(this%B, 1), ' ? 1 : 0'
         this%opcode = gtri
      case ('gtrr')
         !  print *, registerName(this%C), ' = ', registerName(this%A), ' > ', registerName(this%B), ' ? 1 : 0'
         this%opcode = gtrr

      case ('eqir')
         !  print *, registerName(this%C), ' =', int(this%A, 1), ' == ', registerName(this%B), ' ? 1 : 0'
         this%opcode = eqir
      case ('eqri')
         !  print *, registerName(this%C), ' = ', registerName(this%A), ' ==', int(this%B, 1), ' ? 1 : 0'
         this%opcode = eqri
      case ('eqrr')
         !  print *, registerName(this%C), ' = ', registerName(this%A), ' == ', registerName(this%B), ' ? 1 : 0'
         this%opcode = eqrr

      case default
         stop 'Invalid opcode'
      end select
   end subroutine

end module
