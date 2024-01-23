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

   integer, parameter :: divi = 16
   integer, parameter :: noop = 17

   type :: t_instruction
      integer :: opcode, A, B, C
   contains
      procedure :: execute => instruction_execute
      procedure :: load => instruction_load
   end type

   type :: t_program
      integer :: ip_register
      type(t_instruction), dimension(0:50) :: instructions
      integer :: size
   contains
      procedure :: execute => program_execute
      procedure :: resume => program_resume
      procedure :: dump => program_dump
      procedure :: registerName => program_register_name
   end type

   public :: t_instruction, t_program
   public :: addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtri, gtir, gtrr, eqri, eqir, eqrr
   public :: divi, noop ! extra opcode for hacking

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

      case (divi)
         registers(this%C) = registers(this%A)/this%B

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

      case (noop)
         return

      case default
         stop 'Invalid opcode'
      end select
   end subroutine

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
         this%opcode = addr
      case ('addi')
         this%opcode = addi

      case ('mulr')
         this%opcode = mulr
      case ('muli')
         this%opcode = muli

      case ('banr')
         this%opcode = banr
      case ('bani')
         this%opcode = bani

      case ('borr')
         this%opcode = borr
      case ('bori')
         this%opcode = bori

      case ('setr')
         this%opcode = setr
      case ('seti')
         this%opcode = seti

      case ('gtir')
         this%opcode = gtir
      case ('gtri')
         this%opcode = gtri
      case ('gtrr')
         this%opcode = gtrr

      case ('eqir')
         this%opcode = eqir
      case ('eqri')
         this%opcode = eqri
      case ('eqrr')
         this%opcode = eqrr

      case default
         stop 'Invalid opcode'
      end select
   end subroutine

   subroutine program_resume(input, registers, from, last)
      class(t_program), intent(in) :: input
      integer, dimension(:), intent(in), pointer :: registers
      integer, intent(in) :: from, last
      integer :: ip

      ip = from
      do while (ip < input%size)
         call input%instructions(ip)%execute(registers)
         registers(input%ip_register) = registers(input%ip_register) + 1
         ip = registers(input%ip_register)
         if (ip == last) then
            exit
         end if
      end do
   end subroutine

   subroutine program_execute(input, registers, lastIP)
      class(t_program), intent(in) :: input
      integer, dimension(:), intent(in), pointer :: registers
      integer, intent(in), optional :: lastIP
      integer :: last

      if (present(lastIP)) then
         last = lastIP
      else
         last = input%size ! regular stop
      end if

      call input%resume(registers, 0, last)
   end subroutine

   character(2) function program_register_name(this, index)
      class(t_program), intent(in) :: this
      integer :: index, a

      if (index < 0 .or. index >= 26) then
         program_register_name = '?'
      else if (index == this%ip_register) then
         program_register_name = 'ip'
      else
         a = iachar('a')
         program_register_name = achar(index + a)
      end if
   end function

   subroutine program_dump(input)
      class(t_program), intent(in), target :: input
      type(t_instruction), pointer :: this
      integer :: i
      character(2) :: a, b, c
      integer(kind=1) :: ip

      do i = 1, input%size
         this => input%instructions(i - 1)
         ip = int(i - 1, 1)
         a = input%registerName(this%A)
         b = input%registerName(this%B)
         c = input%registerName(this%C)

         if (b == 'ip') then
            b = trim(itoa(i - 1))
         end if

         if (a == 'ip') then
            a = trim(itoa(i - 1))
         end if

         select case (this%opcode)
         case (addr)
            if (c == 'ip') then
               print *, ip, ': ', c, ' = 1 + ', a, ' + ', b
            else
               print *, ip, ': ', c, ' = ', a, ' + ', b
            end if

         case (addi)
            if (c == 'ip') then
               print *, ip, ': ', c, ' = ', a, ' + ', trim(itoa(this%B + 1))
            else
               print *, ip, ': ', c, ' = ', a, ' + ', trim(itoa(this%B))
            end if

         case (mulr)
            if (c == 'ip') then
               print *, ip, ': ', c, ' = 1 + (', a, ' * ', b, ')'
            else
               print *, ip, ': ', c, ' = ', a, ' * ', b
            end if
         case (muli)
            if (c == 'ip') then
               print *, ip, ': ', c, ' = 1 + (', a, ' * ', trim(itoa(this%B)), ')'
            else
               print *, ip, ': ', c, ' = ', a, ' * ', trim(itoa(this%B))
            end if

         case (divi)
            if (c == 'ip') then
               print *, ip, ': ', c, ' = 1 + (', a, ' / ', trim(itoa(this%B)), ')'
            else
               print *, ip, ': ', c, ' = ', a, ' / ', trim(itoa(this%B))
            end if

         case (banr)
            if (c == 'ip') then
               print *, ip, ': ', c, ' = 1 + (', a, ' & ', b, ')'
            else
               print *, ip, ': ', c, ' = ', a, ' & ', b
            end if
         case (bani)
            if (c == 'ip') then
               print *, ip, ': ', c, ' = 1 + (', a, ' & ', trim(itoHex(this%B)), ')'
            else
               print *, ip, ': ', c, ' = ', a, ' & ', trim(itoHex(this%B))
            end if

         case (borr)
            if (c == 'ip') then
               print *, ip, ': ', c, ' = 1 + (', a, ' | ', b, ')'
            else
               print *, ip, ': ', c, ' = ', a, ' | ', b
            end if
         case (bori)
            if (c == 'ip') then
               print *, ip, ': ', c, ' = 1 + (', a, ' | ', trim(itoHex(this%B)), ')'
            else
               print *, ip, ': ', c, ' = ', a, ' | ', trim(itoHex(this%B))
            end if

         case (setr)
            if (c == 'ip') then
               print *, ip, ': ', c, ' = ', a, ' + 1'
            else
               print *, ip, ': ', c, ' = ', a
            end if
         case (seti)
            if (c == 'ip') then
               print *, ip, ': ', c, ' = ', trim(itoa(this%A + 1))
            else
               print *, ip, ': ', c, ' = ', trim(itoa(this%A))
            end if

         case (gtir)
            if (c == 'ip') then
               print *, ip, ': ', c, ' = ', b, ' > ', trim(itoa(this%A)), ' ? 2 : 1'
            else
               print *, ip, ': ', c, ' = ', b, ' > ', trim(itoa(this%A)), ' ? 1 : 0'
            end if
         case (gtri)
            if (c == 'ip') then
               print *, ip, ': ', c, ' = ', a, ' > ', trim(itoa(this%B)), ' ? 2 : 1'
            else
               print *, ip, ': ', c, ' = ', a, ' > ', trim(itoa(this%B)), ' ? 1 : 0'
            end if
         case (gtrr)
            if (c == 'ip') then
               print *, ip, ': ', c, ' = ', a, ' > ', b, ' ? 2 : 1'
            else
               print *, ip, ': ', c, ' = ', a, ' > ', b, ' ? 1 : 0'
            end if

         case (eqir)
            if (c == 'ip') then
               print *, ip, ': ', c, ' = ', b, ' == ', trim(itoa(this%A)), ' ? 2 : 1'
            else
               print *, ip, ': ', c, ' = ', b, ' == ', trim(itoa(this%A)), ' ? 1 : 0'
            end if
         case (eqri)
            if (c == 'ip') then
               print *, ip, ': ', c, ' = ', a, ' == ', trim(itoa(this%B)), ' ? 2 : 1'
            else
               print *, ip, ': ', c, ' = ', a, ' == ', trim(itoa(this%B)), ' ? 1 : 0'
            end if
         case (eqrr)
            if (c == 'ip') then
               print *, ip, ': ', c, ' = ', a, ' == ', b, ' ? 2 : 1'
            else
               print *, ip, ': ', c, ' = ', a, ' == ', b, ' ? 1 : 0'
            end if

         case (noop)
            print *, ip, ': NO-OP'

         case default
            stop 'Invalid opcode'
         end select

      end do
   end subroutine

   type(t_program) function loadProgram(fileId)
      integer :: file, ios, fileId
      type(t_program) :: input
      character(3) :: dummy

      input%size = 0

      file = openFile(fileId)
      read (file, *, iostat=ios) dummy, input%ip_register
      do while (ios == 0)
         call input%instructions(input%size)%load(file, ios)
         if (ios == 0) then
            input%size = input%size + 1
         end if
      end do

      call closeFile(file)

      loadProgram = input
   end function
end module
