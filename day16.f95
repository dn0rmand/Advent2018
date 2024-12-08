module Day16

   use tools
   use opcodes

   implicit none

   private :: Part1, Part2
   public :: Day16Solve

   type :: t_map
      integer(kind=1), dimension(0:15) :: values
   contains
      procedure :: unique => map_unique
   end type

   type :: t_registers
      integer, dimension(0:3) :: values
   contains
      procedure :: equal => registers_equal
   end type

   type :: t_sample
      type(t_registers) :: before, after
      type(t_instruction) :: instruction
   contains
      procedure :: resolve => sample_resolve
   end type

   type :: t_input
      type(t_sample), dimension(800) :: samples
      type(t_instruction), dimension(1000) :: program
      integer :: sampleSize, programSize

      type(t_map), dimension(0:15) :: opcodes
   end type

contains

   integer function map_unique(map)
      class(t_map), intent(inout) :: map
      integer :: i, u

      u = -1
      do i = 0, 15
         if (map%values(i) == 1) then
            if (u >= 0) then
               map_unique = -1 ! not unique
               return
            end if
            u = i
         end if
      end do
      map%values = 0 ! clear to avoid doing all over again
      map_unique = u
   end function

   type(t_input) function loadInput()
      integer :: file, i
      character(:), allocatable :: line
      type(t_input) :: input
      type(t_sample) :: sample
      type(t_instruction) :: instruction
      logical :: dummy

      input%sampleSize = 0
      input%programSize = 0

      do i = 0, 15
         input%opcodes(i)%values = 0
      end do

      file = openFile(16)

      do while (readLine(file, line))
         if (line(1:9) == 'Before: [') then
            ! read before registry values
            read (line(10:len(line) - 1), *) sample%before%values
            deallocate (line)

            ! read instructions
            dummy = readLine(file, line)
            read (line, *) sample%instruction
            deallocate (line)

            ! read after registry values
            dummy = readLine(file, line)
            read (line(10:len(line) - 1), *) sample%after%values

            ! add sample
            input%sampleSize = input%sampleSize + 1
            input%samples(input%sampleSize) = sample
         else if (len(line) /= 0) then
            ! must be the program
            read (line, *) instruction
            input%programSize = input%programSize + 1
            input%program(input%programSize) = instruction
         end if
         deallocate (line)
      end do

      call closeFile(file)

      loadInput = input
   end function

   logical function registers_equal(a, b)
      class(t_registers), intent(in) :: a
      type(t_registers), intent(in) :: b
      logical :: eq

      eq = a%values(0) == b%values(0) .and. a%values(1) == b%values(1)
      eq = eq .and. a%values(2) == b%values(2) .and. a%values(3) == b%values(3)

      registers_equal = eq
   end function

   integer function sample_resolve(this, input)
      class(t_sample), intent(in) :: this
      type(t_input), intent(inout) :: input
      type(t_registers), target :: registers
      integer, dimension(:), pointer :: reg
      type(t_instruction) :: instruction
      integer :: i, count, opcode

      instruction = this%instruction
      opcode = instruction%opcode
      count = 0

      reg => registers%values

      do i = 0, 15
         registers = this%before
         instruction%opcode = i
         call instruction%execute(reg)
         if (registers%equal(this%after)) then
            input%opcodes(opcode)%values(i) = 1
            count = count + 1
         end if
      end do

      sample_resolve = count
   end function

   subroutine finishMapping(input, mapping)
      type(t_input), intent(inout) :: input
      integer, intent(out) :: mapping(0:15)
      integer :: found
      logical :: done
      integer :: opcode, i, realCode

      mapping = -1
      found = 0
      done = .false.

      do while (.not. done)
         done = .true.
         do opcode = 0, 15
            realCode = input%opcodes(opcode)%unique()
            if (realCode >= 0) then
               ! find it
               found = found + 1
               mapping(opcode) = realCode
               done = .false.
               do i = 0, 15
                  input%opcodes(i)%values(realCode) = 0
               end do
               exit
            end if
         end do
      end do
      if (found /= 16) then
         stop 'did not find them all'
      end if
   end subroutine

   integer function Part1(input)
      type(t_input), intent(inout) :: input
      type(t_sample):: sample
      integer :: i, total

      total = 0

      do i = 1, input%sampleSize
         sample = input%samples(i)
         if (sample%resolve(input) >= 3) then
            total = total + 1
         end if
      end do

      Part1 = total
   end function

   integer function Part2(input)
      type(t_input), intent(inout) :: input
      integer :: mapping(0:15)
      type(t_registers), target :: registers
      integer, dimension(:), pointer :: regs
      type(t_instruction) :: instruction
      integer :: ip

      call finishMapping(input, mapping)

      registers%values = 0
      regs => registers%values

      do ip = 1, input%programSize
         instruction = input%program(ip)
         instruction%opcode = mapping(instruction%opcode)
         call instruction%execute(regs)
      end do
      Part2 = registers%values(0)
   end function

   subroutine Day16Solve()
      integer :: answer
      type(t_input) :: input
      print *, '--- Day 16 ---'

      input = loadInput()

      answer = Part1(input)
      print *, 'Answer to part 1 is ', answer

      answer = Part2(input)
      print *, 'Answer to part 2 is ', answer
   end subroutine

end module
