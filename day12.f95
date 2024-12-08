module Day12

   use tools

   implicit none

   private :: Part1, Part2
   public :: Day12Solve

   type :: t_state
      logical, dimension(:), allocatable :: pots
      integer(kind=8) :: first
      integer :: length
   contains
      procedure :: trim => state_trim
      procedure :: equal => state_equal
      procedure :: dump => state_dump
      procedure :: total => state_total
   end type

   type :: t_input
      logical, dimension(32) :: transforms
      type(t_state) :: state
   contains
      procedure :: get => input_get
      procedure :: length => input_len
      procedure :: transform => input_transform
   end type

contains

   logical function state_equal(state1, state2)
      class(t_state), intent(in) :: state1
      class(t_state), intent(in) :: state2
      integer :: i
      if (state1%length == state2%length) then
         state_equal = .true.
         do i = 1, state1%length
            if (state1%pots(i) .neqv. state2%pots(i)) then
               state_equal = .false.
               exit
            end if
         end do
      else
         state_equal = .false.
      end if

   end function

   integer(kind=8) function state_total(state)
      class(t_state), intent(in) :: state
      integer(kind=8) :: total
      integer :: i

      total = 0
      do i = 1, state%length
         if (state%pots(i)) then
            total = total + (i - 1) + state%first
         end if
      end do
      state_total = total
   end function

   subroutine state_dump(state)
      class(t_state), intent(in) :: state

      print *, state%first, state%length
      print *, state%pots
   end subroutine

   subroutine state_trim(state)
      class(t_state), intent(inout) :: state
      integer :: i, j

      i = 1
      j = state%length

      do while (i < j .and. (.not. state%pots(i)))
         i = i + 1
         state%first = state%first + 1
      end do

      do while (i < j .and. (.not. state%pots(j)))
         j = j - 1
      end do

      state%pots = state%pots(i:j)
      state%length = j - i + 1
   end subroutine

   type(t_input) function loadInput()
      integer :: file, i, key
      type(t_input) :: input
      type(t_state) :: state
      character(:), allocatable :: line

      input%transforms = .false.

      file = openFile(12)

      if (.not. readLine(file, line)) then
         stop 'Syntax error'
      end if

      state%first = 0
      state%length = len(line) - 15
      allocate (state%pots(state%length))
      state%pots = .false.

      do i = 16, len(line)
         if (line(i:i) == '#') then
            state%pots(i - 15) = .true.
         end if
      end do
      deallocate (line)

      if (.not. readLine(file, line)) then
         stop 'Syntax error'
      end if
      deallocate (line)

      do while (readLine(file, line))
         key = 0
         do i = 1, 5
            key = ishft(key, 1)
            if (line(i:i) == '#') then
               key = ibset(key, 0)
            end if
         end do
         input%transforms(key + 1) = line(10:10) == '#'
         deallocate (line)
      end do

      call closeFile(file)

      call state%trim()
      input%state = state
      loadInput = input
   end function

   integer function input_len(input)
      class(t_input), intent(in) :: input
      input_len = input%state%length
   end function

   logical function input_get(input, index)
      class(t_input), intent(in) :: input
      integer, intent(in) :: index

      if (index >= 1 .and. index <= input%length()) then
         input_get = input%state%pots(index)
      else
         input_get = .false.
      end if
   end function

   subroutine input_transform(input)
      class(t_input), intent(inout) :: input
      integer :: i, j, key
      type(t_state) :: newState

      newState%length = input%length() + 8
      newState%first = input%state%first - 4
      allocate (newState%pots(newstate%length + 2))
      newState%pots = .false.

      do i = -3, input%length() + 4
         key = 0
         do j = 0, 4
            key = ishft(key, 1)
            if (input%get(i + j)) then
               key = ibset(key, 0)
            end if
         end do
         newState%pots(i + 6) = input%transforms(key + 1)
      end do

      call newState%trim()

      input%state = newState
   end subroutine

   integer(kind=8) function calculate(iterations)
      type(t_input) :: input
      type(t_state) :: previous
      integer(kind=8) :: i
      integer(kind=8) :: iterations
      logical :: same

      input = loadInput()
      do i = 1, iterations
         previous = input%state
         call input%transform()
         same = input%state%equal(previous)
         deallocate (previous%pots)
         if (same) then
            input%state%first = input%state%first + iterations - i
            exit
         end if
      end do

      calculate = input%state%total()
   end function

   integer(kind=8) function Part1()
      Part1 = calculate(int(20, 8))
   end function

   integer(kind=8) function Part2()
      integer(kind=8) :: iterations

      iterations = 500000
      iterations = iterations*100000
      Part2 = calculate(iterations)
   end function

   subroutine Day12Solve()
      integer(kind=8) :: answer

      print *, '--- Day 12 ---'

      answer = Part1()
      print *, 'Answer to part 1 is ', answer

      answer = Part2()
      print *, 'Answer to part 2 is ', answer
   end subroutine

end module
