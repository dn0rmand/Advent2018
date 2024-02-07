module Day15

   use tools

   implicit none

   private :: Part1, Part2
   public :: Day15Solve

   integer(kind=1), parameter :: EMPTY = 0
   integer(kind=1), parameter :: ELF = 1
   integer(kind=1), parameter :: GOBLIN = 2
   integer(kind=1), parameter :: WALL = 10

   integer(kind=2), parameter :: DEFAULT_HIT_POINTS = 200
   integer, parameter :: SIZE = 32

   type :: t_state
      integer :: x, y
      integer :: x0, y0
   end type

   type :: t_state_collection
      type(t_state), dimension(500) :: states1, states2
      type(t_state), dimension(:), pointer :: states, newStates
      logical, dimension(SIZE, SIZE) :: visited
      integer :: count, newCount
   contains
      procedure :: initialize => states_initialize
      procedure :: add => states_add
      procedure :: swap => states_swap
      procedure :: get => states_get
   end type

   type :: t_unit
      integer(kind=1) :: type
      integer(kind=2) :: hit_points
      integer :: round
   end type

   type :: t_input
      type(t_unit), dimension(SIZE, SIZE) :: map
      integer :: round
      integer :: goblins
      integer :: elfs
      integer(kind=2) :: elfPower
   contains
      procedure :: get => input_get_unit
      procedure :: getType => input_get_unit_type
      procedure :: getHitPoints => input_get_unit_hit_points
      procedure :: targetType => input_get_target_type
      procedure :: canAttack => input_canAttack
      procedure :: attack => input_attack
      procedure :: move => input_move
      procedure :: score => input_score
      procedure :: dump => input_dump
   end type

contains

   ! --- STATES COLLECTION ---

   subroutine states_initialize(collection)
      class(t_state_collection), intent(inout), target :: collection

      collection%visited = .false.
      collection%newCount = 0
      collection%count = 0
      collection%states => collection%states1
      collection%newStates => collection%states2
   end subroutine

   subroutine states_swap(collection)
      class(t_state_collection), intent(inout) :: collection
      type(t_state), dimension(:), pointer :: tmp

      tmp => collection%states
      collection%states => collection%newStates
      collection%newStates => tmp

      collection%count = collection%newCount
      collection%newCount = 0
   end subroutine

   subroutine states_add(collection, input, player, x, y, x0, y0)
      class(t_state_collection), intent(inout), target :: collection
      type(t_input), intent(in) :: input
      integer, intent(in) :: x, y, x0, y0
      integer(kind=1), intent(in) :: player
      type(t_state) :: state

      if (collection%visited(x, y)) then
         return
      end if
      collection%visited(x, y) = .true.

      if (input%getType(x, y) == WALL .or. input%getType(x, y) == player) then
         return
      end if

      state%x0 = x0
      state%y0 = y0
      state%x = x
      state%y = y
      collection%newCount = collection%newCount + 1
      collection%newStates(collection%newCount) = state
   end subroutine

   type(t_state) function states_get(collection, index)
      class(t_state_collection), intent(in) :: collection
      integer, intent(in) :: index

      states_get = collection%states(index)
   end function

   ! --- END STATES COLLECTION ---

   ! --- INPUT METHODS ---

   type(t_unit) function input_get_unit(input, x, y)
      class(t_input), intent(in) :: input
      integer, intent(in) :: x, y
      type(t_unit) :: unit

      unit%type = WALL

      if (x < 1 .or. x > SIZE .or. y < 1 .or. y > SIZE) then
         input_get_unit = unit
      else
         input_get_unit = input%map(x, y)
      end if
   end function

   integer(kind=1) function input_get_unit_type(input, x, y)
      class(t_input), intent(in) :: input
      integer, intent(in) :: x, y
      type(t_unit) :: unit

      unit = input%get(x, y)
      input_get_unit_type = unit%type
   end function

   integer(kind=2) function input_get_unit_hit_points(input, x, y)
      class(t_input), intent(in) :: input
      integer, intent(in) :: x, y
      type(t_unit) :: unit

      unit = input%get(x, y)
      input_get_unit_hit_points = unit%hit_points
   end function

   integer(kind=1) function input_get_target_type(input, x, y)
      class(t_input), intent(in) :: input
      integer, intent(in) :: x, y

      select case (input%getType(x, y))
      case (ELF)
         input_get_target_type = GOBLIN
      case (GOBLIN)
         input_get_target_type = ELF
      case default
         input_get_target_type = EMPTY
         return
      end select
   end function

   logical function input_canAttack(input, x, y)
      class(t_input), intent(in) :: input
      integer, intent(in) :: x, y
      integer(kind=1) :: type

      type = input%targetType(x, y)
      if (type == EMPTY) then
         input_canAttack = .false.
      else if (input%getType(x - 1, y) == type .or. input%getType(x + 1, y) == type) then
         input_canAttack = .true.
      else if (input%getType(x, y - 1) == type .or. input%getType(x, y + 1) == type) then
         input_canAttack = .true.
      else
         input_canAttack = .false.
      end if
   end function

   subroutine input_move(input, x, y)
      class(t_input), intent(inout) :: input
      integer, intent(inout) :: x, y
      type(t_state_collection) :: states
      integer(kind=1) :: player
      type(t_state) :: state
      type(t_state) :: best
      type(t_unit) :: unit
      integer :: i

      best%x = 0
      player = input%getType(x, y)

      call states%initialize()

      call states%add(input, player, x, y - 1, x, y - 1)
      call states%add(input, player, x - 1, y, x - 1, y)
      call states%add(input, player, x + 1, y, x + 1, y)
      call states%add(input, player, x, y + 1, x, y + 1)
      call states%swap()

      do while (states%count > 0)
         do i = 1, states%count
            state = states%get(i)
            if (input%getType(state%x, state%y) /= EMPTY) then
               ! found
               if (best%x == 0) then
                  best = state
               else if (best%y > state%y .or. (best%y == state%y .and. best%x > state%x)) then
                  best = state
               end if
            else
               call states%add(input, player, state%x, state%y - 1, state%x0, state%y0)
               call states%add(input, player, state%x - 1, state%y, state%x0, state%y0)
               call states%add(input, player, state%x + 1, state%y, state%x0, state%y0)
               call states%add(input, player, state%x, state%y + 1, state%x0, state%y0)
            end if
         end do
         if (best%x /= 0) then
            unit = input%map(x, y)
            input%map(x, y)%type = EMPTY
            x = best%x0
            y = best%y0
            input%map(x, y) = unit
            return
         end if
         call states%swap()
      end do
   end subroutine

   subroutine input_attack(input, x, y)
      class(t_input), intent(inout) :: input
      integer(kind=2) :: power, hp
      integer :: target_hit_points
      logical :: hasTarget
      integer, intent(inout) :: x, y
      integer :: x0, y0
      integer(kind=1) :: type

      x0 = x
      y0 = y
      type = input%targetType(x0, y0)
      hasTarget = .false.

      if (type == EMPTY) then
         return
      end if

      if (input%getType(x0, y0 - 1) == type) then
         hasTarget = .true.
         x = x0
         y = y0 - 1
         target_hit_points = input%getHitPoints(x, y)
      end if

      if (input%getType(x0 - 1, y0) == type) then
         if ((.not. hasTarget) .or. input%getHitPoints(x0 - 1, y0) < target_hit_points) then
            hasTarget = .true.
            x = x0 - 1
            y = y0
            target_hit_points = input%getHitPoints(x, y)
         end if
      end if

      if (input%getType(x0 + 1, y0) == type) then
         if ((.not. hasTarget) .or. input%getHitPoints(x0 + 1, y0) < target_hit_points) then
            hasTarget = .true.
            x = x0 + 1
            y = y0
            target_hit_points = input%getHitPoints(x, y)
         end if
      end if

      if (input%getType(x0, y0 + 1) == type) then
         if ((.not. hasTarget) .or. input%getHitPoints(x0, y0 + 1) < target_hit_points) then
            hasTarget = .true.
            x = x0
            y = y0 + 1
            target_hit_points = input%getHitPoints(x, y)
         end if
      end if

      if (hasTarget) then
         power = 3
         if (type == ELF) then
            power = 3
         else
            power = input%elfPower
         end if
         hp = input%map(x, y)%hit_points
         hp = hp - min(hp, power)
         input%map(x, y)%hit_points = hp
         if (hp == 0) then
            ! DEAD
            if (type == ELF) then
               input%elfs = input%elfs - 1
            else
               input%goblins = input%goblins - 1
            end if
            input%map(x, y)%type = EMPTY
         end if
      end if
   end subroutine

   integer function input_score(input)
      class(t_input), intent(in) :: input
      integer :: x, y, score
      type(t_unit) :: unit

      score = 0
      do y = 1, SIZE
         do x = 1, SIZE
            unit = input%map(x, y)
            if (unit%type == ELF .or. unit%type == GOBLIN) then
               score = score + unit%hit_points
            end if
         end do
      end do

      input_score = input%round*score
   end function

   subroutine input_dump(input)
      class(t_input), intent(in) :: input
      integer :: x, y
      character(SIZE) :: line

      print *, 'Round', input%round
      do y = 1, SIZE
         do x = 1, SIZE
            select case (input%getType(x, y))
            case (WALL)
               line(x:x) = '#'
            case (EMPTY)
               line(x:x) = '.'
            case (ELF)
               line(x:x) = 'E'
            case (GOBLIN)
               line(x:x) = 'G'
            end select
         end do

         print *, line
      end do
   end subroutine

   ! --- END INPUT METHODS ---

   subroutine play_turn(input, x1, y1)
      type(t_input), intent(inout) :: input
      integer, intent(in) :: x1, y1
      integer :: x, y

      x = x1
      y = y1

      if (.not. input%canAttack(x, y)) then
         call input%move(x, y)
      end if

      call input%attack(x, y)
   end subroutine

   subroutine play_round(input)
      type(t_input), intent(inout) :: input
      type(t_unit) :: unit
      integer :: x, y

      input%round = input%round + 1
      do y = 1, SIZE
         do x = 1, SIZE
            unit = input%map(x, y)
            if (unit%round == input%round) then
               cycle
            end if
            input%map(x, y)%round = input%round
            if (unit%type /= ELF .and. unit%type /= GOBLIN) then
               cycle
            end if
            if (input%elfs == 0 .or. input%goblins == 0) then
               input%round = input%round - 1
               return
            end if
            call play_turn(input, x, y)
         end do
      end do
   end subroutine

   type(t_input) function loadInput()
      type(t_input) :: input
      type(t_unit) :: unit
      integer :: file, x, y
      character(:), allocatable :: line

      file = openFile(15)

      unit%hit_points = 0
      unit%type = EMPTY
      unit%round = 0

      input%map = unit
      input%elfs = 0
      input%goblins = 0
      input%round = 0
      input%elfPower = 3

      y = 0
      do while (readLine(file, line))
         y = y + 1
         do x = 1, len(line)
            select case (line(x:x))
            case ('#')
               unit%type = WALL
            case ('.')
               unit%type = EMPTY
            case ('E')
               unit%hit_points = DEFAULT_HIT_POINTS
               unit%type = ELF
               input%elfs = input%elfs + 1
            case ('G')
               unit%hit_points = DEFAULT_HIT_POINTS
               unit%type = GOBLIN
               input%goblins = input%goblins + 1
            case default
               unit%type = EMPTY
            end select

            input%map(x, y) = unit
         end do
         deallocate (line)
      end do

      call closeFile(file)

      loadInput = input
   end function

   integer function Part1()
      type(t_input) :: input

      input = loadInput()
      do while (input%elfs /= 0 .and. input%goblins /= 0)
         call play_round(input)
      end do
      Part1 = input%score()
   end function

   integer function Part2()
      type(t_input) :: input
      type(t_input) :: backup
      integer :: min, max, current
      integer :: elfs

      input = loadInput()
      elfs = input%elfs

      min = 4
      max = 200

      backup = input

      do while (min < max)
         input = backup
         current = (min + max)/2
         input%elfPower = int(current, 2)

         do while (input%elfs == elfs .and. input%goblins /= 0)
            call play_round(input)
         end do

         if (input%goblins == 0 .and. input%elfs == elfs) then
            ! won
            max = current
         else
            ! lost
            min = current + 1
         end if
      end do

      input = backup
      input%elfPower = int(max, 2)
      do while (input%goblins /= 0)
         call play_round(input)
      end do

      Part2 = input%score()
   end function

   subroutine Day15Solve()
      integer :: answer

      print *, '--- Day 15 ---'

      answer = Part1()
      print *, 'Answer to part 1 is ', answer

      answer = Part2()
      print *, 'Answer to part 2 is ', answer
   end subroutine

end module
