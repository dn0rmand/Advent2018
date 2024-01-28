module Day22

   use tools

   implicit none

   private :: Part1, Part2
   public :: Day22Solve

   integer(kind=2), parameter :: ZERO = 0
   integer(kind=2), parameter :: ONE = 1
   integer(kind=2), parameter :: SEVEN = 7
   integer(kind=2), parameter :: MAX_TIME = 1500

   integer, parameter :: MAP_SIZE_X = 50
   integer, parameter :: MAP_SIZE_Y = 800

   integer, parameter :: STATE_SIZE = 6000
   integer, parameter :: ADDED_SIZE = (MAP_SIZE_Y*(MAP_SIZE_X + 1) + MAP_SIZE_X)*4 + 3

   integer, parameter :: ROCKY = 0
   integer, parameter :: WET = 1
   integer, parameter :: NARROW = 2
   integer, parameter :: OUTSIDE = 3

   integer(kind=1), parameter :: NEITHER = 0
   integer(kind=1), parameter :: CLIMBING_GEAR = 1
   integer(kind=1), parameter :: TORCH = 2

   type :: t_map
      integer, dimension(0:MAP_SIZE_X, 0:MAP_SIZE_Y) :: map
   contains
      procedure :: fill => fill_map
      procedure :: get => get_type
   end type

   type :: t_state
      integer(kind=2) :: x, y, time
      integer(kind=1) :: tool
   end type

   type :: t_states
      type(t_state), dimension(STATE_SIZE) :: states1
      type(t_state), dimension(STATE_SIZE) :: states2
      type(t_state), dimension(:), pointer :: states => null()
      type(t_state), dimension(:), pointer :: newStates => null()
      integer, dimension(ADDED_SIZE) :: added
      integer :: count, newCount
   contains
      procedure :: init => states_init
      procedure :: swap => states_swap
      procedure :: add => states_add
      procedure :: get => states_get
   end type

   type :: t_engine
      type(t_map), pointer :: map => null()
      integer(kind=2), allocatable, dimension(:, :, :) :: visited
      type(t_states), allocatable :: states
      integer(kind=2) :: targetX, targetY, maxTime
   contains
      procedure :: init => engine_initialize
      procedure :: free => engine_free
      procedure :: execute => engine_execute
      procedure :: add => engine_add
      procedure :: moves => engine_moves
   end type

   type :: t_input
      integer :: depth
      integer(kind=2) :: targetX, targetY
   end type

contains

! STATES COLLECTION

   subroutine states_init(this)
      class(t_states), intent(inout), target :: this

      this%states => this%states1
      this%newStates => this%states2
      this%newCount = 0
      this%count = 0
   end subroutine

   subroutine states_swap(this)
      class(t_states), intent(inout) :: this
      type(t_state), dimension(:), pointer :: tmp

      this%added = 0

      tmp => this%states
      this%states => this%newStates
      this%newStates => tmp
      this%count = this%newCount
      this%newCount = 0
   end subroutine

   subroutine states_add(this, x, y, tool, time)
      class(t_states), intent(inout) :: this
      integer(kind=2), intent(in) :: x, y, time
      integer(kind=1), intent(in) :: tool
      integer :: key, index
      type(t_state) :: state

      if (x < 0 .or. y < 0 .or. x > MAP_SIZE_X .or. y > MAP_SIZE_Y) then
         return
      end if

      state%x = x
      state%y = y
      state%tool = tool
      state%time = time

      key = (int(y, 4)*MAP_SIZE_X + int(x, 4))*4 + int(tool, 4)
      index = this%added(key)
      if (index > 0) then
         if (this%newStates(index)%time > time) then
            this%newStates(index) = state
         end if
      else
         this%newCount = this%newCount + 1
         this%newStates(this%newCount) = state
         this%added(key) = this%newCount
      end if
   end subroutine

   type(t_state) function states_get(this, index)
      class(t_states), intent(inout) :: this
      integer, intent(in) :: index

      states_get = this%states(index)
   end function

! STATE MACHINE ENGINE

   logical function isToolValid(tool, type)
      integer(kind=1) :: type
      integer(kind=1) :: tool

      select case (type)
      case (ROCKY)
         isToolValid = tool /= NEITHER
      case (WET)
         isToolValid = tool /= TORCH
      case (NARROW)
         isToolValid = tool /= CLIMBING_GEAR
      case default
         isToolValid = .false.
      end select
   end function

   subroutine engine_add(this, state, x, y, tool)
      class(t_engine), intent(inout) :: this
      type(t_state), intent(in) :: state
      integer(kind=2), intent(in) :: x, y
      integer(kind=1), intent(in) :: tool
      integer(kind=2) :: time
      logical :: valid

      if (x < 0 .or. x > MAP_SIZE_X .or. y < 0 .or. y > MAP_SIZE_Y) then
         return
      end if

      if (.not. isToolValid(tool, this%map%get(x, y))) then
         return
      end if

      time = state%time
      if (state%x /= x .or. state%y /= y) then
         time = time + ONE
      end if
      if (tool /= state%tool) then
         time = time + SEVEN
      end if

      if (time >= this%maxTime) then
         return
      end if

      if (time < this%visited(tool, x, y)) then
         if (x == this%targetX .and. y == this%targetY .and. tool == TORCH) then
            this%maxTime = time
         end if
         this%visited(tool, x, y) = time
         call this%states%add(x, y, tool, time)
      end if
   end subroutine

   subroutine engine_moves(this, state)
      class(t_engine), intent(inout) :: this
      type(t_state), intent(in) :: state

      call this%add(state, state%x, state%y, TORCH)
      call this%add(state, state%x, state%y, CLIMBING_GEAR)
      call this%add(state, state%x, state%y, NEITHER)

      call this%add(state, state%x + ONE, state%y, state%tool)
      call this%add(state, state%x - ONE, state%y, state%tool)
      call this%add(state, state%x, state%y + ONE, state%tool)
      call this%add(state, state%x, state%y - ONE, state%tool)
   end subroutine

   subroutine engine_free(this)
      class(t_engine), intent(inout) :: this

      deallocate (this%visited)
      deallocate (this%states)
   end subroutine

   subroutine engine_initialize(this, input, map)
      class(t_engine), intent(inout) :: this
      type(t_map), intent(in), pointer :: map
      type(t_input), intent(in) :: input
      integer(kind=1) :: i
      integer(kind=2) :: x, y

      this%map => map
      this%targetX = input%targetX
      this%targetY = input%targetY
      this%maxTime = MAX_TIME

      allocate (this%states)
      allocate (this%visited(0:2, 0:MAP_SIZE_X, 0:MAP_SIZE_Y))

      this%visited = MAX_TIME

      call this%states%init()
      call this%states%add(ZERO, ZERO, TORCH, ZERO)
      call this%states%swap()
   end subroutine

   integer function engine_execute(this)
      class(t_engine), intent(inout) :: this
      type(t_state) :: state
      integer :: i, maxStates

      maxStates = 0
      do while (this%states%count > 0)
         maxStates = max(maxStates, this%states%count)
         this%maxTime = this%visited(TORCH, this%targetX, this%targetY); 
         do i = 1, this%states%count
            state = this%states%get(i)
            if (state%x /= this%targetX .or. state%y /= this%targetY) then
               call this%moves(state)
            end if
         end do
         call this%states%swap()
      end do
      engine_execute = this%visited(TORCH, this%targetX, this%targetY)
   end function

! MAP

   integer(kind=1) function get_type(this, x, y)
      class(t_map), intent(in) :: this
      integer(kind=2), intent(in) :: x, y

      if (x < 0 .or. x > MAP_SIZE_X) then
         get_type = OUTSIDE
      else if (y < 0 .or. y > MAP_SIZE_Y) then
         get_type = OUTSIDE
      else
         get_type = int(modulo(this%map(x, y), 3), 1)
      end if
   end function

   subroutine fill_map(this, input)
      class(t_map), intent(inout) :: this
      type(t_input), intent(in) :: input
      integer :: x, y, g

      do y = 0, MAP_SIZE_Y
         do x = 0, MAP_SIZE_X
            if (x == input%targetX .and. y == input%targetY) then
               g = 0
            else if (x == 0) then
               g = modulo(y*48271, 20183)
            else if (y == 0) then
               g = modulo(x*16807, 20183)
            else
               g = modulo(this%map(x - 1, y)*this%map(x, y - 1), 20183)
            end if
            this%map(x, y) = modulo(g + input%depth, 20183)
         end do
      end do
   end subroutine

! LOAD

   type(t_input) function loadInput()
      integer :: file
      character(7) :: dummy
      type(t_input) :: input

      file = openFile(22)
      read (file, *) dummy, input%depth
      read (file, *) dummy, input%targetX, input%targetY
      call closeFile(file)

      loadInput = input
   end function

! PART 1

   integer function Part1(input, map)
      type(t_input) :: input
      type(t_map), pointer :: map
      integer(kind=2) :: x, y
      integer :: risk

      risk = 0
      do y = ZERO, input%targetY
         do x = ZERO, input%targetX
            risk = risk + map%get(x, y)
         end do
      end do
      Part1 = risk
   end function

! PART 2

   integer function Part2(input, map)
      type(t_input) :: input
      type(t_map), pointer :: map
      type(t_engine) :: engine

      call engine%init(input, map)

      Part2 = engine%execute()

      call engine%free()
   end function

! SOLVE

   subroutine Day22Solve()
      integer :: answer1, answer2
      type(t_input) :: input
      type(t_map), pointer :: map

      print *, '--- Day 22 ---'

      input = loadInput()

      allocate (map)
      call map%fill(input)

      answer1 = Part1(input, map)
      print *, 'Answer to part 1 is ', answer1

      answer2 = Part2(input, map)
      print *, 'Answer to part 2 is ', answer2

      deallocate (map)
   end subroutine

end module
