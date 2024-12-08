module Day7

   use tools

   implicit none

   private :: Part1, Part2
   public :: Day7Solve

   integer, parameter :: max_steps = 26

   type :: t_dependency
      character :: name
      type(t_dependency), pointer :: next => null()
   end type

   type :: t_step
      character :: name
      logical :: defined = .false.
      logical :: processed = .false.
      integer :: worker = 0
      type(t_dependency), pointer :: before => null()
   contains
      procedure :: cleanup => step_cleanup
      procedure :: available => step_is_available
   end type

   type :: t_input
      type(t_step) :: steps(max_steps)
   contains
      procedure :: cleanup => input_cleanup
      procedure :: nextAvailable => input_next_available
   end type

contains

   integer function asKey(letter)
      character, intent(in) :: letter
      asKey = iachar(letter) - 64
   end function

   subroutine step_cleanup(step)
      class(t_step), intent(inout) :: step
      type(t_dependency), pointer :: current, next

      current => step%before
      step%before => null()
      do while (associated(current))
         next => current%next
         deallocate (current)
         current => next
      end do
   end subroutine

   subroutine input_cleanup(input)
      class(t_input), intent(inout) :: input
      integer :: key

      do key = 1, max_steps
         call input%steps(key)%cleanup()
      end do
   end subroutine

   type(t_input) function LoadInput()
      integer :: file
      character :: before, after
      character(:), allocatable :: line
      type(t_input), target :: input
      type(t_step), pointer :: afterStep => null()
      type(t_step), pointer :: beforeStep => null()
      type(t_dependency), pointer :: dep
      file = openFile(7)

      do while (readLine(file, line))
         before = line(6:6)
         after = line(37:37)
         deallocate (line)

         afterStep => input%steps(asKey(after))
         beforeStep => input%steps(asKey(before))

         if (.not. beforeStep%defined) then
            beforeStep%name = before
            beforeStep%defined = .true.
         end if

         if (.not. afterStep%defined) then
            afterStep%name = after
            afterStep%defined = .true.
         end if

         allocate (dep)

         dep%name = before
         dep%next => afterStep%before
         afterStep%before => dep

         afterStep => null()
         beforeStep => null()
      end do

      call closeFile(file)

      loadInput = input
   end function

   logical function step_is_available(step, input)
      class(t_step), intent(in) :: step
      type(t_input), intent(in) :: input
      type(t_dependency), pointer :: dep

      step_is_available = .false.
      if (.not. step%defined) then
         return
      end if
      if (step%processed .or. step%worker > 0) then
         return
      end if

      dep => step%before
      do while (associated(dep))
         if (.not. input%steps(asKey(dep%name))%processed) then
            return
         end if
         dep => dep%next
      end do
      step_is_available = .true.
   end function

   integer function input_next_available(input)
      class(t_input), intent(in) :: input
      integer :: key

      input_next_available = 0

      do key = 1, max_steps
         if (input%steps(key)%available(input)) then
            input_next_available = key
            exit
         end if
      end do
   end function

   character(max_steps) function Part1()
      type(t_input) :: input
      character(max_steps) :: result
      integer :: key, index

      input = loadInput()

      result = ''
      index = 0
      key = input%nextAvailable()
      do while (key > 0)
         input%steps(key)%processed = .true.
         index = index + 1
         result(index:index) = input%steps(key)%name(1:1)
         key = input%nextAvailable()
      end do

      Part1 = trim(result)

      call input%cleanup()
   end function

   integer function Part2()
      type(t_input) :: input
      integer :: worker_times(5)
      integer :: worker_jobs(5)
      integer :: busyness
      integer :: key, time, w
      integer :: status

      input = loadInput()

      worker_times = 0
      worker_jobs = 0
      time = 0
      Part2 = 0

      key = 0

      ! statuses:
      !    1. Wait for a worker to be done
      !    2. Worker finished a job so try assigning a new one
      !    3. we must be done

      status = 2
      do while (status /= 3)
         if (status == 1) then
            busyness = max(worker_times(1), worker_times(2), worker_times(3), worker_times(4), worker_times(5))
            if (busyness == 0) then
               ! no worker busy ... back to 2
               cycle
            end if
            time = time + 1
            do w = 1, 5
               if (worker_times(w) > 0) then
                  worker_times(w) = worker_times(w) - 1
                  if (worker_times(w) == 0) then
                     status = 2
                     input%steps(worker_jobs(w))%processed = .true.
                     worker_jobs(w) = 0
                  end if
               end if
            end do
         else if (status == 2) then
            key = input%nextAvailable()
            if (key == 0) then
               busyness = max(worker_times(1), worker_times(2), worker_times(3), worker_times(4), worker_times(5))
               if (busyness == 0) then
                  status = 3
               else
                  status = 1
               end if
            else
               busyness = min(worker_times(1), worker_times(2), worker_times(3), worker_times(4), worker_times(5))
               if (busyness > 0) then
                  ! no free workers so status back to 1
                  status = 1
                  cycle
               end if
               do w = 1, 5
                  if (worker_times(w) == 0) then
                     worker_jobs(w) = key
                     worker_times(w) = key + 60
                     input%steps(key)%worker = w
                     exit
                  end if
               end do
            end if
         else
            stop 'What should I do ?'
         end if
      end do

      Part2 = time

      call input%cleanup()
   end function

   subroutine Day7Solve()
      character(max_steps) :: answer1
      integer :: answer2

      print *, '--- Day 7 ---'

      answer1 = Part1()
      print *, 'Answer to part 1 is ', answer1

      answer2 = Part2()
      print *, 'Answer to part 2 is ', answer2
   end subroutine

end module
