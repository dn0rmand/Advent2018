module Day9

   use tools

   implicit none

   private :: Part1, Part2
   public :: Day9Solve

   type :: t_marble
      integer :: value = 0
      type(t_marble), pointer :: next => null()
      type(t_marble), pointer :: previous => null()
   end type

   type :: t_circle
      type(t_marble), pointer :: current => null()
   contains
      procedure :: init => circle_init
      procedure :: add => circle_add
      procedure :: score => circle_score
      procedure :: cleanup => circle_cleanup
   end type

contains

   subroutine circle_init(circle)
      class(t_circle), intent(inout) :: circle
      type(t_marble), pointer :: marbleZero

      allocate (marbleZero)

      marbleZero%value = 0
      marbleZero%next => marbleZero
      marbleZero%previous => marbleZero

      circle%current => marbleZero
   end subroutine

   subroutine circle_cleanup(circle)
      class(t_circle), intent(inout) :: circle
      type(t_marble), pointer :: marble, tmp
      integer :: last

      marble => circle%current
      last = circle%current%value

      do while (marble%value /= last)
         tmp => marble
         marble => marble%next
         deallocate (tmp)
      end do
      deallocate (marble)
      circle%current => null()
   end subroutine

   subroutine circle_add(circle, marbleValue)
      class(t_circle), intent(inout) :: circle
      integer, intent(in) :: marbleValue
      type(t_marble), pointer :: marble, before, after

      before => circle%current%next
      after => before%next

      allocate (marble)

      marble%value = marbleValue
      marble%previous => before
      marble%next => after
      after%previous => marble
      before%next => marble

      circle%current => marble
   end subroutine

   integer function circle_score(circle, marbleValue)
      class(t_circle), intent(inout) :: circle
      integer, intent(in) :: marbleValue
      type(t_marble), pointer :: marble, before, after

      before => circle%current%previous%previous%previous%previous%previous%previous%previous%previous ! 7+1 times
      marble => before%next
      after => marble%next

      after%previous => before
      before%next => after
      circle%current => after

      circle_score = marble%value + marbleValue
      deallocate (marble)
   end function

   subroutine loadInput(players, points)
      integer :: file, ios
      integer, intent(out) :: players, points
      character(10)  :: lines(5)

      file = openFile(9)
      read (file, *, iostat=ios) players, lines, points
      call closeFile(file)

   end subroutine

   integer(kind=8) function process(players, points)
      integer, intent(in) :: players, points
      integer :: marble, player
      integer(kind=8) :: maxScore
      integer(kind=8), dimension(:), allocatable :: scores
      type(t_circle) :: circle

      allocate (scores(players))

      scores = 0
      maxScore = 0

      call circle%init()

      do marble = 1, points
         if (modulo(marble, 23) /= 0) then
            call circle%add(marble)
         else
            player = modulo(marble - 1, players) + 1
            scores(player) = scores(player) + circle%score(marble)
            maxScore = max(maxScore, scores(player))
         end if
      end do

      call circle%cleanup()

      process = maxScore
   end function

   integer(kind=8) function Part1()
      integer :: players, points

      call loadInput(players, points)
      Part1 = process(players, points)
   end function

   integer(kind=8) function Part2()
      integer :: players, points

      call loadInput(players, points)
      Part2 = process(players, points*100)
   end function

   subroutine Day9Solve()
      integer(kind=8) :: answer

      print *, '--- Day 9 ---'

      answer = Part1()
      print *, 'Answer to part 1 is ', answer

      answer = Part2()
      print *, 'Answer to part 2 is ', answer
   end subroutine

end module
