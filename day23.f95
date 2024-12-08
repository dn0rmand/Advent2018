module Day23

   use tools

   implicit none

   private :: Part1, Part2
   public :: Day23Solve

   integer, parameter :: MAX_BOXES = 50

   type :: t_point
      integer :: x, y, z
   contains
      procedure :: distance => point_distance
      procedure :: score => point_score
      procedure :: add => point_add
   end type

   type :: t_nanobot
      type(t_point) :: point
      integer :: r
   contains
      procedure :: inRange => bot_in_range
   end type

   type :: t_box
      type(t_point) :: point
      integer :: size, steps
   contains
      procedure :: score => box_score
   end type

   type :: t_boxes
      type(t_box), dimension(MAX_BOXES) :: boxes
      integer :: size = 0
      integer :: score
   contains
      procedure :: add => boxes_add
   end type

   type, extends(sortable) :: t_input
      integer :: minX, maxX, minY, maxY, minZ, maxZ
      type(t_nanobot), dimension(1100) :: bots
   contains
      procedure :: compare => input_compare
      procedure :: swap => input_swap

      procedure :: bestBoxes => input_best_boxes
   end type

contains
   subroutine boxes_add(this, boxes)
      class(t_boxes), intent(inout) :: this
      type(t_boxes), intent(in) :: boxes

      if (this%size + boxes%size > MAX_BOXES) then
         stop 'Too many boxes'
      end if

      this%boxes(this%size + 1:this%size + boxes%size) = boxes%boxes(1:boxes%size)
      this%size = this%size + boxes%size
   end subroutine

   integer function box_score(this, input)
      class(t_box), intent(in) :: this
      type(t_input), intent(in) :: input
      type(t_nanobot) :: bot
      type(t_point) :: pt
      integer :: i, score, x, y, z
      logical :: good

      score = 0
      do i = 1, input%size
         bot = input%bots(i)
         good = .false.
         do x = 0, this%size - 1, this%steps
            pt%x = this%point%x + x
            do y = 0, this%size - 1, this%steps
               pt%y = this%point%y + y
               do z = 0, this%size - 1, this%steps
                  pt%z = this%point%z + z
                  if (bot%inRange(pt)) then
                     good = .true.
                     exit
                  end if
               end do
               if (good) then
                  exit
               end if
            end do
            if (good) then
               exit
            end if
         end do
         if (good) then
            score = score + 1
         end if
      end do

      box_score = score
   end function

   type(t_point) function point_add(this, x, y, z)
      class(t_point), intent(in) :: this
      integer, intent(in) :: x, y, z
      type(t_point) :: p

      p%x = this%x + x
      p%y = this%y + y
      p%z = this%z + z
      point_add = p
   end function

   integer function point_score(this, input)
      class(t_point), intent(in) :: this
      type(t_input), intent(in) :: input
      integer :: i, score

      score = 0
      do i = 1, input%size
         if (input%bots(i)%inRange(this)) then
            score = score + 1
         end if
      end do

      point_score = score
   end function

   integer function point_distance(p1, p2)
      class(t_point), intent(in) :: p1
      type(t_point), intent(in) :: p2

      point_distance = abs(p1%x - p2%x) + abs(p1%y - p2%y) + abs(p1%z - p2%z)
   end function

   logical function bot_in_range(bot, point)
      class(t_nanobot), intent(in) :: bot
      type(t_point), intent(in) :: point

      bot_in_range = bot%point%distance(point) <= bot%r
   end function

   integer function input_compare(this, i1, i2)
      class(t_input), intent(in) :: this
      integer, intent(in) :: i1, i2

      input_compare = this%bots(i2)%r - this%bots(i1)%r
   end function

   subroutine input_swap(this, i1, i2)
      class(t_input), intent(inout) :: this
      integer, intent(in) :: i1, i2
      type(t_nanobot) :: b1, b2

      b1 = this%bots(i1)
      b2 = this%bots(i2)
      this%bots(i1) = b2
      this%bots(i2) = b1
   end subroutine

   type(t_input) function loadInput()
      integer :: file, i
      type(t_nanobot) :: bot
      type(t_input) :: input
      character :: c
      character(:), allocatable :: line

      input%minX = 0
      input%maxX = 0
      input%minY = 0
      input%maxY = 0
      input%minZ = 0
      input%maxZ = 0

      file = openFile(23)

      do while (readLine(file, line))
         ! cleanup the line
         do i = 1, len(line)
            c = line(i:i)
            if ((c < '0' .or. c > '9') .and. c /= '-') then
               line(i:i) = ' '
            end if
         end do

         read (line, *) bot%point%x, bot%point%y, bot%point%z, bot%r
         deallocate (line)

         input%size = input%size + 1
         input%bots(input%size) = bot

         input%minX = min(bot%point%x, input%minX)
         input%minY = min(bot%point%y, input%minY)
         input%minZ = min(bot%point%z, input%minZ)

         input%maxX = max(bot%point%x, input%maxX)
         input%maxY = max(bot%point%y, input%maxY)
         input%maxZ = max(bot%point%z, input%maxZ)
      end do

      call closeFile(file)

      call input%sort()

      loadInput = input
   end function

   type(t_boxes) function input_best_boxes(this, from, to, size, steps)
      class(t_input), intent(in) :: this
      type(t_point), intent(in) :: from, to
      integer, intent(in) :: size, steps
      type(t_box) :: box
      type(t_boxes) :: boxes
      integer :: x, y, z, score

      box%size = size
      box%steps = steps
      boxes%size = 0
      boxes%score = 0

      do x = from%x, to%x, size
         box%point%x = x
         do y = from%y, to%y, size
            box%point%y = y
            do z = from%z, to%z, size
               box%point%z = z
               score = box%score(this)
               if (score > boxes%score) then
                  boxes%size = 1
                  boxes%boxes(1) = box
                  boxes%score = score
               else if (score == boxes%score) then
                  boxes%size = boxes%size + 1
                  boxes%boxes(boxes%size) = box
               end if
            end do
         end do
      end do

      input_best_boxes = boxes
   end function

   integer function solve(this)
      type(t_input), intent(in) :: this
      integer :: SIZE_SPEED = 2
      integer :: STEP_SPEED = 5
      integer :: size, steps, oldSize, i, dist, score, s
      type(t_box) :: box
      type(t_point) :: from, to
      type(t_boxes) :: boxes, oldBoxes, tmp

      size = (min(this%maxX - this%minX, this%maxY - this%minY, this%maxZ - this%minZ)/SIZE_SPEED) + 1
      steps = size/STEP_SPEED
      from%x = this%minX
      from%y = this%minY
      from%z = this%minZ
      to%x = this%maxX
      to%y = this%maxY
      to%z = this%maxZ

      boxes = this%bestBoxes(from, to, size, steps)

      do while (boxes%size > 0 .and. size > 1)
         oldSize = size
         size = (size/SIZE_SPEED) + 1
         if (size == oldSize) then
            size = size/2
         end if
         steps = (size/STEP_SPEED) + 1

         oldBoxes = boxes
         boxes%size = 0
         boxes%score = 0

         do i = 1, oldBoxes%size
            box = oldBoxes%boxes(i)
            from = box%point
            to = from%add(box%size - 1, box%size - 1, box%size - 1)
            tmp = this%bestBoxes(from, to, size, steps)
            if (tmp%score > boxes%score) then
               boxes = tmp
            else if (tmp%score == boxes%score) then
               call boxes%add(tmp)
            end if
         end do
      end do

      score = -1
      dist = -1
      do i = 1, boxes%size
         box = boxes%boxes(i)
         s = box%point%score(this)
         if (s > score) then
            score = s
            dist = abs(box%point%x) + abs(box%point%y) + abs(box%point%z)
         else if (s == score) then
            dist = min(dist, abs(box%point%x) + abs(box%point%y) + abs(box%point%z))
         end if
      end do

      solve = dist
   end function

   integer function Part1()
      type(t_input) :: input
      type(t_nanobot) :: mainBot
      integer :: i, count

      input = loadInput()

      count = 1
      mainBot = input%bots(1)
      do i = 2, input%size
         if (mainBot%inRange(input%bots(i)%point)) then
            count = count + 1
         end if
      end do

      Part1 = count
   end function

   integer function Part2()
      type(t_input) :: input
      input = loadInput()

      Part2 = solve(input)
   end function

   subroutine Day23Solve()
      integer :: answer

      print *, '--- Day 23 ---'

      answer = Part1()
      print *, 'Answer to part 1 is ', answer

      answer = Part2()
      print *, 'Answer to part 2 is ', answer
   end subroutine

end module
