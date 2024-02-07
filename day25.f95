module Day25

   use tools

   implicit none

   public :: Day25Solve

   type :: t_point
      integer :: x, y, z, t
      integer :: group = 0
   end type

   type, extends(sortable) :: t_input
      type(t_point), dimension(1500) :: points
   contains
      procedure :: compare => input_compare
      procedure :: swap => input_swap
   end type
contains

   integer function input_compare(this, i1, i2)
      class(t_input), intent(in) :: this
      integer, intent(in) :: i1, i2

      input_compare = this%points(i1)%group - this%points(i2)%group
   end function

   subroutine input_swap(this, i1, i2)
      class(t_input), intent(inout) :: this
      integer, intent(in) :: i1, i2
      type(t_point) :: p1, p2

      p1 = this%points(i1)
      p2 = this%points(i2)

      this%points(i1) = p2
      this%points(i2) = p1
   end subroutine

   type(t_input) function loadInput()
      integer :: file, ios
      type(t_point) :: p
      type(t_input) :: input

      file = openFile(25)

      do while (.true.)
         read (file, *, iostat=ios) p%x, p%y, p%z, p%t
         if (ios /= 0) then
            exit
         end if
         input%size = input%size + 1
         input%points(input%size) = p
      end do

      call closeFile(file)

      loadInput = input
   end function

   integer function distance(p1, p2)
      type(t_point), pointer :: p1, p2

      distance = abs(p1%x - p2%x) + abs(p1%y - p2%y) + abs(p1%z - p2%z) + abs(p1%t - p2%t); 
   end function

   integer function Part1()
      type(t_input), target :: input
      integer :: groups, id
      integer :: i, j, k, fromId, toId
      type(t_point), pointer :: p1, p2

      input = loadInput()

      ! generate groups
      groups = 0
      id = 0
      do i = 1, input%size
         p1 => input%points(i)
         if (p1%group == 0) then
            id = id + 1
            p1%group = id
         end if
         do j = i + 1, input%size
            p2 => input%points(j)
            if (p1%group == p2%group) then
               cycle
            end if
            if (distance(p1, p2) <= 3) then
               if (p2%group == 0) then
                  p2%group = p1%group
               else
                  fromId = p1%group
                  toId = p2%group
                  do k = 1, input%size
                     p2 => input%points(k)
                     if (p2%group == fromId) then
                        p2%group = toId
                     end if
                  end do
               end if
            end if
         end do
      end do

      groups = 0
      toId = 0

      ! Now count the groups

      call input%sort()

      do i = 1, input%size
         if (input%points(i)%group /= toId) then
            groups = groups + 1
            toId = input%points(i)%group
         end if
      end do

      Part1 = groups
   end function

   subroutine Day25Solve()
      integer :: answer

      print *, '--- Day 25 ---'

      answer = Part1()
      print *, 'Answer to part 1 is ', answer

      print *, 'Merry Christmas'
   end subroutine

end module
