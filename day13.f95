module Day13

   use tools

   implicit none

   private :: Part1, Part2
   public :: Day13Solve

   integer, parameter :: UP = 1
   integer, parameter :: LEFT = 2
   integer, parameter :: DOWN = 3
   integer, parameter :: RIGHT = 4

   type :: t_cart
      integer(kind=1) :: id, direction
      integer :: x, y
      integer(kind=1) :: turnCount = 0
   contains
      procedure :: move => move_cart
      procedure :: key => cart_key
   end type

   type, extends(sortable) :: t_input
      integer :: width = 0, height = 0
      integer(kind=1), dimension(150, 150) :: map = 0
      type(t_cart), dimension(20) :: carts
      integer(kind=1), dimension(151*151) :: positions
   contains
      procedure :: compare => input_compare
      procedure :: swap => input_swap
      procedure :: add => input_add_cart
      procedure :: move => input_move_carts
      procedure :: remove => remove_cart
   end type

contains

   integer function input_compare(this, i1, i2)
      class(t_input), intent(in) :: this
      integer, intent(in) :: i1, i2
      type(t_cart) :: a, b
      integer :: cmpValue

      a = this%carts(i1)
      b = this%carts(i2)

      if (a%direction == 0 .or. b%direction == 0) then
         cmpValue = b%direction - a%direction
      else
         cmpValue = 0
      end if

      if (cmpValue == 0) then
         cmpValue = a%y - b%y
      end if

      if (cmpValue == 0) then
         cmpValue = a%x - b%x
      end if

      input_compare = cmpValue
   end function

   subroutine input_swap(this, i1, i2)
      class(t_input), intent(inout) :: this
      integer, intent(in) :: i1, i2
      type(t_cart) :: t1, t2

      t1 = this%carts(i1)
      t2 = this%carts(i2)

      this%carts(i1) = t2
      this%carts(i2) = t1
   end subroutine

   integer function cart_key(cart)
      class(t_cart), intent(in) :: cart
      cart_key = cart%x*150 + cart%y
   end function

   subroutine move_cart(cart, input)
      class(t_cart), intent(inout) :: cart
      type(t_input), intent(in) :: input
      character :: c

      select case (cart%direction)
      case (UP)
         cart%y = cart%y - 1
      case (DOWN)
         cart%y = cart%y + 1
      case (LEFT)
         cart%x = cart%x - 1
      case (RIGHT)
         cart%x = cart%x + 1
      case default
         stop 'Invalid Direction'
      end select
      if (cart%y < 1 .or. cart%x < 1 .or. cart%y > input%height .or. cart%x > input%width) then
         print *, input%width, input%height
         print *, cart%id, cart%x, cart%y, cart%direction
         print *, 'WHAT'
      end if

      c = achar(input%map(cart%x, cart%y))

      select case (c)
      case ('/')
         select case (cart%direction)
         case (UP)
            cart%direction = RIGHT
         case (DOWN)
            cart%direction = LEFT
         case (LEFT)
            cart%direction = DOWN
         case (RIGHT)
            cart%direction = UP
         end select
      case ('\')
         select case (cart%direction)
         case (UP)
            cart%direction = LEFT
         case (DOWN)
            cart%direction = RIGHT
         case (LEFT)
            cart%direction = UP
         case (RIGHT)
            cart%direction = DOWN
         end select
      case ('+')
         select case (cart%turnCount)
         case (0)
            cart%direction = int(modulo(cart%direction, 4) + 1, 1)
         case (2)
            cart%direction = int(modulo(cart%direction - 2, 4) + 1, 1)
         end select
         cart%turnCount = int(modulo(cart%turnCount + 1, 3), 1)
      case ('|')
         c = c
      case ('-')
         c = c
      case default
         stop 'Should not get here'
      end select
   end subroutine

   subroutine remove_cart(input, id)
      class(t_input), intent(inout) :: input
      integer(kind=1), intent(in) :: id
      integer :: i

      do i = 1, input%size
         if (input%carts(i)%id == id) then
            input%carts(i)%direction = 0
            exit
         end if
      end do
   end subroutine

   integer function input_move_carts(input, autoFix)
      class(t_input), intent(inout) :: input
      logical, intent(in) :: autoFix
      integer :: i, k

      input_move_carts = 0
      do i = 1, input%size
         if (input%carts(i)%direction == 0) then
            cycle
         end if
         k = input%carts(i)%key()
         input%positions(k) = 0
         call input%carts(i)%move(input)
         k = input%carts(i)%key()
         if (input%positions(k) /= 0) then
            if (autoFix) then
               call input%remove(input%positions(k))
               call input%remove(input%carts(i)%id)
               input%positions(k) = 0
            else
               input_move_carts = i
               return
            end if
         else
            input%positions(k) = input%carts(i)%id
         end if
      end do

      call input%sort()

      do while (input%size > 0 .and. input%carts(input%size)%direction == 0)
         input%size = input%size - 1
      end do

      if (input%size == 1) then
         input_move_carts = 1
      end if
   end function

   character function input_add_cart(input, x, y, c)
      class(t_input), intent(inout) :: input
      character, intent(in) :: c
      integer, intent(in) :: x, y
      type(t_cart) :: cart

      cart%direction = 0
      select case (c)
      case ('^')
         cart%direction = UP
         input_add_cart = '|'
      case ('v')
         cart%direction = DOWN
         input_add_cart = '|'
      case ('<')
         cart%direction = LEFT
         input_add_cart = '-'
      case ('>')
         cart%direction = RIGHT
         input_add_cart = '-'
      case ('/')
         input_add_cart = c
      case ('\')
         input_add_cart = c
      case ('-')
         input_add_cart = c
      case ('|')
         input_add_cart = c
      case ('+')
         input_add_cart = c
      case (' ')
         input_add_cart = c
      case default
         stop 'invalid map charaacter'
      end select

      if (cart%direction /= 0) then
         cart%x = x
         cart%y = y
         input%size = input%size + 1
         cart%id = int(input%size, 1)
         input%carts(input%size) = cart
         input%positions(cart%key()) = cart%id
      end if
   end function

   type(t_input) function loadInput()
      integer :: file, y, x
      character :: c
      character(:), allocatable :: line
      type(t_input) :: input

      file = openFile(13)

      y = 0

      input%positions = 0

      do while (readLine(file, line))
         input%width = max(input%width, len(line))
         y = y + 1
         do x = 1, len(line)
            c = line(x:x)
            c = input%add(x, y, c)
            input%map(x, y) = int(iachar(c), 1)
         end do
         deallocate (line)
      end do
      input%height = y
      call closeFile(file)

      loadInput = input
   end function

   character(20) function Execute(autoFix)
      type(t_input), allocatable :: input
      type(t_cart) :: cart
      logical :: autoFix
      integer :: i

      input = loadInput()

      Execute = '?,?'

      do while (.true.)
         i = input%move(autoFix)
         if (i /= 0) then
            cart = input%carts(i)
            Execute = trim(itoa(cart%x - 1))//','//trim(itoa(cart%y - 1))
            exit
         end if
      end do
   end function

   character(20) function Part1()
      Part1 = Execute(.false.)
   end function

   character(20) function Part2()
      Part2 = Execute(.true.)
   end function

   subroutine Day13Solve()
      character(20) :: answer

      print *, '--- Day 13 ---'

      answer = Part1()
      print *, 'Answer to part 1 is ', answer

      answer = Part2()
      print *, 'Answer to part 2 is ', answer
   end subroutine

end module
