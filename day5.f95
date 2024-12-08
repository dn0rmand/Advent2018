module Day5
   use tools

   implicit none
   private :: Part1, Part2
   public :: Day5Solve

   type :: t_chain
      character :: value
      type(t_chain), pointer :: next => null()
      type(t_chain), pointer :: previous => null()
   end type

   type :: t_chain_info
      type(t_chain), pointer :: first => null()
   end type

contains

   subroutine LoadInput(first)
      integer :: file
      integer :: i, l
      character(len=60000) :: buffer
      character(:), allocatable :: line
      type(t_chain), pointer :: next
      type(t_chain), pointer :: current => null()
      type(t_chain), pointer, intent(out) :: first

      file = openFile(5)
      read (file, '(A)') buffer
      call closeFile(file)
      ! buffer = 'dabAcCaCBAcCcaDA'
      line = trim(buffer)
      l = len(line)
      do i = 1, l
         allocate (next)
         next%value = line(i:i)

         if (.not. associated(first)) then
            first => next
            current => next
         else
            next%previous => current
            current%next => next
            current => next
         end if
      end do
      deallocate (line)
   end subroutine

   logical function shouldCollapse(c1, c2)
      character :: c1, c2

      if (iachar(c1) == iachar(c2) + 32 .or. iachar(c1) + 32 == iachar(c2)) then
         shouldCollapse = .true.
      else
         shouldCollapse = .false.
      end if
   end function

   subroutine collapse(chain)
      type(t_chain_info), intent(inout) :: chain
      type(t_chain), pointer:: first
      type(t_chain), pointer :: one, two
      type(t_chain), pointer :: current

      first => chain%first
      current => first
      do while (associated(current%next))
         if (shouldCollapse(current%value, current%next%value)) then
            one => current
            two => current%next

            if (.not. associated(current%previous)) then
               first => current%next%next
               if (associated(first)) then
                  first%previous => null()
               end if
               chain%first = first
            else
               current => one%previous
               current%next => two%next
               if (associated(current%next)) then
                  current%next%previous => current
               end if
            end if

            deallocate (one)
            deallocate (two)
         else
            current => current%next
         end if
      end do
   end subroutine

   subroutine cleanup(chain)
      type(t_chain_info), intent(inout) :: chain
      type(t_chain), pointer :: current
      type(t_chain), pointer :: old

      current => chain%first
      chain%first => null()

      do while (associated(current))
         current%previous => null()
         old => current
         current => current%next
         old%next => null()
         deallocate (old)
      end do
   end subroutine

   type(t_chain_info) function clone(chain, letter1, letter2)
      type(t_chain_info) :: result, chain
      character:: letter1, letter2
      type(t_chain), pointer :: ptr
      type(t_chain), pointer :: current => null()
      type(t_chain), pointer :: next => null()

      ptr => chain%first
      result%first => null()
      do while (associated(ptr))
         if (ptr%value /= letter1 .and. ptr%value /= letter2) then
            allocate (next)
            next%previous => null()
            next%next => null()
            next%value = ptr%value

            if (.not. associated(result%first)) then
               result%first => next
            end if
            if (associated(current)) then
               next%previous => current
               current%next => next
            end if
            current => next
         end if
         ptr => ptr%next
      end do

      clone = result
   end function

   integer function length(chain)
      type(t_chain_info) :: chain
      type(t_chain), pointer :: current
      integer :: size

      size = 0
      current => chain%first
      do while (associated(current))
         size = size + 1
         current => current%next
      end do
      length = size
   end function

   integer function Part1()
      type(t_chain), pointer :: input => null()
      type(t_chain_info) :: chain

      call LoadInput(input)

      chain%first => input

      call collapse(chain)
      Part1 = length(chain)
      call cleanup(chain)
   end function

   integer function Part2()
      type(t_chain_info) :: chain
      type(t_chain_info), dimension(26) :: cloned
      type(t_chain), pointer :: input => null()
      character :: letter1, letter2
      integer :: i, l, min

      call LoadInput(input)

      chain%first => input

      call collapse(chain)

      min = length(chain)
      do i = 1, 26
         letter1 = achar(i + 64)
         letter2 = achar(i + 64 + 32)
         cloned(i) = clone(chain, letter1, letter2)
         call collapse(cloned(i))
         l = length(cloned(i))
         if (l < min) then
            min = l
         end if
      end do
      print *, min
      ! do i = 26, 1, -1
      !    call cleanup(cloned(i))
      ! end do

      call cleanup(chain)

      Part2 = min
   end function Part2

   subroutine Day5Solve()
      integer :: answer

      print *, '--- Day 5 ---'

      answer = Part1()
      print *, 'Answer to part 1 is ', answer

      answer = Part2()
      print *, 'Answer to part 2 is ', answer
   end subroutine Day5Solve

end module Day5
