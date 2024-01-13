module screen

   implicit none

   type :: t_screen
      logical, dimension(:, :), allocatable :: pixels
      integer :: width, height
   contains
      procedure :: create => screen_create
      procedure :: free => screen_free
      procedure :: set => screen_set_pixel
      procedure :: print => screen_print
   end type

contains

   subroutine screen_create(screen, width, height)
      class(t_screen), intent(inout) :: screen
      integer, intent(in) :: width, height
      integer :: x, y

      screen%width = width
      screen%height = height
      allocate (screen%pixels(width, height))
      do x = 1, width
         do y = 1, height
            screen%pixels(x, y) = .false.
         end do
      end do
   end subroutine

   subroutine screen_set_pixel(screen, x, y, pixel)
      class(t_screen), intent(inout) :: screen
      integer, intent(in) :: x, y
      logical, intent(in) :: pixel

      screen%pixels(x, y) = pixel
   end subroutine

   subroutine screen_print(screen)
      class(t_screen), intent(in) :: screen
      character, allocatable :: line(:)
      integer :: x, y

      allocate (line(screen%width))

      do y = 1, screen%height, 1
         ! if (y == 1 .or. y == 5 .or. y == 10) then
         do x = 1, screen%width, 1
            if (screen%pixels(x, y)) then
               line(x:x) = '#'
            else
               line(x:x) = ' '
            end if
         end do
         print *, line
         !  end if
      end do
      print *, ''
      deallocate (line)
   end subroutine

   subroutine screen_free(screen)
      class(t_screen), intent(inout) :: screen

      deallocate (screen%pixels)
   end subroutine

end module
