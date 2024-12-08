module ocr

   use screen

   implicit none

   public :: translate

   type :: t_letter
      character char
      integer(kind=4) value
   end type

   type :: t_letters
      type(t_letter) :: letter(27)
   contains
      procedure :: load => letters_load
      procedure :: get => letters_get
   end type

   integer(kind=4), parameter :: A = int(b'001100100001100001', 4)
   integer(kind=4), parameter :: C = int(b'011110100000011110', 4)
   integer(kind=4), parameter :: E = int(b'111111111110111111', 4)
   integer(kind=4), parameter :: F = int(b'111111111110100000', 4)
   integer(kind=4), parameter :: H = int(b'100001111111100001', 4)
   integer(kind=4), parameter :: J = int(b'000111000010011100', 4)
   integer(kind=4), parameter :: K = int(b'100001110000100001', 4)
   integer(kind=4), parameter :: N = int(b'100001101001100001', 4)
   integer(kind=4), parameter :: P = int(b'111110111110100000', 4)
   integer(kind=4), parameter :: R = int(b'111110111110100001', 4)
   integer(kind=4), parameter :: Z = int(b'111111000100111111', 4)

   ! need other input to figure out how to encode other letters
   integer(kind=4), parameter :: B = -1
   integer(kind=4), parameter :: D = -1
   integer(kind=4), parameter :: G = -1
   integer(kind=4), parameter :: I = -1
   integer(kind=4), parameter :: L = -1
   integer(kind=4), parameter :: M = -1
   integer(kind=4), parameter :: O = -1
   integer(kind=4), parameter :: Q = -1
   integer(kind=4), parameter :: S = -1
   integer(kind=4), parameter :: T = -1
   integer(kind=4), parameter :: U = -1
   integer(kind=4), parameter :: V = -1
   integer(kind=4), parameter :: W = -1
   integer(kind=4), parameter :: X = -1
   integer(kind=4), parameter :: Y = -1

contains

   type(t_letter) function create_letter(char, v)
      type(t_letter) :: letter
      character :: char
      integer(kind=4) :: v

      letter%char = char
      letter%value = v

      create_letter = letter
   end function

   subroutine letters_load(letters)
      class(t_letters), intent(inout) :: letters

      letters%letter(1) = create_letter('A', A)
      letters%letter(2) = create_letter('B', B)
      letters%letter(3) = create_letter('C', C)
      letters%letter(4) = create_letter('D', D)
      letters%letter(5) = create_letter('E', E)
      letters%letter(6) = create_letter('F', F)
      letters%letter(7) = create_letter('G', G)
      letters%letter(8) = create_letter('H', H)
      letters%letter(9) = create_letter('I', I)
      letters%letter(10) = create_letter('J', J)
      letters%letter(11) = create_letter('K', K)
      letters%letter(12) = create_letter('L', L)
      letters%letter(13) = create_letter('M', M)
      letters%letter(14) = create_letter('N', N)
      letters%letter(15) = create_letter('O', O)
      letters%letter(16) = create_letter('P', P)
      letters%letter(17) = create_letter('Q', Q)
      letters%letter(18) = create_letter('R', R)
      letters%letter(19) = create_letter('S', S)
      letters%letter(20) = create_letter('T', T)
      letters%letter(21) = create_letter('U', U)
      letters%letter(22) = create_letter('V', V)
      letters%letter(23) = create_letter('W', W)
      letters%letter(24) = create_letter('X', X)
      letters%letter(25) = create_letter('Y', y)
      letters%letter(26) = create_letter('Z', Z)
   end subroutine

   character function letters_get(letters, screen, index)
      class(t_letters), intent(in) :: letters
      type(t_screen) :: screen
      integer :: index, x
      integer(kind=4) value

      value = 0

      do x = 1, 6
         value = ishft(value, 1)
         if (screen%pixels(x + index, 1)) then
            value = ibset(value, 0)
         end if
      end do
      do x = 1, 6
         value = ishft(value, 1)
         if (screen%pixels(x + index, 5)) then
            value = ibset(value, 0)
         end if
      end do
      do x = 1, 6
         value = ishft(value, 1)
         if (screen%pixels(x + index, 10)) then
            value = ibset(value, 0)
         end if
      end do
      letters_get = '?'
      do x = 1, 26
         if (letters%letter(x)%value == value) then
            letters_get = letters%letter(x)%char
            exit
         end if
      end do
   end function

   character(10) function translate(screen)
      type(t_screen) :: screen
      integer :: i, j
      character(100) :: line
      type(t_letters) :: letters

      call letters%load()

      line = ''
      j = 0
      do i = 0, screen%width, 8
         j = j + 1
         line(j:j) = letters%get(screen, i)
      end do
      translate = trim(line)
   end function

end module
