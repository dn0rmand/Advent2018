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
   end type

   integer(kind=4), parameter :: A = int(b'011010011001111110011001', 4)
   integer(kind=4), parameter :: B = int(b'111010011110100110011110', 4)
   integer(kind=4), parameter :: C = int(b'011010011000100010010110', 4)
   integer(kind=4), parameter :: D = int(b'111010011001100110011110', 4)
   integer(kind=4), parameter :: E = int(b'111110001110100010001111', 4)
   integer(kind=4), parameter :: F = int(b'111110001110100010001000', 4)
   integer(kind=4), parameter :: G = int(b'011010011000101110010111', 4)
   integer(kind=4), parameter :: H = int(b'100110011111100110011001', 4)
   integer(kind=4), parameter :: I = int(b'011100100010001000100111', 4)
   integer(kind=4), parameter :: J = int(b'001100010001000110010110', 4)
   integer(kind=4), parameter :: K = int(b'100110101100101010101001', 4)
   integer(kind=4), parameter :: L = int(b'100010001000100010001111', 4)
   integer(kind=4), parameter :: M = -1
   integer(kind=4), parameter :: N = -1
   integer(kind=4), parameter :: O = int(b'011010011001100110010110', 4)
   integer(kind=4), parameter :: P = int(b'111010011001111010001000', 4)
   integer(kind=4), parameter :: Q = -1
   integer(kind=4), parameter :: R = int(b'111010011001111010101001', 4)
   integer(kind=4), parameter :: S1 = int(b'011110000110100000011110', 4)
   integer(kind=4), parameter :: S2 = int(b'011110001000011000011110', 4)
   integer(kind=4), parameter :: T = -1
   integer(kind=4), parameter :: U = int(b'100110011001100110010110', 4)
   integer(kind=4), parameter :: V = -1
   integer(kind=4), parameter :: W = -1
   integer(kind=4), parameter :: X = -1
   integer(kind=4), parameter :: Y = int(b'100010000101001000100010', 4)
   integer(kind=4), parameter :: Z = int(b'111100010010010010001111', 4)

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
      letters%letter(19) = create_letter('S', S1)
      letters%letter(20) = create_letter('S', S2)
      letters%letter(21) = create_letter('T', T)
      letters%letter(22) = create_letter('U', U)
      letters%letter(23) = create_letter('V', V)
      letters%letter(24) = create_letter('W', W)
      letters%letter(25) = create_letter('X', X)
      letters%letter(26) = create_letter('Y', y)
      letters%letter(27) = create_letter('Z', Z)
   end subroutine

   character function getLetter(screen, index)
      type(t_screen) :: screen
      integer :: index
      getLetter = '?'
   end function

   character(10) function translate(screen)
      type(t_screen) :: screen
      integer :: i, j
      character(100) :: line
      type(t_letters) :: letters

      call letters%load()

      j = 0
      do i = 1, screen%width, 5
         j = j + 1
         line(j:j) = getLetter(screen, j)
      end do
      translate = trim(line)
   end function

end module
