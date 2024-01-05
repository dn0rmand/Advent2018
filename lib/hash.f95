! Module implementing an OO hash table (dictionary) in Fortran 2003.
! Compiles and runs with accompanying test program under the Intel 
! Fortran Compiler, version 11.1.046

! Copyright (c) Izaak Beekman 2010

    ! This program is free software: you can redistribute it and/or modify
    ! it under the terms of the GNU Lesser General Public License as published by
    ! the Free Software Foundation, either version 3 of the License, or
    ! (at your option) any later version.

    ! This program is distributed in the hope that it will be useful,
    ! but WITHOUT ANY WARRANTY; without even the implied warranty of
    ! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    ! GNU Lesser General Public License for more details.

    ! You should have received a copy of the GNU Lesser General Public License
    ! along with this program.  If not, see <http://www.gnu.org/licenses/>.

MODULE hashtbl
  IMPLICIT NONE ! Use strong typing
  INTEGER, PARAMETER :: tbl_size = 50

  TYPE sllist
     TYPE(sllist), POINTER :: child => NULL()
     integer :: key, val
     logical :: inUse = .FALSE.
   CONTAINS
     PROCEDURE :: put  => put_sll
     PROCEDURE :: get  => get_sll
     PROCEDURE :: has  => has_sll
     PROCEDURE :: free => free_sll
  END TYPE sllist

  TYPE hash_tbl_sll
     TYPE(sllist), DIMENSION(:), ALLOCATABLE :: vec
     INTEGER                                 :: vec_len = 0
     LOGICAL                                 :: is_init = .FALSE.
   CONTAINS
     PROCEDURE :: init => init_hash_tbl_sll
     PROCEDURE :: put  => put_hash_tbl_sll
     PROCEDURE :: get  => get_hash_tbl_sll
     PROCEDURE :: has  => has_hash_tbl_sll
     PROCEDURE :: free => free_hash_tbl_sll
  END TYPE hash_tbl_sll

  PUBLIC :: hash_tbl_sll

  CONTAINS

  RECURSIVE SUBROUTINE put_sll(list,key,val)
    CLASS(sllist),    INTENT(inout) :: list
    integer,          INTENT(in)    :: key, val

    IF (.NOT. list%inUse) THEN
      list%key = key
      list%val = val
      list%inUse = .true.
    ELSE 
      IF (list%key .eq. key) THEN
        list%val = val
      ELSE
        IF ( .NOT. ASSOCIATED(list%child) ) THEN
          ALLOCATE(list%child)
        END IF
        CALL put_sll(list%child, key, val)
      END IF
    END IF
  END SUBROUTINE put_sll


  RECURSIVE SUBROUTINE get_sll(list,key,val)
    CLASS(sllist),  INTENT(in)    :: list
    integer,        INTENT(in)    :: key
    integer,        INTENT(out)   :: val

    IF (.not. list%inUse) THEN
      val = -32767
    ELSE IF (list%key .eq. key) THEN
       val = list%val
    ELSE IF (ASSOCIATED(list%child)) THEN ! keep going
       CALL get_sll(list%child,key,val)
    ELSE ! At the end of the list, no key found
       val = -32767
    END IF
  END SUBROUTINE get_sll

  RECURSIVE SUBROUTINE has_sll(list,key,exists)
    CLASS(sllist),  INTENT(in)    :: list
    integer,        INTENT(in)    :: key
    logical,        INTENT(out)   :: exists

    IF (.not. list%inUse) THEN
      exists = .false.
    ELSE IF (list%key .eq. key) THEN
      exists = .true.
    ELSE IF (ASSOCIATED(list%child)) THEN ! keep going
      CALL has_sll(list%child,key,exists)
    ELSE ! At the end of the list, no key found
      exists = .false.
    END IF
  END SUBROUTINE has_sll

  RECURSIVE SUBROUTINE free_sll(list)
    CLASS(sllist), INTENT(inout) :: list
    IF (ASSOCIATED(list%child)) THEN
       CALL free_sll(list%child)
       DEALLOCATE(list%child)
    END IF
    list%child => NULL()
  END SUBROUTINE free_sll

  SUBROUTINE init_hash_tbl_sll(tbl,tbl_len)
    CLASS(hash_tbl_sll),   INTENT(inout) :: tbl
    INTEGER,     OPTIONAL, INTENT(in)    :: tbl_len

    IF (ALLOCATED(tbl%vec)) DEALLOCATE(tbl%vec)
    IF (PRESENT(tbl_len)) THEN
       ALLOCATE(tbl%vec(0:tbl_len+1))
       tbl%vec_len = tbl_len
    ELSE
       ALLOCATE(tbl%vec(0:tbl_size+1))
       tbl%vec_len = tbl_size
    END IF
    tbl%is_init = .TRUE.
  END SUBROUTINE init_hash_tbl_sll

  SUBROUTINE put_hash_tbl_sll(tbl,key,val)
    CLASS(hash_tbl_sll), INTENT(inout) :: tbl
    INTEGER,             INTENT(in)    :: key, val
    INTEGER                            :: hash

    hash = MOD(key + tbl%vec_len,tbl%vec_len)
    CALL tbl%vec(hash)%put(key=key,val=val)
  END SUBROUTINE put_hash_tbl_sll

  SUBROUTINE get_hash_tbl_sll(tbl,key,val)
    CLASS(hash_tbl_sll),  INTENT(in)    :: tbl
    INTEGER,              INTENT(in)    :: key
    INTEGER,              INTENT(out)   :: val
    INTEGER                             :: hash

    hash = MOD(key + tbl%vec_len,tbl%vec_len)
    CALL tbl%vec(hash)%get(key=key,val=val)
  END SUBROUTINE get_hash_tbl_sll

  SUBROUTINE has_hash_tbl_sll(tbl,key,exists)
    CLASS(hash_tbl_sll),  INTENT(in)    :: tbl
    INTEGER,              INTENT(in)    :: key
    LOGICAL,              INTENT(out)   :: exists
    INTEGER                             :: hash

    hash = MOD(key + tbl%vec_len,tbl%vec_len)
    CALL tbl%vec(hash)%has(key=key,exists=exists)
  END SUBROUTINE has_hash_tbl_sll

  SUBROUTINE free_hash_tbl_sll(tbl)
    CLASS(hash_tbl_sll), INTENT(inout) :: tbl    
    INTEGER     :: i, low, high

    low  = LBOUND(tbl%vec,dim=1)
    high = UBOUND(tbl%vec,dim=1) 
    IF (ALLOCATED(tbl%vec)) THEN
       DO i=low,high
          CALL tbl%vec(i)%free()
       END DO
       DEALLOCATE(tbl%vec)
    END IF
    tbl%is_init = .FALSE.
  END SUBROUTINE free_hash_tbl_sll

END MODULE hashtbl