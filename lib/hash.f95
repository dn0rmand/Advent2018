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

module hashtbl
  implicit none ! use strong typing
  integer, parameter :: tbl_size = 50

  type sllist
     type(sllist), pointer :: child => null()
     integer :: key, val
     logical :: inUse = .false.
   contains
     procedure :: put  => put_sll
     procedure :: get  => get_sll
     procedure :: has  => has_sll
     procedure :: free => free_sll
  end type sllist

  type hash_tbl_sll
     type(sllist), dimension(:), allocatable :: vec
     integer                                 :: vec_len = 0
     logical                                 :: is_init = .false.
   contains
     procedure :: init => init_hash_tbl_sll
     procedure :: put  => put_hash_tbl_sll
     procedure :: get  => get_hash_tbl_sll
     procedure :: has  => has_hash_tbl_sll
     procedure :: free => free_hash_tbl_sll
  end type hash_tbl_sll

  public :: hash_tbl_sll

  contains

  integer function getHash(key, modulo)
    integer :: key, modulo, hash

    hash = mod(key, modulo)
    hash = mod(hash + modulo, modulo)

    getHash = hash
  end function

  recursive subroutine put_sll(list,key,val)
    class(sllist),    intent(inout) :: list
    integer,          intent(in)    :: key, val

    if (.not. list%inUse) then
      list%key = key
      list%val = val
      list%inUse = .true.
    else 
      if (list%key .eq. key) then
        list%val = val
      else
        if ( .not. associated(list%child) ) then
          allocate(list%child)
        end if
        call put_sll(list%child, key, val)
      end if
    end if
  end subroutine put_sll

  recursive subroutine get_sll(list,key,val)
    class(sllist),  intent(in)    :: list
    integer,        intent(in)    :: key
    integer,        intent(out)   :: val

    if (.not. list%inUse) then
      val = -32767
    else if (list%key .eq. key) then
       val = list%val
    else if (associated(list%child)) then ! keep going
       call get_sll(list%child,key,val)
    else ! at the end of the list, no key found
       val = -32767
    end if
  end subroutine get_sll

  recursive subroutine has_sll(list,key,exists)
    class(sllist),  intent(in)    :: list
    integer,        intent(in)    :: key
    logical,        intent(out)   :: exists

    if (.not. list%inUse) then
      exists = .false.
    else if (list%key .eq. key) then
      exists = .true.
    else if (associated(list%child)) then ! keep going
      call has_sll(list%child,key,exists)
    else ! at the end of the list, no key found
      exists = .false.
    end if
  end subroutine has_sll

  recursive subroutine free_sll(list)
    class(sllist), intent(inout) :: list
    if (associated(list%child)) then
       call free_sll(list%child)
       deallocate(list%child)
    end if
    list%child => null()
  end subroutine free_sll

  subroutine init_hash_tbl_sll(tbl,tbl_len)
    class(hash_tbl_sll),   intent(inout) :: tbl
    integer,     optional, intent(in)    :: tbl_len

    if (allocated(tbl%vec)) deallocate(tbl%vec)
    if (present(tbl_len)) then
       allocate(tbl%vec(0:tbl_len+1))
       tbl%vec_len = tbl_len
    else
       allocate(tbl%vec(0:tbl_size+1))
       tbl%vec_len = tbl_size
    end if
    tbl%is_init = .true.
  end subroutine init_hash_tbl_sll

  subroutine put_hash_tbl_sll(tbl,key,val)
    class(hash_tbl_sll), intent(inout) :: tbl
    integer,             intent(in)    :: key, val
    integer                            :: hash

    hash = getHash(key, tbl%vec_len)
    call tbl%vec(hash)%put(key=key,val=val)
  end subroutine put_hash_tbl_sll

  integer function get_hash_tbl_sll(tbl,key)
    class(hash_tbl_sll),  intent(in)    :: tbl
    integer,              intent(in)    :: key
    integer                             :: val
    integer                             :: hash

    hash = getHash(key, tbl%vec_len)
    call tbl%vec(hash)%get(key=key,val=val)
    get_hash_tbl_sll = val
end function get_hash_tbl_sll

  logical function has_hash_tbl_sll(tbl, key)
    class(hash_tbl_sll), intent(in)    :: tbl
    integer,             intent(in)    :: key
    logical                            :: exists
    integer                            :: hash

    hash = getHash(key, tbl%vec_len)
    if (hash < 0) then
      print *, key, hash
    end if
    call tbl%vec(hash)%has(key=key,exists=exists)
    has_hash_tbl_sll = exists
  end function has_hash_tbl_sll

  subroutine free_hash_tbl_sll(tbl)
    class(hash_tbl_sll), intent(inout) :: tbl    
    integer     :: i, low, high

    low  = lbound(tbl%vec,dim=1)
    high = ubound(tbl%vec,dim=1) 
    if (allocated(tbl%vec)) then
       do i=low,high
          call tbl%vec(i)%free()
       end do
       deallocate(tbl%vec)
    end if
    tbl%is_init = .false.
  end subroutine free_hash_tbl_sll

end module hashtbl