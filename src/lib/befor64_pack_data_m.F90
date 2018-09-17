!< KISS library for packing heterogeneous data into single (homogeneous) packed one.
!
module befor64_pack_data_m
!< KISS library for packing heterogeneous data into single (homogeneous) packed one.
use penf

implicit none
private
public :: pack_data

interface pack_data
  !< Pack different kinds of data into single I1P array.
  !<
  !< This is useful for encoding different (heterogeneous) kinds variables into a single (homogeneous) stream of bits.
  !< @note This procedure exploits the `transfer` builtin function, that from the standard (2003+) is defined as
  !< `TRANSFER(SOURCE, MOLD [, SIZE])`. Data object having a physical representation identical to that of `SOURCE` but with the type
  !< and type parameters of `MOLD`. The result is of the same type and type parameters as `MOLD`.
  !< If `MOLD` is an array and `SIZE` is absent, the result is an array and of rank one. Its size is as small as possible such
  !< that its physical representation is not shorter than that of `SOURCE`.
  !<
  !< Presently, the following combinations are available:
  !<
  !<* [X] Arrays-Arrays;
  !<* [ ] Scalars-Scalars;
  !<
  !<### Examples of usage
  !<
  !<#### Packing two real arrays, one with kind R8P and one with R4P
  !<```ortran
  !<real(R8P)::                 array_r8(1:12)
  !<real(R4P)::                 array_r4(-1:5)
  !<integer(I1P), allocatable:: rpack
  !<...
  !<call pack_data(a1=array_r8,a2=array_r4,packed=rpack)
  !<```
  !<#### Packing two arrays, one real with kind R4P and one integer with I4P
  !<```ortran
  !<real(R4P)::                 array_r4(2)
  !<integer(I4P)::              array_i4(0:2)
  !<integer(I1P), allocatable:: rpack
  !<...
  !<call pack_data(a1=array_r4,a2=array_i4,packed=rpack)
  !<```
  module procedure pack_data_up_up
endinterface

contains

   pure subroutine pack_data_up_up(a1, a2, packed)
   !< Pack different kinds of data into single I1P array.
   !<
   !<```fortran
   !< use befor64
   !< use penf
   !< integer(I8P)              :: a1(1)
   !< real(R8P)                 :: a2(1)
   !< integer(I1P), allocatable :: pack(:)
   !< a1(1) = 0
   !< a2(1) = 1
   !< call pack_data(a1=a1, a2=a2, packed=pack)
   !< print *, pack(size(pack, dim=1))
   !<```
   !=> 63 <<<
   class(*),                  intent(in)    :: a1(1:)    !< First data stream.
   class(*),                  intent(in)    :: a2(1:)    !< Second data stream.
   integer(I1P), allocatable, intent(inout) :: packed(:) !< Packed data into I1P array.
   integer(I1P), allocatable                :: p1(:)     !< Temporary packed data of first stream.
   integer(I1P), allocatable                :: p2(:)     !< Temporary packed data of second stream.
   integer :: s1, s2
   s1 = sizeof(a1(1)) * size(a1)
   s2 = sizeof(a2(1)) * size(a2)
   if (allocated(packed)) deallocate(packed)
   allocate( packed(s1+s2) )
   packed = [transfer(source=a1, mold=packed), transfer(source=a2, mold=packed)]
   endsubroutine pack_data_up_up

endmodule befor64_pack_data_m
