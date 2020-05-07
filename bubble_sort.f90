program BubbleSort
    implicit none
    integer, dimension(:), allocatable :: ar
    integer :: length
    integer :: i,j,t
    read (*,*) length

    ! memory allocation dynamically
    allocate (ar(length))

    p0: do i=1,length,1
        read(*,*) ar(i)
    end do p0


    p1: do  i=1,length,1
        p2: do  j=1,length-1,1
            swap: if (ar(j) > ar(j+1)) then
                t=ar(j)
                ar(j)=ar(j+1)
                ar(j+1)=t
            end if swap
        end do p2
    end do p1

    p3: do i=1,length,1
        write(*,*) ar(i)
    end do p3


end program BubbleSort