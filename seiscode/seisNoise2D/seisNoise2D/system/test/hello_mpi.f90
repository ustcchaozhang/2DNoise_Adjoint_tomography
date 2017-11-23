program hello_mpi
    ! hello.f90
    ! yanhuay@princeton.edu

#ifdef USE_MPI
    use mpi
#endif

    INTEGER :: nproc,myrank,ier

#ifdef USE_MPI
    call MPI_INIT(ier)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ier)
    call MPI_COMM_RANK(MPI_COMM_WORLD,myrank,ier)
#else
    nproc = 1
    myrank = 0
#endif

print*, "Hello_mpi from process ", myrank," of ", nproc-1

#ifdef USE_MPI
    ! stop all the processes and exit
    call MPI_FINALIZE(ier)
#endif

end program hello_mpi

