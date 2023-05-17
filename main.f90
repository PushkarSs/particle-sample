program main
    use methods
    implicit none


    !number of clusters we desire
    integer,parameter :: n_clusters = 5
    !dummy array to store values
    real*8 :: dummy_array(1:n_clusters,1:3)
    !loop variable
    integer i

    !bounds of square graph
    real*8 :: bounds = 1d0

    !Declare inter-particle seperation
    real*8, parameter :: spac = 1d-2



   dummy_array =  rand_clus_gen(bounds_range=3d0, radius=1d0, n=5)
   do i = 1, n_clusters
    write(*,'(f10.7,a,f10.7,a,f10.7)') dummy_array(i,1),",",dummy_array(i,2),":",dummy_array(i,3)
   end do

   call withinLattice(dummy_array, spac, 3d0)

end program 