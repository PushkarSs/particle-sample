module methods

    implicit none

    contains

        !**********************************************************
            !RAND_CLUS_GEN has its use to cosntruct an array which details the coordinates of randomly placed clusters and their randomly sized radii 
            !INPUTS:
                !bounds_range :- defines square bounds centered at origin of the position of cluster center of mass
                !radius :-
                !n :- describes how many clusters you would like returned in the array
            !OUTPUTS:
                !array with "n" number of random elements 

        function rand_clus_gen(bounds_range, radius, n)
            implicit none
            real*8, intent(in) :: bounds_range
            real*8, intent(in) :: radius
            integer, intent(in) :: n
            real*8 :: rand_clus_gen(1:n, 1:3)

            real*8 :: temp(1:n, 1:3) !variable to store random number array we are currently working with
            real*8 :: range
            range = 2*bounds_range !space between lower and upper bound            
            
            call random_number(rand_clus_gen) !temp now filled with random numbers >=0 & <1

            rand_clus_gen(1:n,1:2) = range*rand_clus_gen(1:n,1:2) !x,y now has the approriate scale
            rand_clus_gen(1:n,1:2) = rand_clus_gen(1:n,1:2) - bounds_range !x,y now has the appropriate scale and starting point
            rand_clus_gen(1:n,3) = radius*rand_clus_gen(1:n,3) !radius index now has appropriate scale

        end function

        
        
        !**********************************************************
            !WITHINLATTICE checks if every point within our bounds falls within at least one of the cluster, then outputs those to a file
            !INPUTS:
                !clus_info :- array describes x,y position of cluster CoM, radius
                !space :- inter-particle seperation distance
                !bounds :- defines square region under observation
            !OUTPUTS:
                !text file with coordinates of particles
        subroutine withinLattice(clus_info, space, bounds) 
            real*8, intent(in) :: clus_info(:,:)
            real*8, intent(in) :: space
            real*8, intent(in) :: bounds

            real*8 :: x, y, i ! loop variables
            real*8 :: radius_eqn ! used to store x2 + y2 = r2
            
            open(unit=100,file='output.txt')
            do x = -bounds, bounds, space
                do y = -bounds, bounds, space

                    do i = 1, size(clus_info, 1)

                        radius_eqn = ((x-clus_info(i, 1))*(x-clus_info(i, 1))) &
                             + ((y-clus_info(i,2))*(y-clus_info(i,2))) &
                             - (clus_info(i,3)*clus_info(i,3))

                        if (radius_eqn <= 0) then
                            write(100, '(2f20.16)') x, y
                        end if
                    end do
                    
                end do
            end do
            
        end subroutine
                
end module