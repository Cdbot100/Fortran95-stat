!Scott Gordon 
!CMPS 5113, Advanced Programming language Concepts
!Written in Fortran 95. 
!Compiled with MingW (gcc fortran compiler for windows)
!This program reads in an size and an array of numbers from the user via 
!the commmand line The user then has the option of printing the array, or to calculate 
!Elementary Statistics. ie: Mean, Variance, Standard Deviation, and / or print them. 

program ScottGordonA1
	implicit none
	real, dimension(:,:),allocatable :: matrix1, matrix2
	real:: myMean , myVariance, myStDev
	integer:: columns, rows, what, whichcol , whichset, matsize
	write(*,*) 'This program reads in an size and an array of numbers' 
	write(*,*) 'The user then has the option of printing the array'
	write(*,*) 'or to calculate Elementary Statistics.'
	write(*,*) 'Please, be careful to enter only reals or intergers!'
	write(*,*) 'Array columns??:'
	read(*,*) columns					!read in array size. 
	write(*,*) 'Array rows??:'
	read(*,*) rows	
	call populateMatrix 				!Call populateMatrix
	
	write(*,*) 'Now what? (An incorrect selection will cause the program to exit)'
	write(*,*) 'Extract = 1, Show = 2, Compute = 3, or Quit = 4:'
	!Give user 3 options, Extract Column, Show Matrix, Compute Stats or Quit
	read(*,*) what
	do while (what /= 4)
		if (what == 1)	then			!if Extract Column
			write(*,*) 'which column?:'
			read(*,*) whichcol			!read in column to extract
			call extractColumn			!call extract column(column)
		else if (what == 2)	then		!else if Show matrix
			call showMeTheMatrix		!call show matrix
		else if (what == 3)	then		!else if compute stats
			call computeStats			!call compute stats
			write(*,*) 'The sample size is:', matsize
			write(*,*) 'The mean is:', myMean  !Print stats to screen
			write(*,*) 'The variance is:', myVariance 
			write(*,*) 'The standard deviation is:', myStDev 
		else
			write(*,*) 'Incorrect Selection'
		end if
		write(*,*) 'Now what? Extract = 1, Show = 2, Compute = 3, or Quit = 4:'
		!Give user 3 options, Extract Column, Show Matrix, Compute Stats or Quit
		read(*,*) what
	end do		
	write(*,*) 'Bye, thanks!'			!end of main
		contains
			subroutine populateMatrix
			!populateMatrix = should initialize a square matrix. 
			!The size of this matrix (full of integer numbers) decided by user.	
				integer:: i  ,  j
				write(*,*) 'Array Values?'
				allocate(matrix1(columns,rows))
				allocate(matrix2(columns,rows))
				do i=1,(columns)
					do j=1,(rows)
						read(*,*) matrix1(i,j)     !read in from cmd line
						matrix2(i,j) = 0           !populate second matrix, (for extraction)
					end do	
				end do
			end subroutine populateMatrix
		
			subroutine extractColumn
			!extractColumn = the user should be able to choose any column. 
			!This function should extract the chosen column and print.
			integer:: i 
				do i=1,(rows)
					write(*,*) matrix1(i,(whichcol))
					matrix2(i,whichcol) = matrix1(i,whichcol) !add values to extracted matrix
				end do
			end subroutine extractColumn
		
			subroutine showMeTheMatrix
			!showMeTheMatrix = (subroutine) should print on screen in a nice, 
			!clean and readable format the matrix created by “populateMatrix”
			integer:: i , j
				do i=1,(columns)
							print *, (matrix1(i,j), j=1,rows)   
				end do
			end subroutine showMeTheMatrix
		
			subroutine computeStats 
			!computeStats = should call three other functions 
			!{myMeanFx, myVarFx, and myStDev} 
			!to compute {the mean, the variance, and the standard deviation} 
			!of the extracted column.
			!In addition, this function should return three values to  
			!variables in “main” {myMean, myVariance, myStDev}
			write(*,*) 'Which set? Whole matrix = 1 Extracted Column = 2:'!ask if whole or just a column
			write(*,*) '(if no column has been extracted, 2 will return garbage values)'
			read(*,*) whichset
				if (whichset == 1) then	 		!whole matrix selected
					matsize = columns * rows
					call myMeanFx		!call myMeanFx
					call myVarFx		!call myVarFx
					call myStdDev		!call myStdDev
				else if (whichset == 2) then 	!extracted matrix selected
					matsize = rows
					call myMeanFx		!call myMeanFx
					call myVarFx		!call myVarFx
					call myStdDev		!call myStdDev
				else
					write(*,*) 'Incorrect Selection, printing garbage values'
				end if
			end subroutine computeStats
		
			subroutine myMeanFx
			!myMeanFx = should compute the mean of the extracted column
			real:: counter  
			integer:: i , j 
			if (whichset == 1) then			!whole set
				do i=1,(columns)
						do j=1,(rows)
							counter= counter + matrix1(i,j)     
						end do	
					end do
			else if (whichset == 2) then !extracted set
				do i=1,(columns)
						do j=1,(rows)
							counter= counter + matrix2(i,j)     
						end do	
				end do
			end if
			myMean = counter/matsize
			end subroutine myMeanFx
			
			subroutine myVarFx
			!myVarFx = should compute the variance, for either extracted set or full set.
			integer:: i , j 
				if (whichset == 1) then				!full set
					do i=1, (columns)
						do j=1,(rows)
							myVariance = myVariance + ( ((matrix1(i,j)-myMean)**2) / (matsize) )
						end do
					end do
				else if (whichset == 2) then		!extracted set
					do i=1, (columns)
						do j=1,(rows)
							if (matrix2(i,j) /= 0) then
								myVariance = myVariance + ( ((matrix2(i,j)-myMean)**2) / (matsize) )
							end if
						end do
					end do
				end if
			end subroutine myVarFx
				
			subroutine myStdDev
				!myStDev = should compute the standard deviation.
				myStDev = sqrt(myVariance)
			end subroutine myStdDev

end program ScottGordonA1
