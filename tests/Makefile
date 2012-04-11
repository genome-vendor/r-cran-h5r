
clean : 
	rm -f *.Rout *.h5

all :
	R CMD BATCH --no-save testRead.R &
	R CMD BATCH --no-save testReadPoints.R &
	R CMD BATCH --no-save testWrite.R &
	R CMD BATCH --no-save testErrors.R & 
	R CMD BATCH --no-save testGetGroup.R &

valgrind:
	R -d "valgrind --tool=memcheck --leak-check=full" --vanilla < testGetGroup.R


