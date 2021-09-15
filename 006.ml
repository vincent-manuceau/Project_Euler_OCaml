let sum_of_squares n =  n * (2*n +1) * (n+1) / 6 
and square_of_sum n = n*n*(n+1)*(n+1)/4 in
(square_of_sum 100) - (sum_of_squares 100) ;;
