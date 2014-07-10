BEGIN {
    sum = 0; nr = 0;
} 
{
    nr += 1; sum += $1
} 
END {
    print sum/nr
}
