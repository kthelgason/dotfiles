BEGIN {
    sum = 0; nr = 0; 
    avg = ARGV[1]; ARGC--;
}
{
{nr += 1; sum += ($1 - avg)**2}
} 

END {
    print sqrt(sum/(nr - 1))
}
