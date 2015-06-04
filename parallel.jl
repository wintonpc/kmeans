rs = rand(0:10, 1000)

function find_primes(max=200000)
    @parallel (vcat) for i=1:max
        isprime(i) ? [i] : []
    end
end
