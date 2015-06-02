abstract AbstractPoint

type Point <: AbstractPoint
    x :: Float64
    y :: Float64
end

type Sample <: AbstractPoint
    x :: Float64
    y :: Float64
    centroid :: Point
end

dist(a::AbstractPoint, b::AbstractPoint) = (a.x - b.x)^2 + (a.y - b.y)^2
dist(a) = b -> dist(a, b)

mean(points::Array{AbstractPoint}) = Point(mean(map(p -> p.x, points)),
                                           mean(map(p -> p.y, points)))

function kmeans(k, samples)
    centroids = initialize_centroids(k, samples)
    while (reassign_samples(samples, centroids))
        move_centroids(samples, centroids)
    end
    map(c -> (c, assigned_samples(c, samples)), centroids)
end

function initialize_centroids(k, samples)
    centroids = [samples.first]
end

function assigned_samples(centroid, samples)
    map(s -> s.centroid = c, samples)
end

function set!(centroid::Point, copy_from::Point)
    centroid.x = copy_from.x
    centroid.y = copy_from.y
    centroid
end

function reassign_samples(samples, centroids)
    any(map(s -> reassign(s, nearest_centroid(s, centroids)), samples))
end

function move_centroids(samples, centroids)
    for c in centroids
        set!(c, mean(assigned_samples(c, samples)))
    end
end

function reassign(sample, centroid)
    if s.centroid == centroid
        false
    else
        s.centroid = centroid
        true
    end
end

function nearest_centroid(sample, centroids)
    min(dist(sample), centroids)
end


# util

repeat(n, thunk) = [thunk() for i=1:n]

function min(p::Function, xs)
    min_idx = map(p, xs) |> indmin
    xs[min_idx]
end

min(p::Function) = xs -> min(p, xs)

function max(p::Function, xs)
    max_idx = map(p, xs) |> indmax
    xs[max_idx]
end

max(p::Function) = xs -> max(p, xs)
