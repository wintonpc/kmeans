using Gadfly
using DataFrames
using Color

import Base: mean, repeat, min, max, string

# types

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

type Cluster
    centroid
    points
end

type KmeansResult
    seeds
    clusters
end


# algorithm

Point(s::Sample) = Point(s.x, s.y)

string(p::AbstractPoint) = @sprintf("(%f, %f)", p.x, p.y)

typealias AbstractPoints{T<:AbstractPoint} Array{T, 1}

dist(a::AbstractPoint, b::AbstractPoint) = (a.x - b.x)^2 + (a.y - b.y)^2
dist(a) = b -> dist(a, b)

xs(ps::AbstractPoints) = map(p -> p.x, ps)
ys(ps::AbstractPoints) = map(p -> p.y, ps)

mean(points::AbstractPoints) = Point(mean(xs(points)), mean(ys(points)))

function kmeans(k, points)
    seeds, samples, centroids = initialize_centroids(k, points)
    while (reassign_samples(samples, centroids))
        move_centroids(samples, centroids)
    end
    make_result(seeds, samples, centroids)
end

function initialize_centroids(k, points)
    seeds = points[1:k]
    centroids = deepcopy(seeds)
    samples = map(p -> Sample(p.x, p.y, nearest_centroid(p, centroids)), points)
    move_centroids(samples, centroids)
    seeds, samples, centroids
end

function reassign_samples(samples, centroids)
    any(map(s -> reassign(s, nearest_centroid(s, centroids)), samples))
end

function nearest_centroid(point, centroids)
    min(dist(point), centroids)
end

function reassign(sample, centroid)
    if sample.centroid == centroid
        false
    else
        sample.centroid = centroid
        true
    end
end

function move_centroids(samples, centroids)
    for c in centroids
        set!(c, mean(assigned_samples(c, samples)))
    end
end

function assigned_samples(centroid, samples)
    filter(s -> s.centroid == centroid, samples)
end

function set!(centroid::Point, copy_from::Point)
    centroid.x = copy_from.x
    centroid.y = copy_from.y
    centroid
end

function make_result(seeds, samples, centroids)
    clusters = map(centroids) do c
        Cluster(c, map(Point, assigned_samples(c, samples)))
    end
    KmeansResult(seeds, clusters)
end
                 
# plotting

frame(points, cluster_name) = DataFrame(X=xs(points), Y=ys(points), Cluster=cluster_name)

function plot_kmeans(result)
    seed_data = frame(result.seeds, "Seed")
    centroid_data = frame(map(cluster -> cluster.centroid, result.clusters), "Center")
    sample_data = apply(vcat, map(result.clusters) do cluster
        frame(cluster.points, string(cluster.centroid))
    end)
    centroid_color = color(RGB(0.3, 0.3, 0.3))
    transition_layers = make_transition_layers(result)
    seed_layer = layer(seed_data, x="X", y="Y", Geom.point,
                       Theme(default_color=centroid_color, default_point_size=3pt))
    centroid_layer = layer(centroid_data, x="X", y="Y", Geom.point,
                           Theme(default_color=centroid_color, default_point_size=5pt))
    sample_layer = layer(sample_data, x="X", y="Y", color="Cluster", Geom.point)
    plot(centroid_layer, seed_layer, transition_layers..., sample_layer)
end

function make_transition_layers(result)
    ts = zip(result.seeds, map(c -> c.centroid, result.clusters))
    dfs = map(t -> frame([first(t), last(t)], "Transition"), ts)
    map(df -> layer(df, x="X", y="Y", Geom.line, Theme(default_color=color("black"))), dfs)
end

        

# util

function repeat(thunk::Function, n)
    arr = [thunk() for i=1:n]
    convert(Array{typeof(first(arr)),1}, arr)
end

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

:kmeans
