using Gadfly
using DataFrames
using Color, Distributions

import Base: mean, repeat, min, max, string, copy!

# types

abstract AbstractPoint

type Point <: AbstractPoint
    x :: Float64
    y :: Float64
end

type Sample <: AbstractPoint
    x :: Float64
    y :: Float64
    centroid :: Union(Point, Nothing)
    changed :: Bool
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
Point(coord::Array) = Point(coord[1], coord[2])
Sample(p::Point) = Sample(p.x, p.y, nothing, true)
==(a::Point, b::Point) = a.x == b.x && a.y == b.y
!=(a::Point, b::Point) = !(a == b)

typealias AbstractPoints{T<:AbstractPoint} Array{T, 1}

dist(a::AbstractPoint, b::AbstractPoint) = (a.x - b.x)^2 + (a.y - b.y)^2
dist(a) = b -> dist(a, b)

xs(ps::AbstractPoints) = map(p -> p.x, ps)
ys(ps::AbstractPoints) = map(p -> p.y, ps)

mean(points::AbstractPoints) = begin
    Point(mean(xs(points)), mean(ys(points)))
end

function kmeans(k, points)
    seeds, samples, centroids = initialize_centroids(k, points)
    while true
        samples = reassign_samples(samples, centroids)
        if (!reassigned_any(samples)) break; end
        centroids = move_centroids(samples, centroids)
    end
    make_result(seeds, samples, centroids)
end

function initialize_centroids(k, points)
    seeds = [first(points)]
    for i=2:k
        dist_table = Float64[dist(seeds[s], points[p]) for s=1:length(seeds), p=1:length(points)]
        push!(seeds, points[indmax(map(min, cols(dist_table)))])
    end
    centroids = deepcopy(seeds)
    samples = map(Sample, points)
    seeds, samples, centroids
end

function reassign_samples(samples, centroids)
    map(s -> reassign(s, nearest_centroid(s, centroids)), samples)
end

function nearest_centroid(point, centroids)
    min(dist(point), centroids)
end

function reassign(sample, centroid)
    Sample(sample.x, sample.y, centroid, sample.centroid != centroid)
end

function move_centroids(samples, centroids)
    map(c -> mean(assigned_samples(c, samples)), centroids)
end

function reassigned_any(samples)
    any(map(s -> s.changed, samples))
end

function assigned_samples(centroid, samples)
    filter(s -> s.centroid == centroid, samples)
end

function make_result(seeds, samples, centroids)
    clusters = map(centroids) do c
        Cluster(c, map(Point, assigned_samples(c, samples)))
    end
    KmeansResult(seeds, clusters)
end
                 
# plotting

string(p::AbstractPoint) = @sprintf("(%f, %f)", p.x, p.y)

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
    plot(centroid_layer, transition_layers..., sample_layer, seed_layer, Coord.cartesian(fixed=true))
end

function make_transition_layers(result)
    ts = zip(result.seeds, map(c -> c.centroid, result.clusters))
    dfs = map(t -> frame([first(t), last(t)], "Transition"), ts)
    map(df -> layer(df, x="X", y="Y", Geom.line, Theme(default_color=color("black"))), dfs)
end

# testing

function cartesian(theta, radius)
    x = radius * cos(theta)
    y = radius * sin(theta)
    [x, y]
end

function gen_points(k, n, max_radius = 0.1)
    normal_distribution = Normal(0, max_radius)
    theta_distribution = Uniform(0, 2pi)
    vcat((repeat(k) do
        center = Point(rand(), rand())
        repeat(n) do
            theta = rand(theta_distribution)
            radius = rand(normal_distribution)
            Point(cartesian(theta, radius) + [center.x, center.y])
        end
    end)...)
end

function run(k=3)
    plot_kmeans(kmeans(k, gen_points(k, 50)))
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

cols(mat) = [mat[:,i] for i=1:size(mat,2)]

min(arr::Array{Float64,1}) = min(arr...)
min(n::Float64) = n
max(arr::Array{Float64,1}) = max(arr...)
max(n::Float64) = n


# extra

function initialize_centroids_randomly(k, points)
    seeds = points[1:k]
    centroids = deepcopy(seeds)
    samples = map(Sample, points)
    seeds, samples, centroids
end

:kmeans
