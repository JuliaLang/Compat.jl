using Compat
using Test

@test isempty(detect_ambiguities(Base, Core, Compat))

@testset "range with positional stop" begin
    @test range(0, 5, length=6) == 0.0:1.0:5.0
    @test range(0, 10, step=2) == 0:2:10
end

mutable struct TLayout
    x::Int8
    y::Int16
    z::Int32
end
@testset "hasfield and hasproperty" begin
    tlayout = TLayout(5,7,11)
    @test hasfield(TLayout, :y)
    @test !hasfield(TLayout, :a)
    @test hasproperty(tlayout, :x)
    @test !hasproperty(tlayout, :p)
end
