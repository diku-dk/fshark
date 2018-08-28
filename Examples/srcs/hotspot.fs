module Hotspot
open FSharkPrelude
// Code and comments based on
// https://github.com/kkushagra/rodinia/blob/master/openmp/hotspot/hotspot_openmp.cpp
//
// ==
// tags { futhark-c futhark-opencl }
// compiled input @ data/64.in
// output @ data/64.out
//
// input @ data/512.in
// output @ data/512.out
//
// input @ data/1024.in
// output @ data/1024.out

// Maximum power density possible (say 300W for a 10mm x 10mm chip)
let max_pd: float32 = 3.0e6f

// Required precision in degrees
let precision : float32 = 0.001f

let spec_heat_si : float32 = 1.75e6f

let k_si : float32 = 100.0f

// Capacitance fitting factor
let factor_chip : float32 = 0.5f

// Chip parameters
let t_chip : float32 = 0.0005f
let chip_height : float32 = 0.016f
let chip_width : float32 = 0.016f

// Ambient temperature assuming no package at all
let amb_temp : float32 = 80.0f

// Single iteration of the transient solver in the grid model.
// advances the solution of the discretized difference equations by
// one time step
let single_iteration (temp: float32 [][]) (power: float32 [][]) (cap: float32)
                     (rx: float32) ( ry: float32) ( rz: float32) (step: float32)
                     : float32 [][] =
  let row = Length temp
  let col = Length temp.[0]
  Map  (fun (r: int) ->
         Map (fun (c: int) ->
               let temp_el = temp.[r].[c]
               let delta =
                 (step / cap) *
                   (power.[r].[c] +
                      (if r = 0 && c = 0 then // Corner 1
                         (temp.[r].[c+1] - temp_el) / rx +
                         (temp.[r+1].[c] - temp_el) / ry
                       else if r = 0 && c = col-1 then // Corner 2
                         (temp.[r].[c-1] - temp_el) / rx +
                         (temp.[r+1].[c] - temp_el) / ry
                       else if r = row-1 && c = col-1 then // Corner 3
                         (temp.[r].[c-1] - temp_el) / rx +
                         (temp.[r-1].[c] - temp_el) / ry
                       else if r = row-1 && c = 0 then // Corner 4
                         (temp.[r].[c+1] - temp_el) / rx +
                         (temp.[r-1].[c] - temp_el) / ry
                       else if r = 0 then // Edge 1
                         (temp.[r].[c+1] + temp.[r].[c-1] - 2.0f*temp_el) / rx +
                         (temp.[r+1].[c] - temp_el) / ry
                       else if c = col-1 then // Edge 2
                         (temp.[r].[c-1] - temp_el) / rx +
                         (temp.[r+1].[c] + temp.[r-1].[c] - 2.0f*temp_el) / ry
                       else if r = row-1 then // Edge 3
                         (temp.[r].[c+1] + temp.[r].[c-1] - 2.0f*temp_el) / rx +
                         (temp.[r-1].[c] - temp_el) / ry
                       else if c = 0 then // Edge 4
                         (temp.[r].[c+1] - temp_el) / rx +
                         (temp.[r+1].[c] + temp.[r-1].[c] - 2.0f*temp_el) / ry
                       else
                         (temp.[r].[c+1] + temp.[r].[c-1] - 2.0f * temp_el) / rx +
                         (temp.[r+1].[c] + temp.[r-1].[c] - 2.0f * temp_el) / ry) +
                      (amb_temp - temp_el) / rz)
                   in temp_el + delta
            ) (Iota(col))) (
         Iota(row))

// Transient solver driver routine: simply converts the heat transfer
// differential equations to difference equations and solves the
// difference equations by iterating.
//
// Returns a new 'temp' array.
let compute_tran_temp (num_iterations: int) (temp: float32 [][]) (power : float32[][]): float32 [][] =
  let row = Length temp
  let col = Length temp.[0]
  let grid_height = chip_height / float32(row)
  let grid_width = chip_width / float32(col)
  let cap = factor_chip * spec_heat_si * t_chip * grid_width * grid_height
  let rx = grid_width / (2.0f * k_si * t_chip * grid_height)
  let ry = grid_height / (2.0f * k_si * t_chip * grid_width)
  let rz = t_chip / (k_si * grid_height * grid_width)
  let max_slope = max_pd / (factor_chip * t_chip * spec_heat_si)
  let step = precision / max_slope
  in Foldr (fun _ temp' -> single_iteration temp' power cap rx ry rz step) temp (Iota num_iterations)

[<FSharkEntry>]
let main (num_iterations : int) (temp : float32[][]) (power: float32 [][]): float32 [][] = compute_tran_temp num_iterations temp power
