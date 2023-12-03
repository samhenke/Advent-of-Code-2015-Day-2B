(*
 * Copyright © 2023 Sam Henke
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the “Software”), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 *)

open Base

(*
let wrapping_paper_required l w h =
    let s1, s2, s3 = w*h, l*h, l*w in
    2 * (s1 + s2 + s3) + Int.min s1 (Int.min s2 s3)
*)

let ribbon_required l w h =
    let max_dim = Int.max l (Int.max w h) in
    2*(l + w + h - max_dim) + l * w * h

let () =
    let f line = Stdlib.Scanf.sscanf line "%ux%ux%u" ribbon_required in
    In_channel.fold_lines (fun acc line -> acc + (f line)) 0 In_channel.stdin
    |> Stdlib.print_int
    |> Stdlib.print_newline
