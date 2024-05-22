open Bimage
open Bimage_unix

let save_to_image table height width =
  let img = Image.v u8 rgb width height in
  for i = 0 to width - 1 do
    for j = 0 to height - 1 do
      let n = (j * width) + i in
      if table.{n} <> 0 then
        Image.set_pixel img i j (Pixel.v rgb [ 0.0; 0.0; 1.0 ])
      else Image.set_pixel img i j (Pixel.v rgb [ 1.0; 0.0; 0.0 ])
    done
  done;
  Stb.write_png "out.png" img
