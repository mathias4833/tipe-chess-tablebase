open Endgame
open Utils
open Bimage
open Bimage_unix

(** [save_to_image table pieces height width] génère une image représentant les positions gagnantes dans un tableau de fin de partie.
    @param table Tableau de fin de partie.
    @param pieces Liste des pièces présentes sur le plateau.
    @param height Hauteur de l'image.
    @param width Largeur de l'image. *)
let save_to_image table pieces height width =
  let counter = ref 0 in
  let img = Image.v u8 rgb width height in
  for i = 0 to width - 1 do
    for j = 0 to height - 1 do
      let n = (j * width) + i in
      let board = Serializer.number_to_board n pieces in
      if (not (Transformations.is_normalized board pieces)) || table.{n} <> 0
      then (
        Image.set_pixel img i j (Pixel.v rgb [ 0.0; 0.0; 1.0 ]);
        counter := !counter + 1)
      else Image.set_pixel img i j (Pixel.v rgb [ 1.0; 0.0; 0.0 ])
    done
  done;
  Printf.printf "Positions gagnantes: %i, Densite: %f\n" !counter
    (float_of_int !counter /. float_of_int (height * width));
  Stb.write_png "out.png" img
