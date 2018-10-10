---
title: 'display_image/[1,3]'
predicates:
 - 'create_image/ [2, 3]' : create an image from a GIF file
 - 'display_image/ [1, 3]' : display an image
---
`create_image/ [2, 3]` — create an image from a GIF file

`display_image/ [1, 3]` — display an image


## FORMS

create_image(ImagePath, ImageName)

create_image(Interp, ImagePath, ImageName)

display_image(ImageName)

display_image(Interp, ImageName, Options)


## DESCRIPTION

These routines provide simple access from ALS Prolog to the image routines of Tk. The current versions support GIF images, but the routines can be extended to any of the types Tk supports. To display images, one must specify a path to the image file, and must first produce an internal Tk form of the image. This is done with :

create_image(ImagePath, ImageName)

:-

create_image(tcli, ImagePath, ImageName) .

create_image(Interp, ImagePath, ImageName)

Assume that

pow_wow_dance.gif

is a file in the current directory. Then the call

? - create_image(' pow_wow_dance.gif ', pow_wow) .

will create the internal form of this image and associate the name pow_wow with it. Display of images which have been created is accomplished with :

display_image(ImageName)

:-

display_image(tcli, ImageName, [ ]) .

display_image(Interp, ImageName, Options)

Thus, the call

? -display_image(pow_wow) .

produces

![](images/pow_wow_dancer.gif)


