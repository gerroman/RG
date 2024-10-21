$resolution = 72; (* dpi *)
$fontsize = 14;		(* default font size *)

Block[{
		(* [pt] = (1/72) [in] = 0.0352 [cm] *)
		aspectRatio = 3/4,
		resolution = $resolution,							(* [1/in] *)
		plotSize = {6.75, 5.},	(* {6.75, 5.} [in] = {17.145, 12.7} [cm] = {486, 360} [pt] @ resolution = 72 [1/in] *)
		graphicsSize = {1., 1.},
		fontsize = $fontsize,	 (* [pt], 14 [pt] = 0.194 [in] = 0.493 [cm] *)
		lineWidth = 1.,	 (* [pt] *)
		fontFamily = "Times"
	},
	SetOptions[Plot, {
	 		AspectRatio -> aspectRatio,
			ImageSize -> resolution * plotSize,
			BaseStyle -> {FontFamily->fontFamily, FontSize->fontsize},
			Axes -> False,
			Frame -> True,
			FrameStyle -> Directive[Black, AbsoluteThickness[1.]],
			FrameTicksStyle -> Directive[Black, AbsoluteThickness[1.]],
			LabelStyle-> {FontFamily->fontFamily, FontSize->fontsize}
		}
	];
	SetOptions[Graphics, {
	 		AspectRatio -> 1,
			ImageSize -> resolution * graphicsSize,
      BaseStyle -> {FontFamily->fontFamily, FontSize->fontsize},
			Frame->False,
			LabelStyle-> {FontFamily->fontFamily, FontSize->fontsize}
    }
  ];
];


Print[ToString@StringForm["[info]: '``' loaded", $InputFileName]];
