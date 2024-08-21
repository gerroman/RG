Needs["RG`Tools`"];
Needs["RG`IOUtils`"];

log["setting default graph options ..."];

Block[{
		(* [pt] = (1/72) [in] = 0.0352 [cm] *)
		aspectRatio = 3/4,
		resolution = 72,							(* [1/in] *)
		imageSize = {6.75, 5.},	(* {6.75, 5.} [in] = {17.145, 12.7} [cm] = {486, 360} [pt] @ resolution = 72 [1/in] *)
		fontsize = 14,	 (* [pt], 14 [pt] = 0.194 [in] = 0.493 [cm] *)
		lineWidth = 1.,	 (* [pt] *)
		fontFamily = "Times"
	},
	SetOptions[Plot, {
	 		AspectRatio -> aspectRatio,
			ImageSize -> resolution * imageSize,
			BaseStyle -> {FontFamily->fontFamily, FontSize->fontsize},
			Axes -> False,
			Frame -> True,
			FrameStyle -> Directive[Black, AbsoluteThickness[1.]],
			FrameTicksStyle -> Directive[Black, AbsoluteThickness[1.]],
			LabelStyle-> {FontFamily->fontFamily, FontSize->fontsize}
		}
	]
];

RG`IOUtils`makeLegend::usage = "makeLegend[expr, {Right, Top}, opts] \[LongDash] create a line legend from expr to use as a value PlotLegends";
RG`IOUtils`makeLegend[expr_, pos_:{Right, Top}, opts:OptionsPattern[{LegendFunction}]] := (
	Placed[LineLegend[expr, opts, LegendFunction -> (Framed[#,FrameMargins->0,FrameStyle->None]&)], pos]
);

info[RG`IOUtils`makeLegend];
