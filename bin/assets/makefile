all : texture-albedo.png map_0.png

texture-albedo.png : texture.psd
	del texture-*.*
	magick -dispose background texture.psd -layers coalesce texture-layer%d.png
	del texture-layer0.png
	rename texture-layer1.png texture-albedo.png
	rename texture-layer2.png texture-ao.png
	rename texture-layer3.png texture-emission.png
	rename texture-layer4.png texture-height.png
	rename texture-layer5.png texture-roughness.png
	rename texture-layer6.png texture-metallic.png
	rename texture-layer7.png texture-normal.png
	
map_0.png : map.psd
	del map_*.*
	magick -dispose background map.psd -layers coalesce map_%d.png
	del map_0.png